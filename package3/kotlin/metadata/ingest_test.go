package metadata

import (
	"archive/zip"
	"bytes"
	"testing"
)

// buildJARBytes creates an in-memory JAR (ZIP) from a map of filename → content.
func buildJARBytes(entries map[string][]byte) []byte {
	var buf bytes.Buffer
	zw := zip.NewWriter(&buf)
	for name, data := range entries {
		w, err := zw.Create(name)
		if err != nil {
			panic(err)
		}
		if _, err := w.Write(data); err != nil {
			panic(err)
		}
	}
	if err := zw.Close(); err != nil {
		panic(err)
	}
	return buf.Bytes()
}

// buildSimpleClassProto builds a trivial ClassProto for testing.
func buildSimpleClassProto(fqNameIdx int, d2 []string) []byte {
	var pb protoBuilder
	classFlags := int32(3 << 3) // public class
	pb.varint(1, uint64(classFlags))
	pb.varint(3, uint64(fqNameIdx))
	return pb.build()
}

func TestIngestJARBytes_TwoWithMetadataOneWithout(t *testing.T) {
	// Class 1: kotlin class Foo
	d2a := []string{"com/example/Foo"}
	classAProto := buildSimpleClassProto(0, d2a)
	classA := buildClassWithMetadata(1, [3]int32{1, 9, 0}, classAProto, d2a)

	// Class 2: kotlin class Bar
	d2b := []string{"com/example/Bar"}
	classBProto := buildSimpleClassProto(0, d2b)
	classB := buildClassWithMetadata(1, [3]int32{1, 9, 0}, classBProto, d2b)

	// Class 3: plain Java class, no kotlin.Metadata
	classC := buildMinimalClass()

	jar := buildJARBytes(map[string][]byte{
		"com/example/Foo.class": classA,
		"com/example/Bar.class": classB,
		"com/example/Baz.class": classC,
	})

	objects, err := IngestJARBytes(jar)
	if err != nil {
		t.Fatalf("IngestJARBytes error: %v", err)
	}
	if len(objects) != 2 {
		t.Errorf("expected 2 API objects, got %d", len(objects))
	}
}

func TestIngestJARBytes_Empty(t *testing.T) {
	jar := buildJARBytes(map[string][]byte{})
	objects, err := IngestJARBytes(jar)
	if err != nil {
		t.Fatalf("IngestJARBytes error: %v", err)
	}
	if len(objects) != 0 {
		t.Errorf("expected 0 objects for empty JAR, got %d", len(objects))
	}
}

func TestIngestJARBytes_NestedClassSkipped(t *testing.T) {
	// Outer class
	d2outer := []string{"com/example/Outer"}
	outerProto := buildSimpleClassProto(0, d2outer)
	outerClass := buildClassWithMetadata(1, [3]int32{1, 9, 0}, outerProto, d2outer)

	// Nested class (filename contains '$')
	d2inner := []string{"com/example/Outer.Inner"}
	innerProto := buildSimpleClassProto(0, d2inner)
	innerClass := buildClassWithMetadata(1, [3]int32{1, 9, 0}, innerProto, d2inner)

	jar := buildJARBytes(map[string][]byte{
		"com/example/Outer.class":       outerClass,
		"com/example/Outer$Inner.class": innerClass,
	})

	objects, err := IngestJARBytes(jar)
	if err != nil {
		t.Fatalf("IngestJARBytes error: %v", err)
	}
	// Only the outer class should be top-level; the nested (Outer$Inner) is skipped.
	if len(objects) != 1 {
		t.Errorf("expected 1 top-level object (nested skipped), got %d", len(objects))
	}
}

func TestIngestJARBytes_NonClassFilesIgnored(t *testing.T) {
	d2 := []string{"com/example/Foo"}
	classProto := buildSimpleClassProto(0, d2)
	classA := buildClassWithMetadata(1, [3]int32{1, 9, 0}, classProto, d2)

	jar := buildJARBytes(map[string][]byte{
		"com/example/Foo.class":  classA,
		"META-INF/MANIFEST.MF":   []byte("Manifest-Version: 1.0\n"),
		"com/example/config.xml": []byte("<config/>"),
	})

	objects, err := IngestJARBytes(jar)
	if err != nil {
		t.Fatalf("IngestJARBytes error: %v", err)
	}
	if len(objects) != 1 {
		t.Errorf("expected 1 object, got %d", len(objects))
	}
}

func TestIngestJARBytes_FileFacade(t *testing.T) {
	// File facade (kind=2)
	d2 := []string{"greet", "kotlin/String"}
	retType := buildTypeProto(1, false, nil, -1)
	fnFlags := publicFunctionFlags(0)
	fn := buildFunctionProto(0, fnFlags, retType, nil, nil)
	pkgProto := buildPackageProto([][]byte{fn}, nil)

	classBytes := buildClassWithMetadata(2, [3]int32{1, 9, 0}, pkgProto, d2)
	jar := buildJARBytes(map[string][]byte{
		"com/example/FooKt.class": classBytes,
	})

	objects, err := IngestJARBytes(jar)
	if err != nil {
		t.Fatalf("IngestJARBytes error: %v", err)
	}
	if len(objects) != 1 {
		t.Fatalf("expected 1 object, got %d", len(objects))
	}
	if objects[0].Kind != ClassKindFileFacade {
		t.Errorf("kind: got %v, want ClassKindFileFacade", objects[0].Kind)
	}
	if len(objects[0].Functions) != 1 {
		t.Errorf("expected 1 function, got %d", len(objects[0].Functions))
	}
}
