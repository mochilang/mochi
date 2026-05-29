package metadata

import (
	"bytes"
	"encoding/binary"
	"errors"
	"testing"

	bridgeerrors "github.com/mochilang/mochi/package3/kotlin/errors"
)

// buildClassWithMetadata synthesises a minimal valid .class file that contains
// exactly one class-level RuntimeVisibleAnnotations attribute holding a
// @kotlin.Metadata annotation with the given fields.
//
// The produced class file has:
//   - Magic: 0xCAFEBABE
//   - Version: minor=0, major=52 (Java 8)
//   - Minimal constant pool with all strings needed for the annotation
//   - Zero interfaces, zero fields, zero methods
//   - One class attribute: RuntimeVisibleAnnotations
func buildClassWithMetadata(kind int32, version [3]int32, d1 []byte, d2 []string) []byte {
	// We build the constant pool incrementally and keep track of indices.
	//
	// Constant pool layout (1-based):
	//  [1]  Utf8  "RuntimeVisibleAnnotations"
	//  [2]  Utf8  "Lkotlin/Metadata;"
	//  [3]  Utf8  "k"
	//  [4]  Integer  <kind>
	//  [5]  Utf8  "mv"
	//  [6]  Integer  version[0]
	//  [7]  Integer  version[1]
	//  [8]  Integer  version[2]
	//  [9]  Utf8  "d1"
	//  [10..10+len(d1strings)-1]  Utf8  each d1 string
	//  [next]  Utf8  "d2"
	//  [next+1 .. next+len(d2)-1]  Utf8  each d2 entry
	//  [next]  Utf8  "xi"
	//  [next+1]  Integer  0  (xi=0)
	//  [next]  Utf8  "ThisClass"   (name used for this_class)
	//  [next+1]  Class -> ThisClass utf8 index
	//
	// To keep it simple we split the original d1 bytes into a single string.
	// (For the multi-string test we pass a slice of strings via d1Parts.)

	// We'll encode d1 as a single string in the constant pool.
	d1Str := string(d1) // single d1 string

	type cpEntry struct {
		tag  byte
		data []byte // serialised bytes following the tag
	}

	var cp []cpEntry
	addEntry := func(tag byte, data []byte) int {
		cp = append(cp, cpEntry{tag, data})
		return len(cp) // 1-based index
	}
	u16 := func(v uint16) []byte { b := make([]byte, 2); binary.BigEndian.PutUint16(b, v); return b }
	u32 := func(v uint32) []byte { b := make([]byte, 4); binary.BigEndian.PutUint32(b, v); return b }
	utf8Entry := func(s string) []byte {
		sb := []byte(s)
		b := make([]byte, 2+len(sb))
		binary.BigEndian.PutUint16(b, uint16(len(sb)))
		copy(b[2:], sb)
		return b
	}

	// [1] Utf8 "RuntimeVisibleAnnotations"
	idx_rva := addEntry(1, utf8Entry("RuntimeVisibleAnnotations"))
	// [2] Utf8 "Lkotlin/Metadata;"
	idx_annType := addEntry(1, utf8Entry("Lkotlin/Metadata;"))
	// [3] Utf8 "k"
	idx_k := addEntry(1, utf8Entry("k"))
	// [4] Integer kind
	idx_kindVal := addEntry(3, u32(uint32(kind)))
	// [5] Utf8 "mv"
	idx_mv := addEntry(1, utf8Entry("mv"))
	// [6,7,8] Integer version[0..2]
	idx_mv0 := addEntry(3, u32(uint32(version[0])))
	idx_mv1 := addEntry(3, u32(uint32(version[1])))
	idx_mv2 := addEntry(3, u32(uint32(version[2])))
	// [9] Utf8 "d1"
	idx_d1 := addEntry(1, utf8Entry("d1"))
	// d1 value (single string)
	idx_d1Val := addEntry(1, utf8Entry(d1Str))
	// Utf8 "d2"
	idx_d2 := addEntry(1, utf8Entry("d2"))
	// d2 values
	d2Indices := make([]int, len(d2))
	for i, s := range d2 {
		d2Indices[i] = addEntry(1, utf8Entry(s))
	}
	// Utf8 "xi"
	idx_xi := addEntry(1, utf8Entry("xi"))
	// Integer 0 for xi
	idx_xiVal := addEntry(3, u32(0))
	// Utf8 for this class name
	idx_className := addEntry(1, utf8Entry("Stub"))
	// Class entry pointing to className
	idx_thisClass := addEntry(7, u16(uint16(idx_className)))

	// Suppress unused variable warnings with _ assignments where applicable.
	_, _, _, _, _, _, _ = idx_rva, idx_annType, idx_k, idx_kindVal, idx_mv, idx_mv0, idx_mv1
	_, _, _, _, _ = idx_mv2, idx_d1, idx_d1Val, idx_d2, idx_xi
	_, _, _ = idx_xiVal, idx_className, idx_thisClass

	// Build the RuntimeVisibleAnnotations attribute content.
	// Structure:
	//   num_annotations = 1
	//   annotation[0]:
	//     type_index = idx_annType
	//     num_element_value_pairs = (number of pairs)
	//     pairs: k, mv, d1, d2, xi
	var annBuf bytes.Buffer
	writeU16 := func(v uint16) { binary.Write(&annBuf, binary.BigEndian, v) } //nolint:errcheck
	writeU8 := func(v byte) { annBuf.WriteByte(v) }                            //nolint:errcheck

	// num_annotations = 1
	writeU16(1)
	// annotation type_index
	writeU16(uint16(idx_annType))

	// Count pairs: k, mv, d1, d2, xi
	numPairs := uint16(5)
	writeU16(numPairs)

	// Pair "k" = I <kind>
	writeU16(uint16(idx_k))
	writeU8('I')
	writeU16(uint16(idx_kindVal))

	// Pair "mv" = [ I I I ]
	writeU16(uint16(idx_mv))
	writeU8('[')
	writeU16(3)
	writeU8('I'); writeU16(uint16(idx_mv0))
	writeU8('I'); writeU16(uint16(idx_mv1))
	writeU8('I'); writeU16(uint16(idx_mv2))

	// Pair "d1" = [ s ] (single string)
	writeU16(uint16(idx_d1))
	writeU8('[')
	writeU16(1)
	writeU8('s'); writeU16(uint16(idx_d1Val))

	// Pair "d2" = [ s ... ]
	writeU16(uint16(idx_d2))
	writeU8('[')
	writeU16(uint16(len(d2)))
	for _, di := range d2Indices {
		writeU8('s'); writeU16(uint16(di))
	}

	// Pair "xi" = I 0
	writeU16(uint16(idx_xi))
	writeU8('I')
	writeU16(uint16(idx_xiVal))

	annData := annBuf.Bytes()

	// Build the class file.
	var buf bytes.Buffer
	writeU16Class := func(v uint16) { binary.Write(&buf, binary.BigEndian, v) } //nolint:errcheck
	writeU32Class := func(v uint32) { binary.Write(&buf, binary.BigEndian, v) } //nolint:errcheck

	// Magic + version
	writeU32Class(0xCAFEBABE)
	writeU16Class(0)  // minor version
	writeU16Class(52) // major version (Java 8)

	// Constant pool count (1-based, so count = len(cp)+1)
	writeU16Class(uint16(len(cp) + 1))
	for _, e := range cp {
		buf.WriteByte(e.tag)
		buf.Write(e.data)
	}

	// access_flags = ACC_PUBLIC (0x0001)
	writeU16Class(0x0001)
	// this_class
	writeU16Class(uint16(idx_thisClass))
	// super_class = 0 (no super, valid for interfaces; for simplicity use 0)
	writeU16Class(0)
	// interfaces_count = 0
	writeU16Class(0)
	// fields_count = 0
	writeU16Class(0)
	// methods_count = 0
	writeU16Class(0)
	// attributes_count = 1
	writeU16Class(1)
	// Attribute: RuntimeVisibleAnnotations
	writeU16Class(uint16(idx_rva))
	writeU32Class(uint32(len(annData)))
	buf.Write(annData)

	return buf.Bytes()
}

// buildClassWithMetadataMultiD1 is like buildClassWithMetadata but accepts
// multiple d1 parts (strings that will be concatenated by ExtractMetadata).
func buildClassWithMetadataMultiD1(kind int32, version [3]int32, d1Parts []string, d2 []string) []byte {
	type cpEntry struct {
		tag  byte
		data []byte
	}
	var cp []cpEntry
	addEntry := func(tag byte, data []byte) int {
		cp = append(cp, cpEntry{tag, data})
		return len(cp)
	}
	u16 := func(v uint16) []byte { b := make([]byte, 2); binary.BigEndian.PutUint16(b, v); return b }
	u32 := func(v uint32) []byte { b := make([]byte, 4); binary.BigEndian.PutUint32(b, v); return b }
	utf8Entry := func(s string) []byte {
		sb := []byte(s)
		b := make([]byte, 2+len(sb))
		binary.BigEndian.PutUint16(b, uint16(len(sb)))
		copy(b[2:], sb)
		return b
	}

	idx_rva := addEntry(1, utf8Entry("RuntimeVisibleAnnotations"))
	idx_annType := addEntry(1, utf8Entry("Lkotlin/Metadata;"))
	idx_k := addEntry(1, utf8Entry("k"))
	idx_kindVal := addEntry(3, u32(uint32(kind)))
	idx_mv := addEntry(1, utf8Entry("mv"))
	idx_mv0 := addEntry(3, u32(uint32(version[0])))
	idx_mv1 := addEntry(3, u32(uint32(version[1])))
	idx_mv2 := addEntry(3, u32(uint32(version[2])))
	idx_d1 := addEntry(1, utf8Entry("d1"))
	d1Indices := make([]int, len(d1Parts))
	for i, s := range d1Parts {
		d1Indices[i] = addEntry(1, utf8Entry(s))
	}
	idx_d2 := addEntry(1, utf8Entry("d2"))
	d2Indices := make([]int, len(d2))
	for i, s := range d2 {
		d2Indices[i] = addEntry(1, utf8Entry(s))
	}
	idx_xi := addEntry(1, utf8Entry("xi"))
	idx_xiVal := addEntry(3, u32(0))
	idx_className := addEntry(1, utf8Entry("Stub"))
	idx_thisClass := addEntry(7, u16(uint16(idx_className)))

	_, _, _, _, _, _, _ = idx_rva, idx_annType, idx_k, idx_kindVal, idx_mv, idx_mv0, idx_mv1
	_, _, _, _, _ = idx_mv2, idx_d1, idx_d2, idx_xi, idx_xiVal
	_, _, _ = idx_className, idx_thisClass, d1Indices

	var annBuf bytes.Buffer
	writeU16 := func(v uint16) { binary.Write(&annBuf, binary.BigEndian, v) } //nolint:errcheck
	writeU8 := func(v byte) { annBuf.WriteByte(v) }                            //nolint:errcheck

	writeU16(1)
	writeU16(uint16(idx_annType))
	writeU16(5) // k, mv, d1, d2, xi

	writeU16(uint16(idx_k))
	writeU8('I'); writeU16(uint16(idx_kindVal))

	writeU16(uint16(idx_mv))
	writeU8('['); writeU16(3)
	writeU8('I'); writeU16(uint16(idx_mv0))
	writeU8('I'); writeU16(uint16(idx_mv1))
	writeU8('I'); writeU16(uint16(idx_mv2))

	writeU16(uint16(idx_d1))
	writeU8('['); writeU16(uint16(len(d1Parts)))
	for _, di := range d1Indices {
		writeU8('s'); writeU16(uint16(di))
	}

	writeU16(uint16(idx_d2))
	writeU8('['); writeU16(uint16(len(d2)))
	for _, di := range d2Indices {
		writeU8('s'); writeU16(uint16(di))
	}

	writeU16(uint16(idx_xi))
	writeU8('I'); writeU16(uint16(idx_xiVal))

	annData := annBuf.Bytes()

	var buf bytes.Buffer
	writeU16C := func(v uint16) { binary.Write(&buf, binary.BigEndian, v) } //nolint:errcheck
	writeU32C := func(v uint32) { binary.Write(&buf, binary.BigEndian, v) } //nolint:errcheck

	writeU32C(0xCAFEBABE)
	writeU16C(0); writeU16C(52)
	writeU16C(uint16(len(cp) + 1))
	for _, e := range cp {
		buf.WriteByte(e.tag)
		buf.Write(e.data)
	}
	writeU16C(0x0001)
	writeU16C(uint16(idx_thisClass))
	writeU16C(0)
	writeU16C(0)
	writeU16C(0)
	writeU16C(0)
	writeU16C(1) // attributes_count = 1
	writeU16C(uint16(idx_rva))
	writeU32C(uint32(len(annData)))
	buf.Write(annData)

	return buf.Bytes()
}

// buildMinimalClass builds a class file with no annotations (no @kotlin.Metadata).
func buildMinimalClass() []byte {
	type cpEntry struct {
		tag  byte
		data []byte
	}
	var cp []cpEntry
	addEntry := func(tag byte, data []byte) int {
		cp = append(cp, cpEntry{tag, data})
		return len(cp)
	}
	u16 := func(v uint16) []byte { b := make([]byte, 2); binary.BigEndian.PutUint16(b, v); return b }
	utf8Entry := func(s string) []byte {
		sb := []byte(s)
		b := make([]byte, 2+len(sb))
		binary.BigEndian.PutUint16(b, uint16(len(sb)))
		copy(b[2:], sb)
		return b
	}

	idx_className := addEntry(1, utf8Entry("Stub"))
	idx_thisClass := addEntry(7, u16(uint16(idx_className)))
	_ = idx_thisClass

	var buf bytes.Buffer
	writeU16 := func(v uint16) { binary.Write(&buf, binary.BigEndian, v) } //nolint:errcheck
	writeU32 := func(v uint32) { binary.Write(&buf, binary.BigEndian, v) } //nolint:errcheck

	writeU32(0xCAFEBABE)
	writeU16(0); writeU16(52)
	writeU16(uint16(len(cp) + 1))
	for _, e := range cp {
		buf.WriteByte(e.tag)
		buf.Write(e.data)
	}
	writeU16(0x0001)
	writeU16(uint16(idx_thisClass))
	writeU16(0)
	writeU16(0) // interfaces
	writeU16(0) // fields
	writeU16(0) // methods
	writeU16(0) // attributes

	return buf.Bytes()
}

// --- Tests ---

func TestExtractMetadata_RoundTrip(t *testing.T) {
	d1 := []byte{0x0a, 0x01, 0x02, 0x03}
	d2 := []string{"foo", "bar", "baz"}
	version := [3]int32{1, 9, 0}
	classBytes := buildClassWithMetadata(1, version, d1, d2)

	meta, err := ExtractMetadata(classBytes)
	if err != nil {
		t.Fatalf("ExtractMetadata returned error: %v", err)
	}
	if meta.Kind != 1 {
		t.Errorf("kind: got %d, want 1", meta.Kind)
	}
	if meta.Version != version {
		t.Errorf("version: got %v, want %v", meta.Version, version)
	}
	if !bytes.Equal(meta.D1, d1) {
		t.Errorf("D1: got %x, want %x", meta.D1, d1)
	}
	if len(meta.D2) != len(d2) {
		t.Fatalf("D2 len: got %d, want %d", len(meta.D2), len(d2))
	}
	for i, s := range d2 {
		if meta.D2[i] != s {
			t.Errorf("D2[%d]: got %q, want %q", i, meta.D2[i], s)
		}
	}
}

func TestExtractMetadata_NoAnnotation(t *testing.T) {
	classBytes := buildMinimalClass()
	_, err := ExtractMetadata(classBytes)
	if err == nil {
		t.Fatal("expected ErrNoKotlinMetadata, got nil")
	}
	if !errors.Is(err, bridgeerrors.ErrNoKotlinMetadata) {
		t.Errorf("expected ErrNoKotlinMetadata, got: %v", err)
	}
}

func TestExtractMetadata_VersionExtraction(t *testing.T) {
	version := [3]int32{1, 9, 0}
	classBytes := buildClassWithMetadata(1, version, nil, nil)

	meta, err := ExtractMetadata(classBytes)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if meta.Version[0] != 1 || meta.Version[1] != 9 || meta.Version[2] != 0 {
		t.Errorf("version: got %v, want [1 9 0]", meta.Version)
	}
}

func TestExtractMetadata_MultiStringD1(t *testing.T) {
	// Split the d1 data across 3 strings; should be joined on extraction.
	part1 := "hello"
	part2 := " world"
	part3 := "!"
	expected := []byte(part1 + part2 + part3)

	classBytes := buildClassWithMetadataMultiD1(1, [3]int32{1, 9, 0}, []string{part1, part2, part3}, nil)

	meta, err := ExtractMetadata(classBytes)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !bytes.Equal(meta.D1, expected) {
		t.Errorf("D1: got %q, want %q", meta.D1, expected)
	}
}

func TestExtractMetadata_EmptyD2(t *testing.T) {
	classBytes := buildClassWithMetadata(1, [3]int32{1, 9, 0}, []byte{0x01}, nil)
	meta, err := ExtractMetadata(classBytes)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(meta.D2) != 0 {
		t.Errorf("D2: got %v, want empty", meta.D2)
	}
}

func TestExtractMetadata_BadMagic(t *testing.T) {
	classBytes := []byte{0xDE, 0xAD, 0xBE, 0xEF, 0x00, 0x00, 0x00, 0x34}
	_, err := ExtractMetadata(classBytes)
	if err == nil {
		t.Fatal("expected error for bad magic, got nil")
	}
}

func TestExtractMetadata_TooShort(t *testing.T) {
	_, err := ExtractMetadata([]byte{0xCA, 0xFE})
	if err == nil {
		t.Fatal("expected error for too-short data, got nil")
	}
}
