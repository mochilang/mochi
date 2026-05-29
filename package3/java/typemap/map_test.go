package typemap

import (
	"testing"

	javaerrors "mochi/package3/java/errors"
)

func TestMapPrimitives(t *testing.T) {
	cases := []struct {
		java     string
		wantKind Kind
	}{
		{"int", KindInt},
		{"long", KindLong},
		{"short", KindInt},
		{"byte", KindInt},
		{"char", KindInt},
		{"float", KindFloat},
		{"double", KindDouble},
		{"boolean", KindBool},
		{"void", KindVoid},
	}
	for _, c := range cases {
		m, sr, detail := Map(c.java)
		if sr != javaerrors.SkipUnknown || detail != "" {
			t.Errorf("Map(%q) skipped: %v %s", c.java, sr, detail)
			continue
		}
		if m.Kind != c.wantKind {
			t.Errorf("Map(%q).Kind = %v; want %v", c.java, m.Kind, c.wantKind)
		}
	}
}

func TestMapBoxed(t *testing.T) {
	cases := []struct {
		java     string
		wantKind Kind
	}{
		{"java.lang.Integer", KindInt},
		{"java.lang.Long", KindLong},
		{"java.lang.Float", KindFloat},
		{"java.lang.Double", KindDouble},
		{"java.lang.Boolean", KindBool},
		{"java.lang.Short", KindInt},
		{"java.lang.Byte", KindInt},
		{"java.lang.Character", KindInt},
	}
	for _, c := range cases {
		m, sr, _ := Map(c.java)
		if sr != javaerrors.SkipUnknown {
			t.Errorf("Map(%q) skipped: %v", c.java, sr)
			continue
		}
		if m.Kind != c.wantKind {
			t.Errorf("Map(%q).Kind = %v; want %v", c.java, m.Kind, c.wantKind)
		}
	}
}

func TestMapString(t *testing.T) {
	m, sr, _ := Map("java.lang.String")
	if sr != javaerrors.SkipUnknown {
		t.Errorf("Map(String) skipped: %v", sr)
	}
	if m.Kind != KindString {
		t.Errorf("Kind = %v; want KindString", m.Kind)
	}
	if m.MochiType != "string" {
		t.Errorf("MochiType = %q; want string", m.MochiType)
	}
}

func TestMapByteArray(t *testing.T) {
	m, sr, _ := Map("byte[]")
	if sr != javaerrors.SkipUnknown {
		t.Errorf("Map(byte[]) skipped: %v", sr)
	}
	if m.Kind != KindBytes {
		t.Errorf("Kind = %v; want KindBytes", m.Kind)
	}
}

func TestMapPrimitiveArrays(t *testing.T) {
	for _, java := range []string{"int[]", "long[]", "double[]", "boolean[]"} {
		m, sr, _ := Map(java)
		if sr != javaerrors.SkipUnknown {
			t.Errorf("Map(%q) skipped: %v", java, sr)
			continue
		}
		if m.Kind != KindList {
			t.Errorf("Map(%q).Kind = %v; want KindList", java, m.Kind)
		}
	}
}

func TestMapRefusals(t *testing.T) {
	cases := []struct {
		java   string
		wantSR javaerrors.SkipReason
	}{
		{"java.lang.Object", javaerrors.SkipRawObject},
		{"?", javaerrors.SkipWildcard},
		{"? extends java.lang.Number", javaerrors.SkipWildcard},
		{"? super java.lang.String", javaerrors.SkipWildcard},
		{"java.util.List", javaerrors.SkipGeneric},
		{"java.util.Map", javaerrors.SkipGeneric},
		{"java.util.Optional", javaerrors.SkipGeneric},
		{"com.example.Outer$Inner", javaerrors.SkipInnerClass},
		{"java.lang.reflect.Method", javaerrors.SkipReflective},
		{"sun.misc.Unsafe", javaerrors.SkipReflective},
	}
	for _, c := range cases {
		_, sr, _ := Map(c.java)
		if sr != c.wantSR {
			t.Errorf("Map(%q): SkipReason = %v; want %v", c.java, sr, c.wantSR)
		}
	}
}

func TestMapGenericList(t *testing.T) {
	m, sr, _ := Map("java.util.List<java.lang.String>")
	if sr != javaerrors.SkipUnknown {
		t.Errorf("Map(List<String>) skipped: %v", sr)
	}
	if m.Kind != KindList {
		t.Errorf("Kind = %v; want KindList", m.Kind)
	}
	if m.Elem == nil || m.Elem.Kind != KindString {
		t.Errorf("Elem kind = %v; want KindString", m.Elem)
	}
}

func TestMapGenericOptional(t *testing.T) {
	m, sr, _ := Map("java.util.Optional<java.lang.String>")
	if sr != javaerrors.SkipUnknown {
		t.Errorf("Map(Optional<String>) skipped: %v", sr)
	}
	if m.Kind != KindOptional {
		t.Errorf("Kind = %v; want KindOptional", m.Kind)
	}
}

func TestMapGenericFuture(t *testing.T) {
	m, sr, _ := Map("java.util.concurrent.CompletableFuture<java.lang.String>")
	if sr != javaerrors.SkipUnknown {
		t.Errorf("Map(CompletableFuture<String>) skipped: %v", sr)
	}
	if m.Kind != KindFuture {
		t.Errorf("Kind = %v; want KindFuture", m.Kind)
	}
}

func TestMapGenericMap(t *testing.T) {
	m, sr, _ := Map("java.util.Map<java.lang.String,java.lang.Integer>")
	if sr != javaerrors.SkipUnknown {
		t.Errorf("Map(Map<String,Integer>) skipped: %v", sr)
	}
	if m.Kind != KindMap {
		t.Errorf("Kind = %v; want KindMap", m.Kind)
	}
}

func TestMapIsScalar(t *testing.T) {
	scalars := []string{"int", "long", "float", "double", "boolean", "void"}
	for _, java := range scalars {
		m, _, _ := Map(java)
		if m != nil && !m.IsScalar() {
			t.Errorf("Map(%q).IsScalar() = false; want true", java)
		}
	}
	nonScalars := []string{"java.lang.String", "byte[]", "java.util.List<java.lang.String>"}
	for _, java := range nonScalars {
		m, sr, _ := Map(java)
		if sr != javaerrors.SkipUnknown {
			continue
		}
		if m.IsScalar() {
			t.Errorf("Map(%q).IsScalar() = true; want false", java)
		}
	}
}

func TestMapOpaqueHandle(t *testing.T) {
	// An unknown class should become a KindHandle.
	m, sr, _ := Map("com.example.SomeClass")
	if sr != javaerrors.SkipUnknown {
		t.Errorf("Map(com.example.SomeClass) should succeed (as handle), got skip %v", sr)
	}
	if m.Kind != KindHandle {
		t.Errorf("Kind = %v; want KindHandle", m.Kind)
	}
	if m.FFIRepr != "long" {
		t.Errorf("FFIRepr = %q; want long", m.FFIRepr)
	}
}
