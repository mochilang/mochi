package typemap

import (
	"testing"

	javaerrors "mochi/package3/java/errors"
)

// TestPhase4TypeMapping is the phase-gate sentinel test for MEP-67 phase 4.
func TestPhase4TypeMapping(t *testing.T) {
	t.Run("primitive_table", func(t *testing.T) {
		for java, wantKind := range map[string]Kind{
			"int":     KindInt,
			"long":    KindLong,
			"float":   KindFloat,
			"double":  KindDouble,
			"boolean": KindBool,
			"void":    KindVoid,
		} {
			m, sr, _ := Map(java)
			if sr != javaerrors.SkipUnknown {
				t.Errorf("Map(%q) skipped %v", java, sr)
				continue
			}
			if m.Kind != wantKind {
				t.Errorf("Map(%q).Kind = %v; want %v", java, m.Kind, wantKind)
			}
		}
	})

	t.Run("boxed_equivalence", func(t *testing.T) {
		pairs := [][2]string{
			{"int", "java.lang.Integer"},
			{"long", "java.lang.Long"},
			{"float", "java.lang.Float"},
			{"double", "java.lang.Double"},
			{"boolean", "java.lang.Boolean"},
		}
		for _, p := range pairs {
			m1, sr1, _ := Map(p[0])
			m2, sr2, _ := Map(p[1])
			if sr1 != javaerrors.SkipUnknown || sr2 != javaerrors.SkipUnknown {
				t.Errorf("pair %v: skip %v %v", p, sr1, sr2)
				continue
			}
			if m1.Kind != m2.Kind {
				t.Errorf("Map(%q).Kind=%v != Map(%q).Kind=%v", p[0], m1.Kind, p[1], m2.Kind)
			}
		}
	})

	t.Run("refusal_table", func(t *testing.T) {
		refusals := map[string]javaerrors.SkipReason{
			"java.lang.Object":                javaerrors.SkipRawObject,
			"?":                               javaerrors.SkipWildcard,
			"java.util.List":                  javaerrors.SkipGeneric,
			"com.example.Outer$Inner":          javaerrors.SkipInnerClass,
			"java.lang.reflect.Method":         javaerrors.SkipReflective,
		}
		for java, want := range refusals {
			_, sr, _ := Map(java)
			if sr != want {
				t.Errorf("Map(%q): SkipReason=%v; want %v", java, sr, want)
			}
		}
	})

	t.Run("generic_list", func(t *testing.T) {
		m, sr, _ := Map("java.util.List<java.lang.String>")
		if sr != javaerrors.SkipUnknown {
			t.Fatalf("Map(List<String>) skipped: %v", sr)
		}
		if m.Kind != KindList {
			t.Errorf("Kind = %v; want KindList", m.Kind)
		}
		if m.Elem == nil || m.Elem.Kind != KindString {
			t.Errorf("Elem.Kind = %v; want KindString", m.Elem)
		}
	})

	t.Run("generic_map", func(t *testing.T) {
		m, sr, _ := Map("java.util.Map<java.lang.String,java.lang.Integer>")
		if sr != javaerrors.SkipUnknown {
			t.Fatalf("Map(Map<String,Integer>) skipped: %v", sr)
		}
		if m.Kind != KindMap {
			t.Errorf("Kind = %v; want KindMap", m.Kind)
		}
	})

	t.Run("opaque_handle", func(t *testing.T) {
		m, sr, _ := Map("com.example.ComplexClass")
		if sr != javaerrors.SkipUnknown {
			t.Fatalf("unknown class should be handled, got skip %v", sr)
		}
		if m.Kind != KindHandle {
			t.Errorf("Kind = %v; want KindHandle", m.Kind)
		}
	})
}
