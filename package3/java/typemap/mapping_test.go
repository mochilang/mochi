package typemap

import (
	"testing"
)

func TestMappingIsScalar(t *testing.T) {
	scalars := []Kind{KindBool, KindInt, KindLong, KindFloat, KindDouble, KindVoid}
	for _, k := range scalars {
		m := &Mapping{Kind: k}
		if !m.IsScalar() {
			t.Errorf("Kind=%v: IsScalar() = false; want true", k)
		}
	}
	nonScalars := []Kind{KindString, KindBytes, KindList, KindMap, KindOptional, KindHandle}
	for _, k := range nonScalars {
		m := &Mapping{Kind: k}
		if m.IsScalar() {
			t.Errorf("Kind=%v: IsScalar() = true; want false", k)
		}
	}
}
