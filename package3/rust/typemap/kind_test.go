package typemap

import "testing"

func TestKindString(t *testing.T) {
	cases := []struct {
		k    Kind
		want string
	}{
		{KindUnit, "unit"},
		{KindBool, "bool"},
		{KindInt, "int"},
		{KindInt64, "int64"},
		{KindUInt, "uint"},
		{KindUInt64, "uint64"},
		{KindFloat, "float"},
		{KindFloat64, "float64"},
		{KindByte, "byte"},
		{KindChar, "char"},
		{KindString, "string"},
		{KindBytes, "bytes"},
		{KindList, "list"},
		{KindMap, "map"},
		{KindOption, "option"},
		{KindResult, "result"},
		{KindTuple, "tuple"},
		{KindStruct, "struct"},
		{KindEnum, "enum"},
		{KindHandle, "handle"},
		{KindInvalid, "invalid"},
	}
	for _, c := range cases {
		if got := c.k.String(); got != c.want {
			t.Errorf("Kind(%d).String() = %q; want %q", c.k, got, c.want)
		}
	}
}

func TestDirectionString(t *testing.T) {
	if DirectionIn.String() != "in" {
		t.Errorf("DirectionIn.String() = %q; want in", DirectionIn.String())
	}
	if DirectionOut.String() != "out" {
		t.Errorf("DirectionOut.String() = %q; want out", DirectionOut.String())
	}
}
