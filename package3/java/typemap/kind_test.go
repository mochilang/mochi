package typemap

import (
	"testing"
)

func TestKindString(t *testing.T) {
	cases := []struct {
		k    Kind
		want string
	}{
		{KindBool, "bool"},
		{KindInt, "int"},
		{KindLong, "long"},
		{KindFloat, "float"},
		{KindDouble, "double"},
		{KindString, "string"},
		{KindVoid, "void"},
		{KindBytes, "bytes"},
		{KindList, "list"},
		{KindMap, "map"},
		{KindOptional, "optional"},
		{KindFuture, "future"},
		{KindHandle, "handle"},
		{KindEnum, "enum"},
		{KindRecord, "record"},
		{KindUnknown, "unknown"},
	}
	for _, c := range cases {
		if got := c.k.String(); got != c.want {
			t.Errorf("Kind(%d).String() = %q; want %q", c.k, got, c.want)
		}
	}
}
