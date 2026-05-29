package typemap_test

import (
	"testing"

	"mochi/package3/dotnet/typemap"
)

func TestKind_String_allNonEmpty(t *testing.T) {
	kinds := []typemap.Kind{
		typemap.KindBool,
		typemap.KindByte,
		typemap.KindInt,
		typemap.KindInt64,
		typemap.KindUInt,
		typemap.KindUInt64,
		typemap.KindFloat,
		typemap.KindFloat64,
		typemap.KindChar,
		typemap.KindString,
		typemap.KindBytes,
		typemap.KindUnit,
		typemap.KindList,
		typemap.KindMap,
		typemap.KindSet,
		typemap.KindOption,
		typemap.KindTask,
		typemap.KindTuple,
		typemap.KindRecord,
		typemap.KindHandle,
		typemap.KindEnum,
	}
	for _, k := range kinds {
		s := k.String()
		if s == "" {
			t.Errorf("Kind(%d).String() is empty", int(k))
		}
		if s == "unknown" {
			t.Errorf("Kind(%d).String() returned 'unknown' for a named constant", int(k))
		}
	}
}

func TestKind_String_zeroIsUnknown(t *testing.T) {
	var k typemap.Kind
	if got := k.String(); got != "unknown" {
		t.Errorf("zero Kind.String() = %q; want %q", got, "unknown")
	}
}

func TestKind_String_specificValues(t *testing.T) {
	cases := []struct {
		k    typemap.Kind
		want string
	}{
		{typemap.KindBool, "bool"},
		{typemap.KindByte, "byte"},
		{typemap.KindInt, "int"},
		{typemap.KindInt64, "int64"},
		{typemap.KindUInt, "uint"},
		{typemap.KindUInt64, "uint64"},
		{typemap.KindFloat, "float"},
		{typemap.KindFloat64, "float64"},
		{typemap.KindChar, "char"},
		{typemap.KindString, "string"},
		{typemap.KindBytes, "bytes"},
		{typemap.KindUnit, "unit"},
		{typemap.KindList, "list"},
		{typemap.KindMap, "map"},
		{typemap.KindSet, "set"},
		{typemap.KindOption, "option"},
		{typemap.KindTask, "task"},
		{typemap.KindTuple, "tuple"},
		{typemap.KindRecord, "record"},
		{typemap.KindHandle, "handle"},
		{typemap.KindEnum, "enum"},
	}
	for _, tc := range cases {
		if got := tc.k.String(); got != tc.want {
			t.Errorf("Kind.String() = %q; want %q", got, tc.want)
		}
	}
}
