package typemap

import "testing"

func TestMappingMochiTypeScalars(t *testing.T) {
	cases := []struct {
		m    Mapping
		want string
	}{
		{Mapping{Kind: KindUnit}, "unit"},
		{Mapping{Kind: KindBool}, "bool"},
		{Mapping{Kind: KindInt}, "int"},
		{Mapping{Kind: KindInt64}, "int64"},
		{Mapping{Kind: KindUInt}, "uint"},
		{Mapping{Kind: KindUInt64}, "uint64"},
		{Mapping{Kind: KindFloat}, "float"},
		{Mapping{Kind: KindFloat64}, "float64"},
		{Mapping{Kind: KindByte}, "byte"},
		{Mapping{Kind: KindChar}, "char"},
		{Mapping{Kind: KindString}, "string"},
		{Mapping{Kind: KindBytes}, "bytes"},
	}
	for _, c := range cases {
		if got := c.m.MochiType(); got != c.want {
			t.Errorf("MochiType(%s) = %q; want %q", c.m.Kind, got, c.want)
		}
	}
}

func TestMappingMochiTypeList(t *testing.T) {
	m := Mapping{Kind: KindList, Elem: &Mapping{Kind: KindInt64}}
	if got := m.MochiType(); got != "list[int64]" {
		t.Errorf("list[int64] MochiType = %q", got)
	}
	// nested
	nested := Mapping{Kind: KindList, Elem: &Mapping{Kind: KindList, Elem: &Mapping{Kind: KindString}}}
	if got := nested.MochiType(); got != "list[list[string]]" {
		t.Errorf("list[list[string]] MochiType = %q", got)
	}
	// nil elem (degenerate)
	deg := Mapping{Kind: KindList}
	if got := deg.MochiType(); got != "list[?]" {
		t.Errorf("nil-elem list MochiType = %q", got)
	}
}

func TestMappingMochiTypeMap(t *testing.T) {
	m := Mapping{
		Kind:  KindMap,
		Key:   &Mapping{Kind: KindString},
		Value: &Mapping{Kind: KindInt},
	}
	if got := m.MochiType(); got != "map[string]int" {
		t.Errorf("map[string]int MochiType = %q", got)
	}
}

func TestMappingMochiTypeOption(t *testing.T) {
	m := Mapping{Kind: KindOption, Elem: &Mapping{Kind: KindString}}
	if got := m.MochiType(); got != "?string" {
		t.Errorf("?string MochiType = %q", got)
	}
}

func TestMappingMochiTypeResult(t *testing.T) {
	m := Mapping{Kind: KindResult, OK: &Mapping{Kind: KindString}, Err: &Mapping{Kind: KindInt}}
	if got := m.MochiType(); got != "result[string,int]" {
		t.Errorf("result MochiType = %q", got)
	}
}

func TestMappingMochiTypeTuple(t *testing.T) {
	m := Mapping{Kind: KindTuple, Fields: []Mapping{
		{Kind: KindInt},
		{Kind: KindString},
		{Kind: KindBool},
	}}
	if got := m.MochiType(); got != "tuple[int,string,bool]" {
		t.Errorf("tuple MochiType = %q", got)
	}
}

func TestMappingMochiTypeStructEnumHandle(t *testing.T) {
	s := Mapping{Kind: KindStruct, PathName: "serde::de::Error"}
	if got := s.MochiType(); got != "serde::de::Error" {
		t.Errorf("struct MochiType = %q", got)
	}
	e := Mapping{Kind: KindEnum, PathName: "syn::Item"}
	if got := e.MochiType(); got != "syn::Item" {
		t.Errorf("enum MochiType = %q", got)
	}
	h := Mapping{Kind: KindHandle, PathName: "tokio::sync::Mutex"}
	if got := h.MochiType(); got != "tokio::sync::Mutex" {
		t.Errorf("handle MochiType = %q", got)
	}
}

func TestMappingFFIRepr(t *testing.T) {
	cases := []struct {
		m    Mapping
		want string
	}{
		{Mapping{Kind: KindUnit}, "void"},
		{Mapping{Kind: KindBool}, "bool"},
		{Mapping{Kind: KindInt}, "int32_t"},
		{Mapping{Kind: KindInt64}, "int64_t"},
		{Mapping{Kind: KindUInt}, "uint32_t"},
		{Mapping{Kind: KindUInt64}, "uint64_t"},
		{Mapping{Kind: KindFloat}, "float"},
		{Mapping{Kind: KindFloat64}, "double"},
		{Mapping{Kind: KindByte}, "uint8_t"},
		{Mapping{Kind: KindChar}, "uint32_t"},
		{Mapping{Kind: KindString}, "MochiString"},
		{Mapping{Kind: KindBytes}, "MochiSlice"},
		{Mapping{Kind: KindList}, "MochiSlice"},
		{Mapping{Kind: KindMap}, "MochiMap"},
		{Mapping{Kind: KindOption}, "MochiOption"},
		{Mapping{Kind: KindResult}, "MochiResult"},
		{Mapping{Kind: KindTuple}, "MochiTuple"},
		{Mapping{Kind: KindStruct}, "MochiHandle"},
		{Mapping{Kind: KindEnum}, "MochiHandle"},
		{Mapping{Kind: KindHandle}, "MochiHandle"},
		{Mapping{Kind: KindInvalid}, "void"},
	}
	for _, c := range cases {
		if got := c.m.FFIRepr(); got != c.want {
			t.Errorf("FFIRepr(%s) = %q; want %q", c.m.Kind, got, c.want)
		}
	}
}

func TestMappingIsScalar(t *testing.T) {
	scalars := []Kind{
		KindUnit, KindBool, KindInt, KindInt64, KindUInt, KindUInt64,
		KindFloat, KindFloat64, KindByte, KindChar,
	}
	for _, k := range scalars {
		if !(Mapping{Kind: k}).IsScalar() {
			t.Errorf("%s should be scalar", k)
		}
	}
	nonScalars := []Kind{
		KindString, KindBytes, KindList, KindMap, KindOption,
		KindResult, KindTuple, KindStruct, KindEnum, KindHandle,
	}
	for _, k := range nonScalars {
		if (Mapping{Kind: k}).IsScalar() {
			t.Errorf("%s should not be scalar", k)
		}
	}
}
