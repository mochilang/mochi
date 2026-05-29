package typemap_test

import (
	"testing"

	"mochi/package3/dotnet/errors"
	"mochi/package3/dotnet/metacli"
	"mochi/package3/dotnet/typemap"
)

// ref builds a simple TypeRef for testing.
func ref(fullName string, kind metacli.TypeRefKind) metacli.TypeRef {
	return metacli.TypeRef{FullName: fullName, Kind: kind}
}

// genericRef builds a generic_inst TypeRef.
func genericRef(fullName string, args ...metacli.TypeRef) metacli.TypeRef {
	return metacli.TypeRef{FullName: fullName, Kind: metacli.TypeRefGenericInst, TypeArgs: args}
}

// nullableRef builds a Nullable<T> TypeRef.
func nullableRef(inner metacli.TypeRef) metacli.TypeRef {
	return metacli.TypeRef{
		FullName:   "System.Nullable`1",
		Kind:       metacli.TypeRefGenericInst,
		IsNullable: true,
		TypeArgs:   []metacli.TypeRef{inner},
	}
}

func TestMap_primitives(t *testing.T) {
	cases := []struct {
		fullName string
		wantKind typemap.Kind
		wantFFI  string
	}{
		{"System.Boolean", typemap.KindBool, "byte"},
		{"System.Byte", typemap.KindByte, "byte"},
		{"System.SByte", typemap.KindInt, "sbyte"},
		{"System.Int16", typemap.KindInt, "short"},
		{"System.Int32", typemap.KindInt, "int"},
		{"System.Int64", typemap.KindInt64, "long"},
		{"System.UInt16", typemap.KindUInt, "ushort"},
		{"System.UInt32", typemap.KindUInt, "uint"},
		{"System.UInt64", typemap.KindUInt64, "ulong"},
		{"System.Single", typemap.KindFloat, "float"},
		{"System.Double", typemap.KindFloat64, "double"},
		{"System.Char", typemap.KindChar, "char"},
		{"System.String", typemap.KindString, "byte*, int"},
		{"System.Void", typemap.KindUnit, "void"},
	}
	for _, tc := range cases {
		t := t
		tc := tc
		t.Run(tc.fullName, func(t *testing.T) {
			m, sr, det := typemap.Map(ref(tc.fullName, metacli.TypeRefPrimitive), typemap.DirectionIn)
			if sr != errors.SkipUnknown || det != "" {
				t.Fatalf("Map(%s) refused: %s %s", tc.fullName, sr, det)
			}
			if m.Kind != tc.wantKind {
				t.Errorf("Kind = %s; want %s", m.Kind, tc.wantKind)
			}
			if m.FFIRepr() != tc.wantFFI {
				t.Errorf("FFIRepr = %q; want %q", m.FFIRepr(), tc.wantFFI)
			}
		})
	}
}

func TestMap_voidTypeRef(t *testing.T) {
	m, sr, det := typemap.Map(ref("System.Void", metacli.TypeRefVoid), typemap.DirectionOut)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindUnit {
		t.Errorf("Kind = %s; want unit", m.Kind)
	}
}

func TestMap_list(t *testing.T) {
	r := genericRef("System.Collections.Generic.List`1", ref("System.Int32", metacli.TypeRefPrimitive))
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindList {
		t.Errorf("Kind = %s; want list", m.Kind)
	}
	if m.Elem == nil || m.Elem.Kind != typemap.KindInt {
		t.Error("Elem should be KindInt")
	}
}

func TestMap_ienumerable(t *testing.T) {
	r := genericRef("System.Collections.Generic.IEnumerable`1", ref("System.String", metacli.TypeRefPrimitive))
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindList {
		t.Errorf("Kind = %s; want list", m.Kind)
	}
}

func TestMap_hashset(t *testing.T) {
	r := genericRef("System.Collections.Generic.HashSet`1", ref("System.String", metacli.TypeRefPrimitive))
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindSet {
		t.Errorf("Kind = %s; want set", m.Kind)
	}
}

func TestMap_dictionary(t *testing.T) {
	r := genericRef("System.Collections.Generic.Dictionary`2",
		ref("System.String", metacli.TypeRefPrimitive),
		ref("System.Int32", metacli.TypeRefPrimitive),
	)
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindMap {
		t.Errorf("Kind = %s; want map", m.Kind)
	}
	if m.Key == nil || m.Key.Kind != typemap.KindString {
		t.Error("Key should be KindString")
	}
	if m.Value == nil || m.Value.Kind != typemap.KindInt {
		t.Error("Value should be KindInt")
	}
}

func TestMap_ireadOnlyDictionary(t *testing.T) {
	r := genericRef("System.Collections.Generic.IReadOnlyDictionary`2",
		ref("System.String", metacli.TypeRefPrimitive),
		ref("System.Int64", metacli.TypeRefPrimitive),
	)
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindMap {
		t.Errorf("Kind = %s; want map", m.Kind)
	}
}

func TestMap_nullable(t *testing.T) {
	r := nullableRef(ref("System.Int32", metacli.TypeRefPrimitive))
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindOption {
		t.Errorf("Kind = %s; want option", m.Kind)
	}
	if m.Elem == nil || m.Elem.Kind != typemap.KindInt {
		t.Error("Elem should be KindInt")
	}
}

func TestMap_taskT(t *testing.T) {
	r := genericRef("System.Threading.Tasks.Task`1", ref("System.String", metacli.TypeRefPrimitive))
	m, sr, det := typemap.Map(r, typemap.DirectionOut)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindTask {
		t.Errorf("Kind = %s; want task", m.Kind)
	}
	if m.Elem == nil || m.Elem.Kind != typemap.KindString {
		t.Error("Elem should be KindString")
	}
}

func TestMap_taskNonGeneric(t *testing.T) {
	r := ref("System.Threading.Tasks.Task", metacli.TypeRefClass)
	m, sr, det := typemap.Map(r, typemap.DirectionOut)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindUnit {
		t.Errorf("Kind = %s; want unit", m.Kind)
	}
}

func TestMap_valueTuple(t *testing.T) {
	r := genericRef("System.ValueTuple`2",
		ref("System.Int32", metacli.TypeRefPrimitive),
		ref("System.String", metacli.TypeRefPrimitive),
	)
	m, sr, det := typemap.Map(r, typemap.DirectionOut)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindTuple {
		t.Errorf("Kind = %s; want tuple", m.Kind)
	}
	if len(m.Fields) != 2 {
		t.Errorf("Fields len = %d; want 2", len(m.Fields))
	}
}

func TestMap_structHandle(t *testing.T) {
	r := ref("System.Drawing.Point", metacli.TypeRefStruct)
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindRecord {
		t.Errorf("Kind = %s; want record", m.Kind)
	}
	if m.FFIRepr() != "nint" {
		t.Errorf("FFIRepr = %q; want nint", m.FFIRepr())
	}
}

func TestMap_classHandle(t *testing.T) {
	r := ref("Newtonsoft.Json.JsonSerializer", metacli.TypeRefClass)
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindHandle {
		t.Errorf("Kind = %s; want handle", m.Kind)
	}
	if m.FFIRepr() != "nint" {
		t.Errorf("FFIRepr = %q; want nint", m.FFIRepr())
	}
	if m.HandleName == "" {
		t.Error("HandleName should be set")
	}
}

func TestMap_interfaceHandle(t *testing.T) {
	r := ref("System.IDisposable", metacli.TypeRefInterface)
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindHandle {
		t.Errorf("Kind = %s; want handle", m.Kind)
	}
}

func TestMap_enum(t *testing.T) {
	r := ref("System.DayOfWeek", metacli.TypeRefEnum)
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindEnum {
		t.Errorf("Kind = %s; want enum", m.Kind)
	}
	if m.FFIRepr() != "int" {
		t.Errorf("FFIRepr = %q; want int", m.FFIRepr())
	}
}

func TestMap_byteArray(t *testing.T) {
	r := metacli.TypeRef{FullName: "System.Byte", Kind: metacli.TypeRefArray, ArrayRank: 1}
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindBytes {
		t.Errorf("Kind = %s; want bytes", m.Kind)
	}
}

// Refusal tests.

func TestMap_refuse_pointer(t *testing.T) {
	r := ref("System.Int32*", metacli.TypeRefPointer)
	_, sr, _ := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipPointer {
		t.Errorf("expected SkipPointer, got %s", sr)
	}
}

func TestMap_refuse_byRef(t *testing.T) {
	r := ref("System.Int32", metacli.TypeRefByRef)
	_, sr, _ := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipRefType {
		t.Errorf("expected SkipRefType, got %s", sr)
	}
}

func TestMap_refuse_dynamic(t *testing.T) {
	r := ref("dynamic", metacli.TypeRefDynamic)
	_, sr, _ := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipDynamic {
		t.Errorf("expected SkipDynamic, got %s", sr)
	}
}

func TestMap_refuse_delegate(t *testing.T) {
	r := ref("System.Action", metacli.TypeRefDelegate)
	_, sr, _ := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipDelegate {
		t.Errorf("expected SkipDelegate, got %s", sr)
	}
}

func TestMap_refuse_genericParam(t *testing.T) {
	r := ref("T", metacli.TypeRefGenericParam)
	_, sr, _ := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipGeneric {
		t.Errorf("expected SkipGeneric, got %s", sr)
	}
}

func TestMap_refuse_span(t *testing.T) {
	r := genericRef("System.Span`1", ref("System.Byte", metacli.TypeRefPrimitive))
	_, sr, _ := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipSpan {
		t.Errorf("expected SkipSpan, got %s", sr)
	}
}

func TestMap_refuse_readOnlySpan(t *testing.T) {
	r := genericRef("System.ReadOnlySpan`1", ref("System.Char", metacli.TypeRefPrimitive))
	_, sr, _ := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipSpan {
		t.Errorf("expected SkipSpan, got %s", sr)
	}
}

func TestMap_refuse_valueTask(t *testing.T) {
	r := genericRef("System.Threading.Tasks.ValueTask`1", ref("System.Int32", metacli.TypeRefPrimitive))
	_, sr, _ := typemap.Map(r, typemap.DirectionOut)
	if sr != errors.SkipValueTask {
		t.Errorf("expected SkipValueTask, got %s", sr)
	}
}

func TestMap_refuse_actionGeneric(t *testing.T) {
	r := genericRef("System.Action`1", ref("System.String", metacli.TypeRefPrimitive))
	_, sr, _ := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipDelegate {
		t.Errorf("expected SkipDelegate, got %s", sr)
	}
}

func TestMap_refuse_funcGeneric(t *testing.T) {
	r := genericRef("System.Func`2",
		ref("System.String", metacli.TypeRefPrimitive),
		ref("System.Int32", metacli.TypeRefPrimitive),
	)
	_, sr, _ := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipDelegate {
		t.Errorf("expected SkipDelegate, got %s", sr)
	}
}

// Mapping helpers.

func TestMapping_IsScalar(t *testing.T) {
	scalars := []typemap.Kind{
		typemap.KindBool, typemap.KindByte, typemap.KindInt, typemap.KindInt64,
		typemap.KindUInt, typemap.KindUInt64, typemap.KindFloat, typemap.KindFloat64,
		typemap.KindChar, typemap.KindUnit, typemap.KindEnum,
	}
	for _, k := range scalars {
		m := &typemap.Mapping{Kind: k}
		if !m.IsScalar() {
			t.Errorf("Kind %s should be scalar", k)
		}
	}
	nonScalars := []typemap.Kind{
		typemap.KindString, typemap.KindBytes, typemap.KindList, typemap.KindMap,
		typemap.KindHandle, typemap.KindRecord,
	}
	for _, k := range nonScalars {
		m := &typemap.Mapping{Kind: k}
		if m.IsScalar() {
			t.Errorf("Kind %s should not be scalar", k)
		}
	}
}

func TestMap_listByte_becomesBytes(t *testing.T) {
	r := genericRef("System.Collections.Generic.List`1", ref("System.Byte", metacli.TypeRefPrimitive))
	m, sr, det := typemap.Map(r, typemap.DirectionIn)
	if sr != errors.SkipUnknown || det != "" {
		t.Fatalf("refused: %s %s", sr, det)
	}
	if m.Kind != typemap.KindBytes {
		t.Errorf("List<byte> should map to KindBytes, got %s", m.Kind)
	}
}

func TestMap_mochiType_list(t *testing.T) {
	r := genericRef("System.Collections.Generic.List`1", ref("System.Int32", metacli.TypeRefPrimitive))
	m, _, _ := typemap.Map(r, typemap.DirectionIn)
	if m.MochiType != "list<int>" {
		t.Errorf("MochiType = %q; want %q", m.MochiType, "list<int>")
	}
}

func TestMap_mochiType_map(t *testing.T) {
	r := genericRef("System.Collections.Generic.Dictionary`2",
		ref("System.String", metacli.TypeRefPrimitive),
		ref("System.Boolean", metacli.TypeRefPrimitive),
	)
	m, _, _ := typemap.Map(r, typemap.DirectionIn)
	if m.MochiType != "map<string,bool>" {
		t.Errorf("MochiType = %q; want map<string,bool>", m.MochiType)
	}
}

func TestMap_mochiType_option(t *testing.T) {
	r := nullableRef(ref("System.Double", metacli.TypeRefPrimitive))
	m, _, _ := typemap.Map(r, typemap.DirectionIn)
	if m.MochiType != "?float64" {
		t.Errorf("MochiType = %q; want ?float64", m.MochiType)
	}
}
