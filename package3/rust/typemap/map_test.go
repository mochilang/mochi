package typemap

import (
	"testing"

	"mochi/package3/rust/errors"
	"mochi/package3/rust/rustdoc"
)

// primT builds a primitive Type for tests.
func primT(name string) rustdoc.Type {
	return rustdoc.Type{Primitive: name}
}

// pathT builds a resolved_path Type with optional type args.
func pathT(path string, args ...rustdoc.Type) rustdoc.Type {
	pt := &rustdoc.PathType{ID: "id:" + path, Path: path}
	if len(args) > 0 {
		ga := &rustdoc.GenericArgs{AngleBracketed: &rustdoc.AngleBracketedArgs{}}
		for i := range args {
			a := args[i]
			ga.AngleBracketed.Args = append(ga.AngleBracketed.Args,
				rustdoc.GenericArg{Type: &a})
		}
		pt.Args = ga
	}
	return rustdoc.Type{ResolvedPath: pt}
}

// borrowT builds a borrowed_ref Type.
func borrowT(lifetime string, mut bool, inner rustdoc.Type) rustdoc.Type {
	return rustdoc.Type{BorrowedRef: &rustdoc.BorrowedRefType{
		Lifetime:  lifetime,
		IsMutable: mut,
		Type:      inner,
	}}
}

func sliceT(elem rustdoc.Type) rustdoc.Type {
	return rustdoc.Type{Slice: &elem}
}

func arrayT(elem rustdoc.Type, length string) rustdoc.Type {
	return rustdoc.Type{Array: &rustdoc.ArrayType{Type: elem, Len: length}}
}

func tupleT(elems ...rustdoc.Type) rustdoc.Type {
	if len(elems) == 0 {
		// Preserve a non-nil empty slice so the unit type round-trips.
		return rustdoc.Type{Tuple: []rustdoc.Type{}}
	}
	return rustdoc.Type{Tuple: elems}
}

// expectMapping asserts a successful Map call.
func expectMapping(t *testing.T, name string, m *Mapping, sr errors.SkipReason, det string) *Mapping {
	t.Helper()
	if sr != errors.SkipUnknown {
		t.Fatalf("%s: SkipReason = %s; want SkipUnknown (success), detail=%q", name, sr, det)
	}
	if det != "" {
		t.Fatalf("%s: detail = %q; want empty (success)", name, det)
	}
	if m == nil {
		t.Fatalf("%s: nil Mapping on success", name)
	}
	return m
}

// expectSkip asserts a failed Map call with a specific reason.
func expectSkip(t *testing.T, name string, want errors.SkipReason, sr errors.SkipReason, det string) {
	t.Helper()
	if sr != want {
		t.Errorf("%s: SkipReason = %s; want %s (detail=%q)", name, sr, want, det)
	}
	if det == "" {
		t.Errorf("%s: empty detail on skip", name)
	}
}

func TestMapPrimitives(t *testing.T) {
	cases := []struct {
		rust string
		want Kind
	}{
		{"bool", KindBool},
		{"i8", KindInt},
		{"i16", KindInt},
		{"i32", KindInt},
		{"i64", KindInt64},
		{"i128", KindInt64},
		{"u8", KindByte},
		{"u16", KindUInt},
		{"u32", KindUInt},
		{"u64", KindUInt64},
		{"u128", KindUInt64},
		{"isize", KindInt64},
		{"usize", KindUInt64},
		{"f32", KindFloat},
		{"f64", KindFloat64},
		{"char", KindChar},
		{"str", KindString},
		{"never", KindUnit},
		{"!", KindUnit},
	}
	for _, c := range cases {
		m, sr, det := Map(primT(c.rust), DirectionIn)
		got := expectMapping(t, "primitive "+c.rust, m, sr, det)
		if got.Kind != c.want {
			t.Errorf("primitive %s -> %s; want %s", c.rust, got.Kind, c.want)
		}
	}
}

func TestMapPrimitiveUnknown(t *testing.T) {
	_, sr, det := Map(primT("future_prim_42"), DirectionIn)
	expectSkip(t, "unknown primitive", errors.SkipUnknown, sr, det)
}

func TestMapUnit(t *testing.T) {
	m, sr, det := Map(primT("()"), DirectionIn)
	mp := expectMapping(t, "unit", m, sr, det)
	if mp.Kind != KindUnit {
		t.Errorf("() -> %s; want unit", mp.Kind)
	}
}

func TestMapString(t *testing.T) {
	m, sr, det := Map(pathT("std::string::String"), DirectionIn)
	mp := expectMapping(t, "String", m, sr, det)
	if mp.Kind != KindString {
		t.Errorf("String -> %s; want string", mp.Kind)
	}
}

func TestMapVecBytes(t *testing.T) {
	m, sr, det := Map(pathT("std::vec::Vec", primT("u8")), DirectionIn)
	mp := expectMapping(t, "Vec<u8>", m, sr, det)
	if mp.Kind != KindBytes {
		t.Errorf("Vec<u8> -> %s; want bytes", mp.Kind)
	}
}

func TestMapVecList(t *testing.T) {
	m, sr, det := Map(pathT("alloc::vec::Vec", primT("i64")), DirectionIn)
	mp := expectMapping(t, "Vec<i64>", m, sr, det)
	if mp.Kind != KindList || mp.Elem == nil || mp.Elem.Kind != KindInt64 {
		t.Errorf("Vec<i64> -> %+v", mp)
	}
}

func TestMapVecNested(t *testing.T) {
	inner := pathT("std::vec::Vec", primT("i32"))
	m, sr, det := Map(pathT("std::vec::Vec", inner), DirectionIn)
	mp := expectMapping(t, "Vec<Vec<i32>>", m, sr, det)
	if mp.Kind != KindList || mp.Elem == nil || mp.Elem.Kind != KindList {
		t.Fatalf("Vec<Vec<i32>> -> %+v", mp)
	}
	if mp.MochiType() != "list[list[int]]" {
		t.Errorf("Vec<Vec<i32>> MochiType = %q", mp.MochiType())
	}
}

func TestMapOption(t *testing.T) {
	m, sr, det := Map(pathT("core::option::Option", primT("i32")), DirectionIn)
	mp := expectMapping(t, "Option<i32>", m, sr, det)
	if mp.Kind != KindOption || mp.Elem == nil || mp.Elem.Kind != KindInt {
		t.Errorf("Option<i32> -> %+v", mp)
	}
	if mp.MochiType() != "?int" {
		t.Errorf("Option<i32> MochiType = %q", mp.MochiType())
	}
}

func TestMapResult(t *testing.T) {
	m, sr, det := Map(pathT("core::result::Result", pathT("std::string::String"), primT("i32")), DirectionIn)
	mp := expectMapping(t, "Result<String,i32>", m, sr, det)
	if mp.Kind != KindResult || mp.OK.Kind != KindString || mp.Err.Kind != KindInt {
		t.Errorf("Result -> %+v", mp)
	}
	if mp.MochiType() != "result[string,int]" {
		t.Errorf("Result MochiType = %q", mp.MochiType())
	}
}

func TestMapHashMap(t *testing.T) {
	m, sr, det := Map(pathT("std::collections::HashMap", pathT("std::string::String"), primT("i64")), DirectionIn)
	mp := expectMapping(t, "HashMap<String,i64>", m, sr, det)
	if mp.Kind != KindMap {
		t.Fatalf("HashMap -> %+v", mp)
	}
	if mp.Key.Kind != KindString || mp.Value.Kind != KindInt64 {
		t.Errorf("HashMap K/V = %s/%s", mp.Key.Kind, mp.Value.Kind)
	}
	if mp.MochiType() != "map[string]int64" {
		t.Errorf("HashMap MochiType = %q", mp.MochiType())
	}
}

func TestMapBTreeMap(t *testing.T) {
	m, sr, det := Map(pathT("std::collections::BTreeMap", primT("i32"), primT("bool")), DirectionIn)
	mp := expectMapping(t, "BTreeMap<i32,bool>", m, sr, det)
	if mp.Kind != KindMap {
		t.Errorf("BTreeMap -> %+v", mp)
	}
}

func TestMapHashSet(t *testing.T) {
	m, sr, det := Map(pathT("std::collections::HashSet", primT("i32")), DirectionIn)
	mp := expectMapping(t, "HashSet<i32>", m, sr, det)
	if mp.Kind != KindList || mp.Elem.Kind != KindInt {
		t.Errorf("HashSet -> %+v", mp)
	}
}

func TestMapBTreeSet(t *testing.T) {
	m, sr, det := Map(pathT("std::collections::BTreeSet", primT("u64")), DirectionIn)
	mp := expectMapping(t, "BTreeSet<u64>", m, sr, det)
	if mp.Kind != KindList {
		t.Errorf("BTreeSet -> %+v", mp)
	}
}

func TestMapBoxTransparent(t *testing.T) {
	m, sr, det := Map(pathT("alloc::boxed::Box", primT("i64")), DirectionIn)
	mp := expectMapping(t, "Box<i64>", m, sr, det)
	if mp.Kind != KindInt64 {
		t.Errorf("Box<i64> -> %s; want int64 (transparent)", mp.Kind)
	}
}

func TestMapRcArcTransparent(t *testing.T) {
	m, sr, det := Map(pathT("alloc::rc::Rc", pathT("std::string::String")), DirectionIn)
	mp := expectMapping(t, "Rc<String>", m, sr, det)
	if mp.Kind != KindString {
		t.Errorf("Rc<String> -> %s; want string", mp.Kind)
	}
	m2, sr2, det2 := Map(pathT("alloc::sync::Arc", primT("u32")), DirectionIn)
	mp2 := expectMapping(t, "Arc<u32>", m2, sr2, det2)
	if mp2.Kind != KindUInt {
		t.Errorf("Arc<u32> -> %s; want uint", mp2.Kind)
	}
}

func TestMapCowSkip(t *testing.T) {
	_, sr, det := Map(pathT("std::borrow::Cow", primT("str")), DirectionIn)
	expectSkip(t, "Cow<str>", errors.SkipCow, sr, det)
}

func TestMapOsStringSkip(t *testing.T) {
	for _, name := range []string{"OsString", "OsStr", "PathBuf", "Path", "CString", "CStr"} {
		_, sr, det := Map(pathT("std::ffi::"+name), DirectionIn)
		expectSkip(t, name, errors.SkipOsString, sr, det)
	}
}

func TestMapPinSkip(t *testing.T) {
	_, sr, det := Map(pathT("core::pin::Pin", pathT("alloc::boxed::Box", primT("i32"))), DirectionIn)
	expectSkip(t, "Pin", errors.SkipPin, sr, det)
}

func TestMapTuple(t *testing.T) {
	m, sr, det := Map(tupleT(primT("i32"), pathT("std::string::String")), DirectionIn)
	mp := expectMapping(t, "(i32, String)", m, sr, det)
	if mp.Kind != KindTuple || len(mp.Fields) != 2 {
		t.Errorf("tuple -> %+v", mp)
	}
	if mp.MochiType() != "tuple[int,string]" {
		t.Errorf("tuple MochiType = %q", mp.MochiType())
	}
}

func TestMapEmptyTupleIsUnit(t *testing.T) {
	m, sr, det := Map(tupleT(), DirectionIn)
	mp := expectMapping(t, "()", m, sr, det)
	if mp.Kind != KindUnit {
		t.Errorf("empty tuple -> %s; want unit", mp.Kind)
	}
}

func TestMapSliceBytes(t *testing.T) {
	m, sr, det := Map(sliceT(primT("u8")), DirectionIn)
	mp := expectMapping(t, "[u8]", m, sr, det)
	if mp.Kind != KindBytes {
		t.Errorf("[u8] -> %s; want bytes", mp.Kind)
	}
}

func TestMapSliceList(t *testing.T) {
	m, sr, det := Map(sliceT(primT("i32")), DirectionIn)
	mp := expectMapping(t, "[i32]", m, sr, det)
	if mp.Kind != KindList || mp.Elem.Kind != KindInt {
		t.Errorf("[i32] -> %+v", mp)
	}
}

func TestMapArrayBytes(t *testing.T) {
	m, sr, det := Map(arrayT(primT("u8"), "32"), DirectionIn)
	mp := expectMapping(t, "[u8;32]", m, sr, det)
	if mp.Kind != KindBytes {
		t.Errorf("[u8;32] -> %s; want bytes", mp.Kind)
	}
}

func TestMapArrayList(t *testing.T) {
	m, sr, det := Map(arrayT(primT("i32"), "4"), DirectionIn)
	mp := expectMapping(t, "[i32;4]", m, sr, det)
	if mp.Kind != KindList {
		t.Errorf("[i32;4] -> %s; want list", mp.Kind)
	}
}

func TestMapBorrowedRefStrInput(t *testing.T) {
	m, sr, det := Map(borrowT("'a", false, primT("str")), DirectionIn)
	mp := expectMapping(t, "&'a str input", m, sr, det)
	if mp.Kind != KindString {
		t.Errorf("&str -> %s; want string", mp.Kind)
	}
}

func TestMapBorrowedRefOutputNonStatic(t *testing.T) {
	_, sr, det := Map(borrowT("'a", false, primT("str")), DirectionOut)
	expectSkip(t, "&'a str output", errors.SkipLifetime, sr, det)
}

func TestMapBorrowedRefOutputStatic(t *testing.T) {
	m, sr, det := Map(borrowT("'static", false, primT("str")), DirectionOut)
	mp := expectMapping(t, "&'static str output", m, sr, det)
	if mp.Kind != KindString {
		t.Errorf("&'static str output -> %s", mp.Kind)
	}
}

func TestMapMutBorrowSkip(t *testing.T) {
	_, sr, det := Map(borrowT("'a", true, primT("i32")), DirectionIn)
	if sr == errors.SkipUnknown && det == "" {
		t.Errorf("&mut should not produce a successful mapping")
	}
}

func TestMapRawPointerSkip(t *testing.T) {
	rp := rustdoc.Type{RawPointer: &rustdoc.RawPointerType{IsMutable: false, Type: primT("u8")}}
	_, sr, det := Map(rp, DirectionIn)
	expectSkip(t, "*const u8", errors.SkipRawPointer, sr, det)
}

func TestMapGenericSkip(t *testing.T) {
	_, sr, det := Map(rustdoc.Type{Generic: "T"}, DirectionIn)
	expectSkip(t, "generic T", errors.SkipGeneric, sr, det)
}

func TestMapDynTraitSkip(t *testing.T) {
	dt := rustdoc.Type{DynTrait: &rustdoc.DynTraitType{
		Traits: []rustdoc.PolyTrait{{Trait: rustdoc.PathType{Path: "core::fmt::Debug"}}},
	}}
	_, sr, det := Map(dt, DirectionIn)
	expectSkip(t, "dyn Trait", errors.SkipDynTrait, sr, det)
}

func TestMapImplTraitSkip(t *testing.T) {
	it := rustdoc.Type{ImplTrait: []rustdoc.GenericBound{
		{TraitBound: &rustdoc.TraitBound{Trait: rustdoc.PathType{Path: "core::iter::Iterator"}}},
	}}
	_, sr, det := Map(it, DirectionOut)
	expectSkip(t, "impl Trait", errors.SkipImplTrait, sr, det)
}

func TestMapQualifiedPathSkip(t *testing.T) {
	qp := rustdoc.Type{QualifiedPath: &rustdoc.QualifiedPathType{
		Name:     "Item",
		SelfType: primT("u8"),
	}}
	_, sr, det := Map(qp, DirectionIn)
	expectSkip(t, "<T as Trait>::Item", errors.SkipQualifiedPath, sr, det)
}

func TestMapFunctionPointerSkip(t *testing.T) {
	fp := rustdoc.Type{FunctionPointer: &rustdoc.FunctionPointer{}}
	_, sr, det := Map(fp, DirectionIn)
	if sr != errors.SkipUnknown || det == "" {
		t.Errorf("function_pointer: sr=%s det=%q", sr, det)
	}
}

func TestMapInferSkip(t *testing.T) {
	_, sr, det := Map(rustdoc.Type{Infer: true}, DirectionIn)
	if sr != errors.SkipUnknown || det == "" {
		t.Errorf("infer: sr=%s det=%q", sr, det)
	}
}

func TestMapPatSkip(t *testing.T) {
	_, sr, det := Map(rustdoc.Type{Pat: &rustdoc.PatType{Base: primT("u8")}}, DirectionIn)
	if sr != errors.SkipUnknown || det == "" {
		t.Errorf("pat: sr=%s det=%q", sr, det)
	}
}

func TestMapUnknownVariant(t *testing.T) {
	_, sr, det := Map(rustdoc.Type{Unknown: "weird_future_kind"}, DirectionIn)
	if sr != errors.SkipUnknown || det == "" {
		t.Errorf("unknown variant: sr=%s det=%q", sr, det)
	}
}

func TestMapUserStructHandle(t *testing.T) {
	m, sr, det := Map(pathT("tokio::sync::Mutex", primT("i32")), DirectionIn)
	mp := expectMapping(t, "tokio::sync::Mutex", m, sr, det)
	if mp.Kind != KindStruct {
		t.Errorf("user path -> %s; want struct (opaque handle)", mp.Kind)
	}
	if mp.PathName != "tokio::sync::Mutex" || mp.PathID == "" {
		t.Errorf("user path PathName=%q PathID=%q", mp.PathName, mp.PathID)
	}
}

func TestMapEmptyType(t *testing.T) {
	_, sr, det := Map(rustdoc.Type{}, DirectionIn)
	if sr != errors.SkipUnknown || det == "" {
		t.Errorf("empty: sr=%s det=%q", sr, det)
	}
}

func TestMapPropagatesNestedSkip(t *testing.T) {
	// Vec<dyn Trait> -> nested SkipDynTrait
	dt := rustdoc.Type{DynTrait: &rustdoc.DynTraitType{
		Traits: []rustdoc.PolyTrait{{Trait: rustdoc.PathType{Path: "core::fmt::Debug"}}},
	}}
	_, sr, det := Map(pathT("std::vec::Vec", dt), DirectionIn)
	expectSkip(t, "Vec<dyn Trait>", errors.SkipDynTrait, sr, det)
}

func TestMapPropagatesTupleSkip(t *testing.T) {
	// (i32, *const u8) -> SkipRawPointer
	rp := rustdoc.Type{RawPointer: &rustdoc.RawPointerType{Type: primT("u8")}}
	_, sr, det := Map(tupleT(primT("i32"), rp), DirectionIn)
	if sr != errors.SkipRawPointer || det == "" {
		t.Errorf("(i32, *const u8): sr=%s det=%q", sr, det)
	}
}

func TestLastSegment(t *testing.T) {
	cases := []struct{ in, want string }{
		{"std::vec::Vec", "Vec"},
		{"Vec", "Vec"},
		{"core::option::Option", "Option"},
		{"", ""},
	}
	for _, c := range cases {
		if got := LastSegment(c.in); got != c.want {
			t.Errorf("LastSegment(%q) = %q; want %q", c.in, got, c.want)
		}
	}
}
