package monomorphise

import (
	"strings"
	"testing"

	"mochi/package3/rust/rustdoc"
)

func TestValidateAcceptsWellFormed(t *testing.T) {
	s := Spec{Entries: []Entry{
		{Item: "serde_json::from_str", TypeArgs: map[string]string{"T": "MyStruct"}},
		{Item: "Vec::new", TypeArgs: map[string]string{"T": "i64"}},
	}}
	if err := s.Validate(); err != nil {
		t.Fatalf("Validate: %v", err)
	}
}

func TestValidateRejectsEmptyItem(t *testing.T) {
	s := Spec{Entries: []Entry{
		{Item: "", TypeArgs: map[string]string{"T": "i64"}},
	}}
	err := s.Validate()
	if err == nil {
		t.Fatalf("Validate: want error for empty item")
	}
	if !strings.Contains(err.Error(), "empty item") {
		t.Fatalf("Validate: error %q does not mention empty item", err.Error())
	}
}

func TestValidateRejectsNoTypeArgs(t *testing.T) {
	s := Spec{Entries: []Entry{
		{Item: "Vec::new", TypeArgs: map[string]string{}},
	}}
	err := s.Validate()
	if err == nil {
		t.Fatalf("Validate: want error for empty type-args")
	}
	if !strings.Contains(err.Error(), "no type-args") {
		t.Fatalf("Validate: error %q does not mention no type-args", err.Error())
	}
}

func TestValidateRejectsEmptyKey(t *testing.T) {
	s := Spec{Entries: []Entry{
		{Item: "Vec::new", TypeArgs: map[string]string{"   ": "i64"}},
	}}
	if err := s.Validate(); err == nil {
		t.Fatalf("Validate: want error for empty type-parameter name")
	}
}

func TestValidateRejectsEmptyValue(t *testing.T) {
	s := Spec{Entries: []Entry{
		{Item: "Vec::new", TypeArgs: map[string]string{"T": "   "}},
	}}
	if err := s.Validate(); err == nil {
		t.Fatalf("Validate: want error for empty substitution")
	}
}

func TestLookupReturnsAllMatches(t *testing.T) {
	s := Spec{Entries: []Entry{
		{Item: "serde_json::from_str", TypeArgs: map[string]string{"T": "A"}},
		{Item: "Vec::new", TypeArgs: map[string]string{"T": "i64"}},
		{Item: "serde_json::from_str", TypeArgs: map[string]string{"T": "B"}},
	}}
	got := s.Lookup("serde_json::from_str")
	if len(got) != 2 {
		t.Fatalf("Lookup: want 2 entries, got %d", len(got))
	}
	if got[0].TypeArgs["T"] != "A" || got[1].TypeArgs["T"] != "B" {
		t.Fatalf("Lookup: want [A,B] iteration order, got [%s,%s]", got[0].TypeArgs["T"], got[1].TypeArgs["T"])
	}
}

func TestLookupMiss(t *testing.T) {
	s := Spec{Entries: []Entry{
		{Item: "Vec::new", TypeArgs: map[string]string{"T": "i64"}},
	}}
	got := s.Lookup("HashMap::new")
	if len(got) != 0 {
		t.Fatalf("Lookup: want empty, got %v", got)
	}
}

func TestExternNameSingleArg(t *testing.T) {
	got := ExternName("serde_json", "serde_json::from_str", map[string]string{"T": "MyStruct"})
	want := "mochi_serde_json_from_str_of_t_mystruct"
	if got != want {
		t.Fatalf("ExternName:\n  got  %q\n  want %q", got, want)
	}
}

func TestExternNameMultiArgSorted(t *testing.T) {
	a := ExternName("collections", "HashMap::new", map[string]string{"K": "String", "V": "i64"})
	b := ExternName("collections", "HashMap::new", map[string]string{"V": "i64", "K": "String"})
	if a != b {
		t.Fatalf("ExternName: map insertion order leaked\n  a=%q\n  b=%q", a, b)
	}
	if !strings.Contains(a, "_of_k_string_v_i64") {
		t.Fatalf("ExternName: want sorted suffix, got %q", a)
	}
}

func TestExternNameSanitisesNonAlphanumeric(t *testing.T) {
	got := ExternName("smallvec", "Vec::new", map[string]string{"T": "Box<i64>"})
	if strings.ContainsAny(got, "<>:") {
		t.Fatalf("ExternName: unsanitised symbol %q", got)
	}
}

func TestExternNameNoTypeArgs(t *testing.T) {
	got := ExternName("smallvec", "Vec::new", nil)
	if strings.Contains(got, "_of") {
		t.Fatalf("ExternName: empty type-args should not emit _of suffix, got %q", got)
	}
}

func TestCallSiteTurbofishSingle(t *testing.T) {
	got := CallSite("serde_json::from_str", map[string]string{"T": "MyStruct"})
	want := "serde_json::from_str::<MyStruct>"
	if got != want {
		t.Fatalf("CallSite:\n  got  %q\n  want %q", got, want)
	}
}

func TestCallSiteTurbofishMultiSortedByKey(t *testing.T) {
	got := CallSite("collections::HashMap::new", map[string]string{"V": "i64", "K": "String"})
	want := "collections::HashMap::new::<String, i64>"
	if got != want {
		t.Fatalf("CallSite:\n  got  %q\n  want %q", got, want)
	}
}

func TestCallSiteNoTypeArgsReturnsPlain(t *testing.T) {
	got := CallSite("serde_json::from_str", nil)
	if got != "serde_json::from_str" {
		t.Fatalf("CallSite: want plain path, got %q", got)
	}
}

func TestParseTOMLEntriesSingleEntry(t *testing.T) {
	body := `[ { item = "serde_json::from_str", T = "MyStruct" } ]`
	spec, err := ParseTOMLEntries(body)
	if err != nil {
		t.Fatalf("ParseTOMLEntries: %v", err)
	}
	if len(spec.Entries) != 1 {
		t.Fatalf("ParseTOMLEntries: want 1 entry, got %d", len(spec.Entries))
	}
	e := spec.Entries[0]
	if e.Item != "serde_json::from_str" {
		t.Fatalf("ParseTOMLEntries: item = %q", e.Item)
	}
	if e.TypeArgs["T"] != "MyStruct" {
		t.Fatalf("ParseTOMLEntries: T = %q", e.TypeArgs["T"])
	}
}

func TestParseTOMLEntriesMultiple(t *testing.T) {
	body := `[
        { item = "serde_json::from_str", T = "MyStruct" },
        { item = "Vec::new", T = "i64" },
    ]`
	spec, err := ParseTOMLEntries(body)
	if err != nil {
		t.Fatalf("ParseTOMLEntries: %v", err)
	}
	if len(spec.Entries) != 2 {
		t.Fatalf("ParseTOMLEntries: want 2 entries, got %d", len(spec.Entries))
	}
	if spec.Entries[0].Item != "serde_json::from_str" {
		t.Fatalf("ParseTOMLEntries[0]: %q", spec.Entries[0].Item)
	}
	if spec.Entries[1].Item != "Vec::new" {
		t.Fatalf("ParseTOMLEntries[1]: %q", spec.Entries[1].Item)
	}
}

func TestParseTOMLEntriesAcceptsStrippedBrackets(t *testing.T) {
	body := `{ item = "Vec::new", T = "i64" }`
	spec, err := ParseTOMLEntries(body)
	if err != nil {
		t.Fatalf("ParseTOMLEntries: %v", err)
	}
	if len(spec.Entries) != 1 {
		t.Fatalf("ParseTOMLEntries: want 1 entry, got %d", len(spec.Entries))
	}
}

func TestParseTOMLEntriesRejectsUnquoted(t *testing.T) {
	body := `[ { item = serde_json, T = "MyStruct" } ]`
	if _, err := ParseTOMLEntries(body); err == nil {
		t.Fatalf("ParseTOMLEntries: want error for unquoted value")
	}
}

func TestParseTOMLEntriesRejectsUnbalanced(t *testing.T) {
	body := `[ { item = "Vec::new", T = "i64" ]`
	if _, err := ParseTOMLEntries(body); err == nil {
		t.Fatalf("ParseTOMLEntries: want error for unbalanced braces")
	}
}

func TestParseTOMLEntriesEmpty(t *testing.T) {
	spec, err := ParseTOMLEntries("[]")
	if err != nil {
		t.Fatalf("ParseTOMLEntries(empty): %v", err)
	}
	if len(spec.Entries) != 0 {
		t.Fatalf("ParseTOMLEntries(empty): want 0 entries, got %d", len(spec.Entries))
	}
}

func TestExternNameByteStableAcrossRuns(t *testing.T) {
	args := map[string]string{"K": "String", "V": "i64", "T": "MyStruct"}
	first := ExternName("crate", "Module::fn", args)
	for i := 0; i < 64; i++ {
		got := ExternName("crate", "Module::fn", args)
		if got != first {
			t.Fatalf("ExternName not byte-stable: %q vs %q", first, got)
		}
	}
}

func TestSubstituteReplacesGenericWithPrimitive(t *testing.T) {
	in := rustdoc.Type{Generic: "T"}
	out := Substitute(in, map[string]string{"T": "i64"})
	if out.Primitive != "i64" {
		t.Fatalf("Substitute: want primitive i64, got %+v", out)
	}
}

func TestSubstituteReplacesGenericWithPath(t *testing.T) {
	in := rustdoc.Type{Generic: "T"}
	out := Substitute(in, map[string]string{"T": "my_crate::MyStruct"})
	if out.ResolvedPath == nil || out.ResolvedPath.Path != "my_crate::MyStruct" {
		t.Fatalf("Substitute: want resolved_path, got %+v", out)
	}
}

func TestSubstituteWalksTuple(t *testing.T) {
	in := rustdoc.Type{Tuple: []rustdoc.Type{
		{Generic: "T"},
		{Primitive: "bool"},
	}}
	out := Substitute(in, map[string]string{"T": "i64"})
	if out.Tuple[0].Primitive != "i64" {
		t.Fatalf("Substitute tuple[0]: %+v", out.Tuple[0])
	}
	if out.Tuple[1].Primitive != "bool" {
		t.Fatalf("Substitute tuple[1] mutated: %+v", out.Tuple[1])
	}
}

func TestSubstituteWalksVecArg(t *testing.T) {
	tArg := rustdoc.Type{Generic: "T"}
	in := rustdoc.Type{ResolvedPath: &rustdoc.PathType{
		Path: "Vec",
		Args: &rustdoc.GenericArgs{AngleBracketed: &rustdoc.AngleBracketedArgs{
			Args: []rustdoc.GenericArg{{Type: &tArg}},
		}},
	}}
	out := Substitute(in, map[string]string{"T": "i64"})
	got := out.ResolvedPath.Args.AngleBracketed.Args[0].Type
	if got == nil || got.Primitive != "i64" {
		t.Fatalf("Substitute Vec<T>: arg = %+v", got)
	}
}

func TestSubstituteWalksBorrowedRef(t *testing.T) {
	in := rustdoc.Type{BorrowedRef: &rustdoc.BorrowedRefType{
		Type: rustdoc.Type{Generic: "T"},
	}}
	out := Substitute(in, map[string]string{"T": "str"})
	if out.BorrowedRef.Type.Primitive != "str" {
		t.Fatalf("Substitute &T: %+v", out.BorrowedRef.Type)
	}
}

func TestSubstituteLeavesUnknownGenericIntact(t *testing.T) {
	in := rustdoc.Type{Generic: "U"}
	out := Substitute(in, map[string]string{"T": "i64"})
	if out.Generic != "U" {
		t.Fatalf("Substitute: unknown generic should remain, got %+v", out)
	}
}

func TestCallSiteByteStableAcrossRuns(t *testing.T) {
	args := map[string]string{"K": "String", "V": "i64", "T": "MyStruct"}
	first := CallSite("Module::fn", args)
	for i := 0; i < 64; i++ {
		got := CallSite("Module::fn", args)
		if got != first {
			t.Fatalf("CallSite not byte-stable: %q vs %q", first, got)
		}
	}
}
