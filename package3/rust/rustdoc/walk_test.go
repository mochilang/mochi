package rustdoc

import (
	"strings"
	"testing"

	"mochi/package3/rust/errors"
)

// hexlikeDoc is a hand-written rustdoc fixture modelled after the
// `hex` crate (one of the 24 fixture crates in the MEP-73 corpus).
// It contains:
//   - the crate root module
//   - a public `encode` function: fn(&[u8]) -> String
//   - a public `decode` function: fn(&str) -> Result<Vec<u8>, FromHexError>
//   - a public unit struct `FromHexError`
//   - a public enum `FromHexErrorKind` with three variants
//   - a public type alias `Bytes = Vec<u8>`
//   - a public const `MAX_LENGTH: usize = 1024`
//   - a generic fn `decode_to_slice<T>` that should produce a SkipGeneric
//   - an unsafe fn `as_str_unchecked` that should produce SkipExternFnUnsafe
//   - a non-public helper struct that must NOT appear in the surface
//   - a trait `FromHex` that should record a SkipTrait
const hexlikeDoc = `{
  "root": "0:0:0",
  "crate_version": "0.4.3",
  "includes_private": false,
  "index": {
    "0:0:0": {
      "id": "0:0:0", "crate_id": 0, "name": "hex", "visibility": "public",
      "inner": {"module": {"is_crate": true, "items": ["0:0:1","0:0:2","0:0:3","0:0:4","0:0:6","0:0:7","0:0:8","0:0:9","0:0:10","0:0:11"], "is_stripped": false}}
    },
    "0:0:1": {
      "id": "0:0:1", "crate_id": 0, "name": "encode", "visibility": "public",
      "inner": {"function": {
        "sig": {"inputs": [["bytes", {"borrowed_ref": {"is_mutable": false, "type": {"slice": {"primitive": "u8"}}}}]], "output": {"resolved_path": {"id":"std:String","path":"alloc::string::String"}}, "is_c_variadic": false},
        "generics": {"params": [], "where_predicates": []},
        "header": {"is_const": false, "is_unsafe": false, "is_async": false, "abi": "Rust"},
        "has_body": true
      }}
    },
    "0:0:2": {
      "id": "0:0:2", "crate_id": 0, "name": "decode", "visibility": "public",
      "inner": {"function": {
        "sig": {"inputs": [["s", {"borrowed_ref": {"is_mutable": false, "type": {"primitive": "str"}}}]], "output": {"resolved_path": {"id":"std:Result","path":"core::result::Result"}}, "is_c_variadic": false},
        "generics": {"params": [], "where_predicates": []},
        "header": {"is_const": false, "is_unsafe": false, "is_async": false, "abi": "Rust"},
        "has_body": true
      }}
    },
    "0:0:3": {
      "id": "0:0:3", "crate_id": 0, "name": "FromHexError", "visibility": "public",
      "inner": {"struct": {"kind": {"unit": {}}, "generics": {"params": [], "where_predicates": []}}}
    },
    "0:0:4": {
      "id": "0:0:4", "crate_id": 0, "name": "FromHexErrorKind", "visibility": "public",
      "inner": {"enum": {"generics": {"params": [], "where_predicates": []}, "variants": ["0:0:4a","0:0:4b","0:0:4c"], "has_stripped_variants": false}}
    },
    "0:0:4a": {
      "id": "0:0:4a", "crate_id": 0, "name": "InvalidLength", "visibility": "default",
      "inner": {"variant": {"kind": {"plain": {}}}}
    },
    "0:0:4b": {
      "id": "0:0:4b", "crate_id": 0, "name": "InvalidChar", "visibility": "default",
      "inner": {"variant": {"kind": {"tuple": ["0:0:4b0"]}}}
    },
    "0:0:4b0": {
      "id": "0:0:4b0", "crate_id": 0, "name": "0", "visibility": "default",
      "inner": {"struct_field": {"primitive": "u8"}}
    },
    "0:0:4c": {
      "id": "0:0:4c", "crate_id": 0, "name": "Custom", "visibility": "default",
      "inner": {"variant": {"kind": {"struct": {"fields": ["0:0:4c0"], "has_stripped_fields": false}}}}
    },
    "0:0:4c0": {
      "id": "0:0:4c0", "crate_id": 0, "name": "code", "visibility": "default",
      "inner": {"struct_field": {"primitive": "u32"}}
    },
    "0:0:6": {
      "id": "0:0:6", "crate_id": 0, "name": "Bytes", "visibility": "public",
      "inner": {"type_alias": {"type": {"resolved_path": {"id":"std:Vec","path":"alloc::vec::Vec"}}, "generics": {"params": [], "where_predicates": []}}}
    },
    "0:0:7": {
      "id": "0:0:7", "crate_id": 0, "name": "MAX_LENGTH", "visibility": "public",
      "inner": {"constant": {"type": {"primitive": "usize"}, "const": {"expr": "1024", "value": "1024", "is_literal": true}}}
    },
    "0:0:8": {
      "id": "0:0:8", "crate_id": 0, "name": "decode_to_slice", "visibility": "public",
      "inner": {"function": {
        "sig": {"inputs": [], "is_c_variadic": false},
        "generics": {"params": [{"name": "T", "kind": {"type": {"bounds": [], "synthetic": false}}}], "where_predicates": []},
        "header": {"is_const": false, "is_unsafe": false, "is_async": false, "abi": "Rust"},
        "has_body": true
      }}
    },
    "0:0:9": {
      "id": "0:0:9", "crate_id": 0, "name": "as_str_unchecked", "visibility": "public",
      "inner": {"function": {
        "sig": {"inputs": [], "is_c_variadic": false},
        "generics": {"params": [], "where_predicates": []},
        "header": {"is_const": false, "is_unsafe": true, "is_async": false, "abi": "Rust"},
        "has_body": true
      }}
    },
    "0:0:10": {
      "id": "0:0:10", "crate_id": 0, "name": "internal_helper", "visibility": "default",
      "inner": {"function": {
        "sig": {"inputs": [], "is_c_variadic": false},
        "generics": {"params": [], "where_predicates": []},
        "header": {"is_const": false, "is_unsafe": false, "is_async": false, "abi": "Rust"},
        "has_body": true
      }}
    },
    "0:0:11": {
      "id": "0:0:11", "crate_id": 0, "name": "FromHex", "visibility": "public",
      "inner": {"trait": {"is_auto": false, "is_unsafe": false, "items": [], "generics": {"params": [], "where_predicates": []}, "bounds": []}}
    }
  },
  "paths": {
    "0:0:0": {"crate_id": 0, "path": ["hex"], "kind": "module"}
  },
  "external_crates": {},
  "format_version": 39
}`

func parseHexDoc(t *testing.T) *Document {
	t.Helper()
	doc, err := Parse(strings.NewReader(hexlikeDoc))
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	return doc
}

func TestWalkHexLike(t *testing.T) {
	doc := parseHexDoc(t)
	surface, err := Walk(doc)
	if err != nil {
		t.Fatalf("Walk: %v", err)
	}
	if surface.CrateName != "hex" {
		t.Errorf("CrateName = %q", surface.CrateName)
	}
	if surface.CrateVersion != "0.4.3" {
		t.Errorf("CrateVersion = %q", surface.CrateVersion)
	}
	if surface.FormatVersion != 39 {
		t.Errorf("FormatVersion = %d", surface.FormatVersion)
	}
	if got := len(surface.Functions); got != 2 {
		t.Errorf("len(Functions) = %d; want 2 (encode, decode)", got)
	}
	if got := len(surface.Structs); got != 1 {
		t.Errorf("len(Structs) = %d; want 1 (FromHexError)", got)
	}
	if got := len(surface.Enums); got != 1 {
		t.Errorf("len(Enums) = %d; want 1 (FromHexErrorKind)", got)
	}
	if got := len(surface.TypeAliases); got != 1 {
		t.Errorf("len(TypeAliases) = %d; want 1 (Bytes)", got)
	}
	if got := len(surface.Constants); got != 1 {
		t.Errorf("len(Constants) = %d; want 1 (MAX_LENGTH)", got)
	}
	if got := len(surface.Traits); got != 1 {
		t.Errorf("len(Traits) = %d; want 1 (FromHex)", got)
	}
}

func TestWalkHexLikeSkipReasons(t *testing.T) {
	doc := parseHexDoc(t)
	surface, err := Walk(doc)
	if err != nil {
		t.Fatalf("Walk: %v", err)
	}
	reasonCounts := map[errors.SkipReason]int{}
	for _, sr := range surface.Skipped {
		reasonCounts[sr.Reason]++
	}
	if reasonCounts[errors.SkipGeneric] != 1 {
		t.Errorf("SkipGeneric count = %d; want 1 (decode_to_slice)", reasonCounts[errors.SkipGeneric])
	}
	if reasonCounts[errors.SkipExternFnUnsafe] != 1 {
		t.Errorf("SkipExternFnUnsafe count = %d; want 1 (as_str_unchecked)", reasonCounts[errors.SkipExternFnUnsafe])
	}
	if reasonCounts[errors.SkipTrait] != 1 {
		t.Errorf("SkipTrait count = %d; want 1 (FromHex)", reasonCounts[errors.SkipTrait])
	}
}

func TestWalkHexLikeFunctionEncode(t *testing.T) {
	doc := parseHexDoc(t)
	surface, err := Walk(doc)
	if err != nil {
		t.Fatalf("Walk: %v", err)
	}
	var encode *FunctionEntry
	for i := range surface.Functions {
		if surface.Functions[i].Path[len(surface.Functions[i].Path)-1] == "encode" {
			encode = &surface.Functions[i]
			break
		}
	}
	if encode == nil {
		t.Fatalf("no encode function in surface")
	}
	if len(encode.Inputs) != 1 {
		t.Errorf("encode inputs = %d; want 1", len(encode.Inputs))
	}
	if encode.Inputs[0].Name != "bytes" {
		t.Errorf("encode input name = %q", encode.Inputs[0].Name)
	}
	if encode.Inputs[0].Type.Kind() != "borrowed_ref" {
		t.Errorf("encode input type = %s; want borrowed_ref", encode.Inputs[0].Type.Kind())
	}
}

func TestWalkHexLikeEnumVariants(t *testing.T) {
	doc := parseHexDoc(t)
	surface, err := Walk(doc)
	if err != nil {
		t.Fatalf("Walk: %v", err)
	}
	if len(surface.Enums) != 1 {
		t.Fatalf("len(Enums) = %d; want 1", len(surface.Enums))
	}
	e := surface.Enums[0]
	if len(e.Variants) != 3 {
		t.Fatalf("variants = %d; want 3", len(e.Variants))
	}
	got := map[string]string{}
	for _, v := range e.Variants {
		got[v.Name] = v.Kind
	}
	if got["InvalidLength"] != "plain" {
		t.Errorf("InvalidLength kind = %q; want plain", got["InvalidLength"])
	}
	if got["InvalidChar"] != "tuple" {
		t.Errorf("InvalidChar kind = %q; want tuple", got["InvalidChar"])
	}
	if got["Custom"] != "struct" {
		t.Errorf("Custom kind = %q; want struct", got["Custom"])
	}
}

func TestWalkHexLikePathRendering(t *testing.T) {
	doc := parseHexDoc(t)
	surface, err := Walk(doc)
	if err != nil {
		t.Fatalf("Walk: %v", err)
	}
	for _, fn := range surface.Functions {
		if len(fn.Path) < 2 {
			t.Errorf("function path too short: %v", fn.Path)
		}
		if fn.Path[0] != "hex" {
			t.Errorf("function path[0] = %q; want hex", fn.Path[0])
		}
	}
	for _, sk := range surface.Skipped {
		if !strings.HasPrefix(sk.ItemPath, "hex::") {
			t.Errorf("skip path = %q; want hex:: prefix", sk.ItemPath)
		}
	}
}

func TestWalkHexLikeSkipsNonPublic(t *testing.T) {
	doc := parseHexDoc(t)
	surface, err := Walk(doc)
	if err != nil {
		t.Fatalf("Walk: %v", err)
	}
	for _, fn := range surface.Functions {
		if fn.Path[len(fn.Path)-1] == "internal_helper" {
			t.Errorf("internal_helper should not appear in surface")
		}
	}
}

func TestWalkSnapshot(t *testing.T) {
	doc := parseHexDoc(t)
	surface, _ := Walk(doc)
	got := surface.Snapshot()
	want := Counts{Functions: 2, Structs: 1, Enums: 1, TypeAliases: 1, Constants: 1, Traits: 1, Skipped: 3}
	if got != want {
		t.Errorf("Snapshot = %+v; want %+v", got, want)
	}
}

func TestWalkSortSkippedDeterministic(t *testing.T) {
	doc := parseHexDoc(t)
	surface, _ := Walk(doc)
	surface.SortSkipped()
	for i := 1; i < len(surface.Skipped); i++ {
		if surface.Skipped[i-1].ItemPath > surface.Skipped[i].ItemPath {
			t.Errorf("SortSkipped not ascending: %q > %q",
				surface.Skipped[i-1].ItemPath, surface.Skipped[i].ItemPath)
		}
	}
}

func TestWalkNilDoc(t *testing.T) {
	if _, err := Walk(nil); err == nil {
		t.Errorf("Walk(nil) err = nil; want error")
	}
}

func TestWalkMissingRoot(t *testing.T) {
	doc := &Document{Root: "missing", FormatVersion: 39, Index: map[string]Item{}}
	if _, err := Walk(doc); err == nil {
		t.Errorf("Walk(missing root) err = nil; want error")
	}
}
