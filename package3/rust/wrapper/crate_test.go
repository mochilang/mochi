package wrapper

import (
	"strings"
	"testing"

	"mochi/package3/rust/errors"
	"mochi/package3/rust/rustdoc"
)

// hexLikeSurface mimics the hex crate's public surface:
//   - encode(data: &[u8]) -> String
//   - decode(input: &str) -> Result<Vec<u8>, FromHexError>
//   - to_upper_hex(value: u8) -> char  (scalar in/out, all primitive)
// Plus a SkipReport for a generic helper.
func hexLikeSurface() *rustdoc.ApiSurface {
	str := rustdoc.Type{Primitive: "str"}
	u8 := rustdoc.Type{Primitive: "u8"}
	char := rustdoc.Type{Primitive: "char"}
	bytesSlice := rustdoc.Type{BorrowedRef: &rustdoc.BorrowedRefType{
		Lifetime: "'a",
		Type:     rustdoc.Type{Slice: &u8},
	}}
	strBorrow := rustdoc.Type{BorrowedRef: &rustdoc.BorrowedRefType{
		Lifetime: "'a",
		Type:     str,
	}}
	stringPath := rustdoc.Type{ResolvedPath: &rustdoc.PathType{
		ID: "id:String", Path: "std::string::String",
	}}
	vecU8 := rustdoc.Type{ResolvedPath: &rustdoc.PathType{
		ID: "id:Vec_u8", Path: "std::vec::Vec",
		Args: &rustdoc.GenericArgs{AngleBracketed: &rustdoc.AngleBracketedArgs{
			Args: []rustdoc.GenericArg{{Type: &u8}},
		}},
	}}
	fromHexError := rustdoc.Type{ResolvedPath: &rustdoc.PathType{
		ID: "id:FromHexError", Path: "hex::FromHexError",
	}}
	resultVecU8 := rustdoc.Type{ResolvedPath: &rustdoc.PathType{
		ID: "id:Result", Path: "core::result::Result",
		Args: &rustdoc.GenericArgs{AngleBracketed: &rustdoc.AngleBracketedArgs{
			Args: []rustdoc.GenericArg{{Type: &vecU8}, {Type: &fromHexError}},
		}},
	}}
	_ = stringPath
	return &rustdoc.ApiSurface{
		CrateName:    "hex",
		CrateVersion: "0.4.3",
		Functions: []rustdoc.FunctionEntry{
			{
				ID:   "fn:encode",
				Path: []string{"hex", "encode"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "data", Type: bytesSlice},
				},
				Output: &stringPath,
			},
			{
				ID:   "fn:decode",
				Path: []string{"hex", "decode"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "input", Type: strBorrow},
				},
				Output: &resultVecU8,
			},
			{
				ID:   "fn:to_upper_hex",
				Path: []string{"hex", "to_upper_hex"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "value", Type: u8},
				},
				Output: &char,
			},
		},
		Skipped: []errors.SkipReport{
			{
				ItemPath: "hex::decode_to_slice",
				Reason:   errors.SkipGeneric,
				Detail:   "generic parameter T",
			},
		},
	}
}

func TestSynthCounts(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	if c.Name != "mochi_wrap_hex" {
		t.Errorf("Name = %q", c.Name)
	}
	if c.Upstream != "hex" || c.UpstreamVersion != "0.4.3" {
		t.Errorf("Upstream/Ver = %q/%q", c.Upstream, c.UpstreamVersion)
	}
	if len(c.Functions) != 3 {
		t.Fatalf("Functions = %d; want 3 (encode, decode, to_upper_hex)", len(c.Functions))
	}
	// encode: 1 string param (from &[u8] -> bytes), string return.
	if c.Functions[0].ExternName != "mochi_hex_encode" {
		t.Errorf("encode ExternName = %q", c.Functions[0].ExternName)
	}
	if c.Functions[0].UpstreamPath != "hex::encode" {
		t.Errorf("encode UpstreamPath = %q", c.Functions[0].UpstreamPath)
	}
	// to_upper_hex: scalar in/out.
	if c.Functions[2].ExternName != "mochi_hex_to_upper_hex" {
		t.Errorf("to_upper_hex ExternName = %q", c.Functions[2].ExternName)
	}
}

func TestSynthPropagatesWalkerSkips(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	var found bool
	for _, s := range c.Skipped {
		if s.ItemPath == "hex::decode_to_slice" && s.Reason == errors.SkipGeneric {
			found = true
		}
	}
	if !found {
		t.Errorf("walker SkipReport not propagated; Skipped = %+v", c.Skipped)
	}
}

func TestSynthFnFailureSkip(t *testing.T) {
	// A function with a generic param produces a SkipGeneric report.
	surface := &rustdoc.ApiSurface{
		CrateName: "demo",
		Functions: []rustdoc.FunctionEntry{
			{
				ID:   "fn:identity",
				Path: []string{"demo", "identity"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "x", Type: rustdoc.Type{Generic: "T"}},
				},
				Output: &rustdoc.Type{Generic: "T"},
			},
		},
	}
	c := Synth("demo", "0.1.0", surface)
	if len(c.Functions) != 0 {
		t.Fatalf("expected zero synthesised fns, got %d", len(c.Functions))
	}
	if len(c.Skipped) != 1 {
		t.Fatalf("expected one skip, got %d", len(c.Skipped))
	}
	s := c.Skipped[0]
	if s.Reason != errors.SkipGeneric {
		t.Errorf("Reason = %s; want SkipGeneric", s.Reason)
	}
	if s.ItemPath != "demo::identity" {
		t.Errorf("ItemPath = %q", s.ItemPath)
	}
	if !strings.Contains(s.Detail, "param 0") || !strings.Contains(s.Detail, "x") {
		t.Errorf("Detail = %q; want to mention param 0 (x)", s.Detail)
	}
}

func TestSynthReturnSkipDetail(t *testing.T) {
	// Return type is dyn Trait -> SkipDynTrait with "return: " detail prefix.
	surface := &rustdoc.ApiSurface{
		CrateName: "demo",
		Functions: []rustdoc.FunctionEntry{
			{
				ID:   "fn:debugger",
				Path: []string{"demo", "debugger"},
				Output: &rustdoc.Type{DynTrait: &rustdoc.DynTraitType{
					Traits: []rustdoc.PolyTrait{{Trait: rustdoc.PathType{Path: "core::fmt::Debug"}}},
				}},
			},
		},
	}
	c := Synth("demo", "0.1.0", surface)
	if len(c.Skipped) != 1 || c.Skipped[0].Reason != errors.SkipDynTrait {
		t.Fatalf("Skipped = %+v", c.Skipped)
	}
	if !strings.HasPrefix(c.Skipped[0].Detail, "return: ") {
		t.Errorf("Detail = %q; want 'return: ' prefix", c.Skipped[0].Detail)
	}
}

func TestSynthAnonymousParams(t *testing.T) {
	// rustdoc may emit empty input names; synth should substitute arg0, arg1, ...
	surface := &rustdoc.ApiSurface{
		CrateName: "demo",
		Functions: []rustdoc.FunctionEntry{
			{
				ID:   "fn:noisy",
				Path: []string{"demo", "noisy"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "", Type: rustdoc.Type{Primitive: "i32"}},
					{Name: "", Type: rustdoc.Type{Primitive: "i32"}},
				},
			},
		},
	}
	c := Synth("demo", "0.1.0", surface)
	if len(c.Functions) != 1 {
		t.Fatalf("Functions = %d", len(c.Functions))
	}
	if c.Functions[0].Params[0].Name != "arg0" || c.Functions[0].Params[1].Name != "arg1" {
		t.Errorf("anon params: %+v", c.Functions[0].Params)
	}
}

func TestCrateNameMangling(t *testing.T) {
	cases := []struct{ in, want string }{
		{"hex", "mochi_wrap_hex"},
		{"once_cell", "mochi_wrap_once_cell"},
		{"some-Crate", "mochi_wrap_some_crate"},
		{"Foo123", "mochi_wrap_foo123"},
	}
	for _, c := range cases {
		if got := crateName(c.in); got != c.want {
			t.Errorf("crateName(%q) = %q; want %q", c.in, got, c.want)
		}
	}
}

func TestExternNameMangling(t *testing.T) {
	got := externName("once_cell", []string{"once_cell", "sync", "Lazy", "new"})
	want := "mochi_once_cell_sync_lazy_new"
	if got != want {
		t.Errorf("externName = %q; want %q", got, want)
	}
}
