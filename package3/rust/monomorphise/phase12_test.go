package monomorphise_test

import (
	"strings"
	"testing"

	"mochi/package3/rust/monomorphise"
	"mochi/package3/rust/rustdoc"
	"mochi/package3/rust/wrapper"
)

// TestPhase12Monomorphisation is the umbrella sentinel for MEP-73
// Phase 12. It exercises the end-to-end loop: a manifest declares
// two instantiations of one generic fn; the wrapper synthesises two
// extern symbols (one per substitution), each with a turbofish call
// site. Failure modes (empty Spec, unmatched fn, validation errors)
// must keep the wrapper in its closed-typemap behaviour.
func TestPhase12Monomorphisation(t *testing.T) {
	t.Run("parses_spec_from_manifest", parsesSpecFromManifest)
	t.Run("lookup_returns_entries_for_item", lookupReturnsEntries)
	t.Run("extern_name_byte_stable", externNameByteStable)
	t.Run("call_site_turbofish_rendering", callSiteTurbofish)
	t.Run("validate_rejects_empty_item", validateRejectsEmptyItem)
	t.Run("validate_rejects_no_type_args", validateRejectsNoTypeArgs)
	t.Run("wrapper_emits_one_fn_per_entry", wrapperEmitsOneFnPerEntry)
	t.Run("wrapper_skips_generic_when_no_spec", wrapperSkipsGenericWhenNoSpec)
}

func parsesSpecFromManifest(t *testing.T) {
	body := `[
        { item = "smallvec::push", T = "i64" },
        { item = "smallvec::push", T = "u8" },
    ]`
	spec, err := monomorphise.ParseTOMLEntries(body)
	if err != nil {
		t.Fatalf("ParseTOMLEntries: %v", err)
	}
	if len(spec.Entries) != 2 {
		t.Fatalf("entries = %d; want 2", len(spec.Entries))
	}
	if err := spec.Validate(); err != nil {
		t.Fatalf("Validate: %v", err)
	}
}

func lookupReturnsEntries(t *testing.T) {
	spec := monomorphise.Spec{Entries: []monomorphise.Entry{
		{Item: "smallvec::push", TypeArgs: map[string]string{"T": "i64"}},
		{Item: "smallvec::push", TypeArgs: map[string]string{"T": "u8"}},
	}}
	got := spec.Lookup("smallvec::push")
	if len(got) != 2 {
		t.Fatalf("Lookup returned %d entries; want 2", len(got))
	}
}

func externNameByteStable(t *testing.T) {
	a := monomorphise.ExternName("smallvec", "smallvec::push", map[string]string{"T": "i64"})
	b := monomorphise.ExternName("smallvec", "smallvec::push", map[string]string{"T": "i64"})
	if a != b {
		t.Fatalf("ExternName not byte-stable: %q vs %q", a, b)
	}
	if !strings.HasSuffix(a, "_of_t_i64") {
		t.Fatalf("ExternName: missing _of suffix in %q", a)
	}
}

func callSiteTurbofish(t *testing.T) {
	got := monomorphise.CallSite("serde_json::from_str", map[string]string{"T": "MyStruct"})
	want := "serde_json::from_str::<MyStruct>"
	if got != want {
		t.Fatalf("CallSite: got %q want %q", got, want)
	}
}

func validateRejectsEmptyItem(t *testing.T) {
	s := monomorphise.Spec{Entries: []monomorphise.Entry{
		{Item: "", TypeArgs: map[string]string{"T": "i64"}},
	}}
	if err := s.Validate(); err == nil {
		t.Fatalf("Validate: want error for empty item")
	}
}

func validateRejectsNoTypeArgs(t *testing.T) {
	s := monomorphise.Spec{Entries: []monomorphise.Entry{
		{Item: "Vec::new", TypeArgs: nil},
	}}
	if err := s.Validate(); err == nil {
		t.Fatalf("Validate: want error for no type-args")
	}
}

// genericPushFn is a synthetic FunctionEntry mirroring a generic
// upstream fn `smallvec::push<T>(v: T) -> ()`. It's deliberately
// minimal so the test does not have to depend on rustdoc-types JSON
// fixtures.
func genericPushFn() rustdoc.FunctionEntry {
	return rustdoc.FunctionEntry{
		Path: []string{"smallvec", "push"},
		Inputs: []rustdoc.ParamEntry{
			{Name: "v", Type: rustdoc.Type{Generic: "T"}},
		},
		Output: nil,
		Generics: rustdoc.Generics{
			Params: []rustdoc.GenericParamDef{{Name: "T"}},
		},
	}
}

func wrapperEmitsOneFnPerEntry(t *testing.T) {
	surface := &rustdoc.ApiSurface{
		Functions: []rustdoc.FunctionEntry{genericPushFn()},
	}
	spec := monomorphise.Spec{Entries: []monomorphise.Entry{
		{Item: "smallvec::push", TypeArgs: map[string]string{"T": "i64"}},
		{Item: "smallvec::push", TypeArgs: map[string]string{"T": "u8"}},
	}}
	c := wrapper.SynthWithSpec("smallvec", "0.1.0", surface, spec)
	if len(c.Functions) != 2 {
		t.Fatalf("SynthWithSpec emitted %d fns; want 2 (one per entry)", len(c.Functions))
	}
	seen := map[string]bool{}
	for _, fn := range c.Functions {
		if seen[fn.ExternName] {
			t.Fatalf("duplicate extern name %q", fn.ExternName)
		}
		seen[fn.ExternName] = true
		if !strings.Contains(fn.UpstreamPath, "::<") {
			t.Fatalf("call site missing turbofish: %q", fn.UpstreamPath)
		}
	}
}

func wrapperSkipsGenericWhenNoSpec(t *testing.T) {
	surface := &rustdoc.ApiSurface{
		Functions: []rustdoc.FunctionEntry{genericPushFn()},
	}
	c := wrapper.Synth("smallvec", "0.1.0", surface)
	if len(c.Functions) != 0 {
		t.Fatalf("default Synth should not emit unconcretised generic fn; got %d", len(c.Functions))
	}
	if len(c.Skipped) == 0 {
		t.Fatalf("default Synth: want SkipReport for generic fn, got none")
	}
}
