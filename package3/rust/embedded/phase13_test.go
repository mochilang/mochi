package embedded_test

import (
	"strings"
	"testing"

	"mochi/package3/rust/embedded"
	"mochi/package3/rust/errors"
	"mochi/package3/rust/rustdoc"
	"mochi/package3/rust/wrapper"
)

// TestPhase13EmbeddedSubset is the umbrella sentinel for MEP-73
// Phase 13. It exercises the end-to-end loop: a wrapper crate
// synthesised under `[rust] profile = "embedded"` carries
// `#![no_std]` + `extern crate alloc;`, drops `default-features` on
// the upstream dep, refuses async fns at synth time, and produces
// byte-identical output across runs.
func TestPhase13EmbeddedSubset(t *testing.T) {
	t.Run("profile_default_is_hosted", profileDefaultIsHosted)
	t.Run("profile_parses_from_toml", profileParsesFromTOML)
	t.Run("libRS_carries_no_std_under_embedded", libRSCarriesNoStd)
	t.Run("libRS_omits_no_std_under_hosted", libRSOmitsNoStd)
	t.Run("cargo_pins_default_features_off_under_embedded", cargoPinsDefaultFeaturesOff)
	t.Run("async_fns_refused_under_embedded", asyncFnsRefused)
	t.Run("sync_fns_kept_under_embedded", syncFnsKept)
	t.Run("triples_list_sorted_and_nonempty", triplesListSortedAndNonempty)
	t.Run("emit_byte_stable_under_embedded", emitByteStable)
}

func profileDefaultIsHosted(t *testing.T) {
	p, err := embedded.ParseProfile("")
	if err != nil || p != embedded.ProfileHosted {
		t.Fatalf("default profile should be hosted; got %v err=%v", p, err)
	}
}

func profileParsesFromTOML(t *testing.T) {
	p, err := embedded.ParseTOMLBody(`profile = "embedded"`)
	if err != nil {
		t.Fatalf("ParseTOMLBody: %v", err)
	}
	if p != embedded.ProfileEmbedded {
		t.Fatalf("profile should be embedded; got %v", p)
	}
}

func libRSCarriesNoStd(t *testing.T) {
	c := wrapper.SynthWithProfile("tokio_demo", "0.1.0", phase13Surface(), embedded.ProfileEmbedded)
	got := wrapper.EmitLibRS(c)
	if !strings.HasPrefix(got, "#![no_std]\n") {
		t.Fatalf("embedded EmitLibRS should start with #![no_std]; got prefix:\n%s", clip(got, 100))
	}
	if !strings.Contains(got, "extern crate alloc;") {
		t.Fatalf("embedded EmitLibRS should declare extern crate alloc")
	}
}

func libRSOmitsNoStd(t *testing.T) {
	c := wrapper.SynthWithProfile("tokio_demo", "0.1.0", phase13Surface(), embedded.ProfileHosted)
	got := wrapper.EmitLibRS(c)
	if strings.Contains(got, "#![no_std]") {
		t.Fatalf("hosted EmitLibRS should NOT carry #![no_std]")
	}
}

func cargoPinsDefaultFeaturesOff(t *testing.T) {
	c := wrapper.SynthWithProfile("tokio_demo", "0.1.0", phase13Surface(), embedded.ProfileEmbedded)
	got := wrapper.EmitCargoTOML(c)
	if !strings.Contains(got, "default-features = false") {
		t.Fatalf("embedded Cargo.toml should pin upstream default-features off:\n%s", got)
	}
}

func asyncFnsRefused(t *testing.T) {
	c := wrapper.SynthWithProfile("tokio_demo", "0.1.0", phase13Surface(), embedded.ProfileEmbedded)
	for _, fn := range c.Functions {
		if fn.IsAsync {
			t.Fatalf("embedded profile should refuse async fn %q", fn.UpstreamPath)
		}
	}
	var found bool
	for _, s := range c.Skipped {
		if s.Reason == errors.SkipEmbedded {
			found = true
		}
	}
	if !found {
		t.Fatalf("embedded refusal should land in SkipEmbedded; reports = %+v", c.Skipped)
	}
}

func syncFnsKept(t *testing.T) {
	c := wrapper.SynthWithProfile("tokio_demo", "0.1.0", phase13Surface(), embedded.ProfileEmbedded)
	if len(c.Functions) != 1 {
		t.Fatalf("embedded should keep 1 sync fn; got %d", len(c.Functions))
	}
}

func triplesListSortedAndNonempty(t *testing.T) {
	got := embedded.AllowedTriples(embedded.ProfileEmbedded)
	if len(got) == 0 {
		t.Fatalf("embedded triples should be non-empty")
	}
	for i := 1; i < len(got); i++ {
		if got[i-1] > got[i] {
			t.Fatalf("AllowedTriples not sorted at %d: %v", i, got)
		}
	}
}

func emitByteStable(t *testing.T) {
	c := wrapper.SynthWithProfile("tokio_demo", "0.1.0", phase13Surface(), embedded.ProfileEmbedded)
	first := wrapper.EmitLibRS(c) + "\n---\n" + wrapper.EmitCargoTOML(c)
	for i := 0; i < 16; i++ {
		c2 := wrapper.SynthWithProfile("tokio_demo", "0.1.0", phase13Surface(), embedded.ProfileEmbedded)
		got := wrapper.EmitLibRS(c2) + "\n---\n" + wrapper.EmitCargoTOML(c2)
		if got != first {
			t.Fatalf("embedded emit not byte-stable across runs")
		}
	}
}

// phase13Surface mirrors a small async-and-sync mixed surface so
// the refusal can be observed via the after-synth SynthFn list.
func phase13Surface() *rustdoc.ApiSurface {
	str := rustdoc.Type{Primitive: "str"}
	strBorrow := rustdoc.Type{BorrowedRef: &rustdoc.BorrowedRefType{
		Lifetime: "'a", Type: str,
	}}
	return &rustdoc.ApiSurface{
		CrateName:    "tokio_demo",
		CrateVersion: "0.1.0",
		Functions: []rustdoc.FunctionEntry{
			{
				ID:   "fn:greet_async",
				Path: []string{"tokio_demo", "greet_async"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "name", Type: strBorrow},
				},
				Header: rustdoc.FunctionHeader{IsAsync: true},
			},
			{
				ID:   "fn:greet_sync",
				Path: []string{"tokio_demo", "greet_sync"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "name", Type: strBorrow},
				},
				Header: rustdoc.FunctionHeader{IsAsync: false},
			},
		},
	}
}

func clip(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n]
}
