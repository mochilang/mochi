package wrapper

import (
	"strings"
	"testing"

	"mochi/package3/rust/embedded"
	"mochi/package3/rust/errors"
	"mochi/package3/rust/monomorphise"
)

func TestSynthWithProfileHostedKeepsAsync(t *testing.T) {
	c := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileHosted)
	if len(c.Functions) != 2 {
		t.Fatalf("hosted profile should keep all 2 fns; got %d", len(c.Functions))
	}
	for _, s := range c.Skipped {
		if s.Reason == errors.SkipEmbedded {
			t.Fatalf("hosted profile should not produce SkipEmbedded reports; got %+v", s)
		}
	}
}

func TestSynthWithProfileEmbeddedRefusesAsync(t *testing.T) {
	c := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileEmbedded)
	for _, fn := range c.Functions {
		if fn.IsAsync {
			t.Fatalf("embedded profile should refuse async fn %q", fn.UpstreamPath)
		}
	}
	var found bool
	for _, s := range c.Skipped {
		if s.Reason == errors.SkipEmbedded {
			found = true
			if !strings.Contains(s.Detail, "tokio") {
				t.Fatalf("SkipEmbedded detail should mention tokio: %q", s.Detail)
			}
		}
	}
	if !found {
		t.Fatalf("embedded profile should emit a SkipEmbedded report for the async fn; reports = %+v", c.Skipped)
	}
}

func TestSynthWithProfileEmbeddedKeepsSync(t *testing.T) {
	c := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileEmbedded)
	if len(c.Functions) != 1 {
		t.Fatalf("embedded profile should keep the 1 sync fn; got %d", len(c.Functions))
	}
	if c.Functions[0].UpstreamPath != "tokio_demo::greet_sync" {
		t.Fatalf("expected greet_sync to remain; got %q", c.Functions[0].UpstreamPath)
	}
}

func TestEmitLibRSEmbeddedHasNoStdHeader(t *testing.T) {
	c := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileEmbedded)
	got := EmitLibRS(c)
	if !strings.HasPrefix(got, "#![no_std]\n") {
		t.Fatalf("embedded EmitLibRS should start with #![no_std]; got prefix:\n%s", got[:min(120, len(got))])
	}
	if !strings.Contains(got, "extern crate alloc;") {
		t.Fatalf("embedded EmitLibRS should declare extern crate alloc")
	}
}

func TestEmitLibRSHostedNoStdAbsent(t *testing.T) {
	c := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileHosted)
	got := EmitLibRS(c)
	if strings.Contains(got, "#![no_std]") {
		t.Fatalf("hosted EmitLibRS should NOT carry #![no_std]; got:\n%s", got[:min(120, len(got))])
	}
}

func TestEmitCargoTOMLEmbeddedFlipsDefaultFeatures(t *testing.T) {
	c := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileEmbedded)
	got := EmitCargoTOML(c)
	if !strings.Contains(got, "default-features = false") {
		t.Fatalf("embedded Cargo.toml should pin upstream default-features = false:\n%s", got)
	}
}

func TestEmitCargoTOMLHostedKeepsPlainRow(t *testing.T) {
	c := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileHosted)
	got := EmitCargoTOML(c)
	if strings.Contains(got, "default-features = false") {
		t.Fatalf("hosted Cargo.toml should NOT carry default-features flip:\n%s", got)
	}
}

func TestEmitCargoTOMLEmbeddedDropsTokioWhenAsyncRefused(t *testing.T) {
	c := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileEmbedded)
	if c.HasAsync() {
		t.Fatalf("embedded profile should have stripped async fns before emit")
	}
	got := EmitCargoTOML(c)
	// The upstream is named "tokio_demo" so a plain substring match
	// would false-positive. Look for the tokio runtime dep row shape
	// the async bridge emits ("tokio = ").
	if strings.Contains(got, "\ntokio = ") {
		t.Fatalf("embedded Cargo.toml should NOT inject tokio runtime dep (async refused):\n%s", got)
	}
	if strings.Contains(got, "once_cell") {
		t.Fatalf("embedded Cargo.toml should NOT inject once_cell (async refused):\n%s", got)
	}
}

func TestSynthFullCombinesSpecAndProfile(t *testing.T) {
	c := SynthFull("tokio_demo", "0.1.0", asyncSurface(), monomorphise.Spec{}, embedded.ProfileEmbedded)
	if len(c.Functions) != 1 {
		t.Fatalf("SynthFull embedded should keep sync only; got %d", len(c.Functions))
	}
	if c.Profile != embedded.ProfileEmbedded {
		t.Fatalf("Profile field not threaded; got %v", c.Profile)
	}
}

func TestEmitLibRSEmbeddedDeterministic(t *testing.T) {
	c1 := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileEmbedded)
	c2 := SynthWithProfile("tokio_demo", "0.1.0", asyncSurface(), embedded.ProfileEmbedded)
	if EmitLibRS(c1) != EmitLibRS(c2) {
		t.Fatalf("EmitLibRS embedded not deterministic across two synths")
	}
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
