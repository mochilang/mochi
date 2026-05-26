package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase15_0Release verifies that TargetRelease emits a rebar3 project and
// runs `rebar3 release`. The test is skipped automatically when rebar3 is not
// on PATH, so CI environments without rebar3 are unaffected.
//
// Phase 15.0.
func TestPhase15_0Release(t *testing.T) {
	if _, err := exec.LookPath("rebar3"); err != nil {
		t.Skip("rebar3 not on PATH; skipping Phase 15.0 release test")
	}

	src := writeTemp(t, "print(42)\n", "fixture.mochi")
	outDir := filepath.Join(t.TempDir(), "release_proj")

	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, outDir, TargetRelease); err != nil {
		t.Fatalf("Build TargetRelease: %v", err)
	}

	// rebar3 release puts the result under _build/default/rel/<appname>/.
	relDir := filepath.Join(outDir, "_build", "default", "rel", "mochi_app")
	if _, err := os.Stat(relDir); err != nil {
		t.Errorf("release directory not found at %s: %v", relDir, err)
	}
}
