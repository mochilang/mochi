package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase15_3AtomVM verifies that TargetAtomVM calls packbeam to produce
// a .avm bundle. The test is skipped automatically when packbeam is not on
// PATH so CI environments without AtomVM toolchain are unaffected.
//
// Phase 15.3.
func TestPhase15_3AtomVM(t *testing.T) {
	if _, err := exec.LookPath("packbeam"); err != nil {
		t.Skip("packbeam not on PATH; skipping Phase 15.3 AtomVM test")
	}

	src := writeTemp(t, "print(42)\n", "fixture.mochi")
	out := filepath.Join(t.TempDir(), "fixture.avm")

	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, out, TargetAtomVM); err != nil {
		t.Fatalf("Build TargetAtomVM: %v", err)
	}

	if _, err := os.Stat(out); err != nil {
		t.Fatalf(".avm file not produced: %v", err)
	}
}
