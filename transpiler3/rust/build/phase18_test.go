package build

import (
	"bytes"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase18Embedded is the gate for Phase 18 (embedded no_std variant).
//
// Runs `cargo check --no-default-features --features embedded` against the
// mochi-runtime workspace. The embedded feature gates the std-requiring
// modules (io, panic, fetch, llm, json, check, chan, stream) behind
// `cfg(feature = "std")`, exposing only conv and strings (which require
// alloc but not std). This is enough to compile against bare-metal targets
// where libc / fs / net are unavailable.
//
// Skipped in -short mode and when cargo is not on PATH.
func TestPhase18Embedded(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping embedded check in short mode")
	}
	cargo, err := resolveCargo()
	if err != nil {
		t.Skipf("cargo not available: %v", err)
	}
	root := repoRoot(t)
	workspace := filepath.Join(root, "runtime3", "rust")
	cmd := exec.Command(cargo, "check", "--no-default-features", "--features", "embedded")
	cmd.Dir = workspace
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("cargo check --no-default-features --features embedded failed: %v\nstdout:\n%s\nstderr:\n%s",
			err, stdout.String(), stderr.String())
	}
}
