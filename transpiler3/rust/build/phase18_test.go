package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase18RuntimeCheck is the gate for Phase 18 (embedded no_std variant).
//
// Runs `cargo check --no-default-features --features embedded` against the
// mochi-runtime workspace. The embedded feature gates the std-requiring
// modules (io, panic, fetch, llm, json, check, chan, stream) behind
// `cfg(feature = "std")`, exposing only conv and strings (which require
// alloc but not std). This is enough to compile against bare-metal targets
// where libc / fs / net are unavailable.
//
// Skipped in -short mode and when cargo is not on PATH.
func TestPhase18RuntimeCheck(t *testing.T) {
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

// TestPhase18Embedded exercises the embedded profile against Mochi source
// fixture programs. Each fixture is compiled and run as a native executable
// for output verification; the same transpiled source also passes
// cargo check --no-default-features to confirm embedded-safe IR.
//
// Embedded profile constraints (no_std + alloc only):
//   - Integer/float arithmetic, records, sum types, closures: ALLOWED
//   - Compile-time Datalog (static literal result): ALLOWED
//   - Sync agents: ALLOWED
//   - Channels, async/await, file I/O, HTTP, LLM: NOT ALLOWED
//
// Minimum fixture count: 7.
func TestPhase18Embedded(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping embedded fixtures in short mode")
	}
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "rust", "fixtures", "phase18-embedded")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	mochiCount := 0
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		mochiCount++
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		wantFile := filepath.Join(fixtureDir, name+".out")
		t.Run(name, func(t *testing.T) {
			runRustFixture(t, mochiPath, wantFile)
		})
	}
	if mochiCount < 7 {
		t.Fatalf("Phase 18 embedded fixture corpus has %d .mochi files, expected at least 7", mochiCount)
	}
}

// TestPhase18EmbeddedCargoCheck verifies that each embedded fixture's
// transpiled .rs source passes cargo check --no-default-features.
// Skipped when the embedded Rust toolchain is not installed.
func TestPhase18EmbeddedCargoCheck(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping embedded cargo check in short mode")
	}
	if !embeddedToolchainAvailable() {
		t.Skip("embedded Rust toolchain (thumbv7m-none-eabi or wasm32-unknown-unknown) not installed")
	}
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "rust", "fixtures", "phase18-embedded")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		e := e
		t.Run(strings.TrimSuffix(e.Name(), ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			_, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetEmbeddedNoStd)
			if err != nil {
				t.Fatalf("TargetEmbeddedNoStd Build(%s): %v", e.Name(), err)
			}
		})
	}
}

// embeddedToolchainAvailable checks whether a bare-metal Rust target is installed
// via rustup (thumbv7m-none-eabi for ARM Cortex-M3, or wasm32-unknown-unknown).
func embeddedToolchainAvailable() bool {
	rustup, err := exec.LookPath("rustup")
	if err != nil {
		return false
	}
	out, err := exec.Command(rustup, "target", "list", "--installed").Output()
	if err != nil {
		return false
	}
	targets := string(out)
	return strings.Contains(targets, "thumbv7m-none-eabi") ||
		strings.Contains(targets, "wasm32-unknown-unknown")
}
