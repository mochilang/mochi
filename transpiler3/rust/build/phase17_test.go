package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase17Wasm is the gate for Phase 17 (wasm32-wasip1 target).
//
// Builds each fixture to a wasm32-wasip1 binary, runs it under wasmtime,
// and compares stdout against the .out file. Skipped when either the
// wasm32-wasip1 rustup target is not installed or wasmtime is missing.
//
// (Rust 1.78+ renamed wasm32-wasi to wasm32-wasip1; the Mochi driver uses
// the new name.)
func TestPhase17Wasm(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping wasm test in short mode")
	}
	if !hasWasmTarget(t) {
		t.Skip("wasm32-wasip1 rustup target not installed")
	}
	wasmtime, err := exec.LookPath("wasmtime")
	if err != nil {
		t.Skip("wasmtime not installed")
	}
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "rust", "fixtures", "phase17-wasm")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		wantPath := filepath.Join(fixtureDir, name+".out")
		t.Run(name, func(t *testing.T) {
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			outDir := t.TempDir()
			wasmPath, err := d.Build(mochiPath, outDir, TargetWasm32WASI)
			if err != nil {
				t.Fatalf("build wasm: %v", err)
			}
			cmd := exec.Command(wasmtime, "run", wasmPath)
			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("wasmtime run: %v\nstderr: %s", err, stderr.String())
			}
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			if got := stdout.String(); got != string(want) {
				t.Errorf("stdout mismatch\n--- want ---\n%s--- got ---\n%s", string(want), got)
			}
		})
	}
}

func hasWasmTarget(t *testing.T) bool {
	t.Helper()
	out, err := exec.Command("rustup", "target", "list", "--installed").Output()
	if err != nil {
		return false
	}
	for _, line := range strings.Split(string(out), "\n") {
		if strings.TrimSpace(line) == "wasm32-wasip1" {
			return true
		}
	}
	return false
}
