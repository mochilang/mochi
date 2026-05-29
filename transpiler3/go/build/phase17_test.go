package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase17Wasip1 gates the MEP-54 Phase 17 target
// `go-wasip1`. Driver.Build with GOOS=wasip1 GOARCH=wasm must
// produce a single .wasm file, and that file must execute
// under `wasmtime` (if present on PATH) with stdout matching
// the fixture's expect.txt.
//
// `wasmtime` is treated as optional: the build half of the
// gate always runs (verifying the .wasm file is produced),
// the exec half is skipped when wasmtime is unavailable.
func TestPhase17Wasip1(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16.x")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "hello", "hello.mochi")
	want, err := os.ReadFile(filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "hello", "expect.txt"))
	if err != nil {
		t.Fatalf("read expect.txt: %v", err)
	}

	outWasm := filepath.Join(t.TempDir(), "hello.wasm")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, outWasm, string(TargetGoWasiP1), ""); err != nil {
		t.Fatalf("Driver.Build (wasip1): %v", err)
	}
	info, err := os.Stat(outWasm)
	if err != nil {
		t.Fatalf("stat wasip1 output: %v", err)
	}
	if info.Size() == 0 {
		t.Fatalf("wasip1 output is empty")
	}

	wasmtime, err := exec.LookPath("wasmtime")
	if err != nil {
		t.Skip("wasmtime not on PATH; skipping wasip1 exec half")
	}
	cmd := exec.Command(wasmtime, "run", outWasm)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("wasmtime run: %v\n%s", err, stderr.String())
	}
	if got := stdout.String(); got != string(want) {
		t.Fatalf("wasip1 stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(want), got)
	}
}

// TestPhase17WasmJS gates the MEP-54 Phase 17 target
// `go-wasm-js`. Driver.Build with GOOS=js GOARCH=wasm must
// produce a .wasm file plus a sibling wasm_exec.js copied
// from $GOROOT/lib/wasm/wasm_exec.js.
//
// Exec under Node.js is not currently part of the gate
// because Go 1.26's wasm_exec.js requires a `globalThis.fs`
// shim under Node that varies across versions; this phase
// gates only the artifact shape. Phase 17.1 will wire Node
// exec.
func TestPhase17WasmJS(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16.x")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "hello", "hello.mochi")

	outDir := t.TempDir()
	outWasm := filepath.Join(outDir, "hello.wasm")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, outWasm, string(TargetGoWasmJS), ""); err != nil {
		t.Fatalf("Driver.Build (wasm-js): %v", err)
	}

	for _, name := range []string{"hello.wasm", "wasm_exec.js"} {
		info, err := os.Stat(filepath.Join(outDir, name))
		if err != nil {
			t.Fatalf("stat %s: %v", name, err)
		}
		if info.Size() == 0 {
			t.Fatalf("%s is empty", name)
		}
	}
}

// TestPhase17GoEnvGoRoot is a fast unit-level check that
// goEnvGoRoot returns a non-empty path containing lib/wasm/
// (where wasm_exec.js lives in Go 1.26). Without this the
// wasm-js gate would have to rely on an indirect error path
// to detect a broken GOROOT detection.
func TestPhase17GoEnvGoRoot(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 invariant: skipping on Windows")
	}
	root, err := goEnvGoRoot("")
	if err != nil {
		t.Fatalf("goEnvGoRoot: %v", err)
	}
	wasmExec := filepath.Join(root, "lib", "wasm", "wasm_exec.js")
	if _, err := os.Stat(wasmExec); err != nil {
		t.Fatalf("wasm_exec.js not found at %s: %v", wasmExec, err)
	}
}
