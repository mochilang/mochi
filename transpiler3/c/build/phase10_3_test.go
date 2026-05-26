package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"testing"
)

// TestPhase10PythonFFI is the MEP-45 Phase 10.3 gate. It verifies that Python
// functions can be called from Mochi via the subprocess RPC protocol.
//
// The driver detects a <stem>.py file alongside the Mochi source, bakes its
// absolute path into the C binary via -DMOCHI_PYTHON_RPC_PATH_DEFAULT, and at
// runtime the binary forks python3 and communicates via newline-delimited JSON.
//
// Fixtures live under tests/transpiler3/c/fixtures/python_ffi/:
//   py_add_floats: extern python fun py_add(x: float, y: float): float
//   py_str_lower:  extern python fun py_lower(s: string): string
//
// The test skips on Windows (no fork/exec in the stub) and when python3 is absent.
func TestPhase10PythonFFI(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Python FFI via subprocess RPC requires POSIX fork/exec; skipping on Windows")
	}
	if _, err := exec.LookPath("python3"); err != nil {
		if _, err2 := exec.LookPath("python"); err2 != nil {
			t.Skip("python3 / python not found on PATH; skipping Phase 10.3 gate")
		}
	}

	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "python_ffi")
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read python_ffi fixtures dir: %v", err)
	}

	var names []string
	for _, e := range entries {
		if e.IsDir() {
			names = append(names, e.Name())
		}
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatalf("no fixtures under %s", base)
	}

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			fixture := filepath.Join(base, name)
			src := filepath.Join(fixture, name+".mochi")
			expect, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
			if err != nil {
				t.Fatalf("read expect.txt: %v", err)
			}

			outBin := filepath.Join(t.TempDir(), name)
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build %s: %v", src, err)
			}

			cmd := exec.Command(outBin)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s: %v", outBin, err)
			}
			if got := stdout.String(); got != string(expect) {
				t.Fatalf("stdout mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, string(expect), got)
			}
		})
	}
}

// TestPhase10JSFFI is the MEP-45 Phase 10.4 gate. It verifies that JavaScript
// functions can be called from Mochi via the subprocess RPC protocol using node.
//
// The driver detects a <stem>.js file alongside the Mochi source, bakes its
// absolute path into the C binary via -DMOCHI_JS_RPC_PATH_DEFAULT, and at
// runtime the binary forks node and communicates via newline-delimited JSON.
//
// Fixtures live under tests/transpiler3/c/fixtures/js_ffi/:
//   js_mul_ints: extern js fun js_mul(a: int, b: int): int
//   js_str_trim: extern js fun js_trim(s: string): string
//
// The test skips on Windows and when node is absent.
func TestPhase10JSFFI(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("JavaScript FFI via subprocess RPC requires POSIX fork/exec; skipping on Windows")
	}
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not found on PATH; skipping Phase 10.4 gate")
	}

	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "js_ffi")
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read js_ffi fixtures dir: %v", err)
	}

	var names []string
	for _, e := range entries {
		if e.IsDir() {
			names = append(names, e.Name())
		}
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatalf("no fixtures under %s", base)
	}

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			fixture := filepath.Join(base, name)
			src := filepath.Join(fixture, name+".mochi")
			expect, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
			if err != nil {
				t.Fatalf("read expect.txt: %v", err)
			}

			outBin := filepath.Join(t.TempDir(), name)
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build %s: %v", src, err)
			}

			cmd := exec.Command(outBin)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s: %v", outBin, err)
			}
			if got := stdout.String(); got != string(expect) {
				t.Fatalf("stdout mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, string(expect), got)
			}
		})
	}
}
