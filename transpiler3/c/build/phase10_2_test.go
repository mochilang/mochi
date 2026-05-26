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

// TestPhase10GoFFI is the MEP-45 Phase 10.2 gate. It verifies that Go
// functions can be called from Mochi via the subprocess RPC protocol.
//
// Protocol: the Mochi C binary forks a Go companion process, sends
// newline-delimited JSON requests to its stdin, and reads JSON responses
// from its stdout. The driver detects a <stem>.go file alongside the
// source, compiles it to a binary, and bakes the path into the C binary
// via -DMOCHI_GO_RPC_PATH_DEFAULT.
//
// Fixtures live under tests/transpiler3/c/fixtures/go_ffi/:
//   go_add_ints:  extern go fun add_ints(a: int, b: int): int
//
// The test skips on Windows (no fork/exec in the stub).
func TestPhase10GoFFI(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Go FFI via subprocess RPC requires POSIX fork/exec; skipping on Windows")
	}

	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "go_ffi")
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read go_ffi fixtures dir: %v", err)
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
			if _, err := os.Stat(outBin); err != nil {
				t.Fatalf("stat output binary: %v", err)
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
