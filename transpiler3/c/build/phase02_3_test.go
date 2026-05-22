package build

import (
	"bytes"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"testing"
)

// TestPhase2Divzero is the positive half of the MEP-45 Phase 2.3
// gate: every fixture under tests/transpiler3/c/fixtures/divzero/
// is built end-to-end and its stdout must match `expect.txt`
// byte-for-byte. The shape is identical to Phase 2.0/2.2's
// runFixtureSuite, so any regression on the existing int-arith
// path shows up here too.
func TestPhase2Divzero(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "divzero")
}

// TestPhase2DivzeroTrip is the negative half of the MEP-45 Phase 2.3
// gate. Each fixture under tests/transpiler3/c/fixtures/divzero-trip/
// is built and executed. The binary must:
//
//   - exit with code MOCHI_ERR_DIVZERO (5),
//   - write `stderr.txt` to stderr byte-for-byte (full match: stderr
//     should contain exactly the panic diagnostic),
//   - write `stdout.txt` to stdout byte-for-byte (typically empty,
//     but for the `div_in_for_loop` and `div_in_function` fixtures
//     the loop and any earlier prints reach stdout before the trip).
//
// Splitting the trip path from the positive path keeps the gate
// transparent: the positive runner stays a stdout diff, the trip
// runner adds the exit-code + stderr assertions.
func TestPhase2DivzeroTrip(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	const wantExit = 5 // MOCHI_ERR_DIVZERO

	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "divzero-trip")
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read fixtures dir: %v", err)
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
			wantStdout, err := os.ReadFile(filepath.Join(fixture, "stdout.txt"))
			if err != nil {
				t.Fatalf("read stdout.txt: %v", err)
			}
			wantStderr, err := os.ReadFile(filepath.Join(fixture, "stderr.txt"))
			if err != nil {
				t.Fatalf("read stderr.txt: %v", err)
			}

			outBin := filepath.Join(t.TempDir(), name)
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build %s: %v", src, err)
			}
			cmd := exec.Command(outBin)
			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			runErr := cmd.Run()

			var gotExit int
			if runErr == nil {
				t.Fatalf("expected trip with exit %d but binary returned 0\nstdout:%q\nstderr:%q",
					wantExit, stdout.String(), stderr.String())
			}
			var exitErr *exec.ExitError
			if errors.As(runErr, &exitErr) {
				gotExit = exitErr.ExitCode()
			} else {
				t.Fatalf("expected ExitError, got %T: %v", runErr, runErr)
			}
			if gotExit != wantExit {
				t.Fatalf("exit code: want %d, got %d\nstdout:%q\nstderr:%q",
					wantExit, gotExit, stdout.String(), stderr.String())
			}
			if got := stdout.String(); got != string(wantStdout) {
				t.Fatalf("stdout mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, string(wantStdout), got)
			}
			if got := stderr.String(); got != string(wantStderr) {
				t.Fatalf("stderr mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, string(wantStderr), got)
			}
		})
	}
}
