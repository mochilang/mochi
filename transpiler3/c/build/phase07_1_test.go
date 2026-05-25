package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase7TryCatch is the MEP-45 Phase 7.1 + 7.3 gate.
//
// Phase 7.1 adds `try { ... } catch e { ... }` syntax and lowers it to
// setjmp/longjmp frames using the Phase 7.0 runtime. Phase 7.3 adds the
// `panic(code, msg)` builtin which calls mochi_raise directly.
//
// The gate covers eight fixtures under tests/transpiler3/c/fixtures/error_model/:
//
//   - try_catch_div_zero: integer divide-by-zero caught, prints "caught\n5"
//   - try_catch_index_oob: index out-of-bounds caught, prints "caught\n4"
//   - try_catch_nested: nested try/catch, inner catches first
//   - try_catch_no_raise: try with no raise, prints the try body then "ok"
//   - try_catch_reraise: catch then panic again with a different code
//   - user_panic_basic: panic(42, "boom") caught by surrounding try
//   - user_panic_uncaught: panic(1, "bye") with no try exits with code 1
//   - try_catch_in_fun: try/catch inside a user function
func TestPhase7TryCatch(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 7 try/catch gate skipped on Windows; Phase 11 wires Windows CI")
	}

	// Positive fixtures: expect zero exit + stdout matching expect.txt.
	positiveFixtures := []string{
		"try_catch_div_zero",
		"try_catch_index_oob",
		"try_catch_nested",
		"try_catch_no_raise",
		"try_catch_reraise",
		"user_panic_basic",
		"try_catch_in_fun",
	}

	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "error_model")

	for _, name := range positiveFixtures {
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

	// user_panic_uncaught: expects non-zero exit (1), specific stdout and stderr.
	t.Run("user_panic_uncaught", func(t *testing.T) {
		const wantExit = 1
		fixture := filepath.Join(base, "user_panic_uncaught")
		src := filepath.Join(fixture, "user_panic_uncaught.mochi")

		wantStdout, err := os.ReadFile(filepath.Join(fixture, "stdout.txt"))
		if err != nil {
			t.Fatalf("read stdout.txt: %v", err)
		}
		wantStderr, err := os.ReadFile(filepath.Join(fixture, "stderr.txt"))
		if err != nil {
			t.Fatalf("read stderr.txt: %v", err)
		}

		outBin := filepath.Join(t.TempDir(), "user_panic_uncaught")
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(src, outBin, "", ""); err != nil {
			t.Fatalf("Driver.Build %s: %v", src, err)
		}

		cmd := exec.Command(outBin)
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		runErr := cmd.Run()

		if runErr == nil {
			t.Fatalf("expected exit %d but binary returned 0\nstdout:%q\nstderr:%q",
				wantExit, stdout.String(), stderr.String())
		}
		exitErr, ok := runErr.(*exec.ExitError)
		if !ok {
			t.Fatalf("expected ExitError, got %T: %v", runErr, runErr)
		}
		gotExit := exitErr.ExitCode()
		if gotExit != wantExit {
			t.Fatalf("exit code: want %d, got %d\nstdout:%q\nstderr:%q",
				wantExit, gotExit, stdout.String(), stderr.String())
		}
		if got := stdout.String(); got != string(wantStdout) {
			t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q",
				string(wantStdout), got)
		}
		if got := stderr.String(); got != string(wantStderr) {
			t.Fatalf("stderr mismatch:\n--- want ---\n%q\n--- got ---\n%q",
				string(wantStderr), got)
		}
	})
}
