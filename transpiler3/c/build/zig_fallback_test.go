package build

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestZigFallbackDisabled asserts that with NoZigFallback=true and
// no host cc on PATH, resolveCC returns the host-cc-only error
// instead of attempting a network download.
func TestZigFallbackDisabled(t *testing.T) {
	withEmptyPATH(t)
	t.Setenv("CC", "")

	d := &Driver{NoZigFallback: true}
	exe, prefix, err := d.resolveCC()
	if err == nil {
		t.Fatalf("expected error with NoZigFallback + empty PATH, got %q %v", exe, prefix)
	}
	if !strings.Contains(err.Error(), "no C compiler found") {
		t.Fatalf("expected 'no C compiler found' error, got: %v", err)
	}
}

// TestZigFallbackTriggers exercises the real download path. It is
// gated on MOCHI_TEST_ZIG_DOWNLOAD=1 so the default test run never
// hits the network. When enabled, the test wipes PATH, forces
// MOCHI_ZIG_DIR to a tempdir, calls Driver.Build for the hello
// fixture, then runs the produced binary and diffs stdout against
// expect.txt.
func TestZigFallbackTriggers(t *testing.T) {
	if os.Getenv("MOCHI_TEST_ZIG_DOWNLOAD") != "1" {
		t.Skip("set MOCHI_TEST_ZIG_DOWNLOAD=1 to opt into the live network download path")
	}
	if runtime.GOOS == "windows" {
		t.Skip("Phase 11 wires the Windows host-cc story")
	}
	withEmptyPATH(t)
	t.Setenv("CC", "")
	t.Setenv("MOCHI_ZIG_DIR", t.TempDir())
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "hello", "hello.mochi")
	want, err := os.ReadFile(filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "hello", "expect.txt"))
	if err != nil {
		t.Fatalf("read expect: %v", err)
	}

	out := filepath.Join(t.TempDir(), "hello")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, out, "", ""); err != nil {
		t.Fatalf("Driver.Build via zig fallback: %v", err)
	}
	if _, err := os.Stat(out); err != nil {
		t.Fatalf("stat output: %v", err)
	}
	got, err := exec.Command(out).Output()
	if err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			t.Fatalf("run %s: %v\nstderr:\n%s", out, err, exitErr.Stderr)
		}
		t.Fatalf("run %s: %v", out, err)
	}
	if string(got) != string(want) {
		t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(want), string(got))
	}
}

// withEmptyPATH sets PATH to a tempdir that contains nothing for
// the duration of the test, so exec.LookPath("cc"|"clang"|"gcc")
// fails. The original PATH is restored by t.Setenv on test exit.
func withEmptyPATH(t *testing.T) {
	t.Helper()
	empty := t.TempDir()
	t.Setenv("PATH", empty)
}
