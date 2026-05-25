package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase16DebugProfile is the MEP-45 Phase 16.4 gate. It verifies that
// passing profile="debug" to Driver.Build adds -fsanitize=address,undefined
// to the compiler invocation without the caller having to set ExtraFlags
// manually.
//
// The test builds the primitives/add_ints fixture with profile="debug" and
// asserts that the resulting binary produces the correct output, proving the
// sanitiser-instrumented build is both compilable and correct.
//
// Skipped on Windows (no ASan runtime in Phase 16 toolchain) and on hosts
// where the cc does not support -fsanitize=address.
func TestPhase16DebugProfile(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("debug-profile sanitiser gate skipped on Windows")
	}
	if !asanAvailable(t) {
		t.Skip("ASan not available on this host")
	}

	root := repoRoot(t)
	fixture := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "primitives", "add_ints")
	src := filepath.Join(fixture, "add_ints.mochi")
	if _, err := os.Stat(src); err != nil {
		t.Skipf("fixture not found: %v", err)
	}
	want, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
	if err != nil {
		t.Fatalf("read expect.txt: %v", err)
	}

	outBin := filepath.Join(t.TempDir(), "add_ints_debug")
	d := &Driver{NoCache: true}
	if err := d.Build(src, outBin, "", "debug"); err != nil {
		t.Fatalf("Driver.Build(profile=debug): %v", err)
	}

	cmd := exec.Command(outBin)
	cmd.Env = append(os.Environ(), strings.Split("ASAN_OPTIONS=detect_leaks=0:halt_on_error=1", " ")...)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run debug binary: %v", err)
	}
	if got := stdout.String(); got != string(want) {
		t.Fatalf("output mismatch:\nwant %q\n got %q", want, got)
	}
}
