package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

// repoRoot walks up from the test binary's working directory
// until it finds go.mod, so fixture paths resolve regardless
// of the test runner's cwd. Failing to find go.mod is fatal:
// the build cannot proceed without the fixture corpus.
func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("no go.mod found above %s", dir)
		}
		dir = parent
	}
}

// TestHello is the Phase 1 gate. It exercises the end-to-end
// pipeline (parse -> type-check -> lower -> emit -> cc -> link
// -> exec) and diffs the binary's stdout against the fixture's
// expect.txt.
func TestHello(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	root := repoRoot(t)
	fixture := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "hello")
	src := filepath.Join(fixture, "hello.mochi")
	want, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
	if err != nil {
		t.Fatalf("read expect.txt: %v", err)
	}

	outBin := filepath.Join(t.TempDir(), "hello")
	d := &Driver{}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
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
	if got := stdout.String(); got != string(want) {
		t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(want), got)
	}
}
