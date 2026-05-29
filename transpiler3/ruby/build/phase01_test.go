package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase1HelloEnd2End builds the canonical hello fixture, runs it under
// the resolved Ruby, and asserts stdout matches the expected greeting.
func TestPhase1HelloEnd2End(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}

	repoRoot := repoRootForTest(t)
	src := filepath.Join(repoRoot, "examples", "v0.1", "hello.mochi")
	outDir := t.TempDir()

	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, outDir, TargetRubySource); err != nil {
		t.Fatalf("Build: %v", err)
	}

	rb := filepath.Join(outDir, "hello.rb")
	if _, err := os.Stat(rb); err != nil {
		t.Fatalf("expected emitted %s: %v", rb, err)
	}

	runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")
	cmd := exec.Command(tc.Ruby, "-I", runtimeLib, rb)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("ruby run %s failed: %v\noutput: %s", rb, err, out)
	}
	got := strings.TrimRight(string(out), "\n")
	want := "Hello, world"
	if got != want {
		t.Fatalf("hello.rb output mismatch:\ngot:  %q\nwant: %q", got, want)
	}
}
