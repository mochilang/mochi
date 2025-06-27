//go:build cosmo

package cosmo

import (
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestMochiCosmoHello builds the mochi-cosmo command and compiles the
// hello.mochi example, verifying the output.
func TestMochiCosmoHello(t *testing.T) {
	if err := EnsureCosmo(); err != nil {
		t.Skipf("cosmo not installed: %v", err)
	}

	dir := t.TempDir()
	bin := filepath.Join(dir, "mochi-cosmo")
	cmd := exec.Command("go", "build", "-tags", "cosmo libcosmo", "-o", bin, filepath.Join("..", "..", "cmd", "mochi-cosmo"))
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("build: %v\n%s", err, out)
	}

	helloSrc := filepath.Join("..", "..", "examples", "v0.1", "hello.mochi")
	exe := filepath.Join(dir, "hello")
	if out, err := exec.Command(bin, helloSrc, exe).CombinedOutput(); err != nil {
		t.Fatalf("compile: %v\n%s", err, out)
	}

	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	if got != "Hello, world" {
		t.Fatalf("unexpected output: %s", got)
	}
}
