//go:build tcc && libtcc

package tcc

import (
	"bytes"
	"os/exec"
	"path/filepath"
	"testing"
)

func TestMochiTCC_Hello(t *testing.T) {
	if err := EnsureTCC(); err != nil {
		t.Skipf("TinyCC not installed: %v", err)
	}
	bin := filepath.Join(t.TempDir(), "mochi-tcc")
	cmd := exec.Command("go", "build", "-tags", "tcc libtcc", "-o", bin, "../../cmd/mochi-tcc")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("build mochi-tcc: %v\n%s", err, out)
	}
	src := filepath.Join("..", "..", "examples", "v0.1", "hello.mochi")
	exe := filepath.Join(t.TempDir(), "hello")
	cmd = exec.Command(bin, src, exe)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("mochi-tcc compile: %v\n%s", err, out)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	if string(bytes.TrimSpace(out)) != "Hello, world" {
		t.Fatalf("unexpected output: %q", out)
	}
}
