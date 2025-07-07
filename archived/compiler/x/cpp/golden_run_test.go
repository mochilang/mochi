//go:build archived && slow

package cppcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cppcode "mochi/archived/x/cpp"
	"mochi/golden"
)

// TestCPPCompiler_RunGoldenOutput compiles the golden C++ sources and
// verifies they execute with the expected output.
func TestCPPCompiler_RunGoldenOutput(t *testing.T) {
	cpp, err := cppcode.EnsureCPP()
	if err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	run := func(src string) ([]byte, error) {
		code, err := os.ReadFile(src)
		if err != nil {
			return nil, fmt.Errorf("read error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "prog.cpp")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		bin := filepath.Join(dir, "prog")
		if out, err := exec.Command(cpp, file, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ cpp error: %w\n%s", err, out)
		}
		cmd := exec.Command(bin)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".cpp.out") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	}
	golden.Run(t, "tests/compiler/valid", ".cpp.out", ".out", run)
	golden.Run(t, "tests/compiler/cpp", ".cpp.out", ".out", run)
}
