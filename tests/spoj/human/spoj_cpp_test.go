//go:build slow

package human

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"testing"

	"mochi/golden"
)

func TestCppSolutions(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	golden.Run(t, "tests/spoj/human/x/cpp", ".cpp", ".out", func(src string) ([]byte, error) {
		bin := strings.TrimSuffix(src, ".cpp")
		cmd := exec.Command("g++", "-std=c++17", src, "-O2", "-o", bin)
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %s", out)
		}
		defer os.Remove(bin)
		run := exec.Command(bin)
		inPath := strings.TrimSuffix(src, ".cpp") + ".in"
		if data, err := os.ReadFile(inPath); err == nil {
			run.Stdin = bytes.NewReader(data)
		}
		out, err := run.CombinedOutput()
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(out), nil
	})
}
