//go:build slow

package human

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
)

func TestPascalSolutions(t *testing.T) {
	if _, err := exec.LookPath("fpc"); err != nil {
		t.Skip("fpc compiler not installed")
	}
	golden.Run(t, "tests/spoj/human/x/pascal", ".pas", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".pas") + ".in"
		tmpDir, err := os.MkdirTemp("", "pascal")
		if err != nil {
			return nil, err
		}
		defer os.RemoveAll(tmpDir)
		exeName := strings.TrimSuffix(filepath.Base(src), ".pas")
		exePath := filepath.Join(tmpDir, exeName)
		cmd := exec.Command("fpc", "-O2", "-v0", "-FE"+tmpDir, "-FU"+tmpDir, "-o"+exePath, src)
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %v: %s", err, out)
		}
		run := exec.Command(exePath)
		if data, err := os.ReadFile(inPath); err == nil {
			run.Stdin = bytes.NewReader(data)
		}
		out, err := run.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("run error: %v: %s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}
