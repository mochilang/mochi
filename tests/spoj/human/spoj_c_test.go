//go:build slow

package human

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
)

func TestCSolutions(t *testing.T) {
	if _, err := exec.LookPath("gcc"); err != nil {
		t.Skip("gcc not installed")
	}
	golden.Run(t, "tests/spoj/human/x/c", ".c", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".c") + ".in"
		tmpDir, err := os.MkdirTemp("", "spojc")
		if err != nil {
			return nil, err
		}
		bin := filepath.Join(tmpDir, "a.out")
		if out, err := exec.Command("gcc", src, "-O2", "-o", bin).CombinedOutput(); err != nil {
			return nil, err
		} else {
			_ = out // ignore compiler output
		}
		cmd := exec.Command(bin)
		if data, err := os.ReadFile(inPath); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(out), nil
	})
}
