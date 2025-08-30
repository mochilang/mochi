//go:build slow

package human

import (
	"bytes"
	"os"
	"os/exec"
	"strings"
	"testing"

	"mochi/golden"
)

func TestLeanSolutions(t *testing.T) {
	if _, err := exec.LookPath("lean"); err != nil {
		t.Skip("lean toolchain not installed")
	}
	golden.Run(t, "tests/spoj/human/x/lean", ".lean", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".lean") + ".in"
		cmd := exec.Command("lean", "--run", src)
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
