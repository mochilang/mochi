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

func TestDartSolutions(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart toolchain not installed")
	}
	golden.Run(t, "tests/spoj/human/x/dart", ".dart", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".dart") + ".in"
		cmd := exec.Command("dart", src)
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
