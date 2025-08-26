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

func TestPHPSolutions(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	golden.Run(t, "tests/spoj/human/x/php", ".php", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".php") + ".in"
		cmd := exec.Command("php", src)
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
