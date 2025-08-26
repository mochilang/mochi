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

func TestPrologSolutions(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	golden.Run(t, "tests/spoj/human/x/prolog", ".pl", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".pl") + ".in"
		cmd := exec.Command("swipl", "-q", "-f", src)
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
