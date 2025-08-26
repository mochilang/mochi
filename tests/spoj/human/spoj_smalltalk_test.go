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

func TestSmalltalkSolutions(t *testing.T) {
	if _, err := exec.LookPath("gst"); err != nil {
		t.Skip("gnu smalltalk not installed")
	}
	golden.Run(t, "tests/spoj/human/x/smalltalk", ".st", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".st") + ".in"
		cmd := exec.Command("gst", src)
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
