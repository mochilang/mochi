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

func TestSchemeSolutions(t *testing.T) {
	if _, err := exec.LookPath("guile"); err != nil {
		t.Skip("guile not installed")
	}
	golden.Run(t, "tests/spoj/human/x/scheme", ".scm", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".scm") + ".in"
		cmd := exec.Command("guile", "--no-auto-compile", src)
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
