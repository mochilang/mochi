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

func TestMochiSolutions(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}
	golden.Run(t, "tests/spoj/human/x/mochi", ".mochi", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".mochi") + ".in"
		cmd := exec.Command("go", "run", "./cmd/mochi", "run", src)
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
