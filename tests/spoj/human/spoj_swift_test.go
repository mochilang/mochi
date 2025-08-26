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

func TestSwiftSolutions(t *testing.T) {
	if _, err := exec.LookPath("swift"); err != nil {
		t.Skip("swift toolchain not installed")
	}
	golden.Run(t, "tests/spoj/human/x/swift", ".swift", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".swift") + ".in"
		cmd := exec.Command("swift", src)
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
