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

func TestScalaSolutions(t *testing.T) {
	if _, err := exec.LookPath("scala"); err != nil {
		t.Skip("scala toolchain not installed")
	}
	golden.Run(t, "tests/spoj/human/x/scala", ".scala", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".scala") + ".in"
		cmd := exec.Command("scala", src)
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
