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

func TestZigSolutions(t *testing.T) {
	if _, err := exec.LookPath("zig"); err != nil {
		t.Skip("zig toolchain not installed")
	}
	golden.Run(t, "tests/spoj/human/x/zig", ".zig", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".zig") + ".in"
		cmd := exec.Command("zig", "run", src)
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
