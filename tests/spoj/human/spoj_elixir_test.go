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

func TestElixirSolutions(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir toolchain not installed")
	}
	golden.Run(t, "tests/spoj/human/x/elixir", ".ex", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".ex") + ".in"
		cmd := exec.Command("elixir", src)
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
