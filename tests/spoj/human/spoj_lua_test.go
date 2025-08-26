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

func TestLuaSolutions(t *testing.T) {
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua interpreter not installed")
	}
	golden.Run(t, "tests/spoj/human/x/lua", ".lua", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".lua") + ".in"
		cmd := exec.Command("lua", src)
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
