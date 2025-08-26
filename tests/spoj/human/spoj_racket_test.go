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

func TestRacketSolutions(t *testing.T) {
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
	}
	golden.Run(t, "tests/spoj/human/x/racket", ".rkt", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".rkt") + ".in"
		cmd := exec.Command("racket", src)
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
