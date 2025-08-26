//go:build slow

package human

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
)

func TestRustSolutions(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	golden.Run(t, "tests/spoj/human/x/rust", ".rs", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".rs") + ".in"
		tmpDir, err := os.MkdirTemp("", "spoj_rust")
		if err != nil {
			return nil, err
		}
		bin := filepath.Join(tmpDir, "prog")
		if out, err := exec.Command("rustc", "-O", src, "-o", bin).CombinedOutput(); err != nil {
			return out, err
		}
		cmd := exec.Command(bin)
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
