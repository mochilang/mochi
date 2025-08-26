//go:build slow

package human

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"testing"

	"mochi/golden"
)

func TestCobolSolutions(t *testing.T) {
	if _, err := exec.LookPath("cobc"); err != nil {
		t.Skip("cobc not installed")
	}
	golden.Run(t, "tests/spoj/human/x/cobol", ".cob", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".cob") + ".in"
		bin := strings.TrimSuffix(src, ".cob")
		if out, err := exec.Command("cobc", "-x", src, "-o", bin).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %s", out)
		}
		defer os.Remove(bin)
		run := exec.Command(bin)
		if data, err := os.ReadFile(inPath); err == nil {
			run.Stdin = bytes.NewReader(data)
		}
		out, err := run.CombinedOutput()
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(out), nil
	})
}
