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

func TestCsharpSolutions(t *testing.T) {
	if _, err := exec.LookPath("mcs"); err != nil {
		t.Skip("mcs compiler not installed")
	}
	if _, err := exec.LookPath("mono"); err != nil {
		t.Skip("mono runtime not installed")
	}
	golden.Run(t, "tests/spoj/human/x/csharp", ".cs", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".cs") + ".in"
		exePath := strings.TrimSuffix(src, ".cs") + ".exe"
		compile := exec.Command("mcs", "-out:"+exePath, src)
		if out, err := compile.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %v: %s", err, out)
		}
		cmd := exec.Command("mono", exePath)
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
