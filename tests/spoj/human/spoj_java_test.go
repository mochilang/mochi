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

func TestJavaSolutions(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}

	golden.Run(t, "tests/spoj/human/x/java", ".java", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".java") + ".in"
		tmpDir := t.TempDir()
		cmd := exec.Command("javac", "-d", tmpDir, src)
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("javac: %v: %s", err, out)
		}
		className := "Main"
		cmd = exec.Command("java", "-cp", tmpDir, className)
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
