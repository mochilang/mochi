//go:build slow

package human

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
)

func TestKotlinSolutions(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc toolchain not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	golden.Run(t, "tests/spoj/human/x/kotlin", ".kt", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".kt") + ".in"
		jarPath := filepath.Join(t.TempDir(), "prog.jar")
		cmd := exec.Command("kotlinc", src, "-include-runtime", "-d", jarPath)
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile: %v: %s", err, out)
		}
		runCmd := exec.Command("java", "-jar", jarPath)
		if data, err := os.ReadFile(inPath); err == nil {
			runCmd.Stdin = bytes.NewReader(data)
		}
		out, err := runCmd.CombinedOutput()
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(out), nil
	})
}
