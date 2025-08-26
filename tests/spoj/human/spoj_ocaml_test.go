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

func TestOcamlSolutions(t *testing.T) {
	if _, err := exec.LookPath("ocamlopt"); err != nil {
		t.Skip("ocaml toolchain not installed")
	}
	golden.Run(t, "tests/spoj/human/x/ocaml", ".ml", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(src, ".ml")
		inPath := base + ".in"
		exe := base + ".exe"
		compile := exec.Command("ocamlopt", "-o", exe, src)
		if out, err := compile.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %v: %s", err, out)
		}
		defer os.Remove(exe)
		defer os.Remove(base + ".cmi")
		defer os.Remove(base + ".cmx")
		defer os.Remove(base + ".o")
		cmd := exec.Command(exe)
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
