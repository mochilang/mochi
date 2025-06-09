package ktcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	ktcode "mochi/compile/kt"
	"mochi/golden"
	"mochi/parser"
)

func TestKTCompiler_SubsetPrograms(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java not installed")
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		c := ktcode.New()
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "Main.kt")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		jar := filepath.Join(dir, "app.jar")
		cmd := exec.Command("kotlinc", file, "-include-runtime", "-d", jar)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ kotlinc error: %w\n%s", err, out)
		}
		cmd = exec.Command("java", "-jar", jar)
		out, err = cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ java run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}
