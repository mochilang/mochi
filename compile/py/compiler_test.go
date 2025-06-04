package pycode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	pycode "mochi/compile/py"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestPyCompiler_SubsetPrograms(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pycode.New()
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.py")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("python3", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ python run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestPyCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".py.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pycode.New()
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
