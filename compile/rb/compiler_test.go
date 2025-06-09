package rbcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	rbcode "mochi/compile/rb"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestRBCompiler_SubsetPrograms(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
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
		c := rbcode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.rb")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("ruby", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ ruby run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestRBCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".rb.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := rbcode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
