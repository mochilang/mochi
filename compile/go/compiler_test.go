package gocode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestGoCompiler_SubsetPrograms(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := gocode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.go")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("go", "run", file)
		cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ go run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestGoCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".go.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := gocode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
