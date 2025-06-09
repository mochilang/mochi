package swcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	swcode "mochi/compile/sw"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestSwiftCompiler_SubsetPrograms(t *testing.T) {
	t.Skip("Swift compiler backend not fully implemented")
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := swcode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.swift")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		exe := filepath.Join(dir, "main")
		cmd := exec.Command("swiftc", file, "-o", exe)
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ swiftc error: %w\n%s", err, out)
		}
		out, err := exec.Command(exe).CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ swift run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestSwiftCompiler_GoldenOutput(t *testing.T) {
	t.Skip("Swift compiler backend not fully implemented")
}
