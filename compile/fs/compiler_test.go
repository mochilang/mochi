package fscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	fscode "mochi/compile/fs"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestFSCompiler_SubsetPrograms(t *testing.T) {
	if err := fscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "fsi", "--help").Run(); err != nil {
		t.Skipf("dotnet fsi not runnable: %v", err)
	}
	golden.Run(t, "tests/compiler/fs", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := fscode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.fsx")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("dotnet", "fsi", "--quiet", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ fsi error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestFSCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/fs", ".mochi", ".fs.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := fscode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
