package scalacode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	scalacode "mochi/compile/scala"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestScalaCompiler_SubsetPrograms(t *testing.T) {
	t.Skip("scala tooling not available")
	golden.Run(t, "tests/compiler/scala", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := scalacode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "Main.scala")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		if out, err := exec.Command("scalac", file).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ scalac error: %w\n%s", err, out)
		}
		out, err := exec.Command("scala", "Main").CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ scala run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestScalaCompiler_GoldenOutput(t *testing.T) {
	t.Skip("scala tooling not available")
	golden.Run(t, "tests/compiler/scala", ".mochi", ".scala.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := scalacode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
