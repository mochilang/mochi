package javacode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	javacode "mochi/compile/java"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestJavaCompiler_SubsetPrograms(t *testing.T) {
	t.Skip("Java compiler execution tests not implemented")
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
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
		c := javacode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "Main.java")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("javac", "Main.java")
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ javac error: %w\n%s", err, out)
		}
		cmd = exec.Command("java", "-cp", dir, "Main")
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ java run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestJavaCompiler_GoldenOutput(t *testing.T) {
	t.Skip("Java golden outputs not implemented")
	golden.Run(t, "tests/compiler/valid", ".mochi", ".java.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := javacode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
