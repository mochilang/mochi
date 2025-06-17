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
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/java", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := javacode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "Main.java")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		if out, err := exec.Command("javac", file).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ javac error: %w\n%s", err, out)
		}
		out, err := exec.Command("java", "-cp", dir, "Main").CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ java run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestJavaCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/java", ".mochi", ".java.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := javacode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
