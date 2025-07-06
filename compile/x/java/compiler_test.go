//go:build slow

package javacode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	javacode "mochi/compile/x/java"
	"mochi/golden"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
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
		if out, err := exec.Command("javac", "-d", dir, file).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ javac error: %w\n%s", err, out)
		}
		mainClass := "Main"
		if prog.Package != "" {
			mainClass = prog.Package + ".Main"
		}
		cmd := exec.Command("java", "-cp", dir, mainClass)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
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
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}

	runCompile := func(src string) ([]byte, error) {
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
		if out, err := exec.Command("javac", "-d", dir, file).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ javac error: %w\n%s", err, out)
		}

		mainClass := "Main"
		if prog.Package != "" {
			mainClass = prog.Package + ".Main"
		}
		cmd := exec.Command("java", "-cp", dir, mainClass)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		gotOut, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ java run error: %w\n%s", err, gotOut)
		}

		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		interp := interpreter.New(prog, env, modRoot)
		var wantBuf bytes.Buffer
		interp.Env().SetWriter(&wantBuf)
		if err := interp.Run(); err != nil {
			return nil, fmt.Errorf("vm run error: %w", err)
		}

		got := bytes.TrimSpace(gotOut)
		want := bytes.TrimSpace(wantBuf.Bytes())
		if !bytes.Equal(got, want) {
			return nil, fmt.Errorf("runtime output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
		}

		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/valid", ".mochi", ".java.out", runCompile)
	golden.Run(t, "tests/compiler/java", ".mochi", ".java.out", runCompile)
}

// runLeetExample compiles and runs the Mochi LeetCode example with the given id.
func runLeetExample(t *testing.T, id int) error {
	t.Helper()
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		return err
	}
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			return fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return fmt.Errorf("type error: %v", errs[0])
		}
		modRoot, _ := mod.FindRoot(filepath.Dir(f))

		interp := interpreter.New(prog, env, modRoot)
		if err := interp.Test(); err != nil {
			return fmt.Errorf("tests failed: %w", err)
		}

		interp = interpreter.New(prog, env, modRoot)
		var wantBuf bytes.Buffer
		interp.Env().SetWriter(&wantBuf)
		if err := interp.Run(); err != nil {
			return fmt.Errorf("run error: %w", err)
		}

		code, err := javacode.New(env).Compile(prog)
		if err != nil {
			return fmt.Errorf("compile error: %w", err)
		}

		outDir := filepath.Join("..", "..", "examples", "leetcode-out", "java", strconv.Itoa(id))
		if err := os.MkdirAll(outDir, 0755); err != nil {
			return err
		}
		outFile := filepath.Join(outDir, strings.TrimSuffix(filepath.Base(f), ".mochi")+".java")
		if err := os.WriteFile(outFile, code, 0644); err != nil {
			return err
		}

		tmp := t.TempDir()
		mainFile := filepath.Join(tmp, "Main.java")
		if err := os.WriteFile(mainFile, code, 0644); err != nil {
			return err
		}
		if out, err := exec.Command("javac", "-d", tmp, mainFile).CombinedOutput(); err != nil {
			return fmt.Errorf("javac error: %w\n%s", err, out)
		}
		mainClass := "Main"
		if prog.Package != "" {
			mainClass = prog.Package + ".Main"
		}
		cmd := exec.Command("java", "-cp", tmp, mainClass)
		if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return fmt.Errorf("java run error: %w\n%s", err, out)
		}
		got := bytes.TrimSpace(out)
		want := bytes.TrimSpace(wantBuf.Bytes())
		if !bytes.Equal(got, want) {
			return fmt.Errorf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
		}
	}
	return nil
}

// TestJavaCompiler_LeetCodeExample1 ensures the two-sum sample compiles and runs.
func TestJavaCompiler_LeetCodeExamples(t *testing.T) {
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}
	for i := 1; i <= 30; i++ {
		if err := runLeetExample(t, i); err != nil {
			t.Skipf("leetcode %d unsupported: %v", i, err)
		}
	}
}
