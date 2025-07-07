//go:build archived && slow

package stcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	stcode "mochi/archived/x/st"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestSTCompiler_LeetCodeExample1(t *testing.T) {
	runExample(t, 1)
}

func TestSTCompiler_LeetCodeExamples(t *testing.T) {
	for i := 1; i <= 10; i++ {
		runExample(t, i)
	}
}

func TestSTCompiler_SubsetPrograms(t *testing.T) {
	if err := stcode.EnsureSmalltalk(); err != nil {
		t.Skipf("smalltalk not installed: %v", err)
	}
	run := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := stcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.st")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		inData, _ := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in")
		cmd := exec.Command("gst", file)
		if len(inData) > 0 {
			cmd.Stdin = bytes.NewReader(inData)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ gst error: %w\n%s", err, out)
		}
		got := bytes.TrimSpace(out)

		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("❌ vm compile error: %w", err)
		}
		var vmBuf bytes.Buffer
		m := vm.NewWithIO(p, bytes.NewReader(inData), &vmBuf)
		if err := m.Run(); err != nil {
			return nil, fmt.Errorf("❌ vm run error: %w", err)
		}
		want := bytes.TrimSpace(vmBuf.Bytes())
		if !bytes.Equal(got, want) {
			return nil, fmt.Errorf("output mismatch\n\n--- VM ---\n%s\n\n--- ST ---\n%s\n", want, got)
		}
		return got, nil
	}

	golden.Run(t, "tests/compiler/st", ".mochi", ".out", run)
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", run)
}

func TestSTCompiler_GoldenOutput(t *testing.T) {
	if err := stcode.EnsureSmalltalk(); err != nil {
		t.Skipf("smalltalk not installed: %v", err)
	}

	run := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := stcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}

		dir := t.TempDir()
		file := filepath.Join(dir, "main.st")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		inData, _ := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in")
		cmd := exec.Command("gst", file)
		if len(inData) > 0 {
			cmd.Stdin = bytes.NewReader(inData)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ gst error: %w\n%s", err, out)
		}
		got := bytes.TrimSpace(out)

		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("❌ vm compile error: %w", err)
		}
		var vmBuf bytes.Buffer
		m := vm.NewWithIO(p, bytes.NewReader(inData), &vmBuf)
		if err := m.Run(); err != nil {
			return nil, fmt.Errorf("❌ vm run error: %w", err)
		}
		want := bytes.TrimSpace(vmBuf.Bytes())
		if !bytes.Equal(got, want) {
			return nil, fmt.Errorf("output mismatch\n\n--- VM ---\n%s\n\n--- ST ---\n%s\n", want, got)
		}

		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/st", ".mochi", ".st.out", run)
	golden.Run(t, "tests/compiler/valid", ".mochi", ".st.out", run)
}

func runExample(t *testing.T, i int) {
	t.Helper()
	if err := stcode.EnsureSmalltalk(); err != nil {
		t.Skipf("smalltalk not installed: %v", err)
	}
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(i))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no examples found in %s", dir)
	}
	for _, f := range files {
		name := fmt.Sprintf("%d/%s", i, filepath.Base(f))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := stcode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.st")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("gst", file)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("gst error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}
