//go:build slow

package phpcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	phpcode "mochi/compile/php"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestPHPCompiler_LeetCodeExample1(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
	}
	src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := phpcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.php")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("php", file)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("php run error: %v\n%s", err, out)
	}
	got := strings.ReplaceAll(string(out), "\r\n", "\n")
	if strings.TrimSpace(got) != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestPHPCompiler_LeetCodeExamples(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
	}
	runExample(t, 1)
	runExample(t, 2)
	runExample(t, 3)
	runExample(t, 4)
	runExample(t, 5)
}

func runExample(t *testing.T, i int) {
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(i))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
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
			c := phpcode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.php")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("php", file)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("php run error: %v\n%s", err, out)
			}
		})
	}
}

func TestPHPCompiler_SubsetPrograms(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/php", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := phpcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.php")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("php", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ php run error: %w\n%s", err, out)
		}
		res := strings.ReplaceAll(string(out), "\r\n", "\n")
		return []byte(strings.TrimSpace(res)), nil
	})
}

func TestPHPCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/php", ".mochi", ".php.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := phpcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
