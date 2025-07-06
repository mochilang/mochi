//go:build slow

package rktcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rktcode "mochi/compile/x/rkt"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestRacketCompiler_TwoSum(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	root := findRoot(t)
	src := filepath.Join(root, "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := rktcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.rkt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("racket", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("racket run error: %v\n%s", err, out)
	}
	res := bytes.TrimSpace(out)
	if string(res) != "0\n1" {
		t.Fatalf("unexpected output: %q", res)
	}
}

func TestRacketCompiler_SubsetPrograms(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/rkt", ".mochi", ".out", func(src string) ([]byte, error) {
		return compileRun(t, src)
	})
}

func TestRacketCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/rkt", ".mochi", ".rkt.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := rktcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func compileRun(t *testing.T, src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("\u274c parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("\u274c type error: %v", errs[0])
	}
	code, err := rktcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("\u274c compile error: %w", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.rkt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return nil, fmt.Errorf("write error: %w", err)
	}
	cmd := exec.Command("racket", file)
	var inData []byte
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		inData = data
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("\u274c racket run error: %w\n%s", err, out)
	}
	racketOut := bytes.TrimSpace(out)

	vmProg, err := vm.CompileWithSource(prog, env, string(mustRead(src)))
	if err != nil {
		return nil, fmt.Errorf("vm compile error: %w", err)
	}
	var buf bytes.Buffer
	m := vm.NewWithIO(vmProg, bytes.NewReader(inData), &buf)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return nil, fmt.Errorf("vm run error:\n%s", ve.Format(vmProg))
		}
		return nil, fmt.Errorf("vm run error: %v", err)
	}
	want := bytes.TrimSpace(buf.Bytes())
	if !bytes.Equal(racketOut, want) {
		t.Errorf("output mismatch vs VM for %s\n\n--- Racket ---\n%s\n\n--- VM ---\n%s\n", filepath.Base(src), racketOut, want)
	}
	if racketOut == nil {
		racketOut = []byte{}
	}
	return racketOut, nil
}

func mustRead(path string) []byte {
	data, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return data
}

func TestRacketCompiler_LeetCodeExamples(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	for i := 1; i <= 10; i++ {
		runRacketLeetExample(t, i)
	}
}

func runRacketLeetExample(t *testing.T, id int) {
	t.Helper()
	root := findRoot(t)
	dir := filepath.Join(root, "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, f := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(f))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := rktcode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.rkt")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("racket", file)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("racket run error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}

func findRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}
