//go:build archived && slow

package phpcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	phpcode "mochi/archived/x/php"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
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
	for i := 1; i <= 30; i++ {
		runExample(t, i)
	}
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
		var input []byte
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			input = data
		}
		cmd := exec.Command("php", file)
		if input != nil {
			cmd.Stdin = bytes.NewReader(input)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ php run error: %w\n%s", err, out)
		}
		phpOut := strings.TrimSpace(strings.ReplaceAll(string(out), "\r\n", "\n"))

		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("vm compile error: %w", err)
		}
		var vmBuf bytes.Buffer
		m := vm.NewWithIO(p, bytes.NewReader(input), &vmBuf)
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("vm run error:\n%s", ve.Format(p))
			}
			return nil, fmt.Errorf("vm run error: %v", err)
		}
		vmOut := strings.TrimSpace(vmBuf.String())
		if phpOut != vmOut {
			return nil, fmt.Errorf("output mismatch\n-- php --\n%s\n-- vm --\n%s", phpOut, vmOut)
		}

		return []byte(phpOut), nil
	})
}

func TestPHPCompiler_GoldenOutput(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
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

		var inData []byte
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			inData = data
		}

		cmd := exec.Command("php", file)
		if inData != nil {
			cmd.Stdin = bytes.NewReader(inData)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ php run error: %w\n%s", err, out)
		}
		phpOut := bytes.TrimSpace(out)

		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("vm compile error: %w", err)
		}
		var vmBuf bytes.Buffer
		m := vm.NewWithIO(p, bytes.NewReader(inData), &vmBuf)
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("vm run error:\n%s", ve.Format(p))
			}
			return nil, fmt.Errorf("vm run error: %v", err)
		}
		vmOut := bytes.TrimSpace(vmBuf.Bytes())

		if !bytes.Equal(phpOut, vmOut) {
			return nil, fmt.Errorf("vm mismatch\n\n--- PHP ---\n%s\n\n--- VM ---\n%s\n", phpOut, vmOut)
		}

		outPath := strings.TrimSuffix(src, ".mochi") + ".out"
		if want, err := os.ReadFile(outPath); err == nil {
			root := repoRoot()
			if !bytes.Equal(normalizeOutput(root, phpOut), normalizeOutput(root, bytes.TrimSpace(want))) {
				return nil, fmt.Errorf("runtime output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", phpOut, want)
			}
		}

		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/valid", ".mochi", ".php.out", runCompile)
	golden.Run(t, "tests/compiler/php", ".mochi", ".php.out", runCompile)
}

func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	durRE := regexp.MustCompile(`\([0-9]+(\.[0-9]+)?(ns|µs|ms|s)\)`)
	out = durRE.ReplaceAllString(out, "(X)")
	out = strings.TrimSpace(out)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
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
	return dir
}
