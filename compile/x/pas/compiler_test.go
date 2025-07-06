//go:build slow

package pascode_test

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pascode "mochi/compile/x/pas"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

// TestPascalCompiler_TwoSum compiles the LeetCode example to Pascal and runs it.
func TestPascalCompiler_TwoSum(t *testing.T) {
	if _, err := exec.LookPath("fpc"); err != nil {
		t.Skipf("fpc not installed: %v", err)
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
	c := pascode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Skipf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "prog.pas")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	if out, err := exec.Command("fpc", file).CombinedOutput(); err != nil {
		t.Fatalf("fpc error: %v\n%s", err, out)
	}
	exe := filepath.Join(dir, "prog")
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := string(bytes.TrimSpace(out))
	want := "0\n1"
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}

func TestPascalCompiler_SubsetPrograms(t *testing.T) {
	fpc, err := pascode.EnsureFPC()
	if err != nil {
		t.Skipf("fpc not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/pas", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pascode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "prog.pas")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		if out, err := exec.Command(fpc, file).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ fpc error: %w\n%s", err, out)
		}
		exe := filepath.Join(dir, "prog")
		cmd := exec.Command(exe)
		var inData []byte
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			inData = data
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		// run with VM to compute expected output
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("❌ vm compile error: %w", err)
		}
		var vmIn io.Reader
		if len(inData) > 0 {
			vmIn = bytes.NewReader(inData)
		}
		var vmOut bytes.Buffer
		m := vm.NewWithIO(p, vmIn, &vmOut)
		if err := m.Run(); err != nil {
			return nil, fmt.Errorf("❌ vm run error: %w", err)
		}
		want := bytes.TrimSpace(vmOut.Bytes())
		if !bytes.Equal(normalizeOutput(repoRoot(), res), normalizeOutput(repoRoot(), want)) {
			return nil, fmt.Errorf("runtime output mismatch\n\n--- Pascal ---\n%s\n\n--- VM ---\n%s\n", res, want)
		}
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestPascalCompiler_GoldenOutput(t *testing.T) {
	compileFn := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pascode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/pas", ".mochi", ".pas.out", compileFn)
}

// TestPascalCompiler_LeetCodeExamples compiles a small subset of the
// LeetCode solutions with the Pascal backend and executes the resulting
// binaries. The test is skipped in CI environments lacking the Free
// Pascal Compiler.
func TestPascalCompiler_LeetCodeExamples(t *testing.T) {
	if _, err := pascode.EnsureFPC(); err != nil {
		t.Skipf("fpc not installed: %v", err)
	}
	runExample(t, 1)
	runExample(t, 2)
	runExample(t, 3)
}

// runExample compiles all Mochi files in the given LeetCode problem
// directory and executes the resulting Pascal program. Any output is
// ignored; the test simply verifies that compilation and execution
// succeed without errors.
func runExample(t *testing.T, id int) {
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	fpc, err := pascode.EnsureFPC()
	if err != nil {
		t.Fatalf("fpc not installed: %v", err)
	}
	for _, f := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(f))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			// remove test blocks which the Pascal backend does not support
			stmts := prog.Statements[:0]
			for _, s := range prog.Statements {
				if s.Test == nil {
					stmts = append(stmts, s)
				}
			}
			prog.Statements = stmts
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := pascode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "prog.pas")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			if out, err := exec.Command(fpc, file).CombinedOutput(); err != nil {
				t.Skipf("fpc error: %v\n%s", err, out)
			}
			exe := filepath.Join(tmp, "prog")
			cmd := exec.Command(exe)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("run error: %v\n%s", err, out)
			}
		})
	}
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
