//go:build archived && slow

package schemecode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	schemecode "mochi/archived/x/scheme"
	"mochi/archived/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

var exceptionRE = regexp.MustCompile(`#<Exception [0-9]+>`)

func normalizeException(b []byte) []byte {
	return exceptionRE.ReplaceAll(b, []byte("#<Exception>"))
}

func cleanOutput(b []byte) []byte {
	lines := bytes.Split(bytes.TrimSpace(b), []byte("\n"))
	var kept [][]byte
	for _, l := range lines {
		if bytes.HasPrefix(l, []byte("ERROR:")) || bytes.HasPrefix(l, []byte("  called from")) {
			continue
		}
		kept = append(kept, l)
	}
	return bytes.Join(kept, []byte("\n"))
}

func runLeetExample(t *testing.T, id int) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, src := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(src))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := schemecode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.scm")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("scheme run error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}

// TestSchemeCompiler_LeetCode1 compiles the two-sum example and runs it.
func TestSchemeCompiler_LeetCode1(t *testing.T) {
	runLeetExample(t, 1)
}

// TestSchemeCompiler_LeetCode2 compiles the add-two-numbers example and runs it.
func TestSchemeCompiler_LeetCode2(t *testing.T) {
	runLeetExample(t, 2)
}

// TestSchemeCompiler_LeetCode3 compiles the longest-substring example and runs it.
func TestSchemeCompiler_LeetCode3(t *testing.T) {
	runLeetExample(t, 3)
}

// TestSchemeCompiler_LeetCode4 compiles the median-of-two-sorted-arrays example and runs it.
func TestSchemeCompiler_LeetCode4(t *testing.T) {
	runLeetExample(t, 4)
}

// TestSchemeCompiler_LeetCode5 compiles the longest-palindromic-substring example and runs it.
func TestSchemeCompiler_LeetCode5(t *testing.T) {
	runLeetExample(t, 5)
}

// TestSchemeCompiler_LeetCode6 compiles the zigzag-conversion example and runs it.
func TestSchemeCompiler_LeetCode6(t *testing.T) {
	runLeetExample(t, 6)
}

// TestSchemeCompiler_LeetCode7 compiles the reverse-integer example and runs it.
func TestSchemeCompiler_LeetCode7(t *testing.T) {
	runLeetExample(t, 7)
}

// TestSchemeCompiler_LeetCode8 compiles the string-to-integer example and runs it.
func TestSchemeCompiler_LeetCode8(t *testing.T) {
	runLeetExample(t, 8)
}

// TestSchemeCompiler_LeetCode9 compiles the palindrome-number example and runs it.
func TestSchemeCompiler_LeetCode9(t *testing.T) {
	runLeetExample(t, 9)
}

// TestSchemeCompiler_LeetCode10 compiles the regular-expression-matching example and runs it.
func TestSchemeCompiler_LeetCode10(t *testing.T) {
	runLeetExample(t, 10)
}

func TestSchemeCompiler_SubsetPrograms(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/scheme", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := schemecode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.scm")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
		var inData []byte
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
			inData = data
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c scheme run error: %w\n%s", err, out)
		}
		schemeOut := bytes.TrimSpace(normalizeException(cleanOutput(out)))

		// run with the Mochi VM for expected output
		p2, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("vm compile error: %w", err)
		}
		var vmBuf bytes.Buffer
		m := vm.NewWithIO(p2, bytes.NewReader(inData), &vmBuf)
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("vm run error:\n%s", ve.Format(p2))
			}
			return nil, fmt.Errorf("vm run error: %v", err)
		}
		vmOut := bytes.TrimSpace(vmBuf.Bytes())
		if !bytes.Equal(schemeOut, vmOut) {
			return nil, fmt.Errorf("output mismatch:\n-- scheme --\n%s\n-- vm --\n%s", schemeOut, vmOut)
		}
		return vmOut, nil
	})
}

func TestSchemeCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/scheme", ".mochi", ".scm.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := schemecode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func TestSchemeCompiler_JOBQ1(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := schemecode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scheme", "q1.scm.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.scm.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.scm")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
	out, _ := cmd.CombinedOutput()
	gotOut := normalizeException(cleanOutput(out))
	outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scheme", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, normalizeException(cleanOutput(wantOut))) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, normalizeException(cleanOutput(wantOut)))
	}
}

func TestSchemeCompiler_JOBQ2(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q2.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := schemecode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scheme", "q2.scm.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q2.scm.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.scm")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
	out, _ := cmd.CombinedOutput()
	gotOut := normalizeException(cleanOutput(out))
	outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scheme", "q2.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, normalizeException(cleanOutput(wantOut))) {
		t.Errorf("output mismatch for q2.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, normalizeException(cleanOutput(wantOut)))
	}
}
