//go:build archived && slow

package ftncode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ftncode "mochi/archived/x/fortran"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestFortranCompiler_LeetExamples(t *testing.T) {
	for i := 1; i <= 2; i++ {
		runFortranLeetExample(t, fmt.Sprint(i))
	}
}

func TestFortranCompiler_GoldenOutput(t *testing.T) {
	compile := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := ftncode.New().Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/fortran", ".mochi", ".f90.out", compile)
}

func TestFortranCompiler_SubsetPrograms(t *testing.T) {
	gfortran, err := ftncode.EnsureFortran()
	if err != nil {
		t.Skipf("gfortran not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/fortran", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := ftncode.New().Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		dir := t.TempDir()
		ffile := filepath.Join(dir, "prog.f90")
		if err := os.WriteFile(ffile, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		if strings.Contains(string(code), "mylib.f90") {
			data, err := os.ReadFile(filepath.Join(filepath.Dir(src), "mylib.f90"))
			if err == nil {
				_ = os.WriteFile(filepath.Join(dir, "mylib.f90"), data, 0644)
			}
		}
		exe := filepath.Join(dir, "prog")
		if out, err := exec.Command(gfortran, "-ffree-line-length-none", ffile, "-o", exe).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("gfortran error: %w\n%s", err, out)
		}
		out, err := exec.Command(exe).CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("run error: %w\n%s", err, out)
		}
		out = bytes.TrimSpace(out)
		fields := bytes.Fields(out)
		return bytes.Join(fields, []byte("\n")), nil
	})
}

// runFortranLeetExample compiles and executes the Mochi LeetCode example with
// the given ID. The example directory is expected to contain a single `.mochi`
// source file whose output is printed to stdout. For the two-sum problem
// (leetcode/1) the expected output is `0\n1`.
func runFortranLeetExample(t *testing.T, id string) {
	gfortran, err := ftncode.EnsureFortran()
	if err != nil {
		t.Skipf("gfortran not installed: %v", err)
	}
	root := findRoot(t)
	dir := filepath.Join(root, "examples", "leetcode", id)
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("read dir error: %v", err)
	}
	var src string
	for _, e := range entries {
		if filepath.Ext(e.Name()) == ".mochi" {
			src = filepath.Join(dir, e.Name())
			break
		}
	}
	if src == "" {
		t.Fatalf("no .mochi file found in %s", dir)
	}
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := ftncode.New().Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	tmp := t.TempDir()
	ffile := filepath.Join(tmp, "prog.f90")
	if err := os.WriteFile(ffile, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(tmp, "prog")
	if out, err := exec.Command(gfortran, "-ffree-line-length-none", ffile, "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("gfortran error: %v\n%s", err, out)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	fields := bytes.Fields(out)
	got := string(bytes.Join(fields, []byte("\n")))
	if id == "1" {
		if got != "0\n1" {
			t.Fatalf("unexpected output: %q", got)
		}
	} else {
		if got != "" {
			t.Fatalf("unexpected output: %q", got)
		}
	}
}

// findRoot returns the repository root directory by locating go.mod.
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
