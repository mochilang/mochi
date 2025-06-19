//go:build slow

package ftncode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	ftncode "mochi/compile/fortran"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

func runLeet(t *testing.T, id int) {
	t.Helper()
	gfortran, err := ftncode.EnsureFortran()
	if err != nil {
		t.Skipf("gfortran not installed: %v", err)
	}
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
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

			modRoot, _ := mod.FindRoot(filepath.Dir(f))

			interp := interpreter.New(prog, env, modRoot)
			if err := interp.Test(); err != nil {
				t.Fatalf("tests failed: %v", err)
			}

			interp = interpreter.New(prog, env, modRoot)
			var wantBuf bytes.Buffer
			interp.Env().SetWriter(&wantBuf)
			if err := interp.Run(); err != nil {
				t.Fatalf("run error: %v", err)
			}

			code, err := ftncode.New().Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}

			outDir := filepath.Join("..", "..", "examples", "leetcode-out", "fortran", strconv.Itoa(id))
			if err := os.MkdirAll(outDir, 0755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
			outFile := filepath.Join(outDir, strings.TrimSuffix(filepath.Base(f), ".mochi")+".f90")
			if err := os.WriteFile(outFile, code, 0644); err != nil {
				t.Fatalf("write output: %v", err)
			}

			tmp := t.TempDir()
			ffile := filepath.Join(tmp, "main.f90")
			if err := os.WriteFile(ffile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(tmp, "main")
			if out, err := exec.Command(gfortran, ffile, "-o", exe).CombinedOutput(); err != nil {
				t.Fatalf("gfortran error: %v\n%s", err, out)
			}
			cmd := exec.Command(exe)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run error: %v\n%s", err, out)
			}
			got := bytes.Join(bytes.Fields(out), []byte("\n"))
			want := bytes.Join(bytes.Fields(wantBuf.Bytes()), []byte("\n"))
			if !bytes.Equal(got, want) {
				t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
			}
		})
	}
}

func TestFortranCompiler_LeetCodeExamples_Output(t *testing.T) {
	for i := 1; i <= 7; i++ {
		runLeet(t, i)
	}
}
