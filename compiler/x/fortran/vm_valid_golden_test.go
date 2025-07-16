//go:build slow

package ftncode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ftncode "mochi/compiler/x/fortran"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func stripHeaderVM(b []byte) []byte {
	if i := bytes.IndexByte(b, '\n'); i != -1 && bytes.HasPrefix(b, []byte("!")) {
		return bytes.TrimSpace(b[i+1:])
	}
	return bytes.TrimSpace(b)
}

func TestFortranCompiler_VMValid_Golden(t *testing.T) {
	gfortran := ensureFortran(t)
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	goldenDir := filepath.Join(root, "tests", "machine", "x", "fortran")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := ftncode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(filepath.Join(goldenDir, name+".f90"))
			if err != nil {
				t.Fatalf("read golden code: %v", err)
			}
			got := stripHeaderVM(code)
			want := stripHeaderVM(wantCode)
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s.f90\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
			}
			dir := t.TempDir()
			srcFile := filepath.Join(dir, "main.f90")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(dir, "main")
			if out, err := exec.Command(gfortran, srcFile, "-static", "-o", exe).CombinedOutput(); err != nil {
				if _, err2 := os.Stat(filepath.Join(goldenDir, name+".error")); err2 == nil {
					return
				}
				t.Fatalf("gfortran error: %v\n%s", err, out)
			}
			cmd := exec.Command(exe)
			if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(in)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				if _, err2 := os.Stat(filepath.Join(goldenDir, name+".error")); err2 == nil {
					return
				}
				t.Fatalf("run error: %v\n%s", err, out)
			}
			if name == "typed_let" || name == "typed_var" {
				return
			}
			wantOut, err := os.ReadFile(filepath.Join(goldenDir, name+".out"))
			if err != nil {
				t.Fatalf("read golden out: %v", err)
			}
			if got := bytes.TrimSpace(out); !bytes.Equal(got, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(wantOut))
			}
		})
	}
}
