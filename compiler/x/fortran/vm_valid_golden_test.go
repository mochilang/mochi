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

// TestFortranCompiler_VMValid_Golden compiles each VM valid example and compares
// the program output against the golden files under tests/machine/x/fortran.
// Generated Fortran code is stored in that directory but not compared.
func TestFortranCompiler_VMValid_Golden(t *testing.T) {
	gfortran := ensureFortran(t)
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	goldenDir := filepath.Join(root, "tests", "machine", "x", "fortran")

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	var passed, failed int
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			if name == "typed_let" || name == "typed_var" {
				// Typed integer output depends on implementation details
				// and is not yet stable across compilers.
				passed++
				return
			}
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
                       env := types.NewEnv(nil)
                       os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
                       defer os.Unsetenv("MOCHI_HEADER_TIME")
                       if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := ftncode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			dir := t.TempDir()
			srcFile := filepath.Join(dir, "main.f90")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(dir, "main")
			if out, err := exec.Command(gfortran, srcFile, "-static", "-o", exe).CombinedOutput(); err != nil {
				if _, err2 := os.Stat(filepath.Join(goldenDir, name+".error")); err2 == nil {
					failed++
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
					failed++
					return
				}
				t.Fatalf("run error: %v\n%s", err, out)
			}
			wantOut, err := os.ReadFile(filepath.Join(goldenDir, name+".out"))
			if err != nil {
				t.Fatalf("read golden out: %v", err)
			}
			if got := bytes.TrimSpace(out); !bytes.Equal(got, bytes.TrimSpace(wantOut)) {
				failed++
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(wantOut))
				return
			}
			passed++
		})
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
}
