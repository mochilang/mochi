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
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestFortranCompiler_VMValid_Golden compiles each program under tests/vm/valid
// to Fortran, executes it with gfortran and compares the output against the
// golden .out files in tests/machine/x/fortran. Generated .f90 files and any
// .error logs are written to that directory but not compared.
func TestFortranCompiler_VMValid_Golden(t *testing.T) {
	gfortran := ensureFortran(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "fortran")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")

		codePath := filepath.Join(outDir, base+".f90")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
		code, err := ftncode.New(env).Compile(prog)
		os.Unsetenv("MOCHI_HEADER_TIME")
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}

		dir := t.TempDir()
		srcFile := filepath.Join(dir, "main.f90")
		if err := os.WriteFile(srcFile, code, 0o644); err != nil {
			return nil, err
		}
		exe := filepath.Join(dir, "main")
		if out, err := exec.Command(gfortran, srcFile, "-static", "-o", exe).CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd := exec.Command(exe)
		if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(in)
		}
		outBytes, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), outBytes...), 0o644)
			return nil, err
		}
		outBytes = bytes.TrimSpace(outBytes)
		_ = os.WriteFile(filepath.Join(outDir, base+".out"), outBytes, 0o644)
		_ = os.Remove(errPath)
		return outBytes, nil
	})
}
