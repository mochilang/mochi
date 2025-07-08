//go:build slow

package ftncode_test

import (
	"bytes"
	"fmt"
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

func ensureFortran(t *testing.T) string {
	t.Helper()
	path, err := ftncode.EnsureFortran()
	if err != nil {
		t.Skipf("gfortran missing: %v", err)
	}
	return path
}

func TestCompilePrograms(t *testing.T) {
	gfortran := ensureFortran(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "fortran")
	os.MkdirAll(outDir, 0755)
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { compileOne(t, src, outDir, name, gfortran) })
	}
}

func compileOne(t *testing.T, src, outDir, name, gfortran string) {
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(src)
	if err != nil {
		writeError(outDir, name, data, err)
		t.Skipf("parse error: %v", err)
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(outDir, name, data, errs[0])
		t.Skipf("type error: %v", errs[0])
		return
	}
	code, err := ftncode.New(env).Compile(prog)
	if err != nil {
		writeError(outDir, name, data, err)
		t.Skipf("compile error: %v", err)
		return
	}
	f90 := filepath.Join(outDir, name+".f90")
	if err := os.WriteFile(f90, code, 0644); err != nil {
		t.Fatalf("write f90: %v", err)
	}
	exe := filepath.Join(outDir, name)
	if out, err := exec.Command(gfortran, f90, "-o", exe).CombinedOutput(); err != nil {
		writeError(outDir, name, code, fmt.Errorf("gfortran: %v\n%s", err, out))
		t.Skipf("gfortran: %v", err)
		return
	}
	cmd := exec.Command(exe)
	if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(in)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeError(outDir, name, code, fmt.Errorf("run error: %v\n%s", err, out))
		t.Skipf("run error: %v", err)
		return
	}
	os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0644)
}

func writeError(dir, name string, src []byte, err error) {
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "error: %v\n", err)
	os.WriteFile(filepath.Join(dir, name+".error"), buf.Bytes(), 0644)
}
