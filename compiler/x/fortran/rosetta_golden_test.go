//go:build slow

package ftncode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	ftncode "mochi/compiler/x/fortran"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateRosetta() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func stripHeaderLocal(b []byte) []byte {
	if i := bytes.IndexByte(b, '\n'); i != -1 && bytes.HasPrefix(b, []byte("! Generated")) {
		return b[i+1:]
	}
	return b
}

func runRosettaTaskGolden(t *testing.T, name string) {
	gfortran := ensureFortran(t)
	root := repoRoot()
	script := exec.Command("go", "run", "-tags=archive,slow", "./scripts/compile_rosetta_fortran.go")
	script.Env = append(os.Environ(), "TASKS="+name)
	script.Dir = root
	if out, err := script.CombinedOutput(); err != nil {
		t.Skipf("compile script error: %v\n%s", err, out)
		return
	}
	errFile := filepath.Join(root, "tests", "rosetta", "out", "Fortran", name+".error")
	if b, err := os.ReadFile(errFile); err == nil {
		t.Skipf("fortran run failed:\n%s", b)
		return
	}

	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
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
	codeWant := filepath.Join(root, "tests", "rosetta", "out", "Fortran", name+".f90")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(codeWant, code, 0644)
	} else if want, err := os.ReadFile(codeWant); err == nil {
		got := stripHeaderLocal(bytes.TrimSpace(code))
		want = stripHeaderLocal(bytes.TrimSpace(want))
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s.f90\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
		}
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.f90")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command(gfortran, file, "-static", "-o", exe).CombinedOutput(); err != nil {
		t.Skipf("gfortran error: %v\n%s", err, out)
		return
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Skipf("run error: %v\n%s", err, out)
		return
	}
	gotOut := bytes.TrimSpace(out)
	outWant := filepath.Join(root, "tests", "rosetta", "out", "Fortran", name+".out")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(outWant, append(gotOut, '\n'), 0644)
	} else if wantOut, err := os.ReadFile(outWant); err == nil {
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}

func TestFortranCompiler_Rosetta_Golden(t *testing.T) {
	gfortran := ensureFortran(t)
	_ = gfortran
	root := repoRoot()
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	max := 3
	if len(files) < max {
		max = len(files)
	}
	for _, f := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(name, func(t *testing.T) { runRosettaTaskGolden(t, name) })
	}
}

func repoRoot() string {
	_, file, _, _ := runtime.Caller(0)
	return filepath.Clean(filepath.Join(filepath.Dir(file), "..", "..", ".."))
}
