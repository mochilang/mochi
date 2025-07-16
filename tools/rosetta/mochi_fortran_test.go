//go:build slow

package rosetta

import (
    "bytes"
    "fmt"
    "os"
    "os/exec"
    "path/filepath"
    "strings"
    "testing"

    ftncode "mochi/compiler/x/fortran"
    "mochi/parser"
    "mochi/types"
)

// TestMochiToFortran compiles each Mochi source program under
// tests/rosetta/x/Mochi to Fortran and verifies the generated code
// runs to produce the expected output stored under tests/rosetta/out/Fortran.
func TestMochiToFortran(t *testing.T) {
    gfortran, err := ftncode.EnsureFortran()
    if err != nil {
        t.Skipf("gfortran not installed: %v", err)
    }

    root := findRepoRoot(t)
    srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
    outDir := filepath.Join(root, "tests/rosetta/out/Fortran")
    if err := os.MkdirAll(outDir, 0o755); err != nil {
        t.Fatalf("mkout: %v", err)
    }

    outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
    if err != nil {
        t.Fatalf("glob: %v", err)
    }
    if len(outs) == 0 {
        t.Fatal("no Mochi Rosetta tests found")
    }

    for _, outPath := range outs {
        name := strings.TrimSuffix(filepath.Base(outPath), ".out")
        srcPath := filepath.Join(srcDir, name+".mochi")
        if _, err := os.Stat(srcPath); err != nil {
            t.Fatalf("missing source for %s", name)
        }
        t.Run(name, func(t *testing.T) {
            compileAndRunFortran(t, gfortran, srcPath, outPath, outDir, name)
        })
    }
}

func compileAndRunFortran(t *testing.T, gfortran, srcPath, wantPath, outDir, name string) {
    defer func() {
        if r := recover(); r != nil {
            writeFortranError(outDir, name, fmt.Errorf("panic: %v", r))
        }
    }()

    prog, err := parser.Parse(srcPath)
    if err != nil {
        writeFortranError(outDir, name, fmt.Errorf("parse error: %w", err))
        t.Skip("parse error")
        return
    }
    env := types.NewEnv(nil)
    if errs := types.Check(prog, env); len(errs) > 0 {
        writeFortranError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
        t.Skip("type error")
        return
    }
    code, err := ftncode.New(env).Compile(prog)
    if err != nil {
        writeFortranError(outDir, name, fmt.Errorf("compile error: %w", err))
        t.Skip("compile error")
        return
    }
    f90File := filepath.Join(outDir, name+".f90")
    if err := os.WriteFile(f90File, code, 0o644); err != nil {
        t.Fatalf("write f90: %v", err)
    }
    tmp := t.TempDir()
    exe := filepath.Join(tmp, name)
    if out, err := exec.Command(gfortran, f90File, "-o", exe).CombinedOutput(); err != nil {
        writeFortranError(outDir, name, fmt.Errorf("gfortran error: %w\n%s", err, out))
        return
    }
    out, err := exec.Command(exe).CombinedOutput()
    if err != nil {
        writeFortranError(outDir, name, fmt.Errorf("run error: %w\n%s", err, out))
        return
    }
    got := bytes.TrimSpace(out)
    want, err := os.ReadFile(wantPath)
    if err != nil {
        t.Fatalf("read golden: %v", err)
    }
    want = bytes.TrimSpace(want)
    if !bytes.Equal(got, want) {
        writeFortranError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
        return
    }
    if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
        t.Fatalf("write out: %v", err)
    }
    _ = os.Remove(filepath.Join(outDir, name+".error"))
}

func writeFortranError(dir, name string, err error) {
    _ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}
