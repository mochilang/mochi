//go:build slow

package ftncode_test

import (
    "bytes"
    "fmt"
    "os"
    "os/exec"
    "path/filepath"
    "testing"

    ftncode "mochi/compile/fortran"
    "mochi/golden"
    "mochi/parser"
    "mochi/types"
)

func TestFortranCompiler_TwoSum(t *testing.T) {
    gfortran, err := ftncode.EnsureFortran()
    if err != nil {
        t.Skipf("gfortran not installed: %v", err)
    }
    src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
    prog, err := parser.Parse(src)
    if err != nil { t.Fatalf("parse error: %v", err) }
    env := types.NewEnv(nil)
    if errs := types.Check(prog, env); len(errs) > 0 {
        t.Fatalf("type error: %v", errs[0])
    }
    code, err := ftncode.New().Compile(prog)
    if err != nil { t.Fatalf("compile error: %v", err) }
    dir := t.TempDir()
    ffile := filepath.Join(dir, "prog.f90")
    if err := os.WriteFile(ffile, code, 0644); err != nil {
        t.Fatalf("write error: %v", err)
    }
    exe := filepath.Join(dir, "prog")
    if out, err := exec.Command(gfortran, ffile, "-o", exe).CombinedOutput(); err != nil {
        t.Fatalf("gfortran error: %v\n%s", err, out)
    }
    out, err := exec.Command(exe).CombinedOutput()
    if err != nil {
        t.Fatalf("run error: %v\n%s", err, out)
    }
    lines := bytes.Fields(out)
    if len(lines) != 2 || string(lines[0]) != "0" || string(lines[1]) != "1" {
        t.Fatalf("unexpected output: %s", out)
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
