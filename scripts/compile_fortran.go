package main

import (
    "bytes"
    "os"
    "os/exec"
    "path/filepath"
    "strings"

    ftn "mochi/transpiler/x/fortran"
    "mochi/parser"
    "mochi/types"
)

func repoRoot() string {
    dir, _ := os.Getwd()
    for i := 0; i < 10; i++ {
        if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
            return dir
        }
        p := filepath.Dir(dir)
        if p == dir {
            break
        }
        dir = p
    }
    return dir
}

func main() {
    if len(os.Args) < 2 {
        panic("usage: compile_fortran <file.mochi> ...")
    }
    root := repoRoot()
    outDir := filepath.Join(root, "tests", "transpiler", "x", "fortran")
    os.MkdirAll(outDir, 0o755)
    for _, src := range os.Args[1:] {
        base := strings.TrimSuffix(filepath.Base(src), ".mochi")
        f90Path := filepath.Join(outDir, base+".f90")
        outPath := filepath.Join(outDir, base+".out")
        errPath := filepath.Join(outDir, base+".error")
        prog, err := parser.Parse(src)
        if err != nil {
            os.WriteFile(errPath, []byte(err.Error()), 0o644)
            continue
        }
        env := types.NewEnv(nil)
        if errs := types.Check(prog, env); len(errs) > 0 {
            os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
            continue
        }
        ast, err := ftn.Transpile(prog, env)
        if err != nil {
            os.WriteFile(errPath, []byte(err.Error()), 0o644)
            continue
        }
        code := ast.Emit()
        if err := os.WriteFile(f90Path, code, 0o644); err != nil { panic(err) }
        exe := filepath.Join(outDir, base)
        if out, err := exec.Command("gfortran", f90Path, "-o", exe).CombinedOutput(); err != nil {
            os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
            continue
        }
        out, err := exec.Command(exe).CombinedOutput()
        if err != nil {
            os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
            continue
        }
        outBytes := bytes.TrimSpace(out)
        os.WriteFile(outPath, outBytes, 0o644)
        os.Remove(errPath)
    }
}
