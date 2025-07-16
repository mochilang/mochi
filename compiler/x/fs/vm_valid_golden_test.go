//go:build slow

package fscode_test

import (
    "bytes"
    "fmt"
    "os/exec"
    "testing"

    fscode "mochi/compiler/x/fs"
    "mochi/golden"
    "mochi/parser"
    "mochi/types"
)

// TestFSCompiler_VMValid_GoldenCode verifies generated F# code for
// each program in tests/vm/valid matches the checked in .fs.out files.
func TestFSCompiler_VMValid_GoldenCode(t *testing.T) {
    if _, err := exec.LookPath("fsharpc"); err != nil {
        t.Skip("fsharpc not installed")
    }
    golden.Run(t, "tests/vm/valid", ".mochi", ".fs.out", func(src string) ([]byte, error) {
        prog, err := parser.Parse(src)
        if err != nil {
            return nil, fmt.Errorf("parse error: %w", err)
        }
        env := types.NewEnv(nil)
        if errs := types.Check(prog, env); len(errs) > 0 {
            return nil, fmt.Errorf("type error: %v", errs[0])
        }
        code, err := fscode.CompileFile(src)
        if err != nil {
            return nil, fmt.Errorf("compile error: %w", err)
        }
        return bytes.TrimSpace(code), nil
    })
}
