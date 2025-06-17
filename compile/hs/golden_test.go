//go:build slow

package hscode_test

import (
    "bytes"
    "fmt"
    "os"
    "os/exec"
    "path/filepath"
    "strings"
    "testing"

    "mochi/golden"
    hscode "mochi/compile/hs"
    "mochi/parser"
    "mochi/types"
)

func TestHSCompiler_Golden(t *testing.T) {
    if err := hscode.EnsureHaskell(); err != nil {
        t.Skipf("haskell not installed: %v", err)
    }

    golden.Run(t, "tests/compiler/hs", ".mochi", ".out", func(src string) ([]byte, error) {
        prog, err := parser.Parse(src)
        if err != nil {
            return nil, fmt.Errorf("❌ parse error: %w", err)
        }
        env := types.NewEnv(nil)
        if errs := types.Check(prog, env); len(errs) > 0 {
            return nil, fmt.Errorf("❌ type error: %v", errs[0])
        }
        c := hscode.New(env)
        code, err := c.Compile(prog)
        if err != nil {
            return nil, fmt.Errorf("❌ compile error: %w", err)
        }
        dir := t.TempDir()
        file := filepath.Join(dir, "main.hs")
        if err := os.WriteFile(file, code, 0644); err != nil {
            return nil, fmt.Errorf("write error: %w", err)
        }
        cmd := exec.Command("runhaskell", file)
        if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi")+".in"); err == nil {
            cmd.Stdin = bytes.NewReader(data)
        }
        out, err := cmd.CombinedOutput()
        if err != nil {
            return nil, fmt.Errorf("❌ runhaskell error: %w\n%s", err, out)
        }
        res := bytes.TrimSpace(out)
        if res == nil {
            res = []byte{}
        }
        return res, nil
    })

    golden.Run(t, "tests/compiler/hs", ".mochi", ".hs.out", func(src string) ([]byte, error) {
        prog, err := parser.Parse(src)
        if err != nil {
            return nil, fmt.Errorf("❌ parse error: %w", err)
        }
        env := types.NewEnv(nil)
        if errs := types.Check(prog, env); len(errs) > 0 {
            return nil, fmt.Errorf("❌ type error: %v", errs[0])
        }
        c := hscode.New(env)
        code, err := c.Compile(prog)
        if err != nil {
            return nil, fmt.Errorf("❌ compile error: %w", err)
        }
        return bytes.TrimSpace(code), nil
    })
}
