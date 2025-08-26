//go:build slow

package human

import (
    "bytes"
    "os"
    "path/filepath"
    "strings"
    "testing"

    "mochi/golden"
    "mochi/parser"
    vm "mochi/runtime/vm"
    "mochi/types"
)

func TestMochiSolutions(t *testing.T) {
    golden.Run(t, "tests/spoj/human/x/mochi", ".mochi", ".out", func(src string) ([]byte, error) {
        inPath := strings.TrimSuffix(src, ".mochi") + ".in"
        data, err := os.ReadFile(inPath)
        if err != nil {
            data = nil
        }
        prog, err := parser.Parse(src)
        if err != nil {
            return nil, err
        }
        env := types.NewEnv(nil)
        if errs := types.Check(prog, env); len(errs) > 0 {
            return nil, errs[0]
        }
        p, err := vm.Compile(prog, env)
        if err != nil {
            return nil, err
        }
        var out bytes.Buffer
        m := vm.NewWithIO(p, bytes.NewReader(data), &out)
        // set MOCHI_ROOT to repo root
        root := filepath.Join(filepath.Dir(src), "..", "..", "..", "..", "..")
        os.Setenv("MOCHI_ROOT", root)
        if err := m.Run(); err != nil {
            return nil, err
        }
        return bytes.TrimSpace(out.Bytes()), nil
    })
}

