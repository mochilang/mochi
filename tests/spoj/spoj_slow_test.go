//go:build slow
// +build slow

package spoj

import (
	"bytes"
	"os"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestMochiSolutions(t *testing.T) {
	golden.Run(t, "tests/spoj/x/mochi", ".mochi", ".out", func(src string) ([]byte, error) {
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
		inPath := strings.TrimSuffix(src, ".mochi") + ".in"
		if data, err := os.ReadFile(inPath); err == nil {
			m := vm.NewWithIO(p, bytes.NewReader(data), &out)
			if err := m.Run(); err != nil {
				return nil, err
			}
		} else {
			m := vm.New(p, &out)
			if err := m.Run(); err != nil {
				return nil, err
			}
		}
		return bytes.TrimSpace(out.Bytes()), nil
	})
}
