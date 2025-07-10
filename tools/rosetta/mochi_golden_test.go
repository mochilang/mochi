//go:build slow

package rosetta

import (
	"bytes"
	"fmt"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestMochiMD5(t *testing.T) {
	golden.Run(t, "tests/rosetta/x/Mochi/MD5", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		var out bytes.Buffer
		m := vm.New(p, &out)
		if err := m.Run(); err != nil {
			return nil, fmt.Errorf("run error: %w", err)
		}
		b := bytes.TrimSpace(out.Bytes())
		if b == nil {
			b = []byte{}
		}
		return b, nil
	})
}
