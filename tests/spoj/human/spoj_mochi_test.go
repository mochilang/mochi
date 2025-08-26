//go:build slow

package human

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
	golden.Run(t, "tests/spoj/human/x/mochi", ".mochi", ".out", func(src string) ([]byte, error) {
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
		in := bytes.NewReader([]byte{})
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			in = bytes.NewReader(data)
		}
		var out bytes.Buffer
		m := vm.NewWithIO(p, in, &out)
		if err := m.Run(); err != nil {
			return nil, err
		}
		return bytes.TrimSpace(out.Bytes()), nil
	})
}
