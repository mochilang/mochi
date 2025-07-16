//go:build slow

package luacode_test

import (
	"bytes"
	"fmt"
	"testing"

	luacode "mochi/compiler/x/lua"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestLuaCompiler_VM_Golden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	golden.Run(t, "tests/vm/valid", ".mochi", ".lua.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := luacode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
