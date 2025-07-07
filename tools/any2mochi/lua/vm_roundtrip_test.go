//go:build lua_vm && slow

package any2mochi_test

import (
	"fmt"
	"path/filepath"
	"testing"

	luacode "mochi/archived/x/lua"
	"mochi/parser"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func compileMochiToLua(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
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
	return code, nil
}

func TestLuaRoundTripVM(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToLua,
		any2mochi.ConvertLuaFile,
		"lua",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests", "any2mochi", "lua_vm"), status)
}
