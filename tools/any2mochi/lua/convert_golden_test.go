//go:build slow

package any2mochi_test

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertLua_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/lua"), "*.lua.out", any2mochi.ConvertLuaFile, "lua", ".mochi", ".error")
}
