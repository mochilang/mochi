//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertLua_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/lua"), "*.lua.out", ConvertLuaFile, "lua", ".mochi", ".error")
}
