//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	erl "mochi/tools/any2mochi/x/erlang"
)

func TestConvertErl_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/erl"), "*.erl.out", erl.ConvertFile, "erl", ".mochi", ".error")
}
