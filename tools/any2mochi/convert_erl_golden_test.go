//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertErl_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/erl"), "*.erl.out", ConvertErlangFile, "erl", ".mochi", ".error")
}
