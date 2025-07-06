//go:build slow

package ex

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertEx_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/ex"), "*.ex.out", ConvertFile, "ex", ".mochi", ".error")
}
