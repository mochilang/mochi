//go:build slow

package c

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertC_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/c"), "*.c.out", ConvertFile, "c", ".mochi", ".error")
}
