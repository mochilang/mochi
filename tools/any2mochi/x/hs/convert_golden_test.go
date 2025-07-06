//go:build slow

package hs

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertHs_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/hs"), "*.hs.out", ConvertFile, "hs", ".mochi", ".error")
}
