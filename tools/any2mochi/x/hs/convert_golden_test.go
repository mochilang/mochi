//go:build slow

package hs

import (
	"path/filepath"
	"testing"

	hs "mochi/tools/any2mochi/x/hs"
)

func TestConvertHs_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/hs"), "*.hs.out", hs.ConvertFile, "hs", ".mochi", ".error")
}
