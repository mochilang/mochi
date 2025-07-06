//go:build slow

package hs

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertHs_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/hs"), "*.hs.out", ConvertFile, "hs", ".mochi", ".error")
}
