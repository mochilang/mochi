//go:build slow

package ex

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertEx_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/ex"), "*.ex.out", ConvertFile, "ex", ".mochi", ".error")
}
