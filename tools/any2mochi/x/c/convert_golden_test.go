//go:build slow

package c

import (
	"path/filepath"
	"testing"

	"mochi/tools/any2mochi/testutil"
)

func TestConvertC_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/c"), "*.c.out", ConvertFile, "c", ".mochi", ".error")
}
