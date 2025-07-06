//go:build slow

package fs

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertFs_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/fs"), "*.fs.out", ConvertFile, "fs", ".mochi", ".error")
}
