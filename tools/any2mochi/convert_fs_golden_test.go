//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	fs "mochi/tools/any2mochi/x/fs"
)

func TestConvertFs_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/fs"), "*.fs.out", fs.ConvertFile, "fs", ".mochi", ".error")
}
