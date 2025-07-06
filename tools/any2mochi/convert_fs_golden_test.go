//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertFs_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/fs"), "*.fs.out", ConvertFsFile, "fs", ".mochi", ".error")
}
