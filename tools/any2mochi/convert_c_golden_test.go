//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertC_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/c"), "*.c.out", ConvertCFile, "c", ".mochi", ".error")
}
