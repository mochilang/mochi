//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertEx_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ex"), "*.ex.out", ConvertExFile, "ex", ".mochi", ".error")
}
