//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertHs_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/hs"), "*.hs.out", ConvertHsFile, "hs", ".mochi", ".error")
}
