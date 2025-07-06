//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertPl_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/pl"), "*.pl.out", ConvertPlFile, "pl", ".mochi", ".error")
}
