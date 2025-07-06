//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertKt_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/kt"), "*.kt.out", ConvertKtFile, "kt", ".mochi", ".error")
}
