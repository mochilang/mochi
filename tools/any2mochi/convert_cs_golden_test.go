//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertCs_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/cs"), "*.cs.out", ConvertCsFile, "cs", ".mochi", ".error")
}
