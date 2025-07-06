//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	ex "mochi/tools/any2mochi/x/ex"
)

func TestConvertEx_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ex"), "*.ex.out", ex.ConvertFile, "ex", ".mochi", ".error")
}
