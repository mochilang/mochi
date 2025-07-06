//go:build slow

package c

import (
	"path/filepath"
	"testing"

	c "mochi/tools/any2mochi/x/c"
)

func TestConvertC_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/c"), "*.c.out", c.ConvertFile, "c", ".mochi", ".error")
}
