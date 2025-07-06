//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	kt "mochi/tools/any2mochi/x/kt"
)

func TestConvertKt_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/kt"), "*.kt.out", kt.ConvertFile, "kt", ".mochi", ".error")
}
