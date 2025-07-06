//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	clj "mochi/tools/any2mochi/x/clj"
)

func TestConvertClj_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/clj"), "*.clj.out", clj.ConvertFile, "clj", ".mochi", ".error")
}
