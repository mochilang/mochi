//go:build slow

package cpp

import (
	"path/filepath"
	"testing"

	cpp "mochi/tools/any2mochi/x/cpp"
)

func TestConvertCpp_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/cpp"), "*.cpp.out", cpp.ConvertFile, "cpp", ".mochi", ".error")
}
