//go:build slow

package cpp

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertCpp_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/cpp"), "*.cpp.out", ConvertFile, "cpp", ".mochi", ".error")
}
