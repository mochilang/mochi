//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertCpp_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/cpp"), "*.cpp.out", ConvertCppFile, "cpp", ".mochi", ".error")
}
