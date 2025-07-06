//go:build slow

package py

import (
	"path/filepath"
	"testing"
)

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertFile, "py", ".mochi", ".error")
}

func TestConvertCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertFile, "py", ".mochi", ".error")
}
