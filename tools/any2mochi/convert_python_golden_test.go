//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertPython_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertPythonFile, "py", ".mochi", ".error")
}

func TestConvertPythonCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertPythonFile, "py", ".mochi", ".error")
}
