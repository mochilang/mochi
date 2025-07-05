//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	pycode "mochi/compile/py"
)

func TestConvertPython_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = pycode.EnsurePyright()
	runConvertGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertPythonFile, "py", ".py.mochi", ".py.error")
}

func TestConvertPythonCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = pycode.EnsurePyright()
	_ = gocode.EnsureGopls()
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertPythonFile, "py", ".mochi.out", ".error")
}
