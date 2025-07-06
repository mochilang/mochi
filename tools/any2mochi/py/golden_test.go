//go:build slow

package py

import (
	"path/filepath"
	"testing"

	"mochi/tools/any2mochi/testutil"
)

func TestConvert_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertFile, "py", ".mochi", ".error")
}

func TestConvertCompile_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertFile, "py", ".mochi", ".error")
}
