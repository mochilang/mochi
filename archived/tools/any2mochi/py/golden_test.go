//go:build archived && slow

package py

import (
	"path/filepath"
	"testing"
)

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	errs := runConvertRunGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertFile, "py", ".mochi", ".error")
	writeErrorsMarkdown(filepath.Join(root, "tests/any2mochi/py"), errs)
}

func TestConvertCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/py"), "*.py.out", ConvertFile, "py", ".mochi", ".error")
}
