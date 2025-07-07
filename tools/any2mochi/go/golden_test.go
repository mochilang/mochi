//go:build slow

package golang

import (
	"path/filepath"
	"testing"

	gocode "mochi/archived/go"
)

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	errs := runConvertRunGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertFile, "go", ".mochi", ".error")
	writeErrorsMarkdown(filepath.Join(root, "tests/any2mochi/go"), errs)
}

func TestConvertCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertFile, "go", ".mochi", ".error")
}
