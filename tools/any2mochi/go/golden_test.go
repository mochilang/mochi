//go:build slow

package golang

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
)

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	runConvertGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertFile, "go", ".mochi", ".error")
}

func TestConvertCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertFile, "go", ".mochi", ".error")
}
