//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
)

func TestConvertGo_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	runConvertGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertGoFile, "go", ".go.mochi", ".go.error")
}

func TestConvertGoCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertGoFile, "go", ".mochi.out", ".error")
}
