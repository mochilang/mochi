//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	goconv "mochi/tools/any2mochi/go"
)

func TestConvertGo_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	runConvertGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", goconv.ConvertGoFile, "go", ".mochi", ".error")
}

func TestConvertGoCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", goconv.ConvertGoFile, "go", ".mochi", ".error")
}
