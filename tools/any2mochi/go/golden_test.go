//go:build slow

package golang

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	"mochi/tools/any2mochi/testutil"
)

func TestConvertGo_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	_ = gocode.EnsureGopls()
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertGoFile, "go", ".mochi", ".error")
}

func TestConvertGoCompile_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	_ = gocode.EnsureGopls()
	testutil.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertGoFile, "go", ".mochi", ".error")
}
