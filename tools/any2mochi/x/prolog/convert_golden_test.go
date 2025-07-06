//go:build slow

package prolog

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertProlog_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/pl"), "*.pl.out", ConvertFile, "prolog", ".mochi", ".error")
}
func TestConvertPrologCompile_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/pl"), "*.pl.out", ConvertFile, "prolog", ".mochi", ".error")
}
