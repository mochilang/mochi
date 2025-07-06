//go:build slow

package prolog

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertProlog_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/pl"), "*.pl.out", ConvertFile, "prolog", ".mochi", ".error")
}

func TestConvertPrologCompile_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/pl"), "*.pl.out", ConvertFile, "prolog", ".mochi", ".error")
}
