//go:build slow

package prolog

import (
	"path/filepath"
	"testing"

	prolog "mochi/tools/any2mochi/x/prolog"
)

func TestConvertProlog_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/pl"), "*.pl.out", prolog.ConvertFile, "prolog", ".mochi", ".error")
}

func TestConvertPrologCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/pl"), "*.pl.out", prolog.ConvertFile, "prolog", ".mochi", ".error")
}
