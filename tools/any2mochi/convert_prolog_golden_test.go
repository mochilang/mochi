//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertProlog_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/pl"), "*.pl.out", ConvertPrologFile, "prolog", ".mochi", ".error")
}

func TestConvertPrologCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/pl"), "*.pl.out", ConvertPrologFile, "prolog", ".mochi", ".error")
}
