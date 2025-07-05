//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertGo_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertGoFile, "go", ".mochi", ".error")
}

func TestConvertGoCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/go"), "*.go.out", ConvertGoFile, "go", ".mochi", ".error")
}
