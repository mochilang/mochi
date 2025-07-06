//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertDart_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/dart"), "*.dart.out", ConvertDartFile, "dart", ".mochi", ".error")
}
