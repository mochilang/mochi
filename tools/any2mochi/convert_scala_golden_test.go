//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertScala_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/scala"), "*.scala.out", ConvertScalaFile, "scala", ".mochi", ".error")
}
