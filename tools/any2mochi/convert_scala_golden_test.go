//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	scala "mochi/tools/any2mochi/x/scala"
)

func TestConvertScala_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/scala"), "*.scala.out", scala.ConvertFile, "scala", ".mochi", ".error")
}
