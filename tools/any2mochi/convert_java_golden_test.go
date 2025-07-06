//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	java "mochi/tools/any2mochi/x/java"
)

func TestConvertJava_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/java"), "*.java.out", java.ConvertFile, "java", ".mochi", ".error")
}
