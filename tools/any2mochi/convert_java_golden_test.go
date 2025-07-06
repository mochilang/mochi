//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertJava_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/java"), "*.java.out", ConvertJavaFile, "java", ".mochi", ".error")
}
