//go:build slow

package java

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertJava_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	errs := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/java"), "*.java.out", ConvertFile, "java", ".mochi", ".error")
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/java"), errs)
}
