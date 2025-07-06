//go:build slow

package c

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertC_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	errs := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/c"), "*.c.out", ConvertFile, "c", ".mochi", ".error")
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/c"), errs)
}
