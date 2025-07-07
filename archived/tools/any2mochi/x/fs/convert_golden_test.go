//go:build slow

package fs

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/archived/tools/any2mochi"
)

func TestConvertFs_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	errs := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/fs"), "*.fs.out", ConvertFile, "fs", ".mochi", ".error")
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/fs"), errs)
}
