//go:build slow

package hs

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/archived/tools/any2mochi"
)

func TestConvertHs_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	errs := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/hs"), "*.hs.out", ConvertFile, "hs", ".mochi", ".error")
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/hs"), errs)
}
