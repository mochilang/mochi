//go:build slow

package scheme

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/archived/tools/any2mochi"
)

func TestConvertScheme_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	errs := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/scheme"), "*.scm.out", ConvertFile, "scheme", ".mochi", ".error")
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/scheme"), errs)
}

func TestConvertSchemeCompile_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/scheme"), "*.scm.out", ConvertFile, "scheme", ".mochi", ".error")
}
