//go:build slow

package ruby

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/archived/tools/any2mochi"
)

func TestConvertRuby_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	errs := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ConvertFile, "rb", ".mochi", ".error")
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/ruby"), errs)
}

func TestConvertRubyCompile_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ConvertFile, "rb", ".mochi", ".error")
}
