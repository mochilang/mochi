//go:build slow

package ruby

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertRuby_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ConvertFile, "rb", ".mochi", ".error")
}

func TestConvertRubyCompile_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ConvertFile, "rb", ".mochi", ".error")
}
