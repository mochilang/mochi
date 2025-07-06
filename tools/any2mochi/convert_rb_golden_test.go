//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	ruby "mochi/tools/any2mochi/x/ruby"
)

func TestConvertRuby_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ruby.ConvertFile, "rb", ".mochi", ".error")
}

func TestConvertRubyCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ruby.ConvertFile, "rb", ".mochi", ".error")
}
