//go:build slow

package ruby

import (
	"path/filepath"
	"testing"

	"mochi/tools/any2mochi/testutil"
)

func TestConvertRuby_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ConvertFile, "rb", ".mochi", ".error")
}

func TestConvertRubyCompile_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ConvertFile, "rb", ".mochi", ".error")
}
