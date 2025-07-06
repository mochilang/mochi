//go:build slow

package scheme

import (
	"path/filepath"
	"testing"

	"mochi/tools/any2mochi/testutil"
)

func TestConvertScheme_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/scheme"), "*.scm.out", ConvertFile, "scheme", ".mochi", ".error")
}

func TestConvertSchemeCompile_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/scheme"), "*.scm.out", ConvertFile, "scheme", ".mochi", ".error")
}
