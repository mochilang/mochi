//go:build slow

package pas

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertPas_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/pas"), "*.pas.out", ConvertFile, "pas", ".mochi", ".error")
}
