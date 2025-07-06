//go:build slow

package ocaml

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertOcaml_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/ocaml"), "*.ml.out", ConvertFile, "ocaml", ".mochi", ".error")
}
