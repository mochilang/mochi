//go:build slow

package ocaml

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertOcaml_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ocaml"), "*.ml.out", ConvertFile, "ocaml", ".mochi", ".error")
}
