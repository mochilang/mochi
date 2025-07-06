//go:build slow

package ocaml

import (
	"path/filepath"
	"testing"

	ocaml "mochi/tools/any2mochi/x/ocaml"
)

func TestConvertOcaml_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ocaml"), "*.ml.out", ocaml.ConvertFile, "ocaml", ".mochi", ".error")
}
