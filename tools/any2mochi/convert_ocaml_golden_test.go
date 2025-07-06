//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertOcaml_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ocaml"), "*.ml.out", ConvertOcamlFile, "ocaml", ".mochi", ".error")
}
