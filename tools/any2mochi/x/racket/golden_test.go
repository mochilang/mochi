//go:build slow

package racket

import (
	"path/filepath"
	"testing"
)

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/rkt"), "*.rkt.out", ConvertFile, "rkt", ".mochi", ".error")
}
