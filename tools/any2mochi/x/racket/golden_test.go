//go:build slow

package racket

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvert_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/rkt"), "*.rkt.out", ConvertFile, "rkt", ".mochi", ".error")
}
