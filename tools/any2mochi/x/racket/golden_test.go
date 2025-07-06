//go:build slow

package racket

import (
	"path/filepath"
	"testing"

	"mochi/tools/any2mochi/testutil"
)

func TestConvert_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/rkt"), "*.rkt.out", ConvertFile, "rkt", ".mochi", ".error")
}
