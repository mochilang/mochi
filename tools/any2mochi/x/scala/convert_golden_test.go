//go:build slow

package scala

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertScala_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/scala"), "*.scala.out", ConvertFile, "scala", ".mochi", ".error")
}
