//go:build slow

package erlang

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertErl_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/erl"), "*.erl.out", ConvertFile, "erl", ".mochi", ".error")
}
