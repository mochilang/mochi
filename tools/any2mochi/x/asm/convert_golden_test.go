//go:build slow

package asm

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertAsm_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/asm"), "*.s.out", ConvertFile, "asm", ".mochi", ".error")
}
