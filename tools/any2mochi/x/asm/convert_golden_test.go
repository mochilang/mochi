//go:build slow

package asm

import (
	"path/filepath"
	"testing"

	asm "mochi/tools/any2mochi/x/asm"
)

func TestConvertAsm_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/asm"), "*.s.out", asm.ConvertFile, "asm", ".mochi", ".error")
}
