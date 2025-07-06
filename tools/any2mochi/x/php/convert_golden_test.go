//go:build slow

package php

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertPhp_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/php"), "*.php.out", ConvertFile, "php", ".mochi", ".error")
}
