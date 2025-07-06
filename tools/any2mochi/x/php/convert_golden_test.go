//go:build slow

package php

import (
	"path/filepath"
	"testing"

	php "mochi/tools/any2mochi/x/php"
)

func TestConvertPhp_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/php"), "*.php.out", php.ConvertFile, "php", ".mochi", ".error")
}
