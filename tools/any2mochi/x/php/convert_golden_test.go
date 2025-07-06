//go:build slow

package php

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertPhp_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/php"), "*.php.out", ConvertFile, "php", ".mochi", ".error")
}
