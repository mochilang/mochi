//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertTypeScript_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertTypeScriptFile, "ts", ".mochi", ".error")
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertTypeScriptFile, "ts", ".mochi", ".error")
}

func TestConvertTypeScriptCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)

	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertTypeScriptFile, "ts", ".mochi", ".error")
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertTypeScriptFile, "ts", ".mochi", ".error")
}
