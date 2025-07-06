//go:build slow

package ts

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	tscode "mochi/compile/ts"
)

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = tscode.EnsureTSLanguageServer()
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
}

func TestConvertCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	_ = tscode.EnsureTSLanguageServer()
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
}
