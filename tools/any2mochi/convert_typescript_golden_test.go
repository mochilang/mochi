//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	tscode "mochi/compile/ts"
)

func TestConvertTypeScript_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = tscode.EnsureTSLanguageServer()
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertTypeScriptFile, "ts", ".ts.mochi", ".ts.error")
	runConvertGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertTypeScriptFile, "ts", ".ts.mochi", ".ts.error")
}

func TestConvertTypeScriptCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	_ = tscode.EnsureTSLanguageServer()
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertTypeScriptFile, "ts", ".mochi.out", ".error")
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertTypeScriptFile, "ts", ".mochi.out", ".error")
}
