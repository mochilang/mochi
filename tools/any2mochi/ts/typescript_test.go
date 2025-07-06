//go:build slow

package ts

import (
	"testing"

	tscode "mochi/compile/ts"
)

func TestConvertTypeScript(t *testing.T) {
	_ = tscode.EnsureTSLanguageServer()
	requireBinary(t, "typescript-language-server")
	src := "export function add(x: number, y: number): number { return x + y }"
	out, err := ConvertTypeScript(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun add(x: int, y: int): int {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
