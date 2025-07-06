//go:build slow

package ts

import (
	"testing"

	tscode "mochi/compile/ts"
	parent "mochi/tools/any2mochi"
)

func TestParseTypeScript(t *testing.T) {
	_ = tscode.EnsureTSLanguageServer()
	requireBinary(t, "typescript-language-server")
	src := "export function add(x: number, y: number): number { return x + y }"
	syms, diags, err := parent.ParseText("typescript-language-server", []string{"--stdio"}, "typescript", src)
	if err != nil {
		t.Fatalf("parse ts: %v", err)
	}
	if len(diags) > 0 {
		t.Fatalf("unexpected diagnostics: %v", diags)
	}
	if len(syms) == 0 {
		t.Fatalf("expected symbols")
	}
}
