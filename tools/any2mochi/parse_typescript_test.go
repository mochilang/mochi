//go:build slow

package any2mochi

import "testing"

func TestParseTypeScript(t *testing.T) {
	requireBinary(t, "typescript-language-server")
	src := "export function add(x: number, y: number): number { return x + y }"
	syms, diags, err := ParseText("typescript-language-server", []string{"--stdio"}, "typescript", src)
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
