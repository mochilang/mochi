//go:build slow

package any2mochi

import "testing"

func TestParseRust(t *testing.T) {
	ls := Servers["rust"]
	if err := EnsureServer(ls.Command); err != nil {
		t.Skipf("rust-analyzer not installed: %v", err)
	}
	src := "fn main() { println!(\"Hello, world\"); }"
	syms, diags, err := ParseText(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		t.Skipf("parse rust failed: %v", err)
	}
	if len(diags) > 0 {
		t.Fatalf("unexpected diagnostics: %v", diags)
	}
	if len(syms) == 0 {
		t.Fatalf("expected symbols")
	}
}
