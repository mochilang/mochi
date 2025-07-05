//go:build slow

package any2mochi

import (
	"testing"

	gocode "mochi/compile/go"
)

func TestConvertGo(t *testing.T) {
	_ = gocode.EnsureGopls()
	// gopls isn't required when using the built-in parser but keeping the call
	// ensures the test still works when gopls is available.
	src := "package foo\nfunc Add(x int, y int) int { return x + y }"
	out, err := ConvertGo(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun Add(x, y) {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
