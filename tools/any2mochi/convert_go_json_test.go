//go:build slow

package any2mochi

import (
	"testing"
)

func TestConvertGoViaJSON(t *testing.T) {
	src := "package foo\nfunc Add(x int, y int) int { return x + y }"
	out, err := ConvertGoViaJSON(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	want := "fun Add(x: int, y: int): int {}\n"
	if string(out) != want {
		t.Fatalf("unexpected output: %s", out)
	}
}
