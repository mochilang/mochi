//go:build slow

package golang

import (
	"testing"
)

func TestConvertViaJSON(t *testing.T) {
	src := "package foo\nfunc Add(x int, y int) int { return x + y }"
	out, err := ConvertViaJSON(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	want := "fun Add(x: int, y: int): int {}\n"
	if string(out) != want {
		t.Fatalf("unexpected output: %s", out)
	}
}
