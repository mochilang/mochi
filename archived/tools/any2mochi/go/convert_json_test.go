//go:build slow

package golang

import (
	"testing"
)

func TestConvertViaJSON(t *testing.T) {
	src := "package foo\nfunc Pair() (int, int) { return 1, 2 }"
	out, err := ConvertViaJSON(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	want := "fun Pair(): (int, int) {}\n"
	if string(out) != want {
		t.Fatalf("unexpected output: %s", out)
	}
}
