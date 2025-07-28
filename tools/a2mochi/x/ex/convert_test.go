package ex_test

import (
	"testing"

	"mochi/tools/a2mochi/x/ex"
)

func TestConvertBasic(t *testing.T) {
	src := `def main() do
  IO.puts("hi")
end`
	node, err := ex.Convert(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	got := node.String()
	want := "(program\n  (fun main\n    (call print (string hi))\n  )\n  (call main)\n)\n"
	if got != want {
		t.Fatalf("unexpected AST\nGot: %s\nWant: %s", got, want)
	}
}
