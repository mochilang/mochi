package any2mochi

import "testing"

func TestConvertPho(t *testing.T) {
	src := "fun add(x, y) { return x + y; }"
	out, err := ConvertPho(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	want := "fun add(x, y) {\n  return x + y\n}\n"
	if string(out) != want {
		t.Fatalf("unexpected output: %s", out)
	}
}
