package st

import "testing"

func TestConvertFallbackIfTrue(t *testing.T) {
	src := "x > 3 ifTrue: [ y := 1 ]"
	out, err := convertFallback(src)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	expect := "if (x > 3) {\n y = 1\n}\n"
	if string(out) != expect {
		t.Fatalf("expected %q, got %q", expect, out)
	}
}
