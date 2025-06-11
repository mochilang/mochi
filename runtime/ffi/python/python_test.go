package python_test

import (
	"testing"

	"mochi/runtime/ffi/python"
)

func TestAttrVariable(t *testing.T) {
	res, err := python.Attr("math", "pi")
	if err != nil {
		t.Fatalf("attr math.pi: %v", err)
	}
	f, ok := res.(float64)
	if !ok || f <= 3.14 || f >= 3.15 { // rough check
		t.Fatalf("unexpected result: %v", res)
	}
}

func TestAttrFunction(t *testing.T) {
	res, err := python.Attr("math", "pow", 2, 3)
	if err != nil {
		t.Fatalf("attr math.pow: %v", err)
	}
	n, ok := res.(float64)
	if !ok || n != 8 {
		t.Fatalf("unexpected result: %v", res)
	}
}
