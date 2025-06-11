package goffi_test

import (
	"fmt"
	"testing"

	goffi "mochi/runtime/ffi/go"
)

func TestCall(t *testing.T) {
	goffi.Register("add", func(a, b int) int { return a + b })
	res, err := goffi.Call("add", 2, 3)
	if err != nil {
		t.Fatalf("call failed: %v", err)
	}
	if res.(int) != 5 {
		t.Fatalf("expected 5, got %v", res)
	}
}

func TestCallWithError(t *testing.T) {
	goffi.Register("fail", func() (int, error) { return 0, fmt.Errorf("boom") })
	_, err := goffi.Call("fail")
	if err == nil || err.Error() != "boom" {
		t.Fatalf("expected boom error, got %v", err)
	}
}

func TestCallValue(t *testing.T) {
	goffi.Register("pi", 3.14)
	res, err := goffi.Call("pi")
	if err != nil {
		t.Fatalf("call value failed: %v", err)
	}
	if res.(float64) != 3.14 {
		t.Fatalf("expected 3.14, got %v", res)
	}
}
