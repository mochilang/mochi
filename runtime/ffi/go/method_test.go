package goffi_test

import (
	"testing"

	goffi "mochi/runtime/ffi/go"
)

type Calc struct{ Base int }

func (c *Calc) Add(a, b int) int { return a + b + c.Base }

func TestNewAndMethod(t *testing.T) {
	goffi.RegisterType("mymath.Calc", &Calc{})
	inst, err := goffi.New("mymath.Calc", map[string]any{"Base": 10})
	if err != nil {
		t.Fatalf("new failed: %v", err)
	}
	res, err := goffi.CallMethod(inst, "Add", 1, 2)
	if err != nil {
		t.Fatalf("call method failed: %v", err)
	}
	if res.(int) != 13 {
		t.Fatalf("expected 13, got %v", res)
	}
}
