package vm2

import (
	"math"
	"testing"
	"unsafe"
)

func TestCellSize(t *testing.T) {
	if got := unsafe.Sizeof(Cell{}); got != 16 {
		t.Fatalf("Cell size = %d, want 16", got)
	}
}

func TestCellInt(t *testing.T) {
	for _, v := range []int64{0, 1, -1, 42, -42, MaxInlineInt, MinInlineInt, 1 << 30, -(1 << 30)} {
		c := CInt(v)
		if !c.IsInt() {
			t.Fatalf("CInt(%d) not IsInt: %#x", v, c.Bits)
		}
		if c.IsFloat() || c.IsBool() || c.IsNull() || c.IsPtr() {
			t.Fatalf("CInt(%d) tag collision: %#x", v, c.Bits)
		}
		if got := c.Int(); got != v {
			t.Fatalf("CInt(%d).Int() = %d", v, got)
		}
	}
}

func TestCellFloat(t *testing.T) {
	cases := []float64{0, 1, -1, 3.14, -3.14, math.Inf(1), math.Inf(-1), math.MaxFloat64, -math.MaxFloat64, math.SmallestNonzeroFloat64}
	for _, v := range cases {
		c := CFloat(v)
		if !c.IsFloat() {
			t.Fatalf("CFloat(%g) not IsFloat: %#x", v, c.Bits)
		}
		if c.IsInt() || c.IsBool() || c.IsNull() || c.IsPtr() {
			t.Fatalf("CFloat(%g) tag collision: %#x", v, c.Bits)
		}
		if got := c.Float(); got != v {
			t.Fatalf("CFloat(%g).Float() = %g", v, got)
		}
	}
	nan := CFloat(math.NaN())
	if !nan.IsFloat() {
		t.Fatalf("NaN cell not IsFloat: %#x", nan.Bits)
	}
	if !math.IsNaN(nan.Float()) {
		t.Fatalf("NaN cell decoded to non-NaN: %g", nan.Float())
	}
}

func TestCellBool(t *testing.T) {
	if !CBool(true).Bool() {
		t.Fatal("CBool(true).Bool() == false")
	}
	if CBool(false).Bool() {
		t.Fatal("CBool(false).Bool() == true")
	}
	if !CBool(true).IsBool() || !CBool(false).IsBool() {
		t.Fatal("CBool not IsBool")
	}
	if CBool(true).IsInt() || CBool(false).IsFloat() {
		t.Fatal("CBool tag collision")
	}
}

func TestCellNull(t *testing.T) {
	c := CNull()
	if !c.IsNull() {
		t.Fatal("CNull not IsNull")
	}
	if c.IsInt() || c.IsFloat() || c.IsBool() || c.IsPtr() {
		t.Fatal("CNull tag collision")
	}
}

func TestFitsInline(t *testing.T) {
	cases := []struct {
		v  int64
		ok bool
	}{
		{0, true}, {1, true}, {-1, true},
		{MaxInlineInt, true}, {MinInlineInt, true},
		{MaxInlineInt + 1, false}, {MinInlineInt - 1, false},
		{math.MaxInt64, false}, {math.MinInt64, false},
	}
	for _, c := range cases {
		if got := FitsInline(c.v); got != c.ok {
			t.Fatalf("FitsInline(%d) = %v, want %v", c.v, got, c.ok)
		}
	}
}
