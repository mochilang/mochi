package decimal

import (
	"fmt"
	"testing"
)

func TestNewAndString(t *testing.T) {
	if got := New(42).String(); got != "42" {
		t.Errorf("New(42) = %q", got)
	}
	if got := New(-3).String(); got != "-3" {
		t.Errorf("New(-3) = %q", got)
	}
}

func TestFromString(t *testing.T) {
	for _, c := range []struct {
		in, out string
	}{
		{"123", "123"},
		{"-1.5", "-3/2"},
		{"0.001", "1/1000"},
		{"1e3", "1000"},
		{"2.5e-1", "1/4"},
	} {
		d, err := FromString(c.in)
		if err != nil {
			t.Errorf("FromString(%q): %v", c.in, err)
			continue
		}
		if got := d.String(); got != c.out {
			t.Errorf("FromString(%q) = %q, want %q", c.in, got, c.out)
		}
	}
	if _, err := FromString("garbage"); err == nil {
		t.Error("FromString(garbage) should error")
	}
}

func TestArith(t *testing.T) {
	a := MustFromString("1.5")
	b := MustFromString("2.5")
	if got := Add(a, b).String(); got != "4" {
		t.Errorf("Add = %s", got)
	}
	if got := Sub(b, a).String(); got != "1" {
		t.Errorf("Sub = %s", got)
	}
	if got := Mul(a, b).String(); got != "15/4" {
		t.Errorf("Mul = %s", got)
	}
	if got := Div(b, a).String(); got != "5/3" {
		t.Errorf("Div = %s", got)
	}
}

func TestDivByZeroPanics(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Error("Div by zero should panic")
		}
	}()
	_ = Div(New(1), New(0))
}

func TestCmp(t *testing.T) {
	if Cmp(New(1), New(2)) != -1 {
		t.Error("Cmp 1 < 2")
	}
	if Cmp(New(2), New(2)) != 0 {
		t.Error("Cmp ==")
	}
	if Cmp(New(3), New(2)) != 1 {
		t.Error("Cmp >")
	}
}

func TestRoundHalfAway(t *testing.T) {
	cases := []struct {
		in     string
		places int
		out    string
	}{
		{"1.5", 0, "2"},
		{"-1.5", 0, "-2"},
		{"2.5", 0, "3"},
		{"-2.5", 0, "-3"},
		{"0.125", 2, "13/100"}, // 0.13
		{"0.124", 2, "3/25"},   // 0.124 -> 0.12 (= 12/100 = 3/25 reduced)
		{"1.23456", 3, "247/200"},
	}
	for _, c := range cases {
		got := Round(MustFromString(c.in), c.places).String()
		if got != c.out {
			t.Errorf("Round(%q, %d) = %s, want %s", c.in, c.places, got, c.out)
		}
	}
}

func TestNegAndIsZero(t *testing.T) {
	if !New(0).IsZero() {
		t.Error("0 IsZero")
	}
	if got := Neg(New(5)).String(); got != "-5" {
		t.Errorf("Neg(5) = %s", got)
	}
}

func TestFloat64(t *testing.T) {
	f, exact := MustFromString("0.5").Float64()
	if !exact || f != 0.5 {
		t.Errorf("Float64(0.5) = %v exact=%v", f, exact)
	}
}

func ExampleAdd() {
	a := MustFromString("1.25")
	b := MustFromString("2.75")
	fmt.Println(Add(a, b))
	// Output: 4
}
