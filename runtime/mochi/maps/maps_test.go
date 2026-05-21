package maps

import (
	"fmt"
	"testing"
)

func TestKeysSorted(t *testing.T) {
	m := map[string]int{"c": 3, "a": 1, "b": 2}
	ks := Keys(m)
	want := []string{"a", "b", "c"}
	if len(ks) != len(want) {
		t.Fatalf("Keys = %v", ks)
	}
	for i, k := range ks {
		if k != want[i] {
			t.Errorf("Keys[%d] = %q, want %q", i, k, want[i])
		}
	}
}

func TestValuesFollowKeys(t *testing.T) {
	m := map[string]int{"c": 3, "a": 1, "b": 2}
	vs := Values(m)
	want := []int{1, 2, 3}
	for i, v := range vs {
		if v != want[i] {
			t.Errorf("Values[%d] = %d, want %d", i, v, want[i])
		}
	}
}

func TestHasGet(t *testing.T) {
	m := map[string]int{"a": 1}
	if !Has(m, "a") {
		t.Error("Has a")
	}
	if Has(m, "b") {
		t.Error("Has b should be false")
	}
	if v, ok := Get(m, "a"); !ok || v != 1 {
		t.Errorf("Get a = %d ok=%v", v, ok)
	}
	if _, ok := Get(m, "b"); ok {
		t.Error("Get b ok should be false")
	}
}

func TestLen(t *testing.T) {
	if got := Len(map[int]int{1: 1, 2: 2}); got != 2 {
		t.Errorf("Len = %d", got)
	}
}

func ExampleKeys() {
	fmt.Println(Keys(map[string]int{"b": 2, "a": 1, "c": 3}))
	// Output: [a b c]
}
