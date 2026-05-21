package sets

import (
	"fmt"
	"testing"
)

func eq[T comparable](a, b []T) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func TestUnion(t *testing.T) {
	got := Union([]int{1, 2, 3}, []int{3, 2, 4})
	if !eq(got, []int{1, 2, 3, 4}) {
		t.Errorf("Union = %v", got)
	}
	got = Union([]int{}, []int{1, 1, 2})
	if !eq(got, []int{1, 2}) {
		t.Errorf("Union empty = %v", got)
	}
}

func TestIntersect(t *testing.T) {
	got := Intersect([]int{1, 2, 2, 3}, []int{2, 3, 4})
	if !eq(got, []int{2, 3}) {
		t.Errorf("Intersect = %v", got)
	}
	got = Intersect([]int{1, 2}, []int{3, 4})
	if !eq(got, []int{}) {
		t.Errorf("Intersect disjoint = %v (expected [])", got)
	}
}

func TestExcept(t *testing.T) {
	got := Except([]int{1, 2, 2, 3, 4}, []int{2, 4})
	if !eq(got, []int{1, 3}) {
		t.Errorf("Except = %v", got)
	}
}

func TestFrom(t *testing.T) {
	got := From([]string{"a", "b", "a", "c", "b"})
	if !eq(got, []string{"a", "b", "c"}) {
		t.Errorf("From = %v", got)
	}
}

func ExampleUnion() {
	fmt.Println(Union([]int{1, 2, 3}, []int{3, 4}))
	// Output: [1 2 3 4]
}
