package lists

import (
	"fmt"
	"testing"
)

func TestAppendDoesNotMutate(t *testing.T) {
	a := []int{1, 2, 3}
	b := Append(a, 4)
	if len(a) != 3 || a[0] != 1 {
		t.Errorf("a mutated: %v", a)
	}
	if len(b) != 4 || b[3] != 4 {
		t.Errorf("b = %v", b)
	}
}

func TestReverse(t *testing.T) {
	got := Reverse([]int{1, 2, 3})
	if len(got) != 3 || got[0] != 3 || got[2] != 1 {
		t.Errorf("Reverse = %v", got)
	}
	if got := Reverse([]string{}); len(got) != 0 {
		t.Errorf("Reverse([]) = %v", got)
	}
}

func TestFirstLast(t *testing.T) {
	if v, ok := First([]int{1, 2, 3}); !ok || v != 1 {
		t.Errorf("First = %d ok=%v", v, ok)
	}
	if _, ok := First([]int{}); ok {
		t.Error("First([]) should report missing")
	}
	if v, ok := Last([]int{1, 2, 3}); !ok || v != 3 {
		t.Errorf("Last = %d ok=%v", v, ok)
	}
	if _, ok := Last([]int{}); ok {
		t.Error("Last([]) should report missing")
	}
}

func TestConcat(t *testing.T) {
	got := Concat([]int{1, 2}, []int{3}, []int{4, 5})
	if len(got) != 5 || got[0] != 1 || got[4] != 5 {
		t.Errorf("Concat = %v", got)
	}
	if got := Concat[int](); got == nil || len(got) != 0 {
		t.Errorf("Concat() = %v (expected empty non-nil)", got)
	}
}

func TestSliceClamps(t *testing.T) {
	src := []int{1, 2, 3, 4, 5}
	for _, c := range []struct {
		start, end int
		want       []int
	}{
		{0, 5, []int{1, 2, 3, 4, 5}},
		{1, 3, []int{2, 3}},
		{-2, 3, []int{1, 2, 3}},
		{0, 99, []int{1, 2, 3, 4, 5}},
		{3, 1, []int{}},
	} {
		got := Slice(src, c.start, c.end)
		if len(got) != len(c.want) {
			t.Errorf("Slice(_, %d, %d) len = %d want %d", c.start, c.end, len(got), len(c.want))
			continue
		}
		for i := range got {
			if got[i] != c.want[i] {
				t.Errorf("Slice(_, %d, %d)[%d] = %d want %d", c.start, c.end, i, got[i], c.want[i])
			}
		}
	}
}

func TestContainsIndexOf(t *testing.T) {
	if !Contains([]int{1, 2, 3}, 2) {
		t.Error("Contains 2")
	}
	if Contains([]int{1, 2, 3}, 4) {
		t.Error("Contains 4 should be false")
	}
	if got := IndexOf([]string{"a", "b", "c"}, "b"); got != 1 {
		t.Errorf("IndexOf b = %d", got)
	}
	if got := IndexOf([]string{"a", "b", "c"}, "z"); got != -1 {
		t.Errorf("IndexOf z = %d", got)
	}
}

func TestDistinct(t *testing.T) {
	got := Distinct([]int{1, 2, 1, 3, 2, 4})
	want := []int{1, 2, 3, 4}
	if len(got) != len(want) {
		t.Fatalf("Distinct = %v", got)
	}
	for i := range got {
		if got[i] != want[i] {
			t.Errorf("Distinct[%d] = %d want %d", i, got[i], want[i])
		}
	}
}

func TestSortByStable(t *testing.T) {
	type rec struct {
		Key   int
		Order int
	}
	src := []rec{{1, 0}, {1, 1}, {0, 2}, {1, 3}}
	got := SortBy(src, func(r rec) int { return r.Key })
	if got[0].Key != 0 || got[0].Order != 2 {
		t.Errorf("SortBy[0] = %+v", got[0])
	}
	// Stable: the three Key=1 records retain their relative order.
	if got[1].Order != 0 || got[2].Order != 1 || got[3].Order != 3 {
		t.Errorf("SortBy unstable: %+v", got)
	}
}

func TestSumAvg(t *testing.T) {
	if got := SumFloat([]float64{1, 2, 3.5}); got != 6.5 {
		t.Errorf("SumFloat = %v", got)
	}
	if got := SumInt([]int64{1, 2, 3}); got != 6 {
		t.Errorf("SumInt = %v", got)
	}
	if got := AvgFloat([]float64{2, 4, 6}); got != 4 {
		t.Errorf("AvgFloat = %v", got)
	}
	if got := AvgFloat([]float64{}); got != 0 {
		t.Errorf("AvgFloat([]) = %v (want 0)", got)
	}
}

func TestMinMax(t *testing.T) {
	if got := MinOrdered([]int{3, 1, 2}); got != 1 {
		t.Errorf("Min = %d", got)
	}
	if got := MaxOrdered([]int{3, 1, 2}); got != 3 {
		t.Errorf("Max = %d", got)
	}
	defer func() {
		if recover() == nil {
			t.Error("MinOrdered([]) should panic")
		}
	}()
	_ = MinOrdered([]int{})
}

func ExampleAppend() {
	a := []int{1, 2, 3}
	fmt.Println(Append(a, 4))
	// Output: [1 2 3 4]
}

func ExampleConcat() {
	fmt.Println(Concat([]int{1, 2}, []int{3, 4}))
	// Output: [1 2 3 4]
}
