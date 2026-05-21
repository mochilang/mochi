package query

import (
	"fmt"
	"reflect"
	"testing"
)

type person struct {
	ID   int
	Name string
	Age  int
	Team string
}

type score struct {
	Person int
	Pts    int
}

var people = []person{
	{1, "Alice", 30, "red"},
	{2, "Bob", 25, "blue"},
	{3, "Carol", 30, "red"},
	{4, "Dan", 40, "blue"},
}

var scores = []score{
	{1, 10},
	{1, 20},
	{2, 15},
	{3, 5},
	// Person 4 deliberately has no score, exercises Left/Outer.
}

func TestFilterMap(t *testing.T) {
	young := Filter(people, func(p person) bool { return p.Age < 35 })
	if len(young) != 3 {
		t.Errorf("Filter < 35 = %v", young)
	}
	names := Map(young, func(p person) string { return p.Name })
	if !reflect.DeepEqual(names, []string{"Alice", "Bob", "Carol"}) {
		t.Errorf("Map names = %v", names)
	}
}

func TestSortBy(t *testing.T) {
	got := SortBy(people, func(p person) int { return p.Age })
	want := []string{"Bob", "Alice", "Carol", "Dan"}
	for i, p := range got {
		if p.Name != want[i] {
			t.Errorf("SortBy[%d] = %s, want %s", i, p.Name, want[i])
		}
	}
	gotD := SortByDesc(people, func(p person) int { return p.Age })
	wantD := []string{"Dan", "Alice", "Carol", "Bob"}
	for i, p := range gotD {
		if p.Name != wantD[i] {
			t.Errorf("SortByDesc[%d] = %s, want %s", i, p.Name, wantD[i])
		}
	}
}

func TestLimitTake(t *testing.T) {
	got := Limit([]int{1, 2, 3, 4, 5}, 3)
	if !reflect.DeepEqual(got, []int{1, 2, 3}) {
		t.Errorf("Limit = %v", got)
	}
	if got := Limit([]int{1, 2}, 10); !reflect.DeepEqual(got, []int{1, 2}) {
		t.Errorf("Limit oversized = %v", got)
	}
	if got := Take([]int{1, 2, 3}, 2); !reflect.DeepEqual(got, []int{1, 2}) {
		t.Errorf("Take = %v", got)
	}
	if got := Limit([]int{1, 2}, -1); len(got) != 0 {
		t.Errorf("Limit negative = %v", got)
	}
}

func TestGroupBy(t *testing.T) {
	groups := GroupBy(people, func(p person) string { return p.Team })
	if len(groups) != 2 {
		t.Fatalf("groups = %v", groups)
	}
	// Team order is "red", "blue" (first-occurrence of key).
	if groups[0].Key != "red" || len(groups[0].Items) != 2 {
		t.Errorf("groups[0] = %+v", groups[0])
	}
	if groups[1].Key != "blue" || len(groups[1].Items) != 2 {
		t.Errorf("groups[1] = %+v", groups[1])
	}
	// Items within a group keep source order.
	if groups[0].Items[0].Name != "Alice" || groups[0].Items[1].Name != "Carol" {
		t.Errorf("red items = %+v", groups[0].Items)
	}
}

func TestInnerJoin(t *testing.T) {
	type pair struct {
		Name string
		Pts  int
	}
	got := Join(people, scores,
		func(p person) int { return p.ID },
		func(s score) int { return s.Person },
		func(p person, s score) pair { return pair{p.Name, s.Pts} },
	)
	want := []pair{{"Alice", 10}, {"Alice", 20}, {"Bob", 15}, {"Carol", 5}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Join = %v", got)
	}
}

func TestLeftJoin(t *testing.T) {
	type pair struct {
		Name string
		Pts  int
		Has  bool
	}
	got := LeftJoin(people, scores,
		func(p person) int { return p.ID },
		func(s score) int { return s.Person },
		func(p person, s score, has bool) pair { return pair{p.Name, s.Pts, has} },
	)
	want := []pair{
		{"Alice", 10, true},
		{"Alice", 20, true},
		{"Bob", 15, true},
		{"Carol", 5, true},
		{"Dan", 0, false},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("LeftJoin = %v", got)
	}
}

func TestOuterJoin(t *testing.T) {
	// Person 4 has no scores; score with Person=99 has no person.
	extra := append([]score{}, scores...)
	extra = append(extra, score{99, 1})

	type pair struct {
		L, R   int
		hL, hR bool
	}
	got := OuterJoin(people, extra,
		func(p person) int { return p.ID },
		func(s score) int { return s.Person },
		func(p person, s score, hL, hR bool) pair { return pair{p.ID, s.Pts, hL, hR} },
	)
	wantLen := 4 + // Alice+10, Alice+20, Bob+15, Carol+5
		1 + // Dan, no score
		1 // score 99, no person
	if len(got) != wantLen {
		t.Errorf("OuterJoin len = %d want %d: %v", len(got), wantLen, got)
	}
	// Last row is the unmatched right side (Person=99).
	last := got[len(got)-1]
	if last.hL || !last.hR || last.R != 1 {
		t.Errorf("OuterJoin unmatched right = %+v", last)
	}
}

func TestCrossJoin(t *testing.T) {
	got := CrossJoin([]int{1, 2}, []string{"a", "b"},
		func(i int, s string) string { return fmt.Sprintf("%d%s", i, s) })
	want := []string{"1a", "1b", "2a", "2b"}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("CrossJoin = %v", got)
	}
}

func TestDistinct(t *testing.T) {
	got := Distinct([]int{1, 2, 1, 3, 3, 2})
	if !reflect.DeepEqual(got, []int{1, 2, 3}) {
		t.Errorf("Distinct = %v", got)
	}
}

func TestFlatMap(t *testing.T) {
	got := FlatMap([]int{1, 2, 3}, func(i int) []int { return []int{i, i * 10} })
	if !reflect.DeepEqual(got, []int{1, 10, 2, 20, 3, 30}) {
		t.Errorf("FlatMap = %v", got)
	}
}

func ExampleGroupBy() {
	rows := []struct {
		K string
		V int
	}{
		{"a", 1}, {"b", 2}, {"a", 3},
	}
	for _, g := range GroupBy(rows, func(r struct {
		K string
		V int
	}) string {
		return r.K
	}) {
		fmt.Printf("%s: %d\n", g.Key, len(g.Items))
	}
	// Output:
	// a: 2
	// b: 1
}
