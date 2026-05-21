// Package query is the Mochi query-algebra runtime for the Go target.
// Every `from x in ... where ... group by ... join ... select ...`
// clause Mochi emits lowers into a chain of calls here.
//
// The shape is deliberately pull-based slices, not channel-based
// streams: the existing Mochi VM materialises a query result as a
// slice (see runtime/vm/vm.go OpGroupBy and the surrounding ops), and
// the Go target preserves that contract so the emitted code does not
// silently change observable semantics from the VM's behaviour.
//
// Generic helpers:
//   - Filter / Map / FlatMap / Distinct
//   - SortBy / SortByDesc
//   - Limit / Take
//   - GroupBy (returns ordered groups; the group key order matches
//     the first-occurrence order of the source list)
//   - Join / LeftJoin / OuterJoin / CrossJoin (Cartesian product)
//
// The Group type is exported so the emitter can lower
// `group by k into g` to a `query.Group[K, T]` value and then walk
// `g.Items` in a follow-on clause.
package query

import "sort"

// Filter returns the elements of src for which pred returns true.
func Filter[T any](src []T, pred func(T) bool) []T {
	out := make([]T, 0, len(src))
	for _, v := range src {
		if pred(v) {
			out = append(out, v)
		}
	}
	return out
}

// Map applies fn to each element of src.
func Map[T, R any](src []T, fn func(T) R) []R {
	out := make([]R, len(src))
	for i, v := range src {
		out[i] = fn(v)
	}
	return out
}

// FlatMap applies fn to each element and concatenates the results.
func FlatMap[T, R any](src []T, fn func(T) []R) []R {
	out := make([]R, 0, len(src))
	for _, v := range src {
		out = append(out, fn(v)...)
	}
	return out
}

// Distinct returns first-occurrence-unique elements of src.
func Distinct[T comparable](src []T) []T {
	seen := make(map[T]struct{}, len(src))
	out := make([]T, 0, len(src))
	for _, v := range src {
		if _, ok := seen[v]; ok {
			continue
		}
		seen[v] = struct{}{}
		out = append(out, v)
	}
	return out
}

// Ordered re-declares cmp.Ordered locally so this package builds on
// Go 1.20 toolchains (see runtime/mochi/lists for the rationale).
type Ordered interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | ~uintptr |
		~float32 | ~float64 | ~string
}

// SortBy returns a stable-sorted copy of src by the key function.
func SortBy[T any, K Ordered](src []T, key func(T) K) []T {
	out := make([]T, len(src))
	copy(out, src)
	sort.SliceStable(out, func(i, j int) bool { return key(out[i]) < key(out[j]) })
	return out
}

// SortByDesc returns a stable-sorted copy of src by the key function
// in descending order.
func SortByDesc[T any, K Ordered](src []T, key func(T) K) []T {
	out := make([]T, len(src))
	copy(out, src)
	sort.SliceStable(out, func(i, j int) bool { return key(out[i]) > key(out[j]) })
	return out
}

// Limit returns the first n elements of src, or all of src if n
// exceeds len(src). Negative n returns the empty slice.
func Limit[T any](src []T, n int) []T {
	if n <= 0 {
		return src[:0:0]
	}
	if n >= len(src) {
		out := make([]T, len(src))
		copy(out, src)
		return out
	}
	out := make([]T, n)
	copy(out, src[:n])
	return out
}

// Take is an alias for Limit; Mochi uses both spellings interchangeably
// in tests/vm/valid.
func Take[T any](src []T, n int) []T { return Limit(src, n) }

// Group is the result shape of `group by k`: a key plus the items
// that mapped to that key, in their source order.
type Group[K comparable, T any] struct {
	Key   K
	Items []T
}

// GroupBy returns groups in the first-occurrence-of-key order from
// src. Within each group the items appear in source order. This
// matches the VM's OpGroupBy + Items layout (see vm.go around line
// 1630 where the VM materialises a group as a map with `items` and
// `count` keys).
func GroupBy[T any, K comparable](src []T, key func(T) K) []Group[K, T] {
	idx := make(map[K]int, len(src))
	out := make([]Group[K, T], 0)
	for _, v := range src {
		k := key(v)
		if i, ok := idx[k]; ok {
			out[i].Items = append(out[i].Items, v)
			continue
		}
		idx[k] = len(out)
		out = append(out, Group[K, T]{Key: k, Items: []T{v}})
	}
	return out
}

// Join performs an inner equi-join: for each pair (l, r) where
// lkey(l) == rkey(r), produce combine(l, r). Output ordering follows
// left.then(right) lexicographic order, matching Mochi's
// `from l in L from r in R where l.id == r.id` lowering.
func Join[L, R, Out any, K comparable](
	left []L, right []R,
	lkey func(L) K, rkey func(R) K,
	combine func(L, R) Out,
) []Out {
	idx := make(map[K][]R, len(right))
	for _, r := range right {
		k := rkey(r)
		idx[k] = append(idx[k], r)
	}
	out := make([]Out, 0, len(left))
	for _, l := range left {
		for _, r := range idx[lkey(l)] {
			out = append(out, combine(l, r))
		}
	}
	return out
}

// LeftJoin produces combine(l, r) for every l in left, where r is
// either a matching right element or the zero R if no match exists.
// The third argument to combine is true when r is present and false
// when r is the zero value. Mochi's `left join` clause lowers here.
func LeftJoin[L, R, Out any, K comparable](
	left []L, right []R,
	lkey func(L) K, rkey func(R) K,
	combine func(l L, r R, hasR bool) Out,
) []Out {
	idx := make(map[K][]R, len(right))
	for _, r := range right {
		k := rkey(r)
		idx[k] = append(idx[k], r)
	}
	out := make([]Out, 0, len(left))
	var zeroR R
	for _, l := range left {
		matches := idx[lkey(l)]
		if len(matches) == 0 {
			out = append(out, combine(l, zeroR, false))
			continue
		}
		for _, r := range matches {
			out = append(out, combine(l, r, true))
		}
	}
	return out
}

// OuterJoin is a full outer join: every l matched, every r matched,
// and unmatched rows are emitted exactly once with the missing side's
// present-bit false. The output order is left-first then unmatched-
// right in source order, which matches the existing VM `outer join`
// fixture in tests/vm/valid/outer_join.
func OuterJoin[L, R, Out any, K comparable](
	left []L, right []R,
	lkey func(L) K, rkey func(R) K,
	combine func(l L, r R, hasL, hasR bool) Out,
) []Out {
	idx := make(map[K][]R, len(right))
	for _, r := range right {
		k := rkey(r)
		idx[k] = append(idx[k], r)
	}
	matchedR := make(map[K]bool, len(right))
	var zeroL L
	var zeroR R
	out := make([]Out, 0, len(left)+len(right))
	for _, l := range left {
		k := lkey(l)
		matches := idx[k]
		if len(matches) == 0 {
			out = append(out, combine(l, zeroR, true, false))
			continue
		}
		matchedR[k] = true
		for _, r := range matches {
			out = append(out, combine(l, r, true, true))
		}
	}
	for _, r := range right {
		k := rkey(r)
		if matchedR[k] {
			continue
		}
		out = append(out, combine(zeroL, r, false, true))
	}
	return out
}

// CrossJoin returns the Cartesian product of left and right, applying
// combine to every pair in left-major order. Matches Mochi's nested
// `from l in L from r in R` form when there is no `where` clause.
func CrossJoin[L, R, Out any](
	left []L, right []R,
	combine func(L, R) Out,
) []Out {
	out := make([]Out, 0, len(left)*len(right))
	for _, l := range left {
		for _, r := range right {
			out = append(out, combine(l, r))
		}
	}
	return out
}
