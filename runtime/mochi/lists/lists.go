// Package lists is the Mochi-semantics list runtime for the Go
// target. The operations match the VM's list dispatch table: Append
// returns a new slice (matching OpAppend), Reverse returns a new
// slice of the same element type, and the numeric aggregates Sum,
// Min, Max, Avg operate on float64 because the Mochi VM coerces to
// float for these reductions (see runtime/vm/vm.go OpSum/OpMin/etc.).
//
// MEP-43 §3.3: the emitter calls Append/Sum/Min/Max here instead of
// templating its own arithmetic. The functions are generic where Go
// 1.18+ generics fit; non-generic shims live below for the numeric
// reductions where Mochi's semantics fix the result type.
package lists

import (
	"sort"
)

// Append returns a new slice with elem appended. The Mochi VM's
// OpAppend produces a fresh slice value; this matches that contract
// (we do not mutate src).
func Append[T any](src []T, elem T) []T {
	out := make([]T, len(src), len(src)+1)
	copy(out, src)
	return append(out, elem)
}

// Len reports the number of elements in src. Trivially `len(src)`,
// but exposed as a function so the emitter can produce a uniform
// `lists.Len(x)` call regardless of x's static type.
func Len[T any](src []T) int { return len(src) }

// Reverse returns a new slice with src's elements in reverse order.
func Reverse[T any](src []T) []T {
	n := len(src)
	out := make([]T, n)
	for i, v := range src {
		out[n-1-i] = v
	}
	return out
}

// First returns the first element of src and a present bit. The
// present bit is false for an empty list; the VM's `first(xs)`
// returns the zero value in that case, but a Go user calling this
// runtime function expects the missing case to be discoverable. The
// emitter discards the bool when lowering `first(xs)`.
func First[T any](src []T) (T, bool) {
	var zero T
	if len(src) == 0 {
		return zero, false
	}
	return src[0], true
}

// Last returns the last element of src and a present bit, with the
// same convention as First.
func Last[T any](src []T) (T, bool) {
	var zero T
	if len(src) == 0 {
		return zero, false
	}
	return src[len(src)-1], true
}

// Concat concatenates any number of slices into one new slice.
// Matches Mochi's `concat(a, b, c, ...)` builtin (VM op OpUnionAll
// applied left-fold).
func Concat[T any](parts ...[]T) []T {
	n := 0
	for _, p := range parts {
		n += len(p)
	}
	out := make([]T, 0, n)
	for _, p := range parts {
		out = append(out, p...)
	}
	return out
}

// Slice returns src[start:end] with Mochi's clamping semantics
// (matches strings.Substr above and the VM's OpSlice path: clamp
// negative start to 0, clamp end past len to len, inverted range
// returns an empty slice rather than panicking).
func Slice[T any](src []T, start, end int) []T {
	n := len(src)
	if start < 0 {
		start = 0
	}
	if end > n {
		end = n
	}
	if start > end {
		return src[:0:0]
	}
	out := make([]T, end-start)
	copy(out, src[start:end])
	return out
}

// Contains reports whether src contains elem (using == equality).
// Constrained to comparable so it works on the value-equality types
// the VM exposes; structs containing maps or slices need ContainsFunc.
func Contains[T comparable](src []T, elem T) bool {
	for _, v := range src {
		if v == elem {
			return true
		}
	}
	return false
}

// IndexOf returns the index of the first occurrence of elem in src,
// or -1 if elem is not present.
func IndexOf[T comparable](src []T, elem T) int {
	for i, v := range src {
		if v == elem {
			return i
		}
	}
	return -1
}

// Distinct returns a new slice with the order-preserving unique
// elements of src.
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

// SortBy returns a new slice sorted by the key function. The sort is
// stable, matching Mochi's `sort by` query clause which preserves
// input order for equal keys (see runtime/vm/vm.go OpGroupBy +
// SortClause documentation).
func SortBy[T any, K Ordered](src []T, key func(T) K) []T {
	out := make([]T, len(src))
	copy(out, src)
	sort.SliceStable(out, func(i, j int) bool { return key(out[i]) < key(out[j]) })
	return out
}

// Ordered is the local re-declaration of Go's `cmp.Ordered`. We
// duplicate it here so this package compiles on Go 1.20 toolchains as
// well as Go 1.21+, since `runtime/mochi/` is intended to be a normal
// Go library that downstream users might import on older Go.
type Ordered interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | ~uintptr |
		~float32 | ~float64 | ~string
}

// SumFloat returns the float64 sum of src. Mochi's `sum` reduces
// numbers in any numeric type to float64; we encode that contract by
// requiring callers to use ToFloat first or by calling SumInt below.
func SumFloat(src []float64) float64 {
	var s float64
	for _, v := range src {
		s += v
	}
	return s
}

// SumInt returns the int64 sum of src.
func SumInt(src []int64) int64 {
	var s int64
	for _, v := range src {
		s += v
	}
	return s
}

// AvgFloat returns the mean of src as a float64. An empty list yields
// 0, matching the VM's vacuous-zero convention for `avg([])`.
func AvgFloat(src []float64) float64 {
	if len(src) == 0 {
		return 0
	}
	return SumFloat(src) / float64(len(src))
}

// MinOrdered returns the minimum element of src. Panics on empty
// input. Mochi's VM raises a runtime error on `min([])`; we surface
// the same via panic so the emitter does not need to thread an error
// return through every aggregate call.
func MinOrdered[T Ordered](src []T) T {
	if len(src) == 0 {
		panic("mochi/lists: MinOrdered on empty list")
	}
	m := src[0]
	for _, v := range src[1:] {
		if v < m {
			m = v
		}
	}
	return m
}

// MaxOrdered returns the maximum element of src. Panics on empty
// input for the same reason as MinOrdered.
func MaxOrdered[T Ordered](src []T) T {
	if len(src) == 0 {
		panic("mochi/lists: MaxOrdered on empty list")
	}
	m := src[0]
	for _, v := range src[1:] {
		if v > m {
			m = v
		}
	}
	return m
}
