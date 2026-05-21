// Package sets is the Mochi-semantics set runtime for the Go target.
// Mochi does not have a dedicated `set` value: set semantics live
// inside list comprehensions (`distinct`, `union`, `intersect`,
// `except`). The emitter lowers those clauses into the functions here
// so the algebra is centralised, debuggable, and Go-importable as a
// regular library.
//
// The contract is order-preserving for the first occurrence of each
// element, matching `distinct` from runtime/mochi/lists: a user
// reading the emitted Go source expects `Union([1,2,3], [3,2,4])` to
// return `[1,2,3,4]`, not the implementation-defined order a
// `map[T]struct{}` walk would produce.
package sets

// Union returns a slice containing every element that appears in a or
// b, with order taken from a-then-b and the first occurrence
// preserved.
func Union[T comparable](a, b []T) []T {
	seen := make(map[T]struct{}, len(a)+len(b))
	out := make([]T, 0, len(a)+len(b))
	for _, v := range a {
		if _, ok := seen[v]; ok {
			continue
		}
		seen[v] = struct{}{}
		out = append(out, v)
	}
	for _, v := range b {
		if _, ok := seen[v]; ok {
			continue
		}
		seen[v] = struct{}{}
		out = append(out, v)
	}
	return out
}

// Intersect returns the elements present in both a and b, in
// first-occurrence-in-a order.
func Intersect[T comparable](a, b []T) []T {
	inB := make(map[T]struct{}, len(b))
	for _, v := range b {
		inB[v] = struct{}{}
	}
	seen := make(map[T]struct{}, len(a))
	out := make([]T, 0, len(a))
	for _, v := range a {
		if _, ok := inB[v]; !ok {
			continue
		}
		if _, ok := seen[v]; ok {
			continue
		}
		seen[v] = struct{}{}
		out = append(out, v)
	}
	return out
}

// Except returns the elements in a that do not appear in b, in
// first-occurrence-in-a order. This matches Mochi's `except` (also
// known as set difference); duplicates within a are deduplicated.
func Except[T comparable](a, b []T) []T {
	inB := make(map[T]struct{}, len(b))
	for _, v := range b {
		inB[v] = struct{}{}
	}
	seen := make(map[T]struct{}, len(a))
	out := make([]T, 0, len(a))
	for _, v := range a {
		if _, ok := inB[v]; ok {
			continue
		}
		if _, ok := seen[v]; ok {
			continue
		}
		seen[v] = struct{}{}
		out = append(out, v)
	}
	return out
}

// From returns the unique elements of src in first-occurrence order.
// Provided so Mochi's `distinct` lowering can have a single call site
// inside the emitter (it could also go through lists.Distinct; we
// expose it here to keep `sets` the obvious home for set-shaped ops).
func From[T comparable](src []T) []T {
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
