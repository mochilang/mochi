// Package maps is the Mochi-semantics map runtime for the Go target.
// The functions correspond to the VM's map-builtin dispatch table:
// `keys(m)`, `values(m)`, `has(m, k)`, plus the `contains` selector
// which the VM rewrites into a map-key lookup at compile time. Keys
// are returned in stable sorted order so emitted code is deterministic
// (the existing VM walks the map and surfaces whatever order Go gave
// it; Mochi tests that rely on key order would fail intermittently).
//
// The package intentionally has no public mutation primitives:
// Mochi-side `m[k] = v` lowers to the Go assignment `m[k] = v`
// directly in the emitter (Phase 4), not to a function call here.
package maps

import (
	"sort"
)

// Keys returns the keys of m in stable sorted order. The Ordered
// constraint matches Mochi's map-key type rule (keys must compare).
func Keys[K Ordered, V any](m map[K]V) []K {
	out := make([]K, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

// Values returns the values of m in the order matching Keys(m).
// Returning values in unspecified order would defeat the determinism
// guarantee Mochi makes to its callers.
func Values[K Ordered, V any](m map[K]V) []V {
	ks := Keys(m)
	out := make([]V, 0, len(ks))
	for _, k := range ks {
		out = append(out, m[k])
	}
	return out
}

// Has reports whether m contains key k. Matches Mochi's `contains`
// selector lowered onto a map.
func Has[K comparable, V any](m map[K]V, k K) bool {
	_, ok := m[k]
	return ok
}

// Get returns m[k] and a present bit. Matches the desugaring Mochi
// applies to a map index inside a null-safe expression.
func Get[K comparable, V any](m map[K]V, k K) (V, bool) {
	v, ok := m[k]
	return v, ok
}

// Len returns the number of entries in m.
func Len[K comparable, V any](m map[K]V) int { return len(m) }

// Ordered is the local re-declaration of cmp.Ordered. See
// runtime/mochi/lists for the rationale.
type Ordered interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | ~uintptr |
		~float32 | ~float64 | ~string
}
