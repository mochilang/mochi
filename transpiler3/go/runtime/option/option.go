// Package option provides the generic Option[T] helper used by
// the Mochi-to-Go transpiler when Mochi `T?` lowers to a value
// type that cannot share Go's `*T` representation (e.g., when
// T is itself a pointer).
//
// Phase 5 introduces the full implementation. Phase 0 ships an
// empty package so downstream emitter code can import the path.
package option
