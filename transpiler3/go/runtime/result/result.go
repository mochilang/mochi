// Package result provides MochiResult[T, E], the runtime
// representation of Mochi try/catch values when both the OK
// and the error payload need to flow through a single Go
// return value.
//
// Phase 11 introduces the full implementation. Phase 0 ships
// an empty package so the emitter can import the path.
package result
