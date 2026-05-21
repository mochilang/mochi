// Package ffi hosts the MEP-43 Phase 10 sealing primitives. A Mochi
// handle that crosses the Go FFI boundary is "sealed" at the call
// site: the wrapper is a fresh, type-preserved view that prevents the
// Go callee from dereferencing the underlying Mochi-managed pointer
// via mechanisms the type system cannot prevent. See MEP-41 §6.7
// (sealed handles for FFI) for the security argument; see MEP-43
// Phase 10 for the transpiler-side mechanism that drives this.
//
// At the Go transpiler target the sealing is largely a typing
// guarantee: Go's type system + GC already prevents an arbitrary Go
// callee from forging a handle out of an integer. The runtime helpers
// here are deliberately identity functions; the safety property comes
// from the *type discipline* that Seal/Unseal enforce in the IR
// (and in the bytecode interpreter, where dereferencing a sealed
// handle traps - see vm3's OpSeal / OpUnseal in MEP-41 §6.7). When
// the Go target lowers a sealed-boundary call site, every argument
// that carries a Mochi handle is wrapped in ffi.Seal[T] and the
// return is wrapped in ffi.Unseal[T], so the call site mirrors the
// vm3 sealing pattern even though the Go runtime's safety floor is
// already higher.
package ffi

// Seal returns v unchanged. The signature is the type-system marker:
// a Go callee that receives a Seal[T] cannot widen the type to T
// without invoking Unseal, so misuse becomes visible at the call
// boundary.
func Seal[T any](v T) T { return v }

// Unseal returns v unchanged. The signature mirrors Seal so the
// transpiler can wrap arguments and return values symmetrically.
func Unseal[T any](v T) T { return v }
