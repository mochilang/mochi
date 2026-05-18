// Package vm3jit is the MEP-40 Phase 6 baseline JIT for the vm3
// interpreter.
//
// Strategy mirrors vm2jit (MEP-34) but adapted for vm3's three typed
// register banks: i64 values live in a contiguous int64 stack, f64 in
// a float64 stack, Cells in a Cell stack. vm3jit pins active i64
// registers into AArch64 GPRs at function entry and operates entirely
// on raw 64-bit integers in the body, with no NaN-boxing or tag
// arithmetic. This is the central simplification over vm2jit and the
// reason a 2x-of-Go gate is reachable on arithmetic kernels.
//
// # Phase 6.0 scope (this iteration)
//
//   - AArch64 (Darwin) only.
//   - i64-only kernels (NumRegsF64 == 0, NumRegsCell == 0).
//   - Up to 7 i64 registers (x9..x15, caller-saved temps, no prologue
//     frame save needed).
//   - Six opcodes: OpConstI64K, OpAddI64, OpAddI64K, OpCmpGeI64Br,
//     OpJump, OpReturnI64. Anything outside that set returns
//     ErrNotImplemented and the caller falls back to the interpreter.
//
// The 6.0 cut targets one corpus kernel end-to-end (sum_loop). Once
// it shows under 2x of Go, 6.1 extends the opcode and register set to
// the rest of the arithmetic kernels.
//
// # Trampoline
//
// vm3jit reuses runtime/jit/vm2jit/trampoline as-is: the trampoline
// ABI ("entry pointer + one pointer argument, result in x0") is
// generic and contains no vm2-specific assumptions.
package vm3jit
