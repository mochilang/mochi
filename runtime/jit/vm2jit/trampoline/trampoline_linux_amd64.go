//go:build linux && amd64

package trampoline

import "unsafe"

// Call invokes a JIT'd amd64 function via a pure-Go assembly trampoline.
// entry is the mmap'd executable page entry point.
// regs points to the vm3 i64 register file.
//
// Implemented in trampoline_linux_amd64.s; no CGo on the JIT hot path.
func Call(entry unsafe.Pointer, regs unsafe.Pointer) uint64

// CallStatus is the vm3jit variant of Call that also pins a status word
// pointer. The JIT writes a non-zero status code to *status on a deopt
// path (divide-by-zero, future type-check failures, ...) before
// returning. Caller must pre-zero *status; on return, a non-zero value
// means the uint64 result is undefined and the caller should fall back
// to the interpreter with the corresponding error. Implemented in
// trampoline_linux_amd64.s (NOSPLIT so the Go stack cannot grow under
// the JIT and invalidate &status).
func CallStatus(entry unsafe.Pointer, regs unsafe.Pointer, status unsafe.Pointer) uint64

// CallStatusFF is the Phase 6.2b variant that adds a second register
// base pointer for the f64 bank. ABI: RDI = regsI64, RSI = status,
// RDX = regsF64. The JIT moves RDX into a callee-saved GPR in its
// prologue (R14 by convention) so f64 loads/stores can use
// `movsd disp32(%r14), xmm` without flowing through scratch. Result
// is a bare uint64; for an f64 return the JIT bit-casts via
// `movq %xmm<retSlot>, %rax` in its epilogue (caller uses
// math.Float64frombits).
func CallStatusFF(entry unsafe.Pointer, regsI64 unsafe.Pointer, status unsafe.Pointer, regsF64 unsafe.Pointer) uint64

// CallStatusM is the MEP-40 Phase 6.2d.2.a mixed-bank trampoline. ABI on
// AMD64 (SysV): RDI = regsI64, RSI = status, RDX = regsF64, RCX =
// regsCell, R8 = *jitArenaCtx. Linux/AMD64 Cell-bank lowering is the
// Phase 6.2d.2.e gate; until then the JIT rejects any cell-bank fn at
// compile time and this entry is unused on AMD64.
func CallStatusM(entry unsafe.Pointer, regsI64 unsafe.Pointer, status unsafe.Pointer, regsF64 unsafe.Pointer, regsCell unsafe.Pointer, arenaCtx unsafe.Pointer) uint64
