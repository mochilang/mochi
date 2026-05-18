//go:build darwin && arm64

package trampoline

import "unsafe"

// Call invokes a JIT'd arm64 function via a pure-Go assembly trampoline.
// entry is the mmap'd executable page entry point.
// regs points to the vm2 Cell register file (first field of a jitFrame, offset 8).
//
// Implemented in trampoline_darwin_arm64.s; no CGo on the JIT hot path.
func Call(entry unsafe.Pointer, regs unsafe.Pointer) uint64

// CallStatus is the vm3jit variant of Call that also pins a status word
// pointer in x1. The JIT writes a non-zero status code to *status on a
// deopt path (divide-by-zero, future type-check failures, ...) before
// returning. Caller must pre-zero *status; on return, a non-zero value
// means the uint64 result is undefined and the caller should fall back
// to the interpreter with the corresponding error. Implemented in the
// same trampoline_darwin_arm64.s file (NOSPLIT so the Go stack cannot
// grow under the JIT and invalidate &status).
func CallStatus(entry unsafe.Pointer, regs unsafe.Pointer, status unsafe.Pointer) uint64

// CallStatusFF is the Phase 6.2b variant that adds a second register
// base pointer for the f64 bank. ABI: x0 = regsI64, x1 = status, x2 =
// regsF64. The JIT loads/stores f64 slots through [x2 + r*8] and, for
// OpReturnF64, bit-casts the result via FMOV x0, d<retSlot> so the
// return-channel stays a single uint64 (caller does
// math.Float64frombits). Used by all kernels with NumRegsF64 > 0.
func CallStatusFF(entry unsafe.Pointer, regsI64 unsafe.Pointer, status unsafe.Pointer, regsF64 unsafe.Pointer) uint64
