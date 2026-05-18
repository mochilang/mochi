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
