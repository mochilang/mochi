//go:build darwin && arm64

package trampoline

import "unsafe"

// Call invokes a JIT'd arm64 function via a pure-Go assembly trampoline.
// entry is the mmap'd executable page entry point.
// regs points to the vm2 Cell register file (first field of a jitFrame, offset 8).
//
// Implemented in trampoline_darwin_arm64.s; no CGo on the JIT hot path.
func Call(entry unsafe.Pointer, regs unsafe.Pointer) uint64
