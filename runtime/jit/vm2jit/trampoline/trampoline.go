// Package trampoline provides pure-Go .s trampolines for calling JIT'd vm2
// functions without cgo. The trampoline is the only code that crosses the
// Go/native boundary; see MEP-34 §Trampoline for the design contract.
//
// Phase 1 ships two files:
//   - trampoline_arm64.s: AArch64 AAPCS64 calling convention, saves x9-x15
//     across the call (they hold vm2 registers), and provides the frame-slot
//     load/store glue so the JIT'd function sees the vm2 register file.
//   - trampoline_amd64.s: System V AMD64 calling convention, saves rbx, rbp,
//     r12-r15, r10 across the call for the same reason.
//
// The trampolines are TODO(Phase 1): the scaffold ships the stubs below.
package trampoline

import "unsafe"

// Call invokes a JIT'd function with the given register-file pointer.
// The entry pointer must have been produced by vm2jit.pageEntry.
//
// On darwin/arm64 this will be implemented in trampoline_arm64.s using the
// MAP_JIT / pthread_jit_write_protect_np-compatible call pattern.
// On linux/amd64 this will be implemented in trampoline_amd64.s.
//
// TODO(Phase 1): replace stub with real .s implementation.
func Call(_ unsafe.Pointer, _ unsafe.Pointer) { panic("trampoline: not yet implemented") }
