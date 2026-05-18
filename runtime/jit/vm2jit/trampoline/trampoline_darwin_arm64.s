//go:build darwin && arm64

#include "textflag.h"

// func Call(entry unsafe.Pointer, regs unsafe.Pointer) uint64
//
// ABI0: entry at FP+0, regs at FP+8, result at FP+16.
// Sets R0 = regs (the JIT's first argument), then jumps to entry.
// The JIT function returns its result Cell in R0.
TEXT ·Call(SB),NOSPLIT,$0-24
	MOVD	entry+0(FP), R16	// R16 = JIT entry point
	MOVD	regs+8(FP), R0		// R0 = *Cell register file (JIT arg x0)
	CALL	(R16)			// call JIT'd function; result in R0
	MOVD	R0, ret+16(FP)
	RET

// func CallStatus(entry unsafe.Pointer, regs unsafe.Pointer, status unsafe.Pointer) uint64
//
// ABI0: entry at FP+0, regs at FP+8, status at FP+16, result at FP+24.
// Sets R0 = regs and R1 = status. On a clean return the JIT leaves *status
// untouched (caller pre-zeroes it). On a deopt path the JIT writes a non-zero
// status code to *status before RET, and the caller treats R0 as undefined.
// Used by vm3jit for divide-by-zero and other guard-checked paths so the raw
// int64 result channel can carry the full i64 range with no sentinel collision.
TEXT ·CallStatus(SB),NOSPLIT,$0-32
	MOVD	entry+0(FP), R16	// R16 = JIT entry point
	MOVD	regs+8(FP), R0		// R0 = *int64 register file (JIT arg x0)
	MOVD	status+16(FP), R1	// R1 = *int64 status word (JIT arg x1)
	CALL	(R16)
	MOVD	R0, ret+24(FP)
	RET
