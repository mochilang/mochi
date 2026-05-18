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

// func CallStatusFF(entry, regsI64, status, regsF64 unsafe.Pointer) uint64
//
// ABI0: entry at FP+0, regsI64 at FP+8, status at FP+16, regsF64 at FP+24,
// result at FP+32. Adds R2 = regsF64 to the CallStatus ABI so JIT'd f64
// kernels can load/store regsF64[r] via [x2, #r*8]. Result is a bare
// uint64 (the JIT bit-casts an f64 result via FMOV x0, d<retSlot> in
// its epilogue; caller uses math.Float64frombits).
TEXT ·CallStatusFF(SB),NOSPLIT,$0-40
	MOVD	entry+0(FP), R16
	MOVD	regsI64+8(FP), R0
	MOVD	status+16(FP), R1
	MOVD	regsF64+24(FP), R2
	CALL	(R16)
	MOVD	R0, ret+32(FP)
	RET

// func CallStatusM(entry, regsI64, status, regsF64, regsCell, arenaCtx unsafe.Pointer) uint64
//
// MEP-40 Phase 6.2d.2.a mixed-bank trampoline. ABI0: args at FP+0..+40,
// result at FP+48. Pins R0 = regsI64, R1 = status, R2 = regsF64, R3 =
// regsCell, R4 = *jitArenaCtx so a Cell-bank JIT'd fn can access all
// three register windows plus the arena slab base pointers without
// going back through the interpreter. Existing Call / CallStatus /
// CallStatusFF variants stay unchanged so i64- and f64-only kernels
// pay no extra register-pinning cost.
TEXT ·CallStatusM(SB),NOSPLIT,$0-56
	MOVD	entry+0(FP), R16
	MOVD	regsI64+8(FP), R0
	MOVD	status+16(FP), R1
	MOVD	regsF64+24(FP), R2
	MOVD	regsCell+32(FP), R3
	MOVD	arenaCtx+40(FP), R4
	CALL	(R16)
	MOVD	R0, ret+48(FP)
	RET
