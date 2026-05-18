//go:build linux && amd64

#include "textflag.h"

// func Call(entry unsafe.Pointer, regs unsafe.Pointer) uint64
//
// ABI0: entry at FP+0, regs at FP+8, result at FP+16.
// Sets DI = regs (SysV first argument), then calls the entry pointer.
// The JIT function returns its result in AX.
TEXT ·Call(SB),NOSPLIT,$0-24
	MOVQ	entry+0(FP), AX		// AX = JIT entry point
	MOVQ	regs+8(FP), DI		// DI = regs (JIT arg 1)
	CALL	AX
	MOVQ	AX, ret+16(FP)
	RET

// func CallStatus(entry unsafe.Pointer, regs unsafe.Pointer, status unsafe.Pointer) uint64
//
// ABI0: entry at FP+0, regs at FP+8, status at FP+16, result at FP+24.
// Sets DI = regs and SI = status (SysV args 1 and 2). On a clean return
// the JIT leaves *status untouched (caller pre-zeroes it). On a deopt
// path the JIT writes a non-zero status code to *status before RET, and
// the caller treats AX as undefined. Used by vm3jit for divide-by-zero
// and other guard-checked paths so the raw int64 result channel can
// carry the full i64 range with no sentinel collision.
TEXT ·CallStatus(SB),NOSPLIT,$0-32
	MOVQ	entry+0(FP), AX		// AX = JIT entry point
	MOVQ	regs+8(FP), DI		// DI = regs (JIT arg 1)
	MOVQ	status+16(FP), SI	// SI = status (JIT arg 2)
	CALL	AX
	MOVQ	AX, ret+24(FP)
	RET

// func CallStatusFF(entry, regsI64, status, regsF64 unsafe.Pointer) uint64
//
// ABI0: entry at FP+0, regsI64 at FP+8, status at FP+16, regsF64 at FP+24,
// result at FP+32. Adds DX = regsF64 to the CallStatus ABI. The JIT
// loads/stores XMM slots via `movsd disp32(%r14), xmm` after copying DX
// into R14 in its prologue. Result is a bare uint64 (the JIT bit-casts
// an f64 result via `movq %xmm<retSlot>, %rax` in its epilogue).
TEXT ·CallStatusFF(SB),NOSPLIT,$0-40
	MOVQ	entry+0(FP), AX
	MOVQ	regsI64+8(FP), DI
	MOVQ	status+16(FP), SI
	MOVQ	regsF64+24(FP), DX
	CALL	AX
	MOVQ	AX, ret+32(FP)
	RET

// func CallStatusM(entry, regsI64, status, regsF64, regsCell, arenaCtx unsafe.Pointer) uint64
//
// MEP-40 Phase 6.2d.2.a mixed-bank trampoline. ABI0: args at FP+0..+40,
// result at FP+48. AMD64 Cell-bank lowering (Phase 6.2d.2.e) is still
// planned; on Linux/AMD64 today the JIT rejects any cell-bank function
// at compile time, so this entry exists only to keep the Go decl
// symmetric with darwin/arm64. Pinning is: DI = regsI64, SI = status,
// DX = regsF64, CX = regsCell, R8 = *jitArenaCtx (SysV args 1..5).
TEXT ·CallStatusM(SB),NOSPLIT,$0-56
	MOVQ	entry+0(FP), AX
	MOVQ	regsI64+8(FP), DI
	MOVQ	status+16(FP), SI
	MOVQ	regsF64+24(FP), DX
	MOVQ	regsCell+32(FP), CX
	MOVQ	arenaCtx+40(FP), R8
	CALL	AX
	MOVQ	AX, ret+48(FP)
	RET
