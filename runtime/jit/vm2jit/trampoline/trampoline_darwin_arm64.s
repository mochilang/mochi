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
