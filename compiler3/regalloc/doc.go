// Package regalloc is compiler3's linear-scan register allocator.
//
// Each bank (regsI64, regsF64, regsCell) gets its own live-interval
// pass. The cap-17 limitation of vm2jit goes away because compiler3
// produces a frame with separate banks, each with its own size. A
// function with NumRegsI64=20, NumRegsF64=5, NumRegsCell=3 fits AArch64
// GPR + SIMD register sets naturally.
//
// Phase 0 ships only the scaffold.
package regalloc

import "mochi/compiler3/ir"

// Result holds the per-bank slot counts after allocation.
type Result struct {
	NumI64  uint16
	NumF64  uint16
	NumCell uint16
	// Map from IR Value ID to (bank, slot). Phase 4 wires this up.
	Slots map[uint32]Assignment
}

// Assignment names the bank + slot index of one SSA value's lifetime.
type Assignment struct {
	Bank uint8
	Slot uint16
}

// Allocate runs linear-scan allocation per bank over fn's live ranges.
// Phase 0 stub returns an empty result.
func Allocate(fn *ir.Function) Result {
	_ = fn
	return Result{Slots: map[uint32]Assignment{}}
}
