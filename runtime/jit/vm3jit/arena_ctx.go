package vm3jit

import (
	"unsafe"

	"mochi/runtime/vm3"
)

// maxCellRegs is the cap on simultaneously-live Cell registers in the
// vm3jit backend. AArch64 pins Cell regs 0..3 in x25..x28 and Cell regs
// 4..7 in x21..x24 (the latter overlaps the i64 callee-saved lane, so
// fns with NumRegsCell > 4 must keep NumRegsI64 <= 7 — see archCaps and
// the i64-cap gate in compile.go). AMD64 Cell-bank lowering (Phase
// 6.2d.2.e) will mirror this on its own callee-saved range. Bumping the
// cap further requires growing jitFrame3 and allocating more callee-
// saved pairs in the prologue.
const maxCellRegs = 8

// MaxCellRegs is exported for tests and external callers that size
// scratch buffers for the Cell bank.
const MaxCellRegs = maxCellRegs

// jitArenaCtx carries the per-call arena snapshot the JIT prologue
// loads into its pinned base registers (x4 = &jitArenaCtx on AArch64).
// listsBase is the cached &Arenas.Lists[0]; mapsBase is the same for
// Arenas.Maps. The prologue picks one to load into x19 based on which
// slab kind the body references (lists vs maps). Stride/offset
// constants are baked into the JIT stream as immediates from the
// JITListSlabStride / JITMapSlabStride etc. helpers. Future Cell-op
// support (sets, structs, ...) extends this struct with setsBase /
// structsBase / etc.
//
// Phase 6.2d.2.d step 4 admits map kernels (OpMapSetI64I64 /
// OpMapGetI64I64) whose pre-sized table never grows during the call,
// so snapshotting mapsBase once at JIT entry is safe. A future
// grow-aware sub-phase will add a deopt-on-grow guard mirroring
// 6.2d.2.c's StatusListGrow.
type jitArenaCtx struct {
	listsBase   unsafe.Pointer
	mapsBase    unsafe.Pointer
	f64ArrsBase unsafe.Pointer
	i64ArrsBase unsafe.Pointer
	pairsBase   unsafe.Pointer
	// pairsLen is the current append cursor into a.Pairs the JIT sees;
	// initialized from uint64(len(a.Pairs)) and bumped in-register by
	// each inline OpNewPair (Phase 6.3.4.m.4b). The caller copies it
	// back into the Go slice header on clean return so subsequent
	// interp accesses see the JIT-allocated pairs. pairsCap is the
	// matching cap snapshot; the inline alloc deopts via
	// StatusPairGrow when pairsLen >= pairsCap so the slab can be
	// regrown without violating Go's slice invariants.
	pairsLen uint64
	pairsCap uint64
}

// populateArenaCtx snapshots the Arenas slab base pointers the JIT
// needs at the current call. Callers pass a pointer to a jitFrame3's
// arenaCtx field so the trampoline can pin its address in x4 without
// risking a Go-managed pointer escape.
func populateArenaCtx(ctx *jitArenaCtx, arenas *vm3.Arenas) {
	ctx.listsBase = arenas.JITListsBase()
	ctx.mapsBase = arenas.JITMapsBase()
	ctx.f64ArrsBase = arenas.JITF64ArrsBase()
	ctx.i64ArrsBase = arenas.JITI64ArrsBase()
	ctx.pairsBase = arenas.JITPairsBase()
	ctx.pairsLen = uint64(arenas.PairsLen())
	ctx.pairsCap = uint64(arenas.PairsCap())
}

// jitArenaCtxPairsLenOff is the byte offset of the pairsLen field in
// jitArenaCtx; the ARM64 emit bakes it as an LDR/STR imm12 (divide by
// 8 for the 64-bit-stride immediate) so a layout change here is
// picked up by the lowering automatically.
func jitArenaCtxPairsLenOff() uintptr {
	return unsafe.Offsetof(jitArenaCtx{}.pairsLen)
}

// jitArenaCtxPairsCapOff mirrors jitArenaCtxPairsLenOff for pairsCap.
func jitArenaCtxPairsCapOff() uintptr {
	return unsafe.Offsetof(jitArenaCtx{}.pairsCap)
}

// jitArenaCtxPairsBaseOff is the byte offset of pairsBase within
// jitArenaCtx. AMD64 OpPairFst/OpPairSnd (Phase 6.3.4.m.4c.3) load it
// from R14+disp32 to recover the pair slab base; the ARM64 backend
// snapshots it directly into x19 in the prologue so no offset is
// referenced from the lowering on that arch.
func jitArenaCtxPairsBaseOff() uintptr {
	return unsafe.Offsetof(jitArenaCtx{}.pairsBase)
}
