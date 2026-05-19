package vm3jit

import (
	"unsafe"

	"mochi/runtime/vm3"
)

// maxCellRegs is the cap on simultaneously-live Cell registers in the
// vm3jit backend. AArch64 pins Cell regs 0..3 in x25..x28; AMD64
// Cell-bank lowering (Phase 6.2d.2.e) will mirror this on its own
// callee-saved range. Bumping the cap requires growing jitFrame3 and
// allocating more callee-saved pairs in the prologue.
const maxCellRegs = 4

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
	listsBase unsafe.Pointer
	mapsBase  unsafe.Pointer
}

// populateArenaCtx snapshots the Arenas slab base pointers the JIT
// needs at the current call. Callers pass a pointer to a jitFrame3's
// arenaCtx field so the trampoline can pin its address in x4 without
// risking a Go-managed pointer escape.
func populateArenaCtx(ctx *jitArenaCtx, arenas *vm3.Arenas) {
	ctx.listsBase = arenas.JITListsBase()
	ctx.mapsBase = arenas.JITMapsBase()
}
