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
// Only listsBase is dynamic so far; stride/offset constants are baked
// into the JIT stream as immediates from JITListSlabStride and
// JITListCellsOffset. Future Cell-op support (maps, sets, ...) extends
// this struct with mapsBase / setsBase / etc.
//
// Phase 6.2d.2.a step 2 only handles read-only OpListGetI64, which
// cannot grow the slab during the call, so snapshotting listsBase
// once at JIT entry is safe. Phase 6.2d.2.c (inline OpListPushI64)
// will add a deopt-on-grow guard so a slab reallocation cannot leave
// the JIT reading from a stale base.
type jitArenaCtx struct {
	listsBase unsafe.Pointer
}

// populateArenaCtx snapshots the Arenas slab base pointers the JIT
// needs at the current call. Callers pass a pointer to a jitFrame3's
// arenaCtx field so the trampoline can pin its address in x4 without
// risking a Go-managed pointer escape.
func populateArenaCtx(ctx *jitArenaCtx, arenas *vm3.Arenas) {
	ctx.listsBase = arenas.JITListsBase()
}
