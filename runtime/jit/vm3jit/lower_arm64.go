//go:build arm64

package vm3jit

import (
	"fmt"
	"unsafe"

	"mochi/runtime/vm3"
)

// Register pinning:
//
//	i64-only fns:
//	  regsI64[r] -> x(9 + r)        for r in [0, 7)   (caller-saved)
//	  regsI64[r] -> x(19 + r - 7)   for r in [7, 17)  (callee-saved)
//
//	Cell-bank fns (Phase 6.2d.2.a step 2):
//	  regsI64[r]  -> x(9 + r)       for r in [0, 7)   (caller-saved)
//	  regsI64[r]  -> x(21 + r - 7)  for r in [7, 11)  (callee-saved, 4 max)
//	  regsCell[r] -> x(25 + r)      for r in [0, 4)   (callee-saved, 4 max)
//	  x19 = cached arenas.Lists base ptr  (loaded once at prologue)
//	  x20 = cached &arenas.Lists[idx]     when hoistedCellReg(fn) == 0
//	        (Phase 6.2d.2.c.1: loop-invariant slab byte address for
//	         regsCell[0]; ListGet/Push use [x20, #cellsOff+..] instead
//	         of recomputing the slab offset every iter)
//	  x4  = &jitArenaCtx                  (set by trampoline.CallStatusM)
//	  x3  = regsCell base                 (set by trampoline.CallStatusM)
//
// x9..x15 are AArch64 caller-saved temporaries; the JIT can clobber
// them freely. x19..x28 are callee-saved; functions that touch any of
// those slots push them as STP/LDP pairs in the prologue/epilogue.
// x0 carries the regsI64 base pointer (set by the trampoline).
// x1 carries the optional *int64 status word pointer (set by
// trampoline.CallStatus). The JIT writes a non-zero status code via
// [x1] on a deopt path (divide-by-zero etc.) before returning. Phase
// 6.1c+ codegen assumes x1 is always live and never clobbers it; the
// happy path leaves *status at its caller-pre-zeroed value.
// x16/x17 are intra-procedure scratches available to any encoder.
//
// vm3 i64 registers are stored as raw int64 (no NaN-box, no tag). A
// single LDR/STR moves a value between the regsI64 stack window and
// its pinned host register; arithmetic runs directly on the host
// register with no tag arithmetic. This is the key simplification
// over vm2jit and the reason a small instruction count per opcode is
// achievable for arithmetic kernels.

func r2x(fn *vm3.Function, r uint16) uint32 {
	if r < 7 {
		return uint32(r) + 9
	}
	if fn.NumRegsCell > 0 {
		return uint32(r) - 7 + 21
	}
	return uint32(r) - 7 + 19
}

// r2cell maps a vm3 Cell register slot to its pinned AArch64 callee-
// saved register. Cells 0..3 sit in x25..x28 (unchanged); cells 4..7
// sit in x21..x24 (Phase 6.3.4.j.3). Caller has pre-checked
// r < maxCellRegs. When fn.NumRegsCell > 4 the admissibility check in
// compile.go also forbids NumRegsI64 > 7 so the cell-4..7 lane and the
// i64-7..10 callee-saved lane (which also lives at x21..x24) never
// overlap.
func r2cell(r uint16) uint32 {
	if r < 4 {
		return uint32(r) + 25
	}
	return uint32(r) + 17 // r=4→21, r=5→22, r=6→23, r=7→24
}

// r2d maps a vm3 f64 register slot to the AArch64 SIMD register
// number (V0..V7 / D0..D7). Caller pre-checks r < maxF64RegsARM64.
// The same register number is used for both V (128-bit) and D
// (lower 64-bit) views; the instruction encoding chooses the view.
func r2d(r uint16) uint32 { return uint32(r) }

// computeCallSpills runs a backward liveness pass over fn.Code and
// returns, for each bytecode index, the bitset of caller-saved i64
// registers (bank 0..6, mapped to x9..x15) that must be spilled to the
// caller window across the call. For non-call opcodes the entry is 0.
// Self-recursive OpCallI64 sites use this to skip spilling dead regs.
// Shared helpers liveSuccUnion / defUseI64 live in lower_common.go.
func computeCallSpills(fn *vm3.Function) []uint32 {
	n := len(fn.Code)
	spills := make([]uint32, n)
	liveIn := make([]uint32, n+1) // liveIn[n] = 0 (post-exit)
	liveOut := make([]uint32, n)
	for changed := true; changed; {
		changed = false
		for i := n - 1; i >= 0; i-- {
			op := fn.Code[i]
			out := liveSuccUnion(fn, i, liveIn)
			d, u := defUseI64(fn, op)
			in := u | (out &^ d)
			if out != liveOut[i] || in != liveIn[i] {
				liveOut[i] = out
				liveIn[i] = in
				changed = true
			}
		}
	}
	for i, op := range fn.Code {
		switch op.Code {
		case vm3.OpCallI64, vm3.OpCallMixed:
		default:
			continue
		}
		mask := uint32(0x7F) // caller-saved (regs 0..6 → x9..x15)
		// dstBit excludes A only when retBank places the result in the
		// i64 bank. For OpCallI64 the result is always i64; for
		// OpCallMixed read the bank flag.
		var dstBit uint32
		if op.Code == vm3.OpCallI64 || vm3.Bank(op.BankFlags&0x3) == vm3.BankI64 {
			dstBit = uint32(1) << op.A
		}
		spills[i] = (liveOut[i] &^ dstBit) & mask
	}
	return spills
}

// numCalleeSavedPairs returns the total number of x19..x28 STP/LDP
// pairs the frame for fn must push. Each pair covers two register
// slots. The final pair is pushed even if only one of its two regs is
// live, since AArch64 STP is a single 16-byte op and partial pairs
// would waste alignment without saving anything.
//
// For Cell-bank fns (NumRegsCell > 0) the layout always pushes one
// extra "cellscratch" pair (x19:x20) that caches arena base pointers
// (x19 = listsBase) plus one pair per two Cell regs at x25..x28.
// Existing i64-only fns (NumRegsCell == 0) keep the original layout
// at x19..x28.
func numCalleeSavedPairs(fn *vm3.Function) int {
	return numCellScratchPairs(fn) + extraCellScratchPairsARM64(fn) + numI64CalleeSavedPairs(fn) + numCellCalleeSavedPairs(fn) + numTableHoistPairsARM64(fn)
}

// numCellScratchPairs is 1 when fn uses any Cell reg (we push x19:x20
// to cache arena base pointers and free them up across self-tail
// branches), 0 otherwise.
func numCellScratchPairs(fn *vm3.Function) int {
	if fn.NumRegsCell == 0 {
		return 0
	}
	return 1
}

// numI64CalleeSavedPairs is the number of x19..x28 (or x21..x24 when
// cell-bank) pairs that back regsI64 slots 7..NumRegsI64-1.
func numI64CalleeSavedPairs(fn *vm3.Function) int {
	n := int(fn.NumRegsI64)
	if n > maxI64Regs {
		n = maxI64Regs
	}
	if n <= 7 {
		return 0
	}
	return (n - 7 + 1) / 2
}

// numCellCalleeSavedPairs is the number of x25..x28 pairs that back
// regsCell slots 0..NumRegsCell-1.
func numCellCalleeSavedPairs(fn *vm3.Function) int {
	n := int(fn.NumRegsCell)
	if n > maxCellRegs {
		n = maxCellRegs
	}
	if n == 0 {
		return 0
	}
	return (n + 1) / 2
}

// calleeSavedPairRegs returns the first reg of each STP/LDP pair in
// prologue push order. For sum (NumRegsI64=4, NumRegsCell=1, has Get
// only) this is [19, 21, 25]: STP x19:x20 (cellscratch + slab base),
// STP x21:x22 (cells.cap + cells.ptr; cap unused for sum but pushed
// alongside ptr), STP x25:x26 (cell reg 0). For fill (has Push) the
// list grows with STP x23:x24 (cells.len + unused) before x25.
//
// Phase 6.3.4.j.3 extends cell-bank to NumRegsCell up to 8: cells 0..3
// stay at x25..x28 (pairs k=0,1), cells 4..7 land at x21..x24 (pairs
// k=2,3 → first regs 21, 23). The x21:x22 / x23:x24 pairs are
// mutually exclusive with the slab-field hoist (hoist only applies at
// NumRegsCell==1) and with the i64-callee-saved lane (the
// admissibility check forbids NumRegsCell > 4 with NumRegsI64 > 7),
// so the same pair never carries two banks simultaneously.
func calleeSavedPairRegs(fn *vm3.Function) []uint32 {
	pairs := make([]uint32, 0, numCalleeSavedPairs(fn))
	if numCellScratchPairs(fn) > 0 {
		pairs = append(pairs, 19)
	}
	if hoistsCellsPtrARM64(fn) || hoistsCellsCapARM64(fn) {
		pairs = append(pairs, 21)
	}
	if hoistsCellsLenARM64(fn) {
		pairs = append(pairs, 23)
	}
	i64Pairs := numI64CalleeSavedPairs(fn)
	if i64Pairs > 0 {
		i64Start := uint32(19)
		if fn.NumRegsCell > 0 {
			i64Start = 21
		}
		for k := 0; k < i64Pairs; k++ {
			pairs = append(pairs, i64Start+uint32(2*k))
		}
	}
	for k := 0; k < numCellCalleeSavedPairs(fn); k++ {
		// Cells 0..3 live in x25:x26 (k=0) and x27:x28 (k=1).
		// Cells 4..7 live in x21:x22 (k=2) and x23:x24 (k=3).
		var first uint32
		switch k {
		case 0:
			first = 25
		case 1:
			first = 27
		case 2:
			first = 21
		case 3:
			first = 23
		}
		pairs = append(pairs, first)
	}
	hoistStart := tableHoistRegStartARM64(fn)
	for k := 0; k < numTableHoistPairsARM64(fn); k++ {
		pairs = append(pairs, hoistStart+uint32(2*k))
	}
	return pairs
}

// lookupTableIdxsARM64 returns the I64 table indices referenced by
// OpLookupI64KW sites in fn.Code, in first-occurrence order, capped to
// tableHoistCapARM64(fn). Indices beyond the cap fall back to the per-
// site movImm64 + LDR sequence (the unhoisted lowering still works,
// just at 4 extra inner-loop words per site).
//
// Hoisting a table base costs one callee-saved register and a 1..4-word
// movImm64 in the prologue, in exchange for collapsing every per-site
// OpLookupI64KW from `movImm64+LDR` (2..5 words) to a single LDR. For
// dispatch-bound kernels (the Phase 6.4 switch-lookup shape) this is a
// 60..80% reduction in inner-loop instruction count per dispatch.
//
// Empty or out-of-range tables are skipped here (the emitter rejects
// them via ErrNotImplemented at wordCount/emit time); they do not
// consume a hoist slot.
func lookupTableIdxsARM64(fn *vm3.Function) []uint16 {
	capN := tableHoistCapARM64(fn)
	if capN <= 0 {
		return nil
	}
	seen := make(map[uint16]bool)
	idxs := make([]uint16, 0, capN)
	for _, op := range fn.Code {
		if op.Code != vm3.OpLookupI64KW {
			continue
		}
		t := uint16(op.C)
		if seen[t] {
			continue
		}
		if int(t) >= len(fn.I64Tables) || len(fn.I64Tables[t]) == 0 {
			continue
		}
		seen[t] = true
		idxs = append(idxs, t)
		if len(idxs) >= capN {
			break
		}
	}
	return idxs
}

// tableHoistCapARM64 returns the maximum number of OpLookupI64KW table
// bases that can be hoisted into callee-saved regs for fn. The budget
// is the unused tail of x19..x28 after the i64 / cell-bank / cell-
// scratch / extra-scratch pair allocations claim their share. Cell-
// bank fns currently skip table hoists (their x19..x28 layout is
// fully committed to arena base + slab field pins; admitting a hoist
// would need a new layout slot we do not reserve in Phase 6.4.b).
func tableHoistCapARM64(fn *vm3.Function) int {
	if fn.NumRegsCell > 0 {
		return 0
	}
	used := 2 * numI64CalleeSavedPairs(fn)
	avail := 10 - used // x19..x28 is 10 regs
	if avail < 0 {
		return 0
	}
	return avail
}

// tableHoistRegStartARM64 returns the first callee-saved host register
// reserved for table hoists. Sits immediately after the existing i64
// callee-saved range (x19 + 2*numI64CalleeSavedPairs) so the existing
// reg-pinning math is unchanged.
func tableHoistRegStartARM64(fn *vm3.Function) uint32 {
	return 19 + uint32(2*numI64CalleeSavedPairs(fn))
}

// numTableHoistPairsARM64 returns the count of STP/LDP pairs the
// prologue must push to back fn's hoisted table bases. Ceil(numHoists
// / 2); the final pair may carry an unused half (same pattern as
// numI64CalleeSavedPairs for an odd reg count).
func numTableHoistPairsARM64(fn *vm3.Function) int {
	n := len(lookupTableIdxsARM64(fn))
	if n == 0 {
		return 0
	}
	return (n + 1) / 2
}

// tableHoistRegARM64 returns the host register holding the base
// address &fn.I64Tables[tableIdx][0], or 0 (= zero register) when the
// table is not hoisted. Callers must check the return value: a zero
// indicates "fall back to the per-op movImm64 + LDR sequence".
func tableHoistRegARM64(fn *vm3.Function, tableIdx uint16) uint32 {
	idxs := lookupTableIdxsARM64(fn)
	if len(idxs) == 0 {
		return 0
	}
	start := tableHoistRegStartARM64(fn)
	for k, t := range idxs {
		if t == tableIdx {
			return start + uint32(k)
		}
	}
	return 0
}

// tableHoistPrologueWordsARM64 returns the prologue word count needed
// to load every hoisted table's base address into its pinned host reg.
// Sum of movImm64WordCount over each table's heap address.
func tableHoistPrologueWordsARM64(fn *vm3.Function) int {
	idxs := lookupTableIdxsARM64(fn)
	if len(idxs) == 0 {
		return 0
	}
	n := 0
	for _, t := range idxs {
		base := int64(uintptr(unsafe.Pointer(&fn.I64Tables[t][0])))
		n += movImm64WordCount(base)
	}
	return n
}

// hasRegRegDivMod, hasListPushI64, hasListGetI64 live in lower_common.go
// (used by both arch-specific lowering and the arch-independent
// admissibility check in compile.go).

// slabKind discriminates which Arena slab a Cell-bank fn references
// through its inline opcodes. Phase 6.2d.2.d step 4 extends the
// single-slab hoist from list-only to admit map kernels; subsequent
// sub-phases (sets, structs) will add more variants. fns that mix
// kinds (e.g. both list and map opcodes) fall back to slabKindNone
// because the current frame layout only pins one slab base in x19.
type slabKind uint8

const (
	slabKindNone slabKind = iota
	slabKindList
	slabKindMap
)

// slabKindARM64 classifies fn by which slab its inline body references.
// Returns slabKindNone when fn references neither slab or both (mixed
// kernels are rejected by checkCellBankAdmissible separately so they
// never reach codegen).
func slabKindARM64(fn *vm3.Function) slabKind {
	hasList := hasListGetI64(fn) || hasListPushI64(fn)
	hasMap := hasMapOpI64(fn)
	switch {
	case hasList && !hasMap:
		return slabKindList
	case hasMap && !hasList:
		return slabKindMap
	default:
		return slabKindNone
	}
}

// slabBaseOffARM64 returns the byte offset within jitArenaCtx of the
// slab base pointer the prologue should load into x19. Mirrors the
// field order in jitArenaCtx: listsBase at offset 0, mapsBase at 8.
// Fns with no slab kind default to listsBase so the existing list-only
// admit set keeps its prologue word count unchanged.
func slabBaseOffARM64(fn *vm3.Function) uint32 {
	if slabKindARM64(fn) == slabKindMap {
		return 8
	}
	return 0
}

// slabStrideARM64 returns the byte distance between consecutive slab
// entries for fn's slab kind. Used by the slab-base hoist (UXTW + MOV
// stride + MUL + ADD) and by per-op cold-form recompute paths.
func slabStrideARM64(fn *vm3.Function) int64 {
	if slabKindARM64(fn) == slabKindMap {
		return int64(vm3.JITMapSlabStride())
	}
	return int64(vm3.JITListSlabStride())
}

// hoistedCellReg returns the regsCell index whose slab byte address can
// be cached in x20 for the lifetime of the call, or -1 if no hoist
// applies. Phase 6.2d.2.c.1 caches regsCell[0]'s slab base when fn has
// exactly one cell reg AND a list opcode in the body (the
// lists_fill_sum kernel shape: fill/sum each own a single list handle
// that stays put across every OpListGetI64 / OpListPushI64 inside
// their loops). OpListGetI64 / OpListPushI64 emitters detect this and
// skip the per-op UXTW + MUL + ADD lane, dropping 4 instructions per
// opcode in the inner loop.
//
// Why gate on NumRegsCell == 1: when the function carries multiple
// cells we can only pin one slab base in x20, and the bytecode could
// freely interleave Gets/Pushes against different cells; keeping the
// non-hoisted recompute path for those is simpler than picking a
// "primary" cell and tracking which sites hit it.
//
// Why also gate on hasListGetI64 || hasListPushI64: cell-bank fns that
// only thread a Cell through to a cross-fn OpCallMixed (Phase
// 6.2d.2.b "main" shape) have no list op in their body, so the hoist
// would waste the prologue's UXTW+MUL+ADD without ever being read.
// Skipping the hoist here frees x20 for the cross-fn arena ctx stash
// (lowerARM64 emits MOV x20, x4 at prologue end when
// hasCrossFnCallMixed and no slab hoist applies).
func hoistedCellReg(fn *vm3.Function) int {
	if fn.NumRegsCell == 1 && slabKindARM64(fn) != slabKindNone {
		return 0
	}
	return -1
}

// hoistPrologueWordsARM64 returns the extra prologue words needed to
// cache the slab base address for hoistedCellReg(fn) in x20 plus any
// pinned cells header fields (Phase 6.2d.2.c.2). Zero when no hoist
// applies.
func hoistPrologueWordsARM64(fn *vm3.Function) int {
	if hoistedCellReg(fn) < 0 {
		return 0
	}
	stride := slabStrideARM64(fn)
	// Slab base hoist: UXTW + MOV stride + MUL + ADD.
	n := movImm64WordCount(stride) + 3
	if hoistsCellsPtrARM64(fn) {
		n++ // LDR x22, [x20, #cellsPtr]
	}
	if hoistsCellsCapARM64(fn) {
		n++ // LDR x21, [x20, #cellsCap]
	}
	if hoistsCellsLenARM64(fn) {
		n++ // LDR x23, [x20, #cellsLen]
	}
	return n
}

// slabFieldHoistOKARM64 reports whether fn's frame has the x21..x24
// callee-saved register range free for the slab-field hoist. The
// existing layout reserves x21..x24 for regsI64 slots 7..10 (the
// callee-saved i64 lane on Cell-bank fns), so when NumRegsI64 > 7 we
// skip the slab-field hoist and fall back to the 6.2d.2.c.1 slab-base
// hoist alone. Lists_fill_sum's fill (NumRegsI64=3) and sum (=4) fit.
func slabFieldHoistOKARM64(fn *vm3.Function) bool {
	if hoistedCellReg(fn) < 0 {
		return false
	}
	// Map kernels (Phase 6.2d.2.d step 4) read table.ptr / cap /
	// nLive directly off x20 inside the inline kernel and do not pin
	// dedicated x21..x24 hoist regs yet. Skip the list-only field
	// hoists so the frame keeps its smaller layout.
	if slabKindARM64(fn) != slabKindList {
		return false
	}
	return int(fn.NumRegsI64) <= 7
}

// hoistsCellsPtrARM64 reports whether the prologue should pin
// cells.ptr in x22 (Phase 6.2d.2.c.2). Applies whenever the slab-field
// hoist is admissible and fn has at least one OpListGetI64 or
// OpListPushI64 against the hoisted cell. cells.ptr is loop-invariant
// inside the whitelist (only an OpListPushI64 cap-exhaust grow could
// move it; that path deopts before reaching the next op).
func hoistsCellsPtrARM64(fn *vm3.Function) bool {
	if !slabFieldHoistOKARM64(fn) {
		return false
	}
	return hasListGetI64(fn) || hasListPushI64(fn)
}

// hoistsCellsCapARM64 reports whether the prologue should pin
// cells.cap in x21. Only OpListPushI64 reads cap (for the inline
// cap-check); read-only sum fns skip this load.
func hoistsCellsCapARM64(fn *vm3.Function) bool {
	if !slabFieldHoistOKARM64(fn) {
		return false
	}
	return hasListPushI64(fn)
}

// hoistsCellsLenARM64 reports whether the prologue should pin
// cells.len in x23 and bump it in-register across pushes, flushing
// back to memory at every Return* and at the StatusListGrow deopt
// block. Read-only fns (sum) leave cells.len in memory.
func hoistsCellsLenARM64(fn *vm3.Function) bool {
	if !slabFieldHoistOKARM64(fn) {
		return false
	}
	return hasListPushI64(fn)
}

// cellsPtrHoistRefreshPC returns the PC at which fn should refresh
// every K-prefix pinned cell register from `handle` to `cells.ptr`,
// or -1 if the hoist does not apply.
//
// Phase 6.3.4.j.4a: for K-prefix kernels (NumRegsCell>=2) the existing
// slab-field hoist (x21=cap, x22=ptr, x23=len) only applies when
// NumRegsCell==1, because x21..x24 are then claimed by cells 4..7. To
// recover the per-access savings (5 inst per OpListGetF64/SetF64) for
// multi-cell kernels we overwrite each x_cell with `cells.ptr` at one
// post-push refresh PC: before that PC the register holds the handle
// (needed for OpListPushI64 / OpNewList stubs), after it the register
// holds the f64 array base directly so each Get/Set collapses to a
// single LDR/STR Dt.
//
// Detection (n_body's shape):
//  1. Find lastPushPC = max pc of OpListPushI64 in fn.Code.
//  2. Find a CmpGe*Br at pc < lastPushPC whose target > lastPushPC.
//     That target is the loop-exit landing pad; we refresh there.
//  3. Verify no deopt-emitting op (OpListPushI64, reg-reg DivI64/ModI64,
//     OpMapSetI64I64) exists at pc >= refreshPC. A deopt would spill
//     x_cell (now holding cells.ptr) back into regsCell, corrupting
//     the handle.
//  4. Verify no forward branch from pc < refreshPC targets a pc in
//     (refreshPC, end] (which would skip the refresh and reach a
//     post-refresh f64 site with x_cell still holding a handle).
//  5. Verify the op AT refreshPC has no internal `pcMap[idx] + K`
//     arithmetic (which would break if we prepend refresh words to its
//     emission). OpConstI64K / OpAddI64K / OpMovI64 / OpListGet/Set
//     are safe; Cmp*Br variants are not.
//
// Returns -1 when any check fails (the kernel falls back to the cold
// 6-inst Get/Set form for every site).
func cellsPtrHoistRefreshPC(fn *vm3.Function) int {
	if fn.NumRegsCell < 2 {
		return -1
	}
	if int(fn.NumRegsCell) > maxCellRegs {
		return -1
	}
	lastPushPC := -1
	for i, op := range fn.Code {
		if op.Code == vm3.OpListPushI64 {
			lastPushPC = i
		}
	}
	if lastPushPC < 0 {
		return -1
	}
	refreshPC := -1
	for i := 0; i < lastPushPC; i++ {
		op := fn.Code[i]
		switch op.Code {
		case vm3.OpCmpGeI64KBr, vm3.OpCmpGtI64KBr,
			vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
			vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
			vm3.OpCmpGeI64Br, vm3.OpCmpGtI64Br,
			vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
			vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br:
			t := int(uint16(op.C))
			if t > lastPushPC && (refreshPC < 0 || t < refreshPC) {
				refreshPC = t
			}
		}
	}
	if refreshPC < 0 || refreshPC >= len(fn.Code) {
		return -1
	}
	for i := refreshPC; i < len(fn.Code); i++ {
		switch fn.Code[i].Code {
		case vm3.OpListPushI64, vm3.OpDivI64, vm3.OpModI64,
			vm3.OpMapSetI64I64:
			return -1
		}
	}
	for i := 0; i < refreshPC; i++ {
		op := fn.Code[i]
		switch op.Code {
		case vm3.OpJump,
			vm3.OpCmpGeI64KBr, vm3.OpCmpGtI64KBr,
			vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
			vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
			vm3.OpCmpGeI64Br, vm3.OpCmpGtI64Br,
			vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
			vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br:
			t := int(uint16(op.C))
			if t > refreshPC {
				return -1
			}
		}
	}
	switch fn.Code[refreshPC].Code {
	case vm3.OpConstI64K, vm3.OpConstI64KW, vm3.OpConstF64K,
		vm3.OpMovI64, vm3.OpMovF64,
		vm3.OpAddI64K, vm3.OpSubI64K, vm3.OpMulI64K,
		vm3.OpAddI64, vm3.OpSubI64, vm3.OpMulI64,
		vm3.OpAddF64, vm3.OpSubF64, vm3.OpMulF64, vm3.OpDivF64,
		vm3.OpNegI64, vm3.OpNegF64,
		vm3.OpI64ToF64, vm3.OpF64ToI64,
		vm3.OpListGetF64, vm3.OpListSetF64:
	default:
		return -1
	}
	return refreshPC
}

// cellsPtrHoistedCells returns the regsCell indices whose x_cell
// register fn's emit loop will overwrite with `cells.ptr` at the
// refresh PC. Only cells that fn actually touches via OpListGetF64 or
// OpListSetF64 at pc >= refreshPC are hoisted; cells used only in
// the push phase (or unused post-refresh) keep their handle value
// since hoisting them would waste 4 inst per cell in the refresh
// block without any matching per-access savings.
func cellsPtrHoistedCells(fn *vm3.Function) []uint16 {
	refreshPC := cellsPtrHoistRefreshPC(fn)
	if refreshPC < 0 {
		return nil
	}
	nCell := int(fn.NumRegsCell)
	if nCell > maxCellRegs {
		nCell = maxCellRegs
	}
	seen := make([]bool, nCell)
	var cells []uint16
	for i := refreshPC; i < len(fn.Code); i++ {
		op := fn.Code[i]
		var c int = -1
		switch op.Code {
		case vm3.OpListGetF64:
			c = int(op.B)
		case vm3.OpListSetF64:
			c = int(op.A)
		}
		if c >= 0 && c < nCell && !seen[c] {
			seen[c] = true
			cells = append(cells, uint16(c))
		}
	}
	return cells
}

// cellsPtrHoistRefreshWords is the word cost of the cells.ptr refresh
// sequence prepended at refreshPC. 0 when the hoist is not active.
// One shared MOVZ x17, #stride (since stride fits in 16 bits for the
// list slab; movImm64WordCount(40)=1) plus 4 inst per hoisted cell
// (UXTW + MUL + ADD + LDR).
func cellsPtrHoistRefreshWords(fn *vm3.Function) int {
	cells := cellsPtrHoistedCells(fn)
	if len(cells) == 0 {
		return 0
	}
	stride := int64(vm3.JITListSlabStride())
	return movImm64WordCount(stride) + 4*len(cells)
}

// cellsPtrHoistedAt reports whether OpListGetF64 / OpListSetF64 at
// bytecode index idx can use the 1-inst form (x_cell already holds
// cells.ptr). Caller passes the cell index used by the op.
func cellsPtrHoistedAt(fn *vm3.Function, idx int, cell uint16) bool {
	refreshPC := cellsPtrHoistRefreshPC(fn)
	if refreshPC < 0 || idx < refreshPC {
		return false
	}
	for _, c := range cellsPtrHoistedCells(fn) {
		if c == cell {
			return true
		}
	}
	return false
}

// emitCellsPtrRefreshARM64 appends the refresh sequence that overwrites
// every hoisted x_cell with the corresponding cells.ptr. Called once at
// refreshPC, prepended to that PC's normal op emit so any branch that
// targets refreshPC lands on the refresh first.
func emitCellsPtrRefreshARM64(ws []uint32, fn *vm3.Function) []uint32 {
	cells := cellsPtrHoistedCells(fn)
	if len(cells) == 0 {
		return ws
	}
	stride := int64(vm3.JITListSlabStride())
	cellsOff := uint32(vm3.JITListCellsOffset())
	ws = append(ws, movImm64(17, stride)...)
	for _, c := range cells {
		xCell := r2cell(c)
		ws = append(ws, uxtwReg(16, xCell))
		ws = append(ws, mulReg(16, 16, 17))
		ws = append(ws, addReg(16, 16, 19))
		ws = append(ws, ldr64(xCell, 16, cellsOff/8))
	}
	return ws
}

// cellsLenFlushWords returns the number of words a Return* or deopt
// block needs to flush the pinned cells.len (x23) back to memory so
// the interpreter sees the up-to-date list length. Two words when
// cells.len is hoisted (STR x23 into the slice header at byte 16 plus
// STR w23 into vmList.len at byte 4); 0 otherwise.
func cellsLenFlushWords(fn *vm3.Function) int {
	if hoistsCellsLenARM64(fn) {
		return 2
	}
	return 0
}

// emitCellsLenFlushARM64 appends the two STR words that copy x23
// (pinned cells.len) back to memory. The slab base is in x20, so
// cells.len lives at [x20, #16] (imm12=2, 8-byte stride) and the
// 32-bit vmList.len mirror lives at [x20, #4] (imm12=1, 4-byte stride).
func emitCellsLenFlushARM64(ws []uint32) []uint32 {
	cellsOff := uint32(vm3.JITListCellsOffset())
	ws = append(ws, str64(23, 20, (cellsOff+8)/8))
	ws = append(ws, strW(23, 20, 1))
	return ws
}

// extraCellScratchPairsARM64 returns the number of additional STP/LDP
// pairs above the base x19:x20 cellscratch pair that the slab-field
// hoist (Phase 6.2d.2.c.2) needs:
//   - 1 extra pair (x21:x22) when cells.cap or cells.ptr is pinned
//   - 1 more pair (x23:x24) when cells.len is pinned
// 0 otherwise. The pairs always come in adjacent twos because AArch64
// STP/LDP operates on 16-byte slices; x_unused (x22 when only cap
// pinned, x24 always) is pushed/popped along for free.
func extraCellScratchPairsARM64(fn *vm3.Function) int {
	if !slabFieldHoistOKARM64(fn) {
		return 0
	}
	pairs := 0
	if hoistsCellsPtrARM64(fn) || hoistsCellsCapARM64(fn) {
		pairs++
	}
	if hoistsCellsLenARM64(fn) {
		pairs++
	}
	return pairs
}

// deoptStatusesUsedARM64 returns the deopt status codes fn references,
// in emission order. Caller emits one block per status; the order here
// also fixes the per-status block offsets (StatusDivByZero block first,
// then StatusListGrow, ...). Returned slice is empty when fn has no
// deopt sites.
func deoptStatusesUsedARM64(fn *vm3.Function) []int64 {
	var s []int64
	if hasRegRegDivMod(fn) {
		s = append(s, StatusDivByZero)
	}
	if hasListPushI64(fn) {
		s = append(s, StatusListGrow)
	}
	if hasMapSetI64I64(fn) {
		s = append(s, StatusMapGrow)
	}
	return s
}

// deoptBlockWordsARM64Status returns the word count of one deopt block
// for fn. All blocks share the same shape (MOV status, STR status,
// spill pinned regs, frame epilogue, RET); only the status code value
// differs, so the word count is the same per block.
func deoptBlockWordsARM64Status(fn *vm3.Function) int {
	// MOV + STR status + i64 spills + f64 spills + cell spills +
	// epilogue LDPs + RET.
	nI64 := int(fn.NumRegsI64)
	if nI64 > maxI64Regs {
		nI64 = maxI64Regs
	}
	if fn.NumRegsCell > 0 && nI64 > maxI64RegsCellARM64 {
		nI64 = maxI64RegsCellARM64
	}
	nF64 := int(fn.NumRegsF64)
	if nF64 > maxF64RegsARM64 {
		nF64 = maxF64RegsARM64
	}
	nCell := int(fn.NumRegsCell)
	if nCell > maxCellRegs {
		nCell = maxCellRegs
	}
	return 2 + nI64 + nF64 + nCell + numCalleeSavedPairs(fn) + numLRPair(fn) + 1 + cellsLenFlushWords(fn)
}

// deoptStartForStatus returns the word offset where the deopt block
// for `status` lives, given the first-block start (`baseStart`) in the
// emitted stream. Each block is `deoptBlockWordsARM64Status(fn)` words
// long; the order matches deoptStatusesUsedARM64.
func deoptStartForStatus(fn *vm3.Function, baseStart int, status int64) int {
	per := deoptBlockWordsARM64Status(fn)
	for i, s := range deoptStatusesUsedARM64(fn) {
		if s == status {
			return baseStart + i*per
		}
	}
	return baseStart // unreachable for callers that gate emission
}

// isNonLeaf reports whether fn issues any native BL or BLR. Self-
// recursive OpCallI64 lowers to BL, and cross-fn OpCallMixed (Phase
// 6.2d.2.b) lowers to BLR; both write x30, so the caller must push
// x29:x30 as an extra outermost STP pair. OpTailCallMixed lowers to a
// backward B (no link), so it does not by itself force non-leaf.
func isNonLeaf(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpCallI64 || op.Code == vm3.OpCallMixed {
			return true
		}
	}
	return false
}

// hasCrossFnCallMixed reports whether fn contains an OpCallMixed to a
// callee other than itself. Cross-fn callmixed sites need the
// jitArenaCtx address stashed in x20 across the BLR (callee's prologue
// reloads x19 = listsBase from [x4]); self OpCallMixed would conflict
// with self-tail-call's x20 hoist convention and is not admitted by
// the whitelist.
func hasCrossFnCallMixed(fn *vm3.Function, opts Options) bool {
	for _, op := range fn.Code {
		if op.Code != vm3.OpCallMixed {
			continue
		}
		if opts.SelfIdx >= 0 && int(uint16(op.C)) == opts.SelfIdx {
			continue
		}
		return true
	}
	return false
}

// needsArenaCtxStash reports whether the prologue should emit MOV x20,
// x4 to preserve the jitArenaCtx address across a cross-fn BLR. Only
// triggers when fn has a cross-fn OpCallMixed AND the slab-base hoist
// does not also claim x20 (the two uses collide; admitting both would
// need an extra callee-saved reg, which the current frame layout does
// not reserve).
func needsArenaCtxStash(fn *vm3.Function, opts Options) bool {
	if !hasCrossFnCallMixed(fn, opts) {
		return false
	}
	return hoistedCellReg(fn) < 0
}

// resolveCrossFnCallee returns the callee for a cross-fn OpCallMixed at
// op. Mirrors the gating in checkCrossFnCallMixedAdmissible so callers
// in lower_arm64 can fail with ErrNotImplemented if a caller slips past
// the admissibility check (e.g. via test fixtures that bypass compile.go).
func resolveCrossFnCallee(opts Options, op vm3.Op) (*vm3.Function, error) {
	if opts.Prog == nil {
		return nil, fmt.Errorf("%w: CallMixed needs opts.Prog", ErrNotImplemented)
	}
	idx := int(uint16(op.C))
	if idx < 0 || idx >= len(opts.Prog.Funcs) {
		return nil, fmt.Errorf("%w: CallMixed callee idx %d out of range", ErrNotImplemented, idx)
	}
	if opts.SelfIdx >= 0 && idx == opts.SelfIdx {
		return nil, fmt.Errorf("%w: CallMixed to self idx %d not admitted", ErrNotImplemented, idx)
	}
	callee := opts.Prog.Funcs[idx]
	if callee.JITCode == nil {
		return nil, fmt.Errorf("%w: CallMixed callee %s has no JITCode", ErrNotImplemented, callee.Name)
	}
	return callee, nil
}

// countParamBanks returns the per-bank arg counts for callee.ParamBanks.
// Used by crossFnCallMixedWordsARM64 to size the BLR sequence's arg STR
// runs.
func countParamBanks(callee *vm3.Function) (nI64, nF64, nCell int) {
	for _, b := range callee.ParamBanks {
		switch b {
		case vm3.BankI64:
			nI64++
		case vm3.BankF64:
			nF64++
		case vm3.BankCell:
			nCell++
		}
	}
	return
}

// crossFnCallMixedWordsARM64 returns the word count for the BLR
// sequence at a cross-fn OpCallMixed site. The sequence is:
//
//	nSpill * STR x(9+r),[x0,#r*8]        ; spill caller-saved i64
//	nI64Args  * STR src,[x0,#(callerNI64+k)*8]
//	nF64Args  * STR Dsrc,[x2,#(callerNF64+k)*8]
//	nCellArgs * STR src,[x3,#(callerNCell+k)*8]
//	STP x0,x2,[SP,#-16]!
//	STP x3,xzr,[SP,#-16]!
//	[ADD x0,x0,#(callerNI64*8)]          ; only if callerNI64>0
//	[ADD x2,x2,#(callerNF64*8)]          ; only if callerNF64>0
//	[ADD x3,x3,#(callerNCell*8)]         ; only if callerNCell>0
//	MOV x4,x20                            ; restore arena ctx
//	movImm64(x16, &callee.JITCode)
//	BLR x16
//	MOV x17,x0                            ; capture i64 result
//	[LDR x16,[x1]]                        ; only if callee can deopt
//	LDP x3,xzr,[SP],#16
//	LDP x0,x2,[SP],#16
//	nSpill * LDR x(9+r),[x0,#r*8]
//	[CBNZ x16,passthrough]                ; only if callee can deopt
//	MOV xA,x17                            ; deliver result to dst
//
// The 2 extra words (LDR x16,[x1] + CBNZ x16,passthrough) are emitted
// only when the callee has at least one deopt-capable opcode in its
// body. The LDR comes after MOV x17,x0 (x1 is preserved by the callee
// so the post-BLR read sees the same status word the trampoline passed
// in); the CBNZ lands after the LDP+LDR-reload sequence so SP and the
// caller's regs bases are back to their prologue state when the branch
// is taken (the passthrough block runs the frame epilogue against that
// state).
func crossFnCallMixedWordsARM64(fn, callee *vm3.Function, spillMask uint32) int {
	nSpill := popcount32(spillMask)
	nI64Args, nF64Args, nCellArgs := countParamBanks(callee)
	bumps := 0
	if fn.NumRegsI64 > 0 {
		bumps++
	}
	if fn.NumRegsF64 > 0 {
		bumps++
	}
	if fn.NumRegsCell > 0 {
		bumps++
	}
	addr := int64(uintptr(callee.JITCode))
	deoptExtra := 0
	if crossFnDeoptCallee(callee) {
		deoptExtra = 2 // LDR x16,[x1] + CBNZ x16,passthrough
	}
	// 2*nSpill spill+reload + arg STRs + 2 STP + bumps + 1 MOV x4,x20 +
	// movImm64 + 1 BLR + 1 MOV x17,x0 + 2 LDP + 1 MOV xA,x17 +
	// optional deopt-passthrough check (LDR + CBNZ).
	return 2*nSpill + nI64Args + nF64Args + nCellArgs + 2 + bumps + 1 + movImm64WordCount(addr) + 1 + 1 + 2 + 1 + deoptExtra
}

// crossFnDeoptCallee reports whether a cross-fn OpCallMixed callee can
// raise a non-zero status code from a deopt-capable opcode (list push,
// reg-reg div/mod). When true, the caller's BLR site emits the
// LDR x16,[x1] + CBNZ x16,passthrough pair so a callee-side deopt
// propagates back through the caller without writing the caller's own
// status (the callee already did).
func crossFnDeoptCallee(callee *vm3.Function) bool {
	return hasListPushI64(callee) || hasRegRegDivMod(callee)
}

// needsCrossFnDeoptPassthrough reports whether fn must emit a single
// passthrough deopt block at the end of its code stream. True iff fn
// has at least one cross-fn OpCallMixed whose callee can deopt; the
// block is shared by every such site since they all need the same
// shape (spill caller pinned regs, run frame epilogue, RET) and none
// of them write *status (the callee already did).
func needsCrossFnDeoptPassthrough(fn *vm3.Function, opts Options) bool {
	if opts.Prog == nil {
		return false
	}
	for _, op := range fn.Code {
		if op.Code != vm3.OpCallMixed {
			continue
		}
		idx := int(uint16(op.C))
		if opts.SelfIdx >= 0 && idx == opts.SelfIdx {
			continue
		}
		if idx < 0 || idx >= len(opts.Prog.Funcs) {
			continue
		}
		callee := opts.Prog.Funcs[idx]
		if callee.JITCode == nil {
			continue
		}
		if crossFnDeoptCallee(callee) {
			return true
		}
	}
	return false
}

// passthroughBlockWordsARM64 returns the size of fn's cross-fn deopt
// passthrough block. Same shape as a normal per-status deopt block
// minus the MOV x16,#status + STR x16,[x1] pair (the callee already
// wrote the status, so the caller's passthrough must not overwrite
// it). Zero when fn has no deopt-capable cross-fn callees.
func passthroughBlockWordsARM64(fn *vm3.Function, opts Options) int {
	if !needsCrossFnDeoptPassthrough(fn, opts) {
		return 0
	}
	return deoptBlockWordsARM64Status(fn) - 2
}

// emitPassthroughDeoptBlockARM64 emits the cross-fn deopt passthrough
// block. Layout matches emitDeoptBlockARM64 except no MOV x16,#status
// and no STR x16,[x1] (caller did not raise its own status; it is
// propagating the callee's). Lives at the very end of the code stream
// so every cross-fn OpCallMixed site CBNZes to the same offset.
func emitPassthroughDeoptBlockARM64(fn *vm3.Function) []uint32 {
	nI64 := int(fn.NumRegsI64)
	if nI64 > maxI64Regs {
		nI64 = maxI64Regs
	}
	if fn.NumRegsCell > 0 && nI64 > maxI64RegsCellARM64 {
		nI64 = maxI64RegsCellARM64
	}
	nF64 := int(fn.NumRegsF64)
	if nF64 > maxF64RegsARM64 {
		nF64 = maxF64RegsARM64
	}
	nCell := int(fn.NumRegsCell)
	if nCell > maxCellRegs {
		nCell = maxCellRegs
	}
	ws := make([]uint32, 0, deoptBlockWordsARM64Status(fn)-2)
	if hoistsCellsLenARM64(fn) {
		ws = emitCellsLenFlushARM64(ws)
	}
	for r := uint32(0); r < uint32(nI64); r++ {
		ws = append(ws, str64(r2x(fn, uint16(r)), 0, r))
	}
	for r := uint32(0); r < uint32(nF64); r++ {
		ws = append(ws, strD(r2d(uint16(r)), 2, r))
	}
	for r := uint32(0); r < uint32(nCell); r++ {
		ws = append(ws, str64(r2cell(uint16(r)), 3, r))
	}
	ws = emitFrameEpilogueARM64(ws, calleeSavedPairRegs(fn), numLRPair(fn))
	ws = append(ws, ret())
	return ws
}

// passthroughStartARM64 returns the word offset where fn's passthrough
// deopt block begins, given the first per-status deopt block start.
// Each OpCallMixed site uses this to compute the CBNZ branch offset.
func passthroughStartARM64(fn *vm3.Function, deoptStart int) int {
	return deoptStart + len(deoptStatusesUsedARM64(fn))*deoptBlockWordsARM64Status(fn)
}

// numLRPair returns 1 if fn needs the x29:x30 outermost STP/LDP pair,
// 0 otherwise. The pair is independent of numCalleeSavedPairs and lives
// in addition to those (x19..x28 register saves).
func numLRPair(fn *vm3.Function) int {
	if isNonLeaf(fn) {
		return 1
	}
	return 0
}

// emitDeoptBlockARM64 emits one per-status deopt block for fn at the
// end of the instruction stream. statusCode is the int64 value the JIT
// writes to *(x1) before unwinding. Every pinned i64/f64/cell reg is
// spilled back to its base array first so vm3.JITCallFn can copy the
// JIT's final state into vm.deopt* and the interpreter can resume the
// callee from PC=0 with the spilled values.
//
// Block layout:
//
//	STR Xr, [x0, #r*8]   ; for each pinned i64 reg (NumRegsI64 entries)
//	STR Dr, [x2, #r*8]   ; for each pinned f64 reg (NumRegsF64 entries)
//	STR Xr, [x3, #r*8]   ; for each pinned cell reg (NumRegsCell entries)
//	MOV  x16, #status_code
//	STR  x16, [x1]
//	<frame epilogue>
//	RET
func emitDeoptBlockARM64(fn *vm3.Function, statusCode int64) []uint32 {
	nI64 := int(fn.NumRegsI64)
	if nI64 > maxI64Regs {
		nI64 = maxI64Regs
	}
	if fn.NumRegsCell > 0 && nI64 > maxI64RegsCellARM64 {
		nI64 = maxI64RegsCellARM64
	}
	nF64 := int(fn.NumRegsF64)
	if nF64 > maxF64RegsARM64 {
		nF64 = maxF64RegsARM64
	}
	nCell := int(fn.NumRegsCell)
	if nCell > maxCellRegs {
		nCell = maxCellRegs
	}
	ws := make([]uint32, 0, deoptBlockWordsARM64Status(fn))
	if hoistsCellsLenARM64(fn) {
		ws = emitCellsLenFlushARM64(ws)
	}
	for r := uint32(0); r < uint32(nI64); r++ {
		ws = append(ws, str64(r2x(fn, uint16(r)), 0, r))
	}
	for r := uint32(0); r < uint32(nF64); r++ {
		ws = append(ws, strD(r2d(uint16(r)), 2, r))
	}
	for r := uint32(0); r < uint32(nCell); r++ {
		ws = append(ws, str64(r2cell(uint16(r)), 3, r))
	}
	ws = append(ws, movImm64(16, statusCode)...)
	ws = append(ws, str64(16, 1, 0))
	ws = emitFrameEpilogueARM64(ws, calleeSavedPairRegs(fn), numLRPair(fn))
	ws = append(ws, ret())
	return ws
}

// lowerARM64 returns the AArch64 instruction-word stream for fn. Two
// passes: pass 1 computes pcMap[i] = word index where the lowering of
// bytecode i starts; pass 2 emits real instructions, resolving branch
// destinations through pcMap. opts.SelfIdx, when >= 0, enables self-
// recursive OpCallI64 to lower to a native BL inside this page.
func lowerARM64(fn *vm3.Function, opts Options) ([]uint32, error) {
	if fn.NumRegsF64 > 0 && isNonLeaf(fn) {
		return nil, fmt.Errorf("%w: %s has both f64 regs and OpCallI64 (Phase 6.2b leaves f64+self-recursion to a later sub-phase)",
			ErrNotImplemented, fn.Name)
	}
	pairRegs := calleeSavedPairRegs(fn)
	pairs := len(pairRegs)
	lrPair := numLRPair(fn)
	cellScratch := numCellScratchPairs(fn)
	arenaCtxStash := 0
	if needsArenaCtxStash(fn, opts) {
		arenaCtxStash = 1 // MOV x20, x4
	}
	prologueWords := lrPair + pairs + int(fn.NumRegsI64) + int(fn.NumRegsF64) + int(fn.NumRegsCell) + cellScratch + hoistPrologueWordsARM64(fn) + arenaCtxStash + tableHoistPrologueWordsARM64(fn)
	spillSets := computeCallSpills(fn)

	pcMap := make([]int, len(fn.Code)+1)
	pcMap[0] = prologueWords
	for i, op := range fn.Code {
		n, err := wordCountARM64(fn, op, opts, spillSets, i)
		if err != nil {
			return nil, fmt.Errorf("vm3jit/%s: pc %d: %w", fn.Name, i, err)
		}
		pcMap[i+1] = pcMap[i] + n
	}

	deoptStart := pcMap[len(fn.Code)]
	statuses := deoptStatusesUsedARM64(fn)
	passthroughWords := passthroughBlockWordsARM64(fn, opts)
	total := deoptStart + len(statuses)*deoptBlockWordsARM64Status(fn) + passthroughWords
	ws := make([]uint32, 0, total)

	// Prologue: push x29:x30 (outermost) if non-leaf, then push
	// callee-saved pairs, then load each live reg from [x0, #r*8] into
	// its pinned host register, then load each live f64 reg from
	// [x2, #r*8] into V0..V7.
	if lrPair == 1 {
		ws = append(ws, stpPreIdx64(29, 30, 31, -16))
	}
	for _, r := range pairRegs {
		ws = append(ws, stpPreIdx64(r, r+1, 31, -16))
	}
	for r := uint32(0); r < uint32(fn.NumRegsI64); r++ {
		ws = append(ws, ldr64(r2x(fn, uint16(r)), 0, r))
	}
	for r := uint32(0); r < uint32(fn.NumRegsF64); r++ {
		ws = append(ws, ldrD(r2d(uint16(r)), 2, r))
	}
	// Cell-bank fns: load each pinned Cell reg from regsCell base (x3),
	// then cache the slab base pointer in x19 from the jitArenaCtx (x4).
	// listsBase lives at offset 0 of jitArenaCtx; mapsBase at offset 8.
	// slabBaseOffARM64(fn) picks the right one for the body's slab kind.
	for r := uint32(0); r < uint32(fn.NumRegsCell); r++ {
		ws = append(ws, ldr64(r2cell(uint16(r)), 3, r))
	}
	if cellScratch > 0 {
		ws = append(ws, ldr64(19, 4, slabBaseOffARM64(fn)/8))
	}
	// Slab-base hoist (Phase 6.2d.2.c.1): when one cell reg owns every
	// list reference in fn, compute &arenas.Lists[handleIdx] once into
	// x20 so each OpListGet/OpListPush inside the loop skips the UXTW +
	// MOV stride + MUL + ADD recompute. The handle stays put for the
	// lifetime of the call (no opcode in the whitelist mutates a cell
	// reg), so the cached base is loop-invariant by construction.
	if h := hoistedCellReg(fn); h >= 0 {
		stride := slabStrideARM64(fn)
		ws = append(ws, uxtwReg(16, r2cell(uint16(h))))
		ws = append(ws, movImm64(17, stride)...)
		ws = append(ws, mulReg(16, 16, 17))
		ws = append(ws, addReg(20, 16, 19))
		// Phase 6.2d.2.c.2: pin the loop-invariant cells slice header
		// fields in callee-saved regs. cells.ptr (x22) and cells.cap
		// (x21) never change inside the whitelist (a cap-exhaust deopt
		// returns to the interp before reaching the next op); cells.len
		// (x23) is bumped in-register by each push and flushed back at
		// every Return* and at the StatusListGrow deopt block.
		cellsOff := uint32(vm3.JITListCellsOffset())
		if hoistsCellsPtrARM64(fn) {
			ws = append(ws, ldr64(22, 20, cellsOff/8))
		}
		if hoistsCellsCapARM64(fn) {
			ws = append(ws, ldr64(21, 20, (cellsOff+16)/8))
		}
		if hoistsCellsLenARM64(fn) {
			ws = append(ws, ldr64(23, 20, (cellsOff+8)/8))
		}
	}
	// Cross-fn OpCallMixed arena-ctx stash (Phase 6.2d.2.b step 1). x4
	// holds the jitArenaCtx pointer the trampoline pinned at JIT entry;
	// it gets clobbered by callee-saved discipline (intra-procedure
	// scratch) the moment another opcode needs x4, and callees that
	// dereference x4 in their own prologue need the same address. Park
	// x4 in callee-saved x20 here so each cross-fn BLR site can restore
	// x4 from x20 right before the branch.
	if needsArenaCtxStash(fn, opts) {
		ws = append(ws, movReg(20, 4))
	}

	// Phase 6.4.b: hoist OpLookupI64KW table-base addresses into
	// callee-saved regs. The bases are loop-invariant by construction
	// (fn.I64Tables is owned by the Function record and never mutated
	// after CompileProgram), so each OpLookupI64KW in the body becomes
	// a single LDR Xd, [Xhoist, Xidx, LSL #3] instead of the cold
	// `movImm64 + LDR` pair. Mirrors Go CL 756340's dispatch-shape win.
	if idxs := lookupTableIdxsARM64(fn); len(idxs) > 0 {
		start := tableHoistRegStartARM64(fn)
		for k, t := range idxs {
			base := int64(uintptr(unsafe.Pointer(&fn.I64Tables[t][0])))
			ws = append(ws, movImm64(start+uint32(k), base)...)
		}
	}

	for i, op := range fn.Code {
		emit, err := emitInstrARM64(fn, op, i, pcMap, deoptStart, opts, spillSets)
		if err != nil {
			return nil, fmt.Errorf("vm3jit/%s: pc %d op=%d: %w", fn.Name, i, op.Code, err)
		}
		if got, want := len(emit), pcMap[i+1]-pcMap[i]; got != want {
			return nil, fmt.Errorf("vm3jit/%s: pc %d op=%d: emitted %d words, predicted %d",
				fn.Name, i, op.Code, got, want)
		}
		ws = append(ws, emit...)
	}
	if len(ws) != deoptStart {
		return nil, fmt.Errorf("vm3jit/%s: code stream %d words, predicted %d",
			fn.Name, len(ws), deoptStart)
	}
	for _, status := range statuses {
		ws = append(ws, emitDeoptBlockARM64(fn, status)...)
	}
	if passthroughWords > 0 {
		ws = append(ws, emitPassthroughDeoptBlockARM64(fn)...)
	}
	if len(ws) != total {
		return nil, fmt.Errorf("vm3jit/%s: final stream %d words, predicted %d",
			fn.Name, len(ws), total)
	}
	return ws, nil
}

// emitFrameEpilogueARM64 appends LDP instructions to ws in REVERSE
// order of the prologue's pushes so each pop matches the topmost STP
// frame and SP returns to its entry value. pairRegs is the prologue's
// push order (first reg of each pair); lrPair is 1 if x29:x30 was
// pushed as the outermost pair (non-leaf fns).
func emitFrameEpilogueARM64(ws []uint32, pairRegs []uint32, lrPair int) []uint32 {
	for k := len(pairRegs) - 1; k >= 0; k-- {
		r := pairRegs[k]
		ws = append(ws, ldpPostIdx64(r, r+1, 31, 16))
	}
	if lrPair == 1 {
		ws = append(ws, ldpPostIdx64(29, 30, 31, 16))
	}
	return ws
}

// wordCountARM64 returns the exact number of 32-bit words emitInstrARM64
// will produce for op. Used by pass 1 to lay out pcMap. spillSets and
// idx are only consulted for OpCallI64; for other opcodes the spill
// set is irrelevant.
func wordCountARM64(fn *vm3.Function, op vm3.Op, opts Options, spillSets []uint32, idx int) (int, error) {
	base, err := wordCountARM64Body(fn, op, opts, spillSets, idx)
	if err != nil {
		return 0, err
	}
	if idx == cellsPtrHoistRefreshPC(fn) {
		base += cellsPtrHoistRefreshWords(fn)
	}
	return base, nil
}

// wordCountARM64Body is the inner switch over op.Code. wordCountARM64
// wraps it to fold in the cells.ptr refresh prefix at refreshPC.
func wordCountARM64Body(fn *vm3.Function, op vm3.Op, opts Options, spillSets []uint32, idx int) (int, error) {
	switch op.Code {
	case vm3.OpConstI64K:
		// movImm64 emits 1..4 movz/movk words depending on the sign-extended
		// 16-bit constant. For C in [0, 65535] one movz; for negative values
		// a movn or up to 4 movz/movk pairs.
		return movImm64WordCount(int64(op.C)), nil
	case vm3.OpConstI64KW:
		idx := int(uint16(op.C))
		if idx >= len(fn.Consts) {
			return 0, fmt.Errorf("%w: ConstI64KW idx %d out of range", ErrNotImplemented, idx)
		}
		return movImm64WordCount(fn.Consts[idx].Int()), nil
	case vm3.OpMovI64:
		return 1, nil
	case vm3.OpAddI64, vm3.OpSubI64, vm3.OpMulI64:
		return 1, nil
	case vm3.OpDivI64:
		// CBZ xC, deopt ; SDIV xA, xB, xC.
		return 2, nil
	case vm3.OpModI64:
		// CBZ xC, deopt ; SDIV x17, xB, xC ; MSUB xA, x17, xC, xB.
		return 3, nil
	case vm3.OpNegI64:
		return 1, nil
	case vm3.OpAddI64K, vm3.OpSubI64K, vm3.OpMulI64K:
		// MOV imm into x16; <op> xd, xn, x16.
		return movImm64WordCount(int64(op.C)) + 1, nil
	case vm3.OpDivI64K, vm3.OpModI64K:
		if op.C == 0 {
			return 0, fmt.Errorf("%w: opcode %d divide-by-zero immediate", ErrNotImplemented, op.Code)
		}
		if op.Code == vm3.OpDivI64K {
			return movImm64WordCount(int64(op.C)) + 1, nil
		}
		// ModI64K: MOV imm into x16; SDIV x17, xb, x16; MSUB xa, x17, x16, xb.
		return movImm64WordCount(int64(op.C)) + 2, nil
	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br:
		// CMP xA, xB; B.cond <target>.
		return 2, nil
	case vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		// MOV imm into x16; CMP xA, x16; B.cond <target>.
		return movImm64WordCount(int64(int16(op.B))) + 2, nil
	case vm3.OpJump:
		return 1, nil
	case vm3.OpReturnI64:
		// [STR x23,[x20,#16]; STR w23,[x20,#4]] when cells.len pinned;
		// MOV x0, xA; <pop callee-saved frame>; <pop x29:x30 if non-leaf>; RET.
		return 2 + numCalleeSavedPairs(fn) + numLRPair(fn) + cellsLenFlushWords(fn), nil
	case vm3.OpReturnConstK:
		// [flush] movImm64 into x0; <pop callee-saved frame>; <pop x29:x30>; RET.
		return movImm64WordCount(int64(op.C)) + numCalleeSavedPairs(fn) + numLRPair(fn) + 1 + cellsLenFlushWords(fn), nil
	case vm3.OpReturnF64:
		// [flush] FMOV x0, D<retSlot>; <pop callee-saved frame>; <pop x29:x30>; RET.
		// Bit-cast the f64 in d<A> to a uint64 in x0 so the trampoline
		// return channel carries either bank uniformly.
		return 2 + numCalleeSavedPairs(fn) + numLRPair(fn) + cellsLenFlushWords(fn), nil

	case vm3.OpConstF64K:
		// MOV imm into x16 with the IEEE 754 bit-pattern, then
		// FMOV D<A>, x16 to bit-cast into the SIMD register. No
		// constant pool lookup at runtime; the bits are baked into
		// the JIT'd stream.
		idx := int(uint16(op.C))
		if idx >= len(fn.Consts) {
			return 0, fmt.Errorf("%w: ConstF64K idx %d out of range", ErrNotImplemented, idx)
		}
		bits := int64(uint64(fn.Consts[idx]))
		return movImm64WordCount(bits) + 1, nil

	case vm3.OpMovF64:
		return 1, nil
	case vm3.OpAddF64, vm3.OpSubF64, vm3.OpMulF64, vm3.OpDivF64:
		return 1, nil
	case vm3.OpNegF64:
		return 1, nil
	case vm3.OpFmaF64:
		return 1, nil
	case vm3.OpSqrtF64:
		return 1, nil

	case vm3.OpCmpEqF64Br, vm3.OpCmpNeF64Br,
		vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br,
		vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br:
		// FCMP Dn, Dm ; B.cond <target>.
		return 2, nil

	case vm3.OpI64ToF64:
		return 1, nil
	case vm3.OpF64ToI64:
		return 1, nil
	case vm3.OpLookupI64KW:
		// Hoisted form (Phase 6.4.b): 1 word (LDR Xd, [Xhoist, Xidx, LSL #3]).
		// Cold form: movImm64(x16, &fn.I64Tables[c][0]) + LDR.
		tableIdx := int(uint16(op.C))
		if tableIdx >= len(fn.I64Tables) || len(fn.I64Tables[tableIdx]) == 0 {
			return 0, fmt.Errorf("%w: LookupI64KW table %d out of range or empty",
				ErrNotImplemented, tableIdx)
		}
		if tableHoistRegARM64(fn, uint16(tableIdx)) != 0 {
			return 1, nil
		}
		base := int64(uintptr(unsafe.Pointer(&fn.I64Tables[tableIdx][0])))
		return movImm64WordCount(base) + 1, nil
	case vm3.OpCallI64:
		// Self-recursion only. Anything else routes back through the
		// interpreter via ErrNotImplemented.
		if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx {
			return 0, fmt.Errorf("%w: CallI64 to non-self idx %d (SelfIdx=%d)",
				ErrNotImplemented, uint16(op.C), opts.SelfIdx)
		}
		nSpill := popcount32(spillSets[idx])
		nArgs := fn.NumI64Params()
		// nSpill STRs + nArgs STRs + STP x0 + ADD x0 + BL + MOV x16,x0 +
		// LDP x0 + nSpill LDRs + MOV pinned[dst],x16.
		return 2*nSpill + nArgs + 6, nil

	case vm3.OpListGetI64:
		// Inline read-only list[i] -> i64 fast path. See emitInstrARM64
		// for the exact sequence; 7 words in the cold form (UXTW + MOV
		// stride + MUL + ADD + LDR cells + LDR cell + SBFX); 3 words
		// when hoistedCellReg matches op.B and x20 holds the cached
		// slab address (LDR cells + LDR cell + SBFX); 2 words when
		// cells.ptr is additionally pinned in x22 (Phase 6.2d.2.c.2),
		// collapsing to LDR cell + SBFX.
		if h := hoistedCellReg(fn); h >= 0 && int(op.B) == h {
			if hoistsCellsPtrARM64(fn) {
				return 2, nil
			}
			return 3, nil
		}
		stride := int64(vm3.JITListSlabStride())
		return movImm64WordCount(stride) + 6, nil

	case vm3.OpListPushI64:
		// Inline push-with-cap-check fast path. The cold form is
		// movImm64WordCount(stride)+14 words (UXTW + MOV stride + MUL +
		// ADD lane + 11-instr body). When hoistedCellReg(fn) matches
		// op.A the first 4 instructions collapse into the cached x20
		// slab base and the body shrinks to 11 words. When the slab-
		// field hoist also applies (Phase 6.2d.2.c.2, x21=cap, x22=ptr,
		// x23=len) the body shrinks to 6 words: CMP + B.HS + MOVZ +
		// BFI + STR + ADD; cells.len stays in x23 until Return* or
		// deopt flushes it. On cap exhaustion the inline B.HS branches
		// to the StatusListGrow deopt block.
		if h := hoistedCellReg(fn); h >= 0 && int(op.A) == h {
			if hoistsCellsLenARM64(fn) {
				return 6, nil
			}
			return 11, nil
		}
		stride := int64(vm3.JITListSlabStride())
		return movImm64WordCount(stride) + 14, nil

	case vm3.OpListSetI64:
		// Inline write-side fast path. No cap check (caller guarantees
		// index in-bounds via the algorithm; same as the interp which
		// has no bounds check either). Hottest form (x22 == cells.ptr):
		// 3 words (MOVZ tag + BFI payload + STR). Hot form (x20 ==
		// slab base, no cells.ptr pin): 4 words (LDR cells.ptr + MOVZ +
		// BFI + STR). Cold form: movImm64(stride)+7 words (UXTW + MOV
		// stride + MUL + ADD + LDR ptr + MOVZ + BFI + STR).
		if h := hoistedCellReg(fn); h >= 0 && int(op.A) == h {
			if hoistsCellsPtrARM64(fn) {
				return 3, nil
			}
			return 4, nil
		}
		stride := int64(vm3.JITListSlabStride())
		return movImm64WordCount(stride) + 7, nil

	case vm3.OpListGetF64:
		// Inline f64-typed list load. CFloat stores raw IEEE 754 bits
		// so there is no sign-extend step. Cold form (6 inst):
		//   UXTW x16, w_cell ; MOV x17, #stride ; MUL ; ADD x16, x19 ;
		//   LDR x16, [x16, #CELLS_OFFSET] ; LDR Dt, [x16, xIdx, LSL #3]
		// Hot form (Phase 6.3.4.j.4a, x_cell already holds cells.ptr
		// post-refresh): 1 inst (LDR Dt, [x_cell, xIdx, LSL #3]).
		if cellsPtrHoistedAt(fn, idx, op.B) {
			return 1, nil
		}
		stride := int64(vm3.JITListSlabStride())
		return movImm64WordCount(stride) + 5, nil

	case vm3.OpListSetF64:
		// Inline f64-typed list store. CFloat is unbox-free so no
		// MOVZ tag / BFI pack step is needed. Cold form (6 inst):
		//   UXTW x16, w_cell ; MOV x17, #stride ; MUL ; ADD x16, x19 ;
		//   LDR x17, [x16, #CELLS_OFFSET] ; STR Dt, [x17, xIdx, LSL #3]
		// Hot form (Phase 6.3.4.j.4a, x_cell already holds cells.ptr
		// post-refresh): 1 inst (STR Dt, [x_cell, xIdx, LSL #3]).
		if cellsPtrHoistedAt(fn, idx, op.A) {
			return 1, nil
		}
		stride := int64(vm3.JITListSlabStride())
		return movImm64WordCount(stride) + 5, nil

	case vm3.OpMapSetI64I64:
		// Phase 6.2d.2.d step 4: inline open-addressed map set with a
		// load-factor 0.5 grow check that deopts via StatusMapGrow.
		// Only admitted when the map handle lives in the hoisted Cell
		// reg (regsCell[0]) so the slab byte address is pre-pinned in
		// x20. The kernel additionally uses x13/x14/x15 as scratch +
		// loop-invariant pins; gate on NumRegsI64 <= 4 so r2x never
		// emits those host regs for vm3 i64 regs. See
		// mapSetI64I64WordsARM64 for the body shape.
		if h := hoistedCellReg(fn); h < 0 || int(op.A) != h || slabKindARM64(fn) != slabKindMap || fn.NumRegsI64 > 4 {
			return 0, fmt.Errorf("%w: opcode %d (MapSetI64I64 without map-slab hoist or NumRegsI64>4)", ErrNotImplemented, op.Code)
		}
		return mapSetI64I64WordsARM64, nil

	case vm3.OpMapGetI64I64:
		// Phase 6.2d.2.d step 4 mirror of OpMapSetI64I64: inline open-
		// addressed lookup with empty-table early-return. Same hoist
		// gate as MapSet plus the NumRegsI64 <= 4 ceiling so x13..x15
		// stay free for the probe loop.
		if h := hoistedCellReg(fn); h < 0 || int(op.B) != h || slabKindARM64(fn) != slabKindMap || fn.NumRegsI64 > 4 {
			return 0, fmt.Errorf("%w: opcode %d (MapGetI64I64 without map-slab hoist or NumRegsI64>4)", ErrNotImplemented, op.Code)
		}
		return mapGetI64I64WordsARM64, nil

	case vm3.OpTailCallMixed:
		// Self-tail-call with arg-base 0 lowers to a single backward B
		// (interp matches with `pc = 0; continue`). The args already live
		// in their pinned regs by construction. Anything else (different
		// callee or B != 0) routes back through the interpreter.
		if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx || op.B != 0 {
			return 0, fmt.Errorf("%w: TailCallMixed to non-self idx %d or non-zero argBase (SelfIdx=%d, B=%d)",
				ErrNotImplemented, uint16(op.C), opts.SelfIdx, op.B)
		}
		return 1, nil

	case vm3.OpCallMixed:
		callee, err := resolveCrossFnCallee(opts, op)
		if err != nil {
			return 0, err
		}
		return crossFnCallMixedWordsARM64(fn, callee, spillSets[idx]), nil

	case vm3.OpNewList:
		// Phase 6.2d.2.b step 2: when fn.JITPreAllocList is set the JIT
		// skips fn.Code[0]'s OpNewList entirely; jitCall (Go side) writes
		// the pre-allocated list handle into jf.regsCell[A] before the
		// trampoline call, and the prologue's LDR x_cell, [x3, #A*8]
		// picks it up into the pinned reg.
		//
		// Phase 6.3.4.j.3 extends the skip to a K-op prefix of inline
		// OpNewLists; jitCall pre-allocates K lists and seeds each cell.
		if idx == 0 && fn.JITPreAllocList {
			return 0, nil
		}
		if idx < int(fn.JITPreAllocListPrefix) {
			return 0, nil
		}
		return 0, fmt.Errorf("%w: opcode %d (inline NewList unsupported)", ErrNotImplemented, op.Code)

	default:
		return 0, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)
	}
}

// emitInstrARM64 emits the AArch64 instructions for op at bytecode
// index idx. pcMap[i] is the word offset where instruction i's
// lowering begins (and pcMap[len(Code)] is the word offset of the
// fall-through past the last instruction; that is also the deopt
// block start when one is emitted). deoptStart is supplied separately
// so guard-checked opcodes (Div/Mod) can compute CBZ offsets without
// re-deriving it.
func emitInstrARM64(fn *vm3.Function, op vm3.Op, idx int, pcMap []int, deoptStart int, opts Options, spillSets []uint32) ([]uint32, error) {
	body, err := emitInstrARM64Body(fn, op, idx, pcMap, deoptStart, opts, spillSets)
	if err != nil {
		return nil, err
	}
	if idx != cellsPtrHoistRefreshPC(fn) {
		return body, nil
	}
	refresh := emitCellsPtrRefreshARM64(nil, fn)
	if len(refresh) == 0 {
		return body, nil
	}
	return append(refresh, body...), nil
}

// emitInstrARM64Body is the inner emit dispatch. emitInstrARM64 wraps
// it to prepend the cells.ptr refresh sequence at refreshPC.
func emitInstrARM64Body(fn *vm3.Function, op vm3.Op, idx int, pcMap []int, deoptStart int, opts Options, spillSets []uint32) ([]uint32, error) {
	xA := r2x(fn, op.A)
	xB := r2x(fn, op.B)

	switch op.Code {
	case vm3.OpConstI64K:
		return movImm64(xA, int64(op.C)), nil

	case vm3.OpConstI64KW:
		v := fn.Consts[int(uint16(op.C))].Int()
		return movImm64(xA, v), nil

	case vm3.OpMovI64:
		return []uint32{movReg(xA, xB)}, nil

	case vm3.OpAddI64:
		xC := r2x(fn, uint16(op.C))
		return []uint32{addReg(xA, xB, xC)}, nil
	case vm3.OpSubI64:
		xC := r2x(fn, uint16(op.C))
		return []uint32{subReg(xA, xB, xC)}, nil
	case vm3.OpMulI64:
		xC := r2x(fn, uint16(op.C))
		return []uint32{mulReg(xA, xB, xC)}, nil
	case vm3.OpDivI64:
		xC := r2x(fn, uint16(op.C))
		dz := deoptStartForStatus(fn, deoptStart, StatusDivByZero)
		off, err := branchOff(pcMap[idx], dz, 19)
		if err != nil {
			return nil, fmt.Errorf("DivI64 deopt branch: %w", err)
		}
		return []uint32{cbz64(xC, off), sdivReg(xA, xB, xC)}, nil
	case vm3.OpModI64:
		xC := r2x(fn, uint16(op.C))
		dz := deoptStartForStatus(fn, deoptStart, StatusDivByZero)
		off, err := branchOff(pcMap[idx], dz, 19)
		if err != nil {
			return nil, fmt.Errorf("ModI64 deopt branch: %w", err)
		}
		return []uint32{
			cbz64(xC, off),
			sdivReg(17, xB, xC),
			msubReg(xA, 17, xC, xB),
		}, nil
	case vm3.OpNegI64:
		return []uint32{negReg(xA, xB)}, nil

	case vm3.OpAddI64K:
		ws := movImm64(16, int64(op.C))
		return append(ws, addReg(xA, xB, 16)), nil
	case vm3.OpSubI64K:
		ws := movImm64(16, int64(op.C))
		return append(ws, subReg(xA, xB, 16)), nil
	case vm3.OpMulI64K:
		ws := movImm64(16, int64(op.C))
		return append(ws, mulReg(xA, xB, 16)), nil
	case vm3.OpDivI64K:
		ws := movImm64(16, int64(op.C))
		return append(ws, sdivReg(xA, xB, 16)), nil
	case vm3.OpModI64K:
		// x17 = xB / x16 ; xA = xB - x17 * x16.
		ws := movImm64(16, int64(op.C))
		ws = append(ws, sdivReg(17, xB, 16))
		ws = append(ws, msubReg(xA, 17, 16, xB))
		return ws, nil

	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br:
		cond := condForCmpReg(op.Code)
		dstWord := pcMap[int(uint16(op.C))]
		srcWord := pcMap[idx] + 1
		off, err := branchOff(srcWord, dstWord, 19)
		if err != nil {
			return nil, err
		}
		return []uint32{cmpReg(xA, xB), bCond(cond, off)}, nil

	case vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		cond := condForCmpKImm(op.Code)
		imm := int64(int16(op.B))
		ws := movImm64(16, imm)
		cmpWord := pcMap[idx] + len(ws)
		bWord := cmpWord + 1
		dstWord := pcMap[int(uint16(op.C))]
		off, err := branchOff(bWord, dstWord, 19)
		if err != nil {
			return nil, err
		}
		ws = append(ws, cmpReg(xA, 16), bCond(cond, off))
		return ws, nil

	case vm3.OpJump:
		dstWord := pcMap[int(uint16(op.C))]
		srcWord := pcMap[idx]
		off, err := branchOff(srcWord, dstWord, 26)
		if err != nil {
			return nil, err
		}
		return []uint32{bImm(off)}, nil

	case vm3.OpReturnI64:
		// MOV x0, xA BEFORE the LDPs: if xA is one of x19..x28 the
		// epilogue would clobber it.
		ws := make([]uint32, 0, 2+numCalleeSavedPairs(fn)+numLRPair(fn)+cellsLenFlushWords(fn))
		if hoistsCellsLenARM64(fn) {
			ws = emitCellsLenFlushARM64(ws)
		}
		ws = append(ws, movReg(0, xA))
		ws = emitFrameEpilogueARM64(ws, calleeSavedPairRegs(fn), numLRPair(fn))
		ws = append(ws, ret())
		return ws, nil

	case vm3.OpReturnConstK:
		ws := make([]uint32, 0, movImm64WordCount(int64(op.C))+numCalleeSavedPairs(fn)+numLRPair(fn)+1+cellsLenFlushWords(fn))
		if hoistsCellsLenARM64(fn) {
			ws = emitCellsLenFlushARM64(ws)
		}
		ws = append(ws, movImm64(0, int64(op.C))...)
		ws = emitFrameEpilogueARM64(ws, calleeSavedPairRegs(fn), numLRPair(fn))
		ws = append(ws, ret())
		return ws, nil

	case vm3.OpReturnF64:
		// FMOV x0, D<A>: bit-cast the f64 result to a uint64 in x0
		// so the trampoline returns the bit pattern. Caller does
		// math.Float64frombits to recover the f64. Then run the
		// epilogue exactly like OpReturnI64.
		ws := make([]uint32, 0, 2+numCalleeSavedPairs(fn)+numLRPair(fn)+cellsLenFlushWords(fn))
		if hoistsCellsLenARM64(fn) {
			ws = emitCellsLenFlushARM64(ws)
		}
		ws = append(ws, fmovXD(0, r2d(op.A)))
		ws = emitFrameEpilogueARM64(ws, calleeSavedPairRegs(fn), numLRPair(fn))
		ws = append(ws, ret())
		return ws, nil

	case vm3.OpConstF64K:
		dA := r2d(op.A)
		bits := int64(uint64(fn.Consts[int(uint16(op.C))]))
		ws := movImm64(16, bits)
		ws = append(ws, fmovDX(dA, 16))
		return ws, nil

	case vm3.OpMovF64:
		return []uint32{fmovDD(r2d(op.A), r2d(op.B))}, nil
	case vm3.OpAddF64:
		return []uint32{faddD(r2d(op.A), r2d(op.B), r2d(uint16(op.C)))}, nil
	case vm3.OpSubF64:
		return []uint32{fsubD(r2d(op.A), r2d(op.B), r2d(uint16(op.C)))}, nil
	case vm3.OpMulF64:
		return []uint32{fmulD(r2d(op.A), r2d(op.B), r2d(uint16(op.C)))}, nil
	case vm3.OpDivF64:
		return []uint32{fdivD(r2d(op.A), r2d(op.B), r2d(uint16(op.C)))}, nil
	case vm3.OpNegF64:
		return []uint32{fnegD(r2d(op.A), r2d(op.B))}, nil
	case vm3.OpFmaF64:
		mul2 := uint16(op.C) & 0xFF
		addend := (uint16(op.C) >> 8) & 0xFF
		return []uint32{fmaddD(r2d(op.A), r2d(op.B), r2d(mul2), r2d(addend))}, nil
	case vm3.OpSqrtF64:
		return []uint32{fsqrtD(r2d(op.A), r2d(op.B))}, nil

	case vm3.OpCmpEqF64Br, vm3.OpCmpNeF64Br,
		vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br,
		vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br:
		cond := condForCmpF64(op.Code)
		dstWord := pcMap[int(uint16(op.C))]
		srcWord := pcMap[idx] + 1
		off, err := branchOff(srcWord, dstWord, 19)
		if err != nil {
			return nil, err
		}
		return []uint32{fcmpD(r2d(op.A), r2d(op.B)), bCond(cond, off)}, nil

	case vm3.OpI64ToF64:
		return []uint32{scvtfDX(r2d(op.A), r2x(fn, op.B))}, nil
	case vm3.OpF64ToI64:
		return []uint32{fcvtzsXD(r2x(fn, op.A), r2d(op.B))}, nil

	case vm3.OpLookupI64KW:
		// regsI64[A] = fn.I64Tables[uint16(C)][regsI64[B]]
		//
		// The table is a Go-owned []int64 that lives as long as the
		// Function record. Caller must have emitted a prior OpCmpGeI64KBr
		// bounds check against len(table); the load itself is unchecked
		// (mirrors the interpreter's `fn.I64Tables[c][regs[B]]`).
		//
		// Hoisted form (Phase 6.4.b): the prologue pinned &I64Tables[c][0]
		// in a callee-saved register. One indexed load:
		//   LDR Xd, [Xhoist, Xidx, LSL #3]
		//
		// Cold form (no hoist budget left): emit the cold pair:
		//   movImm64(x16, &fn.I64Tables[c][0])    ; 1..4 movz/movk words
		//   LDR Xd, [x16, Xidx, LSL #3]
		tableIdx := int(uint16(op.C))
		if tableIdx >= len(fn.I64Tables) || len(fn.I64Tables[tableIdx]) == 0 {
			return nil, fmt.Errorf("%w: LookupI64KW table %d out of range or empty",
				ErrNotImplemented, tableIdx)
		}
		xDst := r2x(fn, op.A)
		xIdx := r2x(fn, op.B)
		if hoist := tableHoistRegARM64(fn, uint16(tableIdx)); hoist != 0 {
			return []uint32{ldrRegLsl3(xDst, hoist, xIdx)}, nil
		}
		base := int64(uintptr(unsafe.Pointer(&fn.I64Tables[tableIdx][0])))
		ws := movImm64(16, base)
		ws = append(ws, ldrRegLsl3(xDst, 16, xIdx))
		return ws, nil

	case vm3.OpCallI64:
		if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx {
			return nil, fmt.Errorf("%w: CallI64 to non-self idx %d (SelfIdx=%d)",
				ErrNotImplemented, uint16(op.C), opts.SelfIdx)
		}
		spillMask := spillSets[idx]
		nSpill := popcount32(spillMask)
		nArgs := fn.NumI64Params()
		nRegsI64 := int(fn.NumRegsI64)
		ws := make([]uint32, 0, 2*nSpill+nArgs+6)
		// Spill only the caller-saved pinned regs that are live across
		// this call, computed by backward liveness in computeCallSpills.
		for r := uint16(0); r < 7; r++ {
			if spillMask&(1<<r) != 0 {
				ws = append(ws, str64(uint32(9+r), 0, uint32(r)))
			}
		}
		// Write args into callee's window slots at offset NumRegsI64*8.
		for k := 0; k < nArgs; k++ {
			src := r2x(fn, op.B+uint16(k))
			ws = append(ws, str64(src, 0, uint32(nRegsI64+k)))
		}
		// Save caller's x0 (regs base), bump to callee window, BL.
		ws = append(ws, stpPreIdx64(0, 31, 31, -16))
		ws = append(ws, addImm64(0, 0, uint32(nRegsI64*8)))
		entryWord := 0
		callSiteWord := pcMap[idx] + len(ws)
		off, err := branchOff(callSiteWord, entryWord, 26)
		if err != nil {
			return nil, fmt.Errorf("CallI64 BL: %w", err)
		}
		ws = append(ws, bl(off))
		// Capture result into x16, restore caller's x0.
		ws = append(ws, movReg(16, 0))
		ws = append(ws, ldpPostIdx64(0, 31, 31, 16))
		// Reload only the regs we spilled.
		for r := uint16(0); r < 7; r++ {
			if spillMask&(1<<r) != 0 {
				ws = append(ws, ldr64(uint32(9+r), 0, uint32(r)))
			}
		}
		// Move result into caller's pinned dst register.
		ws = append(ws, movReg(xA, 16))
		return ws, nil

	case vm3.OpCallMixed:
		callee, err := resolveCrossFnCallee(opts, op)
		if err != nil {
			return nil, err
		}
		spillMask := spillSets[idx]
		callerI64 := int(fn.NumRegsI64)
		callerF64 := int(fn.NumRegsF64)
		callerCell := int(fn.NumRegsCell)
		needsDeoptCheck := crossFnDeoptCallee(callee)
		ws := make([]uint32, 0, crossFnCallMixedWordsARM64(fn, callee, spillMask))
		for r := uint16(0); r < 7; r++ {
			if spillMask&(1<<r) != 0 {
				ws = append(ws, str64(uint32(9+r), 0, uint32(r)))
			}
		}
		for k, b := range callee.ParamBanks {
			argReg := op.B + uint16(k)
			switch b {
			case vm3.BankI64:
				ws = append(ws, str64(r2x(fn, argReg), 0, uint32(callerI64+k)))
			case vm3.BankF64:
				ws = append(ws, strD(r2d(argReg), 2, uint32(callerF64+k)))
			case vm3.BankCell:
				ws = append(ws, str64(r2cell(argReg), 3, uint32(callerCell+k)))
			}
		}
		ws = append(ws, stpPreIdx64(0, 2, 31, -16))
		ws = append(ws, stpPreIdx64(3, 31, 31, -16))
		if callerI64 > 0 {
			ws = append(ws, addImm64(0, 0, uint32(callerI64*8)))
		}
		if callerF64 > 0 {
			ws = append(ws, addImm64(2, 2, uint32(callerF64*8)))
		}
		if callerCell > 0 {
			ws = append(ws, addImm64(3, 3, uint32(callerCell*8)))
		}
		ws = append(ws, movReg(4, 20))
		ws = append(ws, movImm64(16, int64(uintptr(callee.JITCode)))...)
		ws = append(ws, blr(16))
		ws = append(ws, movReg(17, 0))
		if needsDeoptCheck {
			// Load *(x1) into x16 BEFORE the LDPs restore SP, while x1
			// still points at the trampoline's status word (the callee
			// preserves x1 across its body by convention). Phase
			// 6.2d.2.b step 2: deferred CBNZ comes after the LDR-reload
			// run below so SP and caller bases are at prologue-push
			// level when the branch is taken; the passthrough block
			// runs the same frame epilogue as a normal deopt block.
			ws = append(ws, ldr64(16, 1, 0))
		}
		ws = append(ws, ldpPostIdx64(3, 31, 31, 16))
		ws = append(ws, ldpPostIdx64(0, 2, 31, 16))
		for r := uint16(0); r < 7; r++ {
			if spillMask&(1<<r) != 0 {
				ws = append(ws, ldr64(uint32(9+r), 0, uint32(r)))
			}
		}
		if needsDeoptCheck {
			ptStart := passthroughStartARM64(fn, deoptStart)
			srcWord := pcMap[idx] + len(ws)
			off, err := branchOff(srcWord, ptStart, 19)
			if err != nil {
				return nil, fmt.Errorf("CallMixed deopt passthrough branch: %w", err)
			}
			ws = append(ws, cbnz64(16, off))
		}
		ws = append(ws, movReg(xA, 17))
		return ws, nil

	case vm3.OpNewList:
		if idx == 0 && fn.JITPreAllocList {
			// Skipped: jitCall pre-allocated the list and wrote the
			// handle to regsCell[A]; the prologue loaded it into the
			// pinned reg before this PC. Emit zero words.
			return []uint32{}, nil
		}
		if idx < int(fn.JITPreAllocListPrefix) {
			// Phase 6.3.4.j.3 multi-list prefix: same skip rationale,
			// jitCall pre-seeded K cells before the trampoline.
			return []uint32{}, nil
		}
		return nil, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)

	case vm3.OpListGetI64:
		// regsI64[A] = arenas.Lists[handleIdx(regsCell[B])].cells[regsI64[C]].Int()
		//
		// Cold form (per-op slab recompute, when no hoist applies):
		//   UXTW x16, w_cell                  ; idx = handle & 0xFFFFFFFF
		//   MOV  x17, #SIZEOF_VMLIST           ; stride (baked from runtime layout)
		//   MUL  x16, x16, x17                 ; slab byte offset
		//   ADD  x16, x16, x19                 ; x19 = cached arenas.Lists base
		//   LDR  x16, [x16, #CELLS_OFFSET]     ; cells slice data ptr (head of header)
		//   LDR  x17, [x16, xIdx, lsl #3]      ; cells[idxReg]
		//   SBFX xA, x17, #0, #48              ; sign-extend the 48-bit Int payload
		//
		// Hot form (hoistedCellReg matches op.B, x20 == &arenas.Lists[idx]):
		//   LDR  x17, [x20, #CELLS_OFFSET]     ; cells.ptr
		//   LDR  x17, [x17, xIdx, lsl #3]      ; cells[idxReg]
		//   SBFX xA, x17, #0, #48              ; sign-extend payload
		//
		// Hottest form (Phase 6.2d.2.c.2, x22 == cells.ptr):
		//   LDR  x17, [x22, xIdx, lsl #3]      ; cells[idxReg]
		//   SBFX xA, x17, #0, #48              ; sign-extend payload
		cellsOff := uint32(vm3.JITListCellsOffset())
		xIdx := r2x(fn, uint16(op.C))
		if h := hoistedCellReg(fn); h >= 0 && int(op.B) == h {
			if hoistsCellsPtrARM64(fn) {
				ws := make([]uint32, 0, 2)
				ws = append(ws, ldrRegLsl3(17, 22, xIdx))
				ws = append(ws, sbfx48(xA, 17))
				return ws, nil
			}
			ws := make([]uint32, 0, 3)
			ws = append(ws, ldr64(17, 20, cellsOff/8))
			ws = append(ws, ldrRegLsl3(17, 17, xIdx))
			ws = append(ws, sbfx48(xA, 17))
			return ws, nil
		}
		stride := int64(vm3.JITListSlabStride())
		xCell := r2cell(op.B)
		ws := make([]uint32, 0, movImm64WordCount(stride)+6)
		ws = append(ws, uxtwReg(16, xCell))
		ws = append(ws, movImm64(17, stride)...)
		ws = append(ws, mulReg(16, 16, 17))
		ws = append(ws, addReg(16, 16, 19))
		ws = append(ws, ldr64(16, 16, cellsOff/8))
		ws = append(ws, ldrRegLsl3(17, 16, xIdx))
		ws = append(ws, sbfx48(xA, 17))
		return ws, nil

	case vm3.OpMapSetI64I64:
		// Phase 6.2d.2.d step 4: inline open-addressed map set on the
		// hoisted Cell reg (x20 = &arenas.Maps[idx]).
		//
		//   pre-amble (7 words):
		//     LDR  x4,  [x20, #tableLen]        ; cap (table.len)
		//     LDR  W16, [x20, #nLive]           ; current nLive
		//     ADD  x16, x16, #1                 ; nLive+1
		//     CMP  x4,  x16, LSL #1             ; cap vs 2*(nLive+1)
		//     B.LO deopt_mapgrow                ; load factor >= 0.5
		//     SUB  x14, x4,  #1                 ; mask = cap - 1
		//     MOV  x15, #SIZEOF_MAPENTRY        ; probe stride (24)
		//   splitmix64 → x4 = h (14 words)
		//   AND  x17, x4, x14                   ; pos = h & mask
		//   probe_top:
		//     LDR  x13, [x20, #tablePtr]
		//     MADD x16, x17, x15, x13           ; entry_addr
		//     LDR  x13, [x16, #entry.hash]
		//     CBZ  x13, fill_block              ; empty slot
		//     CMP  x13, x4                       ; hash match?
		//     B.NE next
		//     LDR  x13, [x16, #entry.key]       ; e.key Cell
		//     SBFX x13, x13, #0, #48            ; unbox int48
		//     CMP  x13, xKey                     ; key match?
		//     B.NE next
		//     MOVZ x13, #0xFFFA, LSL #48        ; tag
		//     BFI  x13, xVal, #0, #48           ; box value
		//     STR  x13, [x16, #entry.value]
		//     B    done
		//   next: ADD pos+1; AND pos&mask; B probe_top
		//   fill_block:
		//     STR  x4,  [x16, #entry.hash]      ; e.hash = h
		//     MOVZ x13, #0xFFFA, LSL #48        ; tag
		//     BFI  x13, xKey, #0, #48           ; box key
		//     STR  x13, [x16, #entry.key]
		//     BFI  x13, xVal, #0, #48           ; replace low48 with val (tag preserved)
		//     STR  x13, [x16, #entry.value]
		//     LDR  W13, [x20, #nLive]           ; reload nLive
		//     ADD  x13, x13, #1                 ; nLive+1
		//     STR  W13, [x20, #nLive]           ; commit
		xKey := r2x(fn, op.B)
		xVal := r2x(fn, uint16(op.C))
		mgStart := deoptStartForStatus(fn, deoptStart, StatusMapGrow)
		tableOff := uint32(vm3.JITMapTableOffset())
		tablePtrImm12 := tableOff / 8
		tableLenImm12 := (tableOff + 8) / 8
		entryHashImm12 := uint32(vm3.JITMapEntryHashOffset()) / 8
		entryKeyImm12 := uint32(vm3.JITMapEntryKeyOffset()) / 8
		entryValImm12 := uint32(vm3.JITMapEntryValueOffset()) / 8
		nLiveImm12 := uint32(vm3.JITMapNLiveOffset()) / 4
		stride := uint32(vm3.JITMapEntryStride())

		opStart := pcMap[idx]
		endLabel := pcMap[idx+1]

		ws := make([]uint32, 0, mapSetI64I64WordsARM64)
		// 0..6: pre-amble.
		ws = append(ws, ldr64(4, 20, tableLenImm12))      // 0
		ws = append(ws, ldrW(16, 20, nLiveImm12))         // 1
		ws = append(ws, addImm64(16, 16, 1))              // 2
		ws = append(ws, cmpShiftLSL(4, 16, 1))            // 3
		bWord := opStart + len(ws)                        // 4
		off, err := branchOff(bWord, mgStart, 19)
		if err != nil {
			return nil, fmt.Errorf("MapSetI64I64 grow deopt: %w", err)
		}
		ws = append(ws, bCond(0x3, off))                  // 4: B.LO
		ws = append(ws, subImm64(14, 4, 1))               // 5: SUB mask
		ws = append(ws, movz(15, stride, 0))              // 6: MOV stride

		// 7..20: splitmix64 → x4 = h.
		ws = append(ws, emitSplitmix64ARM64(xKey)...)

		// 21: AND pos.
		ws = append(ws, andReg(17, 4, 14))

		// 22..35: probe loop body.
		probeTopWord := opStart + len(ws)                 // 22
		fillBlockWord := opStart + 39
		nextWord := opStart + 36

		ws = append(ws, ldr64(13, 20, tablePtrImm12))     // 22
		ws = append(ws, maddReg(16, 17, 15, 13))          // 23
		ws = append(ws, ldr64(13, 16, entryHashImm12))    // 24
		cbzWord := opStart + len(ws)                      // 25
		off, err = branchOff(cbzWord, fillBlockWord, 19)
		if err != nil {
			return nil, fmt.Errorf("MapSetI64I64 fill CBZ: %w", err)
		}
		ws = append(ws, cbz64(13, off))                   // 25
		ws = append(ws, cmpReg(13, 4))                    // 26
		bneWord := opStart + len(ws)                      // 27
		off, err = branchOff(bneWord, nextWord, 19)
		if err != nil {
			return nil, fmt.Errorf("MapSetI64I64 hash mismatch: %w", err)
		}
		ws = append(ws, bCond(0x1, off))                  // 27: B.NE
		ws = append(ws, ldr64(13, 16, entryKeyImm12))     // 28
		ws = append(ws, sbfx48(13, 13))                   // 29
		ws = append(ws, cmpReg(13, xKey))                 // 30
		bneWord = opStart + len(ws)                       // 31
		off, err = branchOff(bneWord, nextWord, 19)
		if err != nil {
			return nil, fmt.Errorf("MapSetI64I64 key mismatch: %w", err)
		}
		ws = append(ws, bCond(0x1, off))                  // 31: B.NE
		ws = append(ws, movz(13, 0xFFFA, 3))              // 32
		ws = append(ws, bfi48(13, xVal))                  // 33
		ws = append(ws, str64(13, 16, entryValImm12))     // 34
		bWord = opStart + len(ws)                         // 35
		off, err = branchOff(bWord, endLabel, 26)
		if err != nil {
			return nil, fmt.Errorf("MapSetI64I64 done B: %w", err)
		}
		ws = append(ws, bImm(off))                        // 35: B done

		// 36..38: next.
		ws = append(ws, addImm64(17, 17, 1))              // 36
		ws = append(ws, andReg(17, 17, 14))               // 37
		bWord = opStart + len(ws)                         // 38
		off, err = branchOff(bWord, probeTopWord, 26)
		if err != nil {
			return nil, fmt.Errorf("MapSetI64I64 probe back: %w", err)
		}
		ws = append(ws, bImm(off))                        // 38: B probe_top

		// 39..47: fill_block.
		ws = append(ws, str64(4, 16, entryHashImm12))     // 39
		ws = append(ws, movz(13, 0xFFFA, 3))              // 40
		ws = append(ws, bfi48(13, xKey))                  // 41
		ws = append(ws, str64(13, 16, entryKeyImm12))     // 42
		ws = append(ws, bfi48(13, xVal))                  // 43: replace low48 of x13 with val, tag in bits 48..63 preserved
		ws = append(ws, str64(13, 16, entryValImm12))     // 44
		ws = append(ws, ldrW(13, 20, nLiveImm12))         // 45
		ws = append(ws, addImm64(13, 13, 1))              // 46
		ws = append(ws, strW(13, 20, nLiveImm12))         // 47
		return ws, nil

	case vm3.OpMapGetI64I64:
		// Phase 6.2d.2.d step 4 mirror of OpMapSetI64I64: inline open-
		// addressed lookup. xA receives e.value.Int() on hit, or 0 on
		// empty-table / miss.
		//
		//   pre-amble (4 words):
		//     LDR  x4,  [x20, #tableLen]        ; cap
		//     CBZ  x4,  miss                    ; empty table → 0
		//     SUB  x14, x4, #1                  ; mask
		//     MOV  x15, #SIZEOF_MAPENTRY
		//   splitmix64 → x4 = h (14 words)
		//   AND  x17, x4, x14                   ; pos
		//   probe_top:
		//     LDR  x13, [x20, #tablePtr]
		//     MADD x16, x17, x15, x13
		//     LDR  x13, [x16, #entry.hash]
		//     CBZ  x13, miss                    ; empty entry → 0
		//     CMP  x13, x4
		//     B.NE next
		//     LDR  x13, [x16, #entry.key]
		//     SBFX x13, x13, #0, #48
		//     CMP  x13, xKey
		//     B.NE next
		//     LDR  x13, [x16, #entry.value]
		//     SBFX xA,  x13, #0, #48            ; result
		//     B    done
		//   next: ADD pos+1; AND mask; B probe_top
		//   miss: MOVZ xA, #0                   ; xA = 0
		xKey := r2x(fn, uint16(op.C))
		tableOff := uint32(vm3.JITMapTableOffset())
		tablePtrImm12 := tableOff / 8
		tableLenImm12 := (tableOff + 8) / 8
		entryHashImm12 := uint32(vm3.JITMapEntryHashOffset()) / 8
		entryKeyImm12 := uint32(vm3.JITMapEntryKeyOffset()) / 8
		entryValImm12 := uint32(vm3.JITMapEntryValueOffset()) / 8
		stride := uint32(vm3.JITMapEntryStride())

		opStart := pcMap[idx]
		endLabel := pcMap[idx+1]
		missWord := opStart + 35
		nextWord := opStart + 32

		ws := make([]uint32, 0, mapGetI64I64WordsARM64)
		// 0..3: pre-amble.
		ws = append(ws, ldr64(4, 20, tableLenImm12))      // 0
		cbzWord := opStart + len(ws)                      // 1
		off, err := branchOff(cbzWord, missWord, 19)
		if err != nil {
			return nil, fmt.Errorf("MapGetI64I64 empty CBZ: %w", err)
		}
		ws = append(ws, cbz64(4, off))                    // 1
		ws = append(ws, subImm64(14, 4, 1))               // 2
		ws = append(ws, movz(15, stride, 0))              // 3

		// 4..17: splitmix64 → x4 = h.
		ws = append(ws, emitSplitmix64ARM64(xKey)...)

		// 18: AND pos.
		ws = append(ws, andReg(17, 4, 14))

		// 19..31: probe loop body.
		probeTopWord := opStart + len(ws)                 // 19

		ws = append(ws, ldr64(13, 20, tablePtrImm12))     // 19
		ws = append(ws, maddReg(16, 17, 15, 13))          // 20
		ws = append(ws, ldr64(13, 16, entryHashImm12))    // 21
		cbzWord = opStart + len(ws)                       // 22
		off, err = branchOff(cbzWord, missWord, 19)
		if err != nil {
			return nil, fmt.Errorf("MapGetI64I64 miss CBZ: %w", err)
		}
		ws = append(ws, cbz64(13, off))                   // 22
		ws = append(ws, cmpReg(13, 4))                    // 23
		bneWord := opStart + len(ws)                      // 24
		off, err = branchOff(bneWord, nextWord, 19)
		if err != nil {
			return nil, fmt.Errorf("MapGetI64I64 hash mismatch: %w", err)
		}
		ws = append(ws, bCond(0x1, off))                  // 24: B.NE
		ws = append(ws, ldr64(13, 16, entryKeyImm12))     // 25
		ws = append(ws, sbfx48(13, 13))                   // 26
		ws = append(ws, cmpReg(13, xKey))                 // 27
		bneWord = opStart + len(ws)                       // 28
		off, err = branchOff(bneWord, nextWord, 19)
		if err != nil {
			return nil, fmt.Errorf("MapGetI64I64 key mismatch: %w", err)
		}
		ws = append(ws, bCond(0x1, off))                  // 28: B.NE
		ws = append(ws, ldr64(13, 16, entryValImm12))     // 29
		ws = append(ws, sbfx48(xA, 13))                   // 30: xA = value.Int()
		bWord := opStart + len(ws)                        // 31
		off, err = branchOff(bWord, endLabel, 26)
		if err != nil {
			return nil, fmt.Errorf("MapGetI64I64 done B: %w", err)
		}
		ws = append(ws, bImm(off))                        // 31: B done

		// 32..34: next.
		ws = append(ws, addImm64(17, 17, 1))              // 32
		ws = append(ws, andReg(17, 17, 14))               // 33
		bWord = opStart + len(ws)                         // 34
		off, err = branchOff(bWord, probeTopWord, 26)
		if err != nil {
			return nil, fmt.Errorf("MapGetI64I64 probe back: %w", err)
		}
		ws = append(ws, bImm(off))                        // 34: B probe_top

		// 35: miss → xA = 0.
		ws = append(ws, movz(xA, 0, 0))                   // 35
		return ws, nil

	case vm3.OpTailCallMixed:
		if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx || op.B != 0 {
			return nil, fmt.Errorf("%w: TailCallMixed to non-self idx %d or non-zero argBase (SelfIdx=%d, B=%d)",
				ErrNotImplemented, uint16(op.C), opts.SelfIdx, op.B)
		}
		dstWord := pcMap[0]
		srcWord := pcMap[idx]
		off, err := branchOff(srcWord, dstWord, 26)
		if err != nil {
			return nil, fmt.Errorf("TailCallMixed self-tail branch: %w", err)
		}
		return []uint32{bImm(off)}, nil

	case vm3.OpListPushI64:
		// regsCell[A] holds the list handle, regsI64[B] the i64 to push.
		// Inline write-side fast path mirrors the interp's
		// "append-and-bump" with one extra cap check so a cells-slice
		// regrow is deferred to the interpreter (deopt with
		// StatusListGrow). The OpNewList capHint sized for the bench
		// keeps the steady-state inside the fast path.
		//
		// Cold form (per-op slab recompute):
		//   UXTW x16, w_cell                  ; idx = handle & 0xFFFFFFFF
		//   MOV  x17, #SIZEOF_VMLIST           ; stride
		//   MUL  x16, x16, x17                 ; slab byte offset
		//   ADD  x16, x16, x19                 ; x19 = cached lists base
		//   LDR  x4,  [x16, #CELLS_OFF+8]      ; cells.len
		//   LDR  x17, [x16, #CELLS_OFF+16]     ; cells.cap
		//   CMP  x4,  x17
		//   B.HS deopt_listgrow                ; if len >= cap
		//   LDR  x17, [x16, #CELLS_OFF]        ; cells.ptr
		//   MOVZ x20, #0xFFFA, LSL #48         ; tagInt48
		//   BFI  x20, xVal, #0, #48            ; pack 48-bit payload
		//   STR  x20, [x17, x4, LSL #3]        ; cells[len] = boxed Cell
		//   ADD  x4,  x4, #1                   ; len++
		//   STR  x4,  [x16, #CELLS_OFF+8]      ; write cells.len
		//   STR  w4,  [x16, #4]                ; write vmList.len (u32)
		//
		// Hot form (hoistedCellReg matches op.A, x20 == &arenas.Lists[idx]):
		//   LDR  x4,  [x20, #CELLS_OFF+8]      ; cells.len
		//   LDR  x17, [x20, #CELLS_OFF+16]     ; cells.cap
		//   CMP  x4,  x17
		//   B.HS deopt_listgrow
		//   LDR  x17, [x20, #CELLS_OFF]        ; cells.ptr
		//   MOVZ x16, #0xFFFA, LSL #48
		//   BFI  x16, xVal, #0, #48
		//   STR  x16, [x17, x4, LSL #3]
		//   ADD  x4,  x4, #1
		//   STR  x4,  [x20, #CELLS_OFF+8]
		//   STR  w4,  [x20, #4]
		//
		// Hottest form (Phase 6.2d.2.c.2, x21=cap, x22=ptr, x23=len):
		//   CMP  x23, x21
		//   B.HS deopt_listgrow
		//   MOVZ x16, #0xFFFA, LSL #48
		//   BFI  x16, xVal, #0, #48
		//   STR  x16, [x22, x23, LSL #3]
		//   ADD  x23, x23, #1
		// (no in-loop stores back to cells.len or vmList.len; the
		// epilogue and the deopt block flush x23 to memory.)
		// In the hot form x20 is pinned (cached slab base) so the boxed
		// Cell scratch moves to x16; in the cold form x20 is free and the
		// slab address has already left x16, so the old x20 scratch is
		// kept to minimize churn.
		cellsOff := uint32(vm3.JITListCellsOffset())
		cellsPtrImm12 := cellsOff / 8        // 1
		cellsLenImm12 := (cellsOff + 8) / 8  // 2
		cellsCapImm12 := (cellsOff + 16) / 8 // 3
		xVal := r2x(fn, op.B)
		lgStart := deoptStartForStatus(fn, deoptStart, StatusListGrow)
		if h := hoistedCellReg(fn); h >= 0 && int(op.A) == h {
			if hoistsCellsLenARM64(fn) {
				ws := make([]uint32, 0, 6)
				ws = append(ws, cmpReg(23, 21))
				bWord := pcMap[idx] + len(ws)
				off, err := branchOff(bWord, lgStart, 19)
				if err != nil {
					return nil, fmt.Errorf("ListPushI64 deopt branch: %w", err)
				}
				ws = append(ws, bCond(0x2, off)) // B.HS
				ws = append(ws, movz(16, 0xFFFA, 3))
				ws = append(ws, bfi48(16, xVal))
				ws = append(ws, str64RegLsl3(16, 22, 23))
				ws = append(ws, addImm64(23, 23, 1))
				return ws, nil
			}
			ws := make([]uint32, 0, 11)
			ws = append(ws, ldr64(4, 20, cellsLenImm12))
			ws = append(ws, ldr64(17, 20, cellsCapImm12))
			ws = append(ws, cmpReg(4, 17))
			bWord := pcMap[idx] + len(ws)
			off, err := branchOff(bWord, lgStart, 19)
			if err != nil {
				return nil, fmt.Errorf("ListPushI64 deopt branch: %w", err)
			}
			ws = append(ws, bCond(0x2, off)) // B.HS
			ws = append(ws, ldr64(17, 20, cellsPtrImm12))
			ws = append(ws, movz(16, 0xFFFA, 3))
			ws = append(ws, bfi48(16, xVal))
			ws = append(ws, str64RegLsl3(16, 17, 4))
			ws = append(ws, addImm64(4, 4, 1))
			ws = append(ws, str64(4, 20, cellsLenImm12))
			ws = append(ws, strW(4, 20, 1)) // vmList.len at byte offset 4 (imm12=1, 4-byte stride)
			return ws, nil
		}
		stride := int64(vm3.JITListSlabStride())
		xCell := r2cell(op.A)
		ws := make([]uint32, 0, movImm64WordCount(stride)+14)
		ws = append(ws, uxtwReg(16, xCell))
		ws = append(ws, movImm64(17, stride)...)
		ws = append(ws, mulReg(16, 16, 17))
		ws = append(ws, addReg(16, 16, 19))
		ws = append(ws, ldr64(4, 16, cellsLenImm12))
		ws = append(ws, ldr64(17, 16, cellsCapImm12))
		ws = append(ws, cmpReg(4, 17))
		bWord := pcMap[idx] + len(ws)
		off, err := branchOff(bWord, lgStart, 19)
		if err != nil {
			return nil, fmt.Errorf("ListPushI64 deopt branch: %w", err)
		}
		ws = append(ws, bCond(0x2, off)) // B.HS
		ws = append(ws, ldr64(17, 16, cellsPtrImm12))
		ws = append(ws, movz(20, 0xFFFA, 3))
		ws = append(ws, bfi48(20, xVal))
		ws = append(ws, str64RegLsl3(20, 17, 4))
		ws = append(ws, addImm64(4, 4, 1))
		ws = append(ws, str64(4, 16, cellsLenImm12))
		ws = append(ws, strW(4, 16, 1)) // vmList.len at byte offset 4 (imm12=1, 4-byte stride)
		return ws, nil

	case vm3.OpListSetI64:
		// arenas.Lists[handleIdx(regsCell[A])].cells[regsI64[C]] =
		//   CInt(regsI64[B])
		//
		// Cold form (per-op slab recompute):
		//   UXTW x16, w_cell                  ; idx = handle & 0xFFFFFFFF
		//   MOV  x17, #SIZEOF_VMLIST           ; stride
		//   MUL  x16, x16, x17                 ; slab byte offset
		//   ADD  x16, x16, x19                 ; x19 = cached lists base
		//   LDR  x17, [x16, #CELLS_OFFSET]     ; cells.ptr
		//   MOVZ x20, #0xFFFA, LSL #48         ; tagInt48
		//   BFI  x20, xVal, #0, #48            ; pack 48-bit payload
		//   STR  x20, [x17, xIdx, LSL #3]      ; cells[idxReg] = packed
		//
		// Hot form (hoistedCellReg matches op.A, x20 == slab base):
		//   LDR  x17, [x20, #CELLS_OFFSET]
		//   MOVZ x16, #0xFFFA, LSL #48
		//   BFI  x16, xVal, #0, #48
		//   STR  x16, [x17, xIdx, LSL #3]
		//
		// Hottest form (Phase 6.2d.2.c.2, x22 == cells.ptr):
		//   MOVZ x16, #0xFFFA, LSL #48
		//   BFI  x16, xVal, #0, #48
		//   STR  x16, [x22, xIdx, LSL #3]
		cellsOff := uint32(vm3.JITListCellsOffset())
		xIdx := r2x(fn, uint16(op.C))
		xVal := r2x(fn, op.B)
		if h := hoistedCellReg(fn); h >= 0 && int(op.A) == h {
			if hoistsCellsPtrARM64(fn) {
				ws := make([]uint32, 0, 3)
				ws = append(ws, movz(16, 0xFFFA, 3))
				ws = append(ws, bfi48(16, xVal))
				ws = append(ws, str64RegLsl3(16, 22, xIdx))
				return ws, nil
			}
			ws := make([]uint32, 0, 4)
			ws = append(ws, ldr64(17, 20, cellsOff/8))
			ws = append(ws, movz(16, 0xFFFA, 3))
			ws = append(ws, bfi48(16, xVal))
			ws = append(ws, str64RegLsl3(16, 17, xIdx))
			return ws, nil
		}
		stride := int64(vm3.JITListSlabStride())
		xCell := r2cell(op.A)
		ws := make([]uint32, 0, movImm64WordCount(stride)+7)
		ws = append(ws, uxtwReg(16, xCell))
		ws = append(ws, movImm64(17, stride)...)
		ws = append(ws, mulReg(16, 16, 17))
		ws = append(ws, addReg(16, 16, 19))
		ws = append(ws, ldr64(17, 16, cellsOff/8))
		ws = append(ws, movz(20, 0xFFFA, 3))
		ws = append(ws, bfi48(20, xVal))
		ws = append(ws, str64RegLsl3(20, 17, xIdx))
		return ws, nil

	case vm3.OpListGetF64:
		// regsF64[A] = arenas.Lists[handleIdx(regsCell[B])].cells[regsI64[C]].Float()
		//
		// Cold form (no hoist, 6 inst):
		//   UXTW x16, w_cell                  ; idx = handle & 0xFFFFFFFF
		//   MOV  x17, #SIZEOF_VMLIST           ; stride
		//   MUL  x16, x16, x17                 ; slab byte offset
		//   ADD  x16, x16, x19                 ; x19 = cached lists base
		//   LDR  x16, [x16, #CELLS_OFFSET]     ; cells.ptr
		//   LDR  Dt,  [x16, xIdx, LSL #3]      ; cells[idxReg] as raw f64 bits
		//
		// Hot form (Phase 6.3.4.j.4a, x_cell holds cells.ptr post-refresh):
		//   LDR  Dt,  [x_cell, xIdx, LSL #3]   ; 1 inst.
		//
		// No SBFX: CFloat stores IEEE 754 bits directly, so the load
		// produces the f64 payload bit-for-bit identical to the
		// interp's .Float() decoder.
		cellsOff := uint32(vm3.JITListCellsOffset())
		xIdx := r2x(fn, uint16(op.C))
		xCell := r2cell(op.B)
		dA := r2d(op.A)
		if cellsPtrHoistedAt(fn, idx, op.B) {
			return []uint32{ldrDRegLsl3(dA, xCell, xIdx)}, nil
		}
		stride := int64(vm3.JITListSlabStride())
		ws := make([]uint32, 0, movImm64WordCount(stride)+5)
		ws = append(ws, uxtwReg(16, xCell))
		ws = append(ws, movImm64(17, stride)...)
		ws = append(ws, mulReg(16, 16, 17))
		ws = append(ws, addReg(16, 16, 19))
		ws = append(ws, ldr64(16, 16, cellsOff/8))
		ws = append(ws, ldrDRegLsl3(dA, 16, xIdx))
		return ws, nil

	case vm3.OpListSetF64:
		// arenas.Lists[handleIdx(regsCell[A])].cells[regsI64[C]] = CFloat(regsF64[B])
		//
		// Cold form (no hoist, 6 inst):
		//   UXTW x16, w_cell                  ; idx = handle & 0xFFFFFFFF
		//   MOV  x17, #SIZEOF_VMLIST           ; stride
		//   MUL  x16, x16, x17                 ; slab byte offset
		//   ADD  x16, x16, x19                 ; x19 = cached lists base
		//   LDR  x17, [x16, #CELLS_OFFSET]     ; cells.ptr
		//   STR  Dt,  [x17, xIdx, LSL #3]      ; cells[idxReg] = raw f64 bits
		//
		// Hot form (Phase 6.3.4.j.4a, x_cell holds cells.ptr post-refresh):
		//   STR  Dt,  [x_cell, xIdx, LSL #3]   ; 1 inst.
		cellsOff := uint32(vm3.JITListCellsOffset())
		xIdx := r2x(fn, uint16(op.C))
		xCell := r2cell(op.A)
		dB := r2d(op.B)
		if cellsPtrHoistedAt(fn, idx, op.A) {
			return []uint32{strDRegLsl3(dB, xCell, xIdx)}, nil
		}
		stride := int64(vm3.JITListSlabStride())
		ws := make([]uint32, 0, movImm64WordCount(stride)+5)
		ws = append(ws, uxtwReg(16, xCell))
		ws = append(ws, movImm64(17, stride)...)
		ws = append(ws, mulReg(16, 16, 17))
		ws = append(ws, addReg(16, 16, 19))
		ws = append(ws, ldr64(17, 16, cellsOff/8))
		ws = append(ws, strDRegLsl3(dB, 17, xIdx))
		return ws, nil

	default:
		return nil, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)
	}
}

// condForCmpReg returns the AArch64 B.cond condition code for a vm3
// reg-reg compare-and-branch opcode.
func condForCmpReg(code vm3.OpCode) uint32 {
	switch code {
	case vm3.OpCmpEqI64Br:
		return 0x0 // EQ
	case vm3.OpCmpNeI64Br:
		return 0x1 // NE
	case vm3.OpCmpLtI64Br:
		return 0xB // LT
	case vm3.OpCmpLeI64Br:
		return 0xD // LE
	case vm3.OpCmpGtI64Br:
		return 0xC // GT
	case vm3.OpCmpGeI64Br:
		return 0xA // GE
	}
	return 0
}

// condForCmpKImm returns the AArch64 B.cond condition code for a vm3
// reg-imm compare-and-branch opcode.
func condForCmpKImm(code vm3.OpCode) uint32 {
	switch code {
	case vm3.OpCmpEqI64KBr:
		return 0x0
	case vm3.OpCmpNeI64KBr:
		return 0x1
	case vm3.OpCmpLtI64KBr:
		return 0xB
	case vm3.OpCmpLeI64KBr:
		return 0xD
	case vm3.OpCmpGtI64KBr:
		return 0xC
	case vm3.OpCmpGeI64KBr:
		return 0xA
	}
	return 0
}

// branchOff computes the signed instruction-word displacement from
// srcWord (the word that contains the branch) to dstWord, and checks
// that it fits in the given bit-width.
func branchOff(srcWord, dstWord, bits int) (int32, error) {
	diff := dstWord - srcWord
	lim := 1 << (bits - 1)
	if diff >= lim || diff < -lim {
		return 0, fmt.Errorf("branch offset %d out of %d-bit range", diff, bits)
	}
	return int32(diff), nil
}

// --- AArch64 instruction encoders ---
//
// Mechanically these mirror vm2jit's encoders; vm3jit uses only the
// subset needed for i64 arithmetic + control flow (no NaN-box dance,
// no float, no list/map fast paths).

func movz(xd, imm16, hw uint32) uint32 {
	return 0xD2800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}

func movk(xd, imm16, hw uint32) uint32 {
	return 0xF2800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}

func movn(xd, imm16, hw uint32) uint32 {
	return 0x92800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}

func addReg(xd, xn, xm uint32) uint32 {
	return 0x8B000000 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func subReg(xd, xn, xm uint32) uint32 {
	return 0xCB000000 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func negReg(xd, xm uint32) uint32 {
	// NEG xd, xm == SUB xd, xzr, xm (Rn = 31).
	return 0xCB0003E0 | ((xm & 0x1F) << 16) | (xd & 0x1F)
}

func mulReg(xd, xn, xm uint32) uint32 {
	// MUL xd, xn, xm == MADD xd, xn, xm, xzr (Ra = 31).
	return 0x9B007C00 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func sdivReg(xd, xn, xm uint32) uint32 {
	return 0x9AC00C00 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func msubReg(xd, xn, xm, xa uint32) uint32 {
	// MSUB xd, xn, xm, xa: bit 15 (o0) = 1.
	return 0x9B008000 | ((xm & 0x1F) << 16) | ((xa & 0x1F) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func cmpReg(xn, xm uint32) uint32 {
	return 0xEB00001F | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5)
}

// cmpShiftLSL encodes CMP Xn, Xm, LSL #amount (64-bit shifted register
// compare, alias for SUBS XZR, Xn, Xm, LSL #amount). amount fits in 6
// bits (0..63). Used by OpMapSetI64I64 to fuse the LSL #1 of nLive+1
// into the cap-vs-2*(nLive+1) load-factor check.
func cmpShiftLSL(xn, xm, amount uint32) uint32 {
	return 0xEB00001F | ((xm & 0x1F) << 16) | ((amount & 0x3F) << 10) | ((xn & 0x1F) << 5)
}

func movReg(xd, xm uint32) uint32 {
	return 0xAA0003E0 | ((xm & 0x1F) << 16) | (xd & 0x1F)
}

func ldr64(xt, xn, imm12 uint32) uint32 {
	return 0xF9400000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}

// str64 encodes STR Xt, [Xn, #imm12*8] (unsigned-offset 64-bit). Used
// by the deopt block to write the status code through *(x1).
func str64(xt, xn, imm12 uint32) uint32 {
	return 0xF9000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}

// cbz64 encodes CBZ Xt, <pc-rel> (64-bit, branch if Xt is zero).
// off19 is a signed instruction-word offset; caller must pre-validate
// that it fits in 19 bits via branchOff(_, _, 19).
func cbz64(xt uint32, off19 int32) uint32 {
	return 0xB4000000 | (uint32(off19&0x7FFFF) << 5) | (xt & 0x1F)
}

// cbnz64 encodes CBNZ Xt, <pc-rel> (64-bit, branch if Xt is non-zero).
// Same encoding as cbz64 with bit 24 set. Used by cross-fn OpCallMixed
// to branch into the passthrough deopt block when the callee left a
// non-zero status code in [x1].
func cbnz64(xt uint32, off19 int32) uint32 {
	return 0xB5000000 | (uint32(off19&0x7FFFF) << 5) | (xt & 0x1F)
}

func bImm(off26 int32) uint32 {
	return 0x14000000 | uint32(off26&0x3FFFFFF)
}

// bl encodes BL <pc-rel>: same imm26 field as B, top opcode bit set so
// the CPU writes x30 with the return address. Used by self-recursive
// OpCallI64 to call back into the same JIT page.
func bl(off26 int32) uint32 {
	return 0x94000000 | uint32(off26&0x3FFFFFF)
}

// blr encodes BLR Xn: branch with link to the absolute address held in
// Xn (writes x30 with the return address). Used by cross-fn OpCallMixed
// to invoke a JIT'd callee whose entry pointer lives outside this code
// page (BL's 26-bit pc-rel range tops out at +/-128 MiB, smaller than
// the gap between independently mmap'd JIT pages, so we materialize the
// 64-bit entry in x16 with movImm64 and BLR through it).
func blr(xn uint32) uint32 {
	return 0xD63F0000 | ((xn & 0x1F) << 5)
}

// addImm64 encodes ADD Xd, Xn, #imm12 (64-bit, shift=0). Used by the
// OpCallI64 lowering to bump x0 from caller's regs base to callee's
// window (offset NumRegsI64*8 bytes).
func addImm64(xd, xn, imm12 uint32) uint32 {
	return 0x91000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func bCond(cond uint32, off19 int32) uint32 {
	return 0x54000000 | (uint32(off19&0x7FFFF) << 5) | (cond & 0xF)
}

func ret() uint32 { return 0xD65F03C0 }

// stpPreIdx64 encodes STP Xt1, Xt2, [Xn, #imm]! (64-bit pre-index,
// writeback). imm is a 7-bit signed multiple of 8 bytes, so the
// instruction-level immediate is imm/8. Used to push callee-saved
// pairs in 16-byte chunks (imm = -16).
func stpPreIdx64(xt1, xt2, xn uint32, imm int32) uint32 {
	imm7 := uint32(imm/8) & 0x7F
	return 0xA9800000 | (imm7 << 15) | ((xt2 & 0x1F) << 10) | ((xn & 0x1F) << 5) | (xt1 & 0x1F)
}

// ldpPostIdx64 encodes LDP Xt1, Xt2, [Xn], #imm (64-bit post-index,
// writeback). imm is a 7-bit signed multiple of 8. Used to pop the
// callee-saved pairs in 16-byte chunks (imm = +16).
func ldpPostIdx64(xt1, xt2, xn uint32, imm int32) uint32 {
	imm7 := uint32(imm/8) & 0x7F
	return 0xA8C00000 | (imm7 << 15) | ((xt2 & 0x1F) << 10) | ((xn & 0x1F) << 5) | (xt1 & 0x1F)
}

// uxtwReg encodes UXTW Xd, Wn: zero-extend the low 32 bits of Xn into
// Xd. Equivalent to UBFM Xd, Xn, #0, #31 (immr=0, imms=31, sf=1, N=1).
// Used by OpListGetI64 to extract the 32-bit slab index from a handle.
func uxtwReg(xd, xn uint32) uint32 {
	// UBFM (64): sf=1, N=1, immr=0, imms=31 -> 0xD3407C00 base
	return 0xD3407C00 | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// ldrRegLsl3 encodes LDR Xt, [Xn, Xm, LSL #3]: load 64-bit with
// register-scaled offset (scale=3 for 8-byte stride). Used by
// OpListGetI64 to read cells[idxReg] given the cells slice data ptr in
// xn and the i64 index in xm.
func ldrRegLsl3(xt, xn, xm uint32) uint32 {
	// LDR (register, 64-bit): size=11, V=0, opc=01, option=011 (LSL), S=1
	return 0xF8607800 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}

// ldrDRegLsl3 encodes LDR Dt, [Xn, Xm, LSL #3]: load a 64-bit FP
// register from base+idx*8. Identical to ldrRegLsl3 except V=1
// (SIMD&FP variant). Used by OpListGetF64 to read cells[idxReg] as
// raw f64 bits into a D-register.
func ldrDRegLsl3(dt, xn, xm uint32) uint32 {
	return 0xFC607800 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (dt & 0x1F)
}

// sbfx48 encodes SBFX Xd, Xn, #0, #48: sign-extend the low 48 bits of
// Xn into Xd. Equivalent to SBFM Xd, Xn, #0, #47 (immr=0, imms=47,
// sf=1, N=1). Used by OpListGetI64 to recover the signed Int48 payload
// from a Cell (the high 16 bits hold the NaN-box tag).
func sbfx48(xd, xn uint32) uint32 {
	return 0x9340BC00 | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// bfi48 encodes BFI Xd, Xn, #0, #48: copy the low 48 bits of Xn into
// the low 48 bits of Xd (leaving the upper 16 bits of Xd intact).
// Alias for BFM Xd, Xn, #0, #47 (immr=0, imms=47, sf=1, N=1). Used by
// OpListPushI64 to pack an i64 payload into a pre-tagged Cell.
func bfi48(xd, xn uint32) uint32 {
	return 0xB340BC00 | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// str64RegLsl3 encodes STR Xt, [Xn, Xm, LSL #3]: store 64-bit with
// register-scaled offset (scale=3 for 8-byte stride). Used by
// OpListPushI64 to write cells[len] = boxedCell.
func str64RegLsl3(xt, xn, xm uint32) uint32 {
	return 0xF8207800 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}

// strDRegLsl3 encodes STR Dt, [Xn, Xm, LSL #3]: store a 64-bit FP
// register at base+idx*8. Used by OpListSetF64 to write a raw f64
// payload into cells[idxReg]. The CFloat encoding stores the IEEE
// 754 bits directly, so no NaN-box tag is added on the way in.
func strDRegLsl3(dt, xn, xm uint32) uint32 {
	return 0xFC207800 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (dt & 0x1F)
}

// strW encodes STR Wt, [Xn, #imm12*4] (unsigned-offset 32-bit). Used
// by OpListPushI64 to write the low 32 bits of the updated length back
// into vmList.len at byte offset 4.
func strW(wt, xn, imm12 uint32) uint32 {
	return 0xB9000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (wt & 0x1F)
}

// ldrW encodes LDR Wt, [Xn, #imm12*4] (unsigned-offset 32-bit, zero-
// extended into Xt). Used by OpMapSetI64I64 to read vmMap.nLive (a u32
// at byte offset 4 of vmMap).
func ldrW(wt, xn, imm12 uint32) uint32 {
	return 0xB9400000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (wt & 0x1F)
}

// subImm64 encodes SUB Xd, Xn, #imm12 (64-bit, shift=0). Mirror of
// addImm64 with bit 30 set. Used by OpMapSetI64I64/OpMapGetI64I64 to
// compute mask = cap - 1.
func subImm64(xd, xn, imm12 uint32) uint32 {
	return 0xD1000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// andReg encodes AND Xd, Xn, Xm (64-bit shifted register, shift=LSL #0,
// N=0). Used by OpMapSetI64I64/OpMapGetI64I64 to compute pos = h & mask
// inside the probe loop.
func andReg(xd, xn, xm uint32) uint32 {
	return 0x8A000000 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// eorLsr encodes EOR Xd, Xn, Xm, LSR #amount (64-bit shifted register,
// shift=01 LSR, N=0). Used by the splitmix64 hash sequence.
func eorLsr(xd, xn, xm, amount uint32) uint32 {
	return 0xCA400000 | ((xm & 0x1F) << 16) | ((amount & 0x3F) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// lslImm encodes LSL Xd, Xn, #imm (64-bit logical shift left, alias for
// UBFM Xd, Xn, #(-imm MOD 64), #(63-imm)). imm must be in 1..63. Used
// by OpMapSetI64I64 to compute 2*(nLive+1).
func lslImm(xd, xn, imm uint32) uint32 {
	immr := (64 - imm) & 0x3F
	imms := uint32(63) - imm
	return 0xD3400000 | (immr << 16) | (imms << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// orrImmBit0 encodes ORR Xd, Xn, #1 (logical immediate, N=1, immr=0,
// imms=0 → mask 0x1). Used by the splitmix64 hash sequence to force
// the low bit so a zero-hash never collides with the "empty entry"
// sentinel in mapEntry.hash.
func orrImmBit0(xd, xn uint32) uint32 {
	return 0xB2400000 | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// maddReg encodes MADD Xd, Xn, Xm, Xa (64-bit multiply-add). Used by
// OpMapSetI64I64/OpMapGetI64I64 to compute the probe entry address
// (&entry = pos * sizeof(mapEntry) + tablePtr) in one instruction.
func maddReg(xd, xn, xm, xa uint32) uint32 {
	return 0x9B000000 | ((xm & 0x1F) << 16) | ((xa & 0x1F) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// splitmix64C1 and splitmix64C2 are the multiplier constants for the
// hashI64 sequence in runtime/vm3/maps.go. Kept here as int64 (the
// uint64 → int64 cast is a bit-pattern reinterpret) so the JIT's
// movImm64 helper can bake them into the instruction stream alongside
// the rest of the hash code. The two-step uint64-var → int64 pattern
// dodges Go's constant-overflow rule (both values have bit 63 set).
var (
	splitmix64C1U uint64 = 0xbf58476d1ce4e5b9
	splitmix64C2U uint64 = 0x94d049bb133111eb
	splitmix64C1         = int64(splitmix64C1U)
	splitmix64C2         = int64(splitmix64C2U)
)

// mapSetI64I64WordsARM64 is the static word count of an inline
// OpMapSetI64I64 lowering. The body is:
//
//	7  pre-amble (LDR cap, LDR W nLive, ADD nLive+1, CMP-LSL #1,
//	   B.LO deopt, SUB mask, MOV stride)
//	14 splitmix64 (EOR + 4-word movImm + MUL + EOR + 4-word movImm +
//	   MUL + EOR + ORR-imm-1)
//	1  AND pos
//	14 probe loop body (LDR tablePtr, MADD entry_addr, LDR hash,
//	   CBZ fill, CMP hash, B.NE next, LDR key, SBFX, CMP key,
//	   B.NE next, MOVZ tag, BFI val, STR value, B done)
//	3  next (ADD pos+1, AND mask, B probe_top)
//	9  fill block (STR hash, MOVZ tag, BFI key, STR key, BFI val,
//	   STR value, LDR W nLive, ADD nLive+1, STR W nLive)
const mapSetI64I64WordsARM64 = 48

// mapGetI64I64WordsARM64 is the static word count of an inline
// OpMapGetI64I64 lowering. Same scaffolding as mapSetI64I64WordsARM64
// minus the grow check (LDR cap + CBZ empty), the fill block, and the
// nLive bump.
const mapGetI64I64WordsARM64 = 36

// emitSplitmix64ARM64 emits the 14-word splitmix64 sequence that
// computes h = hashI64(xKey) into x4. Uses x16 as the multiplier
// scratch. Output is identical to runtime/vm3/maps.go's hashI64:
//
//	x4 = uint64(xKey)
//	x4 ^= x4 >> 30
//	x4 *= 0xbf58476d1ce4e5b9
//	x4 ^= x4 >> 27
//	x4 *= 0x94d049bb133111eb
//	x4 ^= x4 >> 31
//	x4 |= 1
func emitSplitmix64ARM64(xKey uint32) []uint32 {
	ws := make([]uint32, 0, 14)
	ws = append(ws, eorLsr(4, xKey, xKey, 30))
	ws = append(ws, movImm64(16, splitmix64C1)...)
	ws = append(ws, mulReg(4, 4, 16))
	ws = append(ws, eorLsr(4, 4, 4, 27))
	ws = append(ws, movImm64(16, splitmix64C2)...)
	ws = append(ws, mulReg(4, 4, 16))
	ws = append(ws, eorLsr(4, 4, 4, 31))
	ws = append(ws, orrImmBit0(4, 4))
	return ws
}

// strD encodes STR Dt, [Xn, #imm12*8] (unsigned-offset 64-bit FP).
// Used by the deopt block to spill pinned f64 regs back to regsF64.
func strD(dt, xn, imm12 uint32) uint32 {
	return 0xFD000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (dt & 0x1F)
}

// movImm64 emits the shortest movz/movn/movk sequence that loads v
// into xd. Negative values prefer movn (one-instruction load for
// -65536..-1 etc.); positive values use movz then movk for each
// non-zero 16-bit lane. Result length is movImm64WordCount(v).
func movImm64(xd uint32, v int64) []uint32 {
	if v == 0 {
		return []uint32{movz(xd, 0, 0)}
	}
	if v > 0 && v <= 0xFFFF {
		return []uint32{movz(xd, uint32(v), 0)}
	}
	if v < 0 && v >= -0x10000 {
		return []uint32{movn(xd, uint32(^v), 0)}
	}
	// General 4-lane build: emit movz for the first non-zero lane,
	// then movk for every subsequent non-zero lane.
	lanes := [4]uint32{
		uint32(v) & 0xFFFF,
		uint32(v>>16) & 0xFFFF,
		uint32(v>>32) & 0xFFFF,
		uint32(v>>48) & 0xFFFF,
	}
	ws := make([]uint32, 0, 4)
	first := -1
	for i, l := range lanes {
		if l != 0 {
			first = i
			break
		}
	}
	if first < 0 {
		return []uint32{movz(xd, 0, 0)}
	}
	ws = append(ws, movz(xd, lanes[first], uint32(first)))
	for i := first + 1; i < 4; i++ {
		if lanes[i] != 0 {
			ws = append(ws, movk(xd, lanes[i], uint32(i)))
		}
	}
	return ws
}

// movImm64WordCount predicts the length of movImm64(xd, v) without
// allocating. Must agree with movImm64 byte-for-byte.
func movImm64WordCount(v int64) int {
	if v == 0 {
		return 1
	}
	if v > 0 && v <= 0xFFFF {
		return 1
	}
	if v < 0 && v >= -0x10000 {
		return 1
	}
	lanes := [4]uint32{
		uint32(v) & 0xFFFF,
		uint32(v>>16) & 0xFFFF,
		uint32(v>>32) & 0xFFFF,
		uint32(v>>48) & 0xFFFF,
	}
	n := 0
	for _, l := range lanes {
		if l != 0 {
			n++
		}
	}
	if n == 0 {
		return 1
	}
	return n
}

// --- AArch64 SIMD/FP encoders (scalar double, V0..V31) ---
//
// All encoders take and return raw register numbers (0..31) in the
// "D" (lower 64-bit lane) view of the SIMD bank. The full-vector "V"
// view shares the same register number.

// ldrD encodes LDR Dt, [Xn, #imm12*8] (load 64-bit FP register from
// memory at base+imm12*8). Used by the prologue to fill the pinned
// f64 slots from regsF64 (x2).
func ldrD(dt, xn, imm12 uint32) uint32 {
	return 0xFD400000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (dt & 0x1F)
}

// fmovDX encodes FMOV Dd, Xn (general -> SIMD bit-cast).
// Used by OpConstF64K after loading the IEEE 754 bit-pattern into x16.
func fmovDX(dd, xn uint32) uint32 {
	return 0x9E670000 | ((xn & 0x1F) << 5) | (dd & 0x1F)
}

// fmovXD encodes FMOV Xd, Dn (SIMD -> general bit-cast).
// Used by OpReturnF64 to copy d<retSlot> into x0 so the trampoline
// can return the raw IEEE 754 bit pattern.
func fmovXD(xd, dn uint32) uint32 {
	return 0x9E660000 | ((dn & 0x1F) << 5) | (xd & 0x1F)
}

// fmovDD encodes FMOV Dd, Dn (SIMD reg-reg copy, double).
func fmovDD(dd, dn uint32) uint32 {
	return 0x1E604000 | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// faddD encodes FADD Dd, Dn, Dm (scalar double).
func faddD(dd, dn, dm uint32) uint32 {
	return 0x1E602800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fsubD encodes FSUB Dd, Dn, Dm.
func fsubD(dd, dn, dm uint32) uint32 {
	return 0x1E603800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fmulD encodes FMUL Dd, Dn, Dm.
func fmulD(dd, dn, dm uint32) uint32 {
	return 0x1E600800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fdivD encodes FDIV Dd, Dn, Dm.
func fdivD(dd, dn, dm uint32) uint32 {
	return 0x1E601800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fnegD encodes FNEG Dd, Dn.
func fnegD(dd, dn uint32) uint32 {
	return 0x1E614000 | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fsqrtD encodes FSQRT Dd, Dn (scalar double square root).
// IEEE 754 correctly-rounded, bit-identical to Go's math.Sqrt on arm64.
func fsqrtD(dd, dn uint32) uint32 {
	return 0x1E61C000 | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fmaddD encodes FMADD Dd, Dn, Dm, Da (scalar double, fused multiply-add).
// Dd = Dn * Dm + Da with a single rounding step (IEEE 754-2008 fused).
func fmaddD(dd, dn, dm, da uint32) uint32 {
	return 0x1F400000 | ((dm & 0x1F) << 16) | ((da & 0x1F) << 10) |
		((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fcmpD encodes FCMP Dn, Dm (scalar double compare). Sets NZCV from
// the IEEE 754 unordered-aware comparison: ordered Eq/Ne/Lt/Le/Gt/Ge
// branch under the standard cond codes; NaN sets V (and C) so ordered
// predicates fail naturally without an extra unordered check.
func fcmpD(dn, dm uint32) uint32 {
	return 0x1E602000 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5)
}

// scvtfDX encodes SCVTF Dd, Xn (signed 64-bit int -> double, round to
// nearest even). Used by OpI64ToF64.
func scvtfDX(dd, xn uint32) uint32 {
	return 0x9E620000 | ((xn & 0x1F) << 5) | (dd & 0x1F)
}

// fcvtzsXD encodes FCVTZS Xd, Dn (double -> signed 64-bit int,
// truncating toward zero). Used by OpF64ToI64.
func fcvtzsXD(xd, dn uint32) uint32 {
	return 0x9E780000 | ((dn & 0x1F) << 5) | (xd & 0x1F)
}

// condForCmpF64 returns the AArch64 B.cond condition code for a vm3
// f64 reg-reg compare-and-branch opcode. Ordered predicates use the
// standard unsigned condition codes (HI/HS/MI/LS) because FCMP sets
// the flags so unordered (NaN) operands fail Lt/Le/Gt/Ge naturally.
func condForCmpF64(code vm3.OpCode) uint32 {
	switch code {
	case vm3.OpCmpEqF64Br:
		return 0x0 // EQ
	case vm3.OpCmpNeF64Br:
		return 0x1 // NE
	case vm3.OpCmpLtF64Br:
		return 0x4 // MI (FCMP sets N=1 iff Dn < Dm ordered; unordered keeps N=0)
	case vm3.OpCmpLeF64Br:
		return 0x9 // LS (C clear OR Z set; works because FCMP sets C=0 if Dn < Dm)
	case vm3.OpCmpGtF64Br:
		return 0xC // GT (Z clear AND N==V)
	case vm3.OpCmpGeF64Br:
		return 0xA // GE (N==V)
	}
	return 0
}
