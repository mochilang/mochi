//go:build arm64

package vm3jit

import (
	"fmt"

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
// saved register (x25..x28). Caller has pre-checked r < maxCellRegs.
func r2cell(r uint16) uint32 { return uint32(r) + 25 }

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
	return numCellScratchPairs(fn) + extraCellScratchPairsARM64(fn) + numI64CalleeSavedPairs(fn) + numCellCalleeSavedPairs(fn)
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
		pairs = append(pairs, 25+uint32(2*k))
	}
	return pairs
}

// hasRegRegDivMod, hasListPushI64, hasListGetI64 live in lower_common.go
// (used by both arch-specific lowering and the arch-independent
// admissibility check in compile.go).

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
	if fn.NumRegsCell == 1 && (hasListGetI64(fn) || hasListPushI64(fn)) {
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
	stride := int64(vm3.JITListSlabStride())
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
//	LDP x3,xzr,[SP],#16
//	LDP x0,x2,[SP],#16
//	nSpill * LDR x(9+r),[x0,#r*8]
//	MOV xA,x17                            ; deliver result to dst
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
	// 2*nSpill spill+reload + arg STRs + 2 STP + bumps + 1 MOV x4,x20 +
	// movImm64 + 1 BLR + 1 MOV x17,x0 + 2 LDP + 1 MOV xA,x17.
	return 2*nSpill + nI64Args + nF64Args + nCellArgs + 2 + bumps + 1 + movImm64WordCount(addr) + 1 + 1 + 2 + 1
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
	prologueWords := lrPair + pairs + int(fn.NumRegsI64) + int(fn.NumRegsF64) + int(fn.NumRegsCell) + cellScratch + hoistPrologueWordsARM64(fn) + arenaCtxStash
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
	total := deoptStart + len(statuses)*deoptBlockWordsARM64Status(fn)
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
	// then cache arenas.Lists base in x19 from the jitArenaCtx (x4).
	// listsBase lives at offset 0 of jitArenaCtx; future bases (mapsBase,
	// ...) move into x20.
	for r := uint32(0); r < uint32(fn.NumRegsCell); r++ {
		ws = append(ws, ldr64(r2cell(uint16(r)), 3, r))
	}
	if cellScratch > 0 {
		ws = append(ws, ldr64(19, 4, 0))
	}
	// Slab-base hoist (Phase 6.2d.2.c.1): when one cell reg owns every
	// list reference in fn, compute &arenas.Lists[handleIdx] once into
	// x20 so each OpListGet/OpListPush inside the loop skips the UXTW +
	// MOV stride + MUL + ADD recompute. The handle stays put for the
	// lifetime of the call (no opcode in the whitelist mutates a cell
	// reg), so the cached base is loop-invariant by construction.
	if h := hoistedCellReg(fn); h >= 0 {
		stride := int64(vm3.JITListSlabStride())
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

	case vm3.OpCmpEqF64Br, vm3.OpCmpNeF64Br,
		vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br,
		vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br:
		// FCMP Dn, Dm ; B.cond <target>.
		return 2, nil

	case vm3.OpI64ToF64:
		return 1, nil
	case vm3.OpF64ToI64:
		return 1, nil
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
		ws = append(ws, ldpPostIdx64(3, 31, 31, 16))
		ws = append(ws, ldpPostIdx64(0, 2, 31, 16))
		for r := uint16(0); r < 7; r++ {
			if spillMask&(1<<r) != 0 {
				ws = append(ws, ldr64(uint32(9+r), 0, uint32(r)))
			}
		}
		ws = append(ws, movReg(xA, 17))
		return ws, nil

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

// strW encodes STR Wt, [Xn, #imm12*4] (unsigned-offset 32-bit). Used
// by OpListPushI64 to write the low 32 bits of the updated length back
// into vmList.len at byte offset 4.
func strW(wt, xn, imm12 uint32) uint32 {
	return 0xB9000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (wt & 0x1F)
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
