//go:build amd64

package vm3jit

import (
	"encoding/binary"
	"fmt"
	"unsafe"

	"mochi/runtime/vm3"
)

// AMD64 SysV register pinning for vm3 i64 slots:
//
//	regsI64[0] -> RSI  (caller-saved)
//	regsI64[1] -> RDI  (caller-saved)
//	regsI64[2] -> R8   (caller-saved)
//	regsI64[3] -> R9   (caller-saved)
//	regsI64[4] -> R10  (caller-saved)
//	regsI64[5] -> R11  (caller-saved)
//	regsI64[6] -> R12  (callee-saved)
//	regsI64[7] -> R13  (callee-saved)
//	regsI64[8] -> R14  (callee-saved)
//	regsI64[9] -> RBP  (callee-saved, i64-only fns; cell-bank fns
//	                    repurpose RBP as the regsCell base so slot 9 is
//	                    only available when fn.NumRegsCell == 0)
//
// Reserved (never holds a pinned slot):
//
//	RAX (0) - scratch, i64 return value, IDIV quotient
//	RCX (1) - scratch / regsCell base on entry (moved to RBP in cell-bank prologue)
//	RDX (2) - scratch, IDIV remainder, CQO sign-extension high half
//	RBX (3) - regsI64 base pointer (set from RDI in prologue,
//	          preserved by the SysV-style callee-save we do for RBX,
//	          so it survives our internal CALL on self-recursion)
//	RSP (4) - stack pointer
//	R8  (8) - *jitArenaCtx on entry when cell-bank (moved to R14)
//	R15(15) - *int64 status word pointer (set from RSI in prologue)
//
// Cell-bank fns additionally pin:
//
//	RBP (5)  - regsCell base pointer (loaded from RCX in prologue)
//	R14 (14) - *jitArenaCtx (loaded from R8 in prologue)
//
// R14 is shared with the f64 base path, so the cell-bank prologue
// rejects fn.NumRegsF64 > 0 to keep the pinning unambiguous. RBP is
// shared between the i64 slot-9 home and the cell-bank regsCell base,
// so cell-bank fns also cap at NumRegsI64 <= 8 (slots 0..7).
//
// The trampoline calls us with the SysV ABI:
//
//	RDI = *int64 regs base   (corresponds to AArch64 x0)
//	RSI = *int64 status word (corresponds to AArch64 x1)
//
// On entry the prologue stashes both into the callee-saved pair
// RBX:R15 so they survive a self-recursive CALL inside the same code
// page.
//
// Phase 6.2a uses fixed-width encodings (imm32, disp32, rel32) so a
// pass-1 byte count is deterministic and pass-2 can patch branches
// through pcMap[] without re-flowing.

// AMD64 register indices.
const (
	xRAX = 0
	xRCX = 1
	xRDX = 2
	xRBX = 3
	xRSP = 4
	xRBP = 5
	xRSI = 6
	xRDI = 7
	xR8  = 8
	xR9  = 9
	xR10 = 10
	xR11 = 11
	xR12 = 12
	xR13 = 13
	xR14 = 14
	xR15 = 15
)

// r2xAMD64 maps a vm3 i64 register slot to the x86_64 GPR it is pinned
// to. Caller must pre-check r < maxI64RegsAMD64.
func r2xAMD64(r uint16) int {
	switch r {
	case 0:
		return xRSI
	case 1:
		return xRDI
	case 2:
		return xR8
	case 3:
		return xR9
	case 4:
		return xR10
	case 5:
		return xR11
	case 6:
		return xR12
	case 7:
		return xR13
	case 8:
		return xR14
	case 9:
		return xRBP
	}
	return -1
}

// calleeSavedSlot reports whether slot r lands in a callee-saved GPR
// (R12..R14, RBP). The prologue pushes those before clobbering them.
func calleeSavedSlot(r uint16) bool { return r >= 6 && r < 10 }

// usesR14ForF64 reports whether the AMD64 backend repurposes R14 as
// the regsF64 base pointer for fn. This applies to non-cell-bank f64
// fns; cell-bank+f64 fns route the f64 base through R12 instead so
// R14 stays free for the *jitArenaCtx pointer (Phase 6.3.4.n.3).
// R14 is also Go's G register, so we must push/pop it regardless of
// how it is used inside the JIT'd body.
func usesR14ForF64(fn *vm3.Function) bool {
	return fn.NumRegsF64 > 0 && !isCellBankAMD64(fn)
}

// usesR12ForF64AMD64 reports whether fn pins the regsF64 base in R12
// instead of R14. Phase 6.3.4.n.3 enables this for cell-bank+f64 fns
// because R14 already carries *jitArenaCtx. The trade-off is one fewer
// i64 slot (R12 is i64 slot 6), so callers must cap NumRegsI64 <= 6
// when both Cell and F64 banks are present (see archCaps + admission
// gate).
func usesR12ForF64AMD64(fn *vm3.Function) bool {
	return fn.NumRegsF64 > 0 && isCellBankAMD64(fn)
}

// f64BaseGPRAMD64 returns the x86_64 GPR holding the regsF64 base
// pointer for fn, or -1 when fn has no f64 bank. Non-cell-bank f64
// fns use R14; cell-bank+f64 fns use R12 (R14 is reserved for the
// arena ctx pointer in that case).
func f64BaseGPRAMD64(fn *vm3.Function) int {
	if fn.NumRegsF64 == 0 {
		return -1
	}
	if isCellBankAMD64(fn) {
		return xR12
	}
	return xR14
}

// isCellBankAMD64 reports whether fn carries the Cell register bank.
// Phase 6.3.4.m.4c.1 cell-bank fns repurpose RBP as the regsCell base
// and R14 as the *jitArenaCtx pointer, so the prologue pushes both and
// the epilogue pops them. Phase 6.3.4.n.3 adds the cell-bank+f64
// admission: R12 takes over as the regsF64 base.
func isCellBankAMD64(fn *vm3.Function) bool { return fn.NumRegsCell > 0 }

// numCalleeSavedPushesAMD64 returns the number of R12..R14/RBP pushes the
// prologue needs for fn, plus the always-present RBX and R15 pushes
// (2). Used by both the prologue emitter and the byte-count predictor.
// R14 is pushed when (a) i64 slot 8 lands in R14 OR (b) R14 holds the
// regsF64 base for an f64-touching fn OR (c) R14 holds *jitArenaCtx for
// a cell-bank fn. RBP is pushed when (a) i64 slot 9 lands in RBP
// (Phase 6.3.4.n.1) OR (b) RBP holds the regsCell base for a cell-bank
// fn. R12 is pushed when (a) i64 slot 6 lands in R12 OR (b) R12 holds
// the regsF64 base for a cell-bank+f64 fn (Phase 6.3.4.n.3).
func numCalleeSavedPushesAMD64(fn *vm3.Function) int {
	n := int(fn.NumRegsI64)
	if n > maxI64RegsAMD64 {
		n = maxI64RegsAMD64
	}
	// RBX and R15 are always pushed; R12..R14 only if their slot is used.
	count := 2
	if n > 6 || usesR12ForF64AMD64(fn) {
		count++
	}
	if n > 7 {
		count++
	}
	if n > 8 || usesR14ForF64(fn) || isCellBankAMD64(fn) {
		count++
	}
	if n > 9 || isCellBankAMD64(fn) {
		count++ // RBP for slot 9 (i64-only) or regsCell base (cell-bank)
	}
	return count
}

// alignFixupBytesAMD64 returns 8 if the function is non-leaf and the
// total prologue pushes are even (so RSP would land at %16==8 instead
// of %16==0 before an internal CALL), else 0. The 8-byte gap is
// inserted via `sub $8, %rsp` and removed via `add $8, %rsp`.
func alignFixupBytesAMD64(fn *vm3.Function) int {
	if !isNonLeafAMD64(fn) {
		return 0
	}
	if numCalleeSavedPushesAMD64(fn)%2 == 0 {
		return 8
	}
	return 0
}

// isNonLeafAMD64 mirrors isNonLeaf for the AMD64 backend. Phase
// 6.3.4.m.4c.5 adds self-recursive OpCallMixed to the set of native
// CALLs the AMD64 backend emits, so cell-bank fns with self-CallMixed
// also need the SP-alignment fixup.
func isNonLeafAMD64(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpCallI64 || op.Code == vm3.OpCallMixed {
			return true
		}
	}
	return false
}

// hasSelfCallMixedAMD64 reports whether fn contains an OpCallMixed
// back to itself (Phase 6.3.4.m.4c.5). Cross-fn OpCallMixed on AMD64
// is admitted in Phase 6.3.4.m.4c.6 (see hasCrossFnCallMixedAMD64).
func hasSelfCallMixedAMD64(fn *vm3.Function, opts Options) bool {
	if opts.SelfIdx < 0 {
		return false
	}
	for _, op := range fn.Code {
		if op.Code != vm3.OpCallMixed {
			continue
		}
		if int(uint16(op.C)) == opts.SelfIdx {
			return true
		}
	}
	return false
}

// hasCrossFnCallMixedAMD64 reports whether fn contains an OpCallMixed
// to a different function (not self). When true, the JIT emits a
// movabs + indirect call sequence (Phase 6.3.4.m.4c.6) and the admission
// gate requires the callee to be JIT-compiled.
func hasCrossFnCallMixedAMD64(fn *vm3.Function, opts Options) bool {
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
		if idx >= 0 && idx < len(opts.Prog.Funcs) {
			return true
		}
	}
	return false
}

// selfDeoptCalleeAMD64 reports whether a self-recursive OpCallMixed
// site needs the post-CALL status-word check + jne-to-passthrough.
// True iff fn itself has any deopt-capable opcode (mirrors the ARM64
// crossFnDeoptCallee gate for self-recursion; the callee here is fn).
func selfDeoptCalleeAMD64(fn *vm3.Function) bool {
	return hasRegRegDivModAMD64(fn) || hasNewPair(fn)
}

// crossFnDeoptCalleeAMD64 mirrors crossFnDeoptCallee in lower_arm64.go.
// True iff the callee has any deopt-capable opcode (reg-reg div/mod,
// OpNewPair). Cell-bank callees on AMD64 only use reg-reg div/mod or
// OpNewPair today; list / map / i64-array deopts are not yet admitted
// on AMD64 cell-bank.
func crossFnDeoptCalleeAMD64(callee *vm3.Function) bool {
	return hasRegRegDivModAMD64(callee) || hasNewPair(callee)
}

// needsCrossFnPassthroughAMD64 reports whether fn has at least one
// cross-fn OpCallMixed whose callee can deopt and thus needs the
// shared passthrough deopt block emitted at the tail of fn's code.
func needsCrossFnPassthroughAMD64(fn *vm3.Function, opts Options) bool {
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
		if crossFnDeoptCalleeAMD64(callee) {
			return true
		}
	}
	return false
}

// needsPassthroughAMD64 reports whether fn must emit the shared
// passthrough deopt block at the end of its code stream. Triggered by
// self-recursive OpCallMixed sites in cell-bank fns whose body can
// deopt (binary_trees make_tree raises StatusPairGrow from inline
// OpNewPair) OR by cross-fn OpCallMixed sites to deopt-capable callees
// (Phase 6.3.4.m.4c.6).
func needsPassthroughAMD64(fn *vm3.Function, opts Options) bool {
	if hasSelfCallMixedAMD64(fn, opts) && selfDeoptCalleeAMD64(fn) {
		return true
	}
	return needsCrossFnPassthroughAMD64(fn, opts)
}

// passthroughBlockBytesAMD64 mirrors passthroughBlockWordsARM64. The
// block is the bare epilogue (no `movq $status, (%r15)` write since
// the callee already wrote the status before propagating).
func passthroughBlockBytesAMD64(fn *vm3.Function, opts Options) int {
	if !needsPassthroughAMD64(fn, opts) {
		return 0
	}
	return epilogueBytesAMD64(fn)
}

// passthroughStartAMD64 returns the byte offset of the passthrough
// deopt block, given the first per-status deopt block start. Lives
// after the per-status blocks.
func passthroughStartAMD64(fn *vm3.Function, deoptStart int) int {
	return deoptStart + deoptBlockBytesAMD64(fn)
}

// emitPassthroughBlockAMD64 emits the shared passthrough deopt block:
// just the epilogue (which restores the caller's regs and RETs with
// RAX carrying whatever the inner deopt left in it). The status word
// is left untouched so jitCall sees the callee's status code.
func emitPassthroughBlockAMD64(fn *vm3.Function) []byte {
	return emitEpilogueAMD64(nil, fn)
}

// hasRegRegDivModAMD64 mirrors hasRegRegDivMod.
func hasRegRegDivModAMD64(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpDivI64 || op.Code == vm3.OpModI64 {
			return true
		}
	}
	return false
}

// hoistsCellsPtrAMD64 reports whether the AMD64 prologue should pre-
// compute cells.ptr from regsCell[0]'s handle and stash it in the
// regsI64[NumRegsI64] backing slot for the lifetime of the call. When
// active, OpListGet/OpListGetK/OpListSet emit the hot form (one disp32
// load from RBX instead of the listsBase + imul + cellsOff chain), and
// OpListPushI64 still reads the slab address cold (it needs cells.cap
// and cells.len fields, not just cells.ptr).
//
// Gates (Phase 6.3.4.n.2.f):
//
//   - NumRegsCell == 1: we only hoist one cell's slab; multi-cell
//     kernels would need a hoist slot per cell and a per-op disambiguator.
//   - has at least one OpListGet/OpListGetK/OpListSet/OpListPushI64:
//     otherwise the prologue would waste ~34 bytes computing a hoist
//     value nothing in the body reads.
//   - !isNonLeafAMD64(fn): any internal CALL (self-recursive or
//     cross-fn) may transitively trigger an OpListPushI64 grow, which
//     moves the arena slab and invalidates the hoisted cells.ptr.
//     Refreshing the hoist after every call would erase most of the
//     win, so we just skip the optimization for non-leaf fns.
//   - !isCellBankAMD64(fn) is false (i.e. fn IS cell-bank): RBP must
//     hold the regsCell base for the prologue to read the handle from.
//
// The hoist disp is hoistDispAMD64(fn) = NumRegsI64 * 8: just past the
// last pinned slot, into a regsI64 backing word that NumRegsI64 stops
// short of. The interpreter never reads that slot (sized by NumRegsI64),
// so the JIT can park a scratch value there without corrupting deopt
// state. The slot is only valid for the duration of one trampoline
// call; jitCall does not preserve it across calls (it is not in the
// pinned-slot range).
func hoistsCellsPtrAMD64(fn *vm3.Function) bool {
	if !isCellBankAMD64(fn) {
		return false
	}
	if fn.NumRegsCell != 1 {
		return false
	}
	if isNonLeafAMD64(fn) {
		return false
	}
	if !(hasListGetI64(fn) || hasListGetI64K(fn) || hasListSetI64(fn) || hasListPushI64(fn)) {
		return false
	}
	return true
}

// hoistDispAMD64 returns the disp from RBX (= &regsI64[0]) at which the
// hoisted cells.ptr is stashed. The slot is just past the live pinned
// regs (NumRegsI64 * 8). Caller must check hoistsCellsPtrAMD64(fn)
// first; the disp is undefined otherwise.
func hoistDispAMD64(fn *vm3.Function) int32 {
	return int32(fn.NumRegsI64) * 8
}

// hoistPrologueBytesAMD64 returns the additional prologue byte count
// needed to compute the hoisted cells.ptr when hoistsCellsPtrAMD64(fn)
// is true. The sequence is:
//
//	mov  disp32(%rbp), %eax     ; idx = regsCell[0] low 32 (6B)
//	imul $stride, %rax, %rax    ; rax = idx*sizeof(vmList) (7B)
//	mov  listsBaseOff(%r14), %rcx ; rcx = arenas.Lists base (7B)
//	add  %rcx, %rax             ; rax = &arenas.Lists[idx] (3B)
//	mov  cellsOff(%rax), %rax   ; rax = cells.ptr          (7B)
//	mov  %rax, hoistDisp(%rbx)  ; store at hoist slot      (4 or 7B)
//
// disp8 fits when hoistDispAMD64(fn) ∈ [-128, 127]; AMD64 cell-bank
// caps NumRegsI64 at 8, so disp is 0..64 — always disp8.
func hoistPrologueBytesAMD64(fn *vm3.Function) int {
	if !hoistsCellsPtrAMD64(fn) {
		return 0
	}
	storeBytes := 4 // mov %rax, disp8(%rbx)
	if hoistDispAMD64(fn) < -128 || hoistDispAMD64(fn) > 127 {
		storeBytes = 7
	}
	return 6 + 7 + 7 + 3 + 7 + storeBytes
}

// computeCallSpillsAMD64 runs backward liveness exactly like the
// AArch64 backend, but emits a mask over slots 0..5 (caller-saved on
// AMD64) instead of 0..6.
func computeCallSpillsAMD64(fn *vm3.Function) []uint32 {
	n := len(fn.Code)
	spills := make([]uint32, n)
	liveIn := make([]uint32, n+1)
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
		case vm3.OpCallI64:
			mask := uint32(0x3F) // caller-saved slots 0..5
			dstBit := uint32(1) << op.A
			spills[i] = (liveOut[i] &^ dstBit) & mask
		case vm3.OpCallMixed:
			// Phase 6.3.4.m.4c.5: self-recursive OpCallMixed spills the
			// same caller-saved slot 0..5 set as OpCallI64. Excluded only
			// when the result lands back in slot A and the bank is I64;
			// for BankCell results the destination is a regsCell slot, so
			// no i64 bit is reused, and all live caller-saved slots are
			// spilled.
			mask := uint32(0x3F)
			live := liveOut[i] & mask
			if vm3.Bank(op.BankFlags&0x3) == vm3.BankI64 {
				live &^= uint32(1) << op.A
			}
			spills[i] = live
		}
	}
	return spills
}

// lowerAMD64 returns the x86_64 byte stream for fn. Mirrors the AArch64
// two-pass structure: pass 1 computes pcMap[i] = byte offset where the
// lowering of bytecode i starts; pass 2 emits bytes and resolves branch
// destinations through pcMap. opts.SelfIdx (>= 0) enables self-recursive
// OpCallI64 as a native CALL inside the same code page.
func lowerAMD64(fn *vm3.Function, opts Options) ([]byte, error) {
	if fn.NumRegsF64 > 0 && isNonLeafAMD64(fn) {
		return nil, fmt.Errorf("%w: %s has both f64 regs and OpCallI64 (Phase 6.2b leaves f64+self-recursion to a later sub-phase)",
			ErrNotImplemented, fn.Name)
	}
	if isCellBankAMD64(fn) && fn.NumRegsF64 > 0 && int(fn.NumRegsI64) > 6 {
		// Phase 6.3.4.n.3: Cell+F64 fns pin the regsF64 base in R12
		// (i64 slot 6). The remaining usable i64 slots are 0..5, i.e.
		// NumRegsI64 <= 6.
		return nil, fmt.Errorf("%w: %s has Cell+F64 banks with NumRegsI64=%d (>6); R12 reserved for regsF64 base on AMD64",
			ErrNotImplemented, fn.Name, fn.NumRegsI64)
	}
	if isCellBankAMD64(fn) && fn.NumRegsF64 == 0 && int(fn.NumRegsI64) > 8 {
		return nil, fmt.Errorf("%w: %s has Cell bank with NumRegsI64=%d (>8); R14 reserved for *jitArenaCtx",
			ErrNotImplemented, fn.Name, fn.NumRegsI64)
	}
	prologueBytes := prologueLenAMD64(fn)
	spillSets := computeCallSpillsAMD64(fn)

	pcMap := make([]int, len(fn.Code)+1)
	pcMap[0] = prologueBytes
	for i, op := range fn.Code {
		n, err := byteCountAMD64(fn, op, opts, spillSets, i)
		if err != nil {
			return nil, fmt.Errorf("vm3jit/%s: pc %d: %w", fn.Name, i, err)
		}
		pcMap[i+1] = pcMap[i] + n
	}
	deoptStart := pcMap[len(fn.Code)]
	passthroughBytes := passthroughBlockBytesAMD64(fn, opts)
	total := deoptStart + deoptBlockBytesAMD64(fn) + passthroughBytes

	buf := make([]byte, 0, total)
	buf = emitPrologueAMD64(buf, fn)
	for i, op := range fn.Code {
		emit, err := emitInstrAMD64(fn, op, i, pcMap, deoptStart, opts, spillSets)
		if err != nil {
			return nil, fmt.Errorf("vm3jit/%s: pc %d op=%d: %w", fn.Name, i, op.Code, err)
		}
		if got, want := len(emit), pcMap[i+1]-pcMap[i]; got != want {
			return nil, fmt.Errorf("vm3jit/%s: pc %d op=%d: emitted %d bytes, predicted %d",
				fn.Name, i, op.Code, got, want)
		}
		buf = append(buf, emit...)
	}
	if len(buf) != deoptStart {
		return nil, fmt.Errorf("vm3jit/%s: code stream %d bytes, predicted %d",
			fn.Name, len(buf), deoptStart)
	}
	for _, status := range deoptStatusesUsedAMD64(fn) {
		buf = append(buf, emitDeoptBlockAMD64(fn, status)...)
	}
	if passthroughBytes > 0 {
		buf = append(buf, emitPassthroughBlockAMD64(fn)...)
	}
	if len(buf) != total {
		return nil, fmt.Errorf("vm3jit/%s: final stream %d bytes, predicted %d",
			fn.Name, len(buf), total)
	}
	return buf, nil
}

// prologueLenAMD64 returns the byte length of the prologue for fn. Used
// to seed pcMap[0].
func prologueLenAMD64(fn *vm3.Function) int {
	n := int(fn.NumRegsI64)
	nF := int(fn.NumRegsF64)
	pushes := numCalleeSavedPushesAMD64(fn)
	// push rbx, push r15: each REX.B-extended push of r12..r15 is 2
	// bytes; push rbx is 1 byte. Compute exactly.
	bytes := 0
	bytes += pushBytes(xRBX)
	bytes += pushBytes(xR15)
	if n > 6 || usesR12ForF64AMD64(fn) {
		bytes += pushBytes(xR12)
	}
	if n > 7 {
		bytes += pushBytes(xR13)
	}
	if n > 8 || usesR14ForF64(fn) || isCellBankAMD64(fn) {
		bytes += pushBytes(xR14)
	}
	if n > 9 || isCellBankAMD64(fn) {
		bytes += pushBytes(xRBP)
	}
	if alignFixupBytesAMD64(fn) != 0 {
		bytes += 4 // sub $8, %rsp == REX.W 83 /5 ib (4 bytes)
	}
	// mov %rdi, %rbx (3 bytes), mov %rsi, %r15 (3 bytes).
	bytes += 3 + 3
	// mov %rdx, %r14 (3 bytes) when R14 holds regsF64 base.
	if usesR14ForF64(fn) {
		bytes += 3
	}
	// Cell-bank: mov %rcx, %rbp (3) + mov %r8, %r14 (3).
	if isCellBankAMD64(fn) {
		bytes += 3 + 3
	}
	// Cell-bank+f64: mov %rdx, %r12 (3) for the regsF64 base.
	if usesR12ForF64AMD64(fn) {
		bytes += 3
	}
	// Each pinned i64 slot load is mov disp32(%rbx), <reg> = 7 bytes.
	bytes += 7 * n
	// Each pinned f64 slot load is movsd disp32(%base), xmm<r> = 9 bytes
	// (REX.B-prefixed because the f64 base is always R12 or R14).
	bytes += 9 * nF
	// Phase 6.3.4.n.2.f: cells.ptr hoist setup (cold form: 0 bytes when
	// gate fails; ~34 bytes when active). See hoistPrologueBytesAMD64.
	bytes += hoistPrologueBytesAMD64(fn)
	_ = pushes
	return bytes
}

// pushBytes returns the byte length of `push <reg>`: 1 for RAX..RDI,
// 2 for R8..R15 (REX.B prefix needed).
func pushBytes(r int) int {
	if r >= 8 {
		return 2
	}
	return 1
}

// popBytes mirrors pushBytes for `pop <reg>`.
func popBytes(r int) int { return pushBytes(r) }

// emitPrologueAMD64 appends the prologue to buf:
//
//	push %rbx                                    ; save caller's RBX
//	push %r15                                    ; save caller's R15
//	[push %r12..%r14 if their slot is used]
//	[sub $8, %rsp if non-leaf and pushes even]
//	mov %rdi, %rbx                               ; RBX = regs base
//	mov %rsi, %r15                               ; R15 = status ptr
//	mov 0*8(%rbx), <slot 0 reg>                  ; load each pinned slot
//	mov 1*8(%rbx), <slot 1 reg>
//	...
func emitPrologueAMD64(buf []byte, fn *vm3.Function) []byte {
	n := int(fn.NumRegsI64)
	nF := int(fn.NumRegsF64)
	buf = append(buf, push64(xRBX)...)
	buf = append(buf, push64(xR15)...)
	if n > 6 || usesR12ForF64AMD64(fn) {
		buf = append(buf, push64(xR12)...)
	}
	if n > 7 {
		buf = append(buf, push64(xR13)...)
	}
	if n > 8 || usesR14ForF64(fn) || isCellBankAMD64(fn) {
		buf = append(buf, push64(xR14)...)
	}
	if n > 9 || isCellBankAMD64(fn) {
		buf = append(buf, push64(xRBP)...)
	}
	if alignFixupBytesAMD64(fn) != 0 {
		buf = append(buf, subRSPImm8(8)...)
	}
	buf = append(buf, mov64RR(xRDI, xRBX)...)
	buf = append(buf, mov64RR(xRSI, xR15)...)
	if usesR14ForF64(fn) {
		buf = append(buf, mov64RR(xRDX, xR14)...)
	}
	if isCellBankAMD64(fn) {
		buf = append(buf, mov64RR(xRCX, xRBP)...)
		buf = append(buf, mov64RR(xR8, xR14)...)
	}
	if usesR12ForF64AMD64(fn) {
		buf = append(buf, mov64RR(xRDX, xR12)...)
	}
	for r := 0; r < n; r++ {
		buf = append(buf, mov64LoadDisp32(r2xAMD64(uint16(r)), xRBX, int32(r*8))...)
	}
	f64Base := f64BaseGPRAMD64(fn)
	for r := 0; r < nF; r++ {
		buf = append(buf, movsdLoadXMMFromGPR(r, f64Base, int32(r*8))...)
	}
	if hoistsCellsPtrAMD64(fn) {
		stride := int64(vm3.JITListSlabStride())
		cellsOff := int32(vm3.JITListCellsOffset())
		listsBaseOff := int32(jitArenaCtxListsBaseOff())
		hoistDisp := hoistDispAMD64(fn)
		buf = append(buf, mov32LoadDisp32(xRAX, xRBP, 0)...)
		buf = append(buf, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		buf = append(buf, mov64LoadDisp32(xRCX, xR14, listsBaseOff)...)
		buf = append(buf, add64RR(xRCX, xRAX)...)
		buf = append(buf, mov64LoadDisp32(xRAX, xRAX, cellsOff)...)
		if hoistDisp >= -128 && hoistDisp <= 127 {
			buf = append(buf, mov64StoreDisp8(xRAX, xRBX, int8(hoistDisp))...)
		} else {
			buf = append(buf, mov64StoreDisp32(xRAX, xRBX, hoistDisp)...)
		}
	}
	return buf
}

// emitEpilogueAMD64 appends the prologue's reverse to buf. The
// caller-side `mov xA, %rax` (for OpReturnI64) or `mov $imm, %rax`
// (for OpReturnConstK) must precede the epilogue so RAX carries the
// result through the pops.
func emitEpilogueAMD64(buf []byte, fn *vm3.Function) []byte {
	n := int(fn.NumRegsI64)
	if alignFixupBytesAMD64(fn) != 0 {
		buf = append(buf, addRSPImm8(8)...)
	}
	if n > 9 || isCellBankAMD64(fn) {
		buf = append(buf, pop64(xRBP)...)
	}
	if n > 8 || usesR14ForF64(fn) || isCellBankAMD64(fn) {
		buf = append(buf, pop64(xR14)...)
	}
	if n > 7 {
		buf = append(buf, pop64(xR13)...)
	}
	if n > 6 || usesR12ForF64AMD64(fn) {
		buf = append(buf, pop64(xR12)...)
	}
	buf = append(buf, pop64(xR15)...)
	buf = append(buf, pop64(xRBX)...)
	buf = append(buf, retByte())
	return buf
}

// epilogueBytesAMD64 returns the byte length of the epilogue emitted by
// emitEpilogueAMD64 (excluding the preceding result-into-RAX move).
func epilogueBytesAMD64(fn *vm3.Function) int {
	n := int(fn.NumRegsI64)
	bytes := 0
	if alignFixupBytesAMD64(fn) != 0 {
		bytes += 4 // add $8, %rsp
	}
	if n > 9 || isCellBankAMD64(fn) {
		bytes += popBytes(xRBP)
	}
	if n > 8 || usesR14ForF64(fn) || isCellBankAMD64(fn) {
		bytes += popBytes(xR14)
	}
	if n > 7 {
		bytes += popBytes(xR13)
	}
	if n > 6 || usesR12ForF64AMD64(fn) {
		bytes += popBytes(xR12)
	}
	bytes += popBytes(xR15)
	bytes += popBytes(xRBX)
	bytes++ // ret
	return bytes
}

// deoptStatusesUsedAMD64 returns the deopt status codes fn references,
// in emission order. Caller emits one block per status; the order here
// also fixes the per-status block offsets (StatusDivByZero first, then
// StatusListGrow / StatusMapGrow / StatusPairGrow). Returned slice is
// empty when fn has no deopt sites. Mirrors deoptStatusesUsedARM64.
func deoptStatusesUsedAMD64(fn *vm3.Function) []int64 {
	var s []int64
	if hasRegRegDivModAMD64(fn) {
		s = append(s, StatusDivByZero)
	}
	if hasListPushI64(fn) {
		s = append(s, StatusListGrow)
	}
	if hasNewPair(fn) {
		s = append(s, StatusPairGrow)
	}
	return s
}

// deoptBlockBytesAMD64Status returns the byte length of ONE deopt block.
// All blocks share the same shape (7-byte mov $status, (%r15) + epilogue)
// so the byte count is the same per block.
func deoptBlockBytesAMD64Status(fn *vm3.Function) int {
	return 7 + epilogueBytesAMD64(fn)
}

// deoptBlockBytesAMD64 returns the byte length of all per-status deopt
// blocks combined, or 0 if no opcode in fn can deopt.
func deoptBlockBytesAMD64(fn *vm3.Function) int {
	statuses := deoptStatusesUsedAMD64(fn)
	if len(statuses) == 0 {
		return 0
	}
	return len(statuses) * deoptBlockBytesAMD64Status(fn)
}

// deoptStartForStatusAMD64 returns the byte offset where the deopt block
// for `status` lives, given the first-block start (`baseStart`) in the
// emitted stream. Each block is deoptBlockBytesAMD64Status(fn) bytes
// long; the order matches deoptStatusesUsedAMD64.
func deoptStartForStatusAMD64(fn *vm3.Function, baseStart int, status int64) int {
	per := deoptBlockBytesAMD64Status(fn)
	for i, s := range deoptStatusesUsedAMD64(fn) {
		if s == status {
			return baseStart + i*per
		}
	}
	return baseStart
}

// emitDeoptBlockAMD64 emits the shared deopt block: write statusCode
// through *r15, then run the epilogue. Every guard branches here.
func emitDeoptBlockAMD64(fn *vm3.Function, statusCode int64) []byte {
	// `movq $statusCode, (%r15)` with imm32 sign-ext. statusCode must
	// fit in int32 (StatusPairGrow = 4, fine).
	out := movMemImm32R15Indirect(int32(statusCode))
	out = emitEpilogueAMD64(out, fn)
	return out
}

// byteCountAMD64 returns the exact byte length emitInstrAMD64 will emit
// for op. Used by pass 1 to lay out pcMap. spillSets and idx are only
// consulted for OpCallI64.
func byteCountAMD64(fn *vm3.Function, op vm3.Op, opts Options, spillSets []uint32, idx int) (int, error) {
	switch op.Code {
	case vm3.OpConstI64K:
		return movImm64ByteCount(int64(op.C), r2xAMD64(op.A)), nil
	case vm3.OpConstI64KW:
		k := int(uint16(op.C))
		if k >= len(fn.Consts) {
			return 0, fmt.Errorf("%w: ConstI64KW idx %d out of range", ErrNotImplemented, k)
		}
		return movImm64ByteCount(fn.Consts[k].Int(), r2xAMD64(op.A)), nil
	case vm3.OpMovI64:
		return 3, nil // mov r64, r64
	case vm3.OpAddI64:
		// Two-operand AMD64 form: dest is also one source. Cases:
		//   A == B : add xA, xC                              (3)
		//   A == C : add xA, xB (commutative, skips the mov) (3)
		//   else   : mov xB, xA; add xA, xC                  (6)
		if op.A == op.B || op.A == uint16(op.C) {
			return 3, nil
		}
		return 6, nil
	case vm3.OpSubI64:
		// Sub is not commutative, so A == C needs a different shape:
		//   A == B : sub xA, xC               (3)
		//   A == C : sub xA, xB; neg xA       (6)  — A = -(C - B) = B - C
		//   else   : mov xB, xA; sub xA, xC   (6)
		if op.A == op.B {
			return 3, nil
		}
		return 6, nil
	case vm3.OpMulI64:
		// imul is commutative, mirror OpAddI64:
		//   A == B : imul xA, xC                              (4)
		//   A == C : imul xA, xB (skip mov via commutativity) (4)
		//   else   : mov xB, xA; imul xA, xC                  (7)
		if op.A == op.B || op.A == uint16(op.C) {
			return 4, nil
		}
		return 7, nil
	case vm3.OpDivI64:
		// test xC, xC (3) ; jz deopt (6) ; mov %rXb, %rax (3) ;
		// cqo (2) ; idiv xC (3) ; mov %rax, xA (3).
		return 20, nil
	case vm3.OpModI64:
		// test (3) ; jz (6) ; mov %rXb, %rax (3) ; cqo (2) ;
		// idiv xC (3) ; mov %rdx, xA (3).
		return 20, nil
	case vm3.OpNegI64:
		// mov xA, xB if A != B (3) ; neg xA (3).
		if op.A == op.B {
			return 3, nil
		}
		return 6, nil
	case vm3.OpAddI64K, vm3.OpSubI64K:
		// mov xA, xB if A != B (3) ; add/sub xA, imm32 (7).
		if op.A == op.B {
			return 7, nil
		}
		return 10, nil
	case vm3.OpMulI64K:
		// imul xA, xB, imm32 (7) — three-operand form, no need to copy first.
		return 7, nil
	case vm3.OpDivI64K, vm3.OpModI64K:
		if op.C == 0 {
			return 0, fmt.Errorf("%w: opcode %d divide-by-zero immediate", ErrNotImplemented, op.Code)
		}
		// mov $imm, %rcx (7) ; mov xB, %rax (3) ; cqo (2) ; idiv %rcx (3) ; mov %rax|%rdx, xA (3).
		return 18, nil
	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br:
		// cmp xA, xB (3) ; jcc rel32 (6).
		return 9, nil
	case vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		// cmp xA, imm32 (7) ; jcc rel32 (6).
		return 13, nil
	case vm3.OpJump:
		return 5, nil // jmp rel32
	case vm3.OpReturnI64:
		// mov xA, %rax (3) + epilogue.
		return 3 + epilogueBytesAMD64(fn), nil
	case vm3.OpReturnConstK:
		// mov $imm, %rax + epilogue.
		return movImm64ByteCount(int64(op.C), xRAX) + epilogueBytesAMD64(fn), nil
	case vm3.OpReturnF64:
		// movq xmm<A>, %rax (5) + epilogue.
		return 5 + epilogueBytesAMD64(fn), nil
	case vm3.OpReturnCell:
		// mov disp32(%rbp), %rax (7) + epilogue. RBP holds the regsCell
		// base for cell-bank fns (Phase 6.3.4.m.4c.2); the cell handle
		// in slot A is loaded into RAX so the trampoline returns it
		// bit-for-bit through Go's uint64 result channel.
		return 7 + epilogueBytesAMD64(fn), nil

	case vm3.OpPairFst, vm3.OpPairSnd:
		// Phase 6.3.4.m.4c.3. Cold form (6 inst):
		//   mov  disp32(%rbp), %eax       ; idx = low 32 of regsCell[B] (zero-ext, 6B)
		//   imul $stride, %rax, %rax      ; rax = idx * 24 (REX.W 69 /r imm32, 7B)
		//   mov  pairsBaseOff(%r14), %rcx ; rcx = pairsBase (REX.WB 8B /r disp32, 7B)
		//   add  %rcx, %rax               ; rax = pairsBase + idx*stride (REX.W 01 /r, 3B)
		//   mov  fst/sndOff(%rax), %rcx   ; rcx = fst/snd Cell (REX.W 8B /r disp32, 7B)
		//   mov  %rcx, disp32(%rbp)       ; regsCell[A] = rcx (REX.W 89 /r disp32, 7B)
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + 7 + 7, nil

	case vm3.OpNewPair:
		// Phase 6.3.4.m.4c.4. Inline pair allocator on AMD64 cell-bank.
		// Mirrors the ARM64 ws kernel: bumps a snapshot of pairsLen kept
		// in jitArenaCtx, deopts on cap exhaustion via StatusPairGrow.
		// Cold form (18 inst):
		//   mov  pairsLenOff(%r14), %rax       ; 7B  rax = pairsLen
		//   mov  pairsCapOff(%r14), %rcx       ; 7B  rcx = pairsCap
		//   cmp  %rcx, %rax                    ; 3B  rax vs rcx
		//   jae  deopt_pairgrow                ; 6B  rel32
		//   mov  pairsBaseOff(%r14), %rdx      ; 7B  rdx = pairsBase
		//   imul $stride, %rax, %rcx           ; 7B  rcx = pairsLen * 24
		//   add  %rdx, %rcx                    ; 3B  rcx = pairsBase + idx*stride
		//   movl $0x10000, (%rcx)              ; 6B  header u32 = flagAlive<<16
		//   mov  disp32(%rbp), %rdx            ; 7B  rdx = regsCell[B] (fst)
		//   mov  %rdx, fstOff(%rcx)            ; 7B  store fst (disp32 form)
		//   mov  disp32(%rbp), %rdx            ; 7B  rdx = regsCell[uint16(C)] (snd)
		//   mov  %rdx, sndOff(%rcx)            ; 7B  store snd (disp32 form)
		//   mov  %eax, %edx                    ; 2B  rdx = idx, high 32 zeroed
		//   movabs $0xFFFF800000000000, %rcx   ; 10B handle tag bits constant
		//   or   %rcx, %rdx                    ; 3B  rdx = handle
		//   mov  %rdx, disp32(%rbp)            ; 7B  regsCell[A] = handle
		//   inc  %rax                          ; 3B  pairsLen++
		//   mov  %rax, pairsLenOff(%r14)       ; 7B  commit pairsLen
		return 7 + 7 + 3 + 6 + 7 + 7 + 3 + 6 + 7 + 7 + 7 + 7 + 2 + 10 + 3 + 7 + 3 + 7, nil

	case vm3.OpListGetI64:
		// Phase 6.3.4.n.2.a cold form, AMD64 mirror of the ARM64
		// OpListGetI64 (no hoist):
		//   mov  disp32(%rbp), %eax       ; idx = low 32 of regsCell[B] (zero-ext, 6B)
		//   imul $stride, %rax, %rax      ; rax = idx * sizeof(vmList) (REX.W 69 /r imm32, 7B)
		//   mov  listsBaseOff(%r14), %rcx ; rcx = arenas.Lists base (REX.WB 8B /r disp32, 7B)
		//   add  %rcx, %rax               ; rax = &arenas.Lists[idx] (REX.W 01 /r, 3B)
		//   mov  cellsOff(%rax), %rax     ; rax = cells.ptr (REX.W 8B /r disp32, 7B)
		//   mov  [%rax + xIdx*8], %rax    ; rax = cells[regsI64[C]] (REX.W 8B /r SIB, 4B)
		//   shl  $16, %rax                ; SBFX prep                  (REX.W C1 /4 ib, 4B)
		//   sar  $16, %rax                ; sign-extend low 48 bits   (REX.W C1 /7 ib, 4B)
		//   mov  %rax, x_A                ; regsI64[A] = signed payload (REX.W 89 /r, 3B)
		//
		// Phase 6.3.4.n.2.f hot form (hoistsCellsPtrAMD64 true): the
		// prologue already stashed cells.ptr at disp8(%rbx). We replace
		// the 5-instruction slab/cells.ptr compute chain (mov32, imul,
		// mov listsBase, add, mov cellsOff = 30B) with a single
		// mov64LoadDisp8 (4B), saving 26B per op.
		//   mov  hoistDisp(%rbx), %rax    ; rax = cells.ptr            (4B)
		//   mov  [%rax + xIdx*8], %rax    ; rax = cells[regsI64[C]]    (4B)
		//   shl  $16, %rax                ; SBFX prep                  (4B)
		//   sar  $16, %rax                ; sign-extend low 48 bits    (4B)
		//   mov  %rax, x_A                ; regsI64[A] = signed payload (3B)
		if hoistsCellsPtrAMD64(fn) {
			return 4 + 4 + 4 + 4 + 3, nil
		}
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + 7 + 4 + 4 + 4 + 3, nil

	case vm3.OpListGetI64K:
		// Phase 6.3.4.n.2.e: constant-index list read. Mirrors
		// OpListGetI64 cold form but bakes the (uint16(C)) idx into a
		// disp32 load instead of the SIB form, freeing the i64 reg
		// that would have held the loop index. Trade: the SIB load is
		// 4 bytes, the disp32 load is 7 bytes, so the op is 3 bytes
		// longer than the reg-form; the win is one fewer live i64
		// register, which matters for fannkuch_redux (squeezes
		// NumRegsI64 from 10 to 8 to fit the AMD64 cell-bank cap).
		//   mov  disp32(%rbp), %eax       ; idx = low 32 of regsCell[B] (zero-ext, 6B)
		//   imul $stride, %rax, %rax      ; rax = idx * sizeof(vmList) (7B)
		//   mov  listsBaseOff(%r14), %rcx ; rcx = arenas.Lists base    (7B)
		//   add  %rcx, %rax               ; rax = &arenas.Lists[idx]   (3B)
		//   mov  cellsOff(%rax), %rax     ; rax = cells.ptr            (7B)
		//   mov  disp32(%rax), %rax       ; rax = cells[idxK]          (7B; idxK*8 in disp32)
		//   shl  $16, %rax                ; SBFX prep                  (4B)
		//   sar  $16, %rax                ; sign-extend low 48 bits    (4B)
		//   mov  %rax, x_A                ; regsI64[A] = signed payload (3B)
		//
		// Phase 6.3.4.n.2.f hot form (hoistsCellsPtrAMD64 true): swap
		// the 5-instruction slab/cells.ptr chain (30B) for the disp8
		// load of the stashed cells.ptr (4B). The disp32 load of the
		// constant-indexed cell is unchanged (7B); shorter disp8 form
		// when idxK*8 fits in [-128, 127] saves another 3B.
		//   mov  hoistDisp(%rbx), %rax    ; rax = cells.ptr            (4B)
		//   mov  idxK*8(%rax), %rax       ; rax = cells[idxK]          (4 or 7B)
		//   shl  $16, %rax                ; SBFX prep                  (4B)
		//   sar  $16, %rax                ; sign-extend low 48 bits    (4B)
		//   mov  %rax, x_A                ; regsI64[A] = signed payload (3B)
		if hoistsCellsPtrAMD64(fn) {
			idxK := int32(uint16(op.C)) * 8
			load := 7
			if idxK >= -128 && idxK <= 127 {
				load = 4
			}
			return 4 + load + 4 + 4 + 3, nil
		}
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + 7 + 7 + 4 + 4 + 3, nil

	case vm3.OpListSetI64:
		// Phase 6.3.4.n.2.b cold form, AMD64 mirror of the ARM64
		// OpListSetI64 (no hoist):
		//   mov  disp32(%rbp), %eax       ; idx = low 32 of regsCell[A] (6B)
		//   imul $stride, %rax, %rax      ; rax = idx*sizeof(vmList)    (7B)
		//   mov  listsBaseOff(%r14), %rcx ; rcx = arenas.Lists base    (7B)
		//   add  %rcx, %rax               ; rax = &arenas.Lists[idx]   (3B)
		//   mov  cellsOff(%rax), %rax     ; rax = cells.ptr            (7B)
		//   mov  xVal, %rdx               ; rdx = val                  (3B)
		//   shl  $16, %rdx                ; rdx <<= 16                  (4B)
		//   shr  $16, %rdx                ; rdx = val & 0xFFFF_FFFF_FFFF (4B)
		//   movabs $0xFFFA<<48, %rcx      ; rcx = Int48 tag             (10B)
		//   or   %rcx, %rdx               ; rdx = tagged payload       (3B)
		//   mov  %rdx, [%rax + xIdx*8]    ; cells[regsI64[C]] = packed (4B)
		//
		// Phase 6.3.4.n.2.f hot form (hoistsCellsPtrAMD64 true): swap
		// the 5-instruction slab/cells.ptr chain (30B) for the disp8
		// load (4B), and replace the mask-and-OR pack (3+4+4+10+3+4=28B)
		// with the OpListPushI64-style "raw SIB store + 16-bit tag
		// overwrite" pair (4+7=11B). The high 16 bits of xVal leak into
		// the cell store, but the 16-bit tag immediate overwrites them
		// in the same line; OpListGetI64's shl/sar pair sign-extends
		// the low 48 regardless of what was there, so round-trip is
		// preserved.
		//   mov  hoistDisp(%rbx), %rax        ; rax = cells.ptr        (4B)
		//   mov  xVal, [%rax + xIdx*8]        ; cells[idx] = raw xVal  (4B)
		//   movw $0xFFFA, 6(%rax,%xIdx,8)     ; overwrite tag bytes    (7 or 8B)
		// The tag-store is 8B when xIdx >= R8 (REX.X needed) and 7B
		// otherwise; the SIB-store is always 4B because mov64StoreIdxLsl3
		// emits REX.W unconditionally.
		if hoistsCellsPtrAMD64(fn) {
			xIdx := r2xAMD64(uint16(op.C))
			tagBytes := 7
			if xIdx >= 8 {
				tagBytes = 8
			}
			return 4 + 4 + tagBytes, nil
		}
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + 7 + 3 + 4 + 4 + 10 + 3 + 4, nil

	case vm3.OpListPushI64:
		// Phase 6.3.4.n.2.c cold form, AMD64 mirror of the ARM64
		// OpListPushI64 (no hoist):
		//   mov  disp32(%rbp), %eax        ; idx = low 32 of regsCell[A] (6B)
		//   imul $stride, %rax, %rax       ; rax = idx*sizeof(vmList)    (7B)
		//   mov  listsBaseOff(%r14), %rcx  ; rcx = arenas.Lists base    (7B)
		//   add  %rcx, %rax                ; rax = &arenas.Lists[idx]  (3B)
		//   mov  cellsLenOff(%rax), %rcx   ; rcx = cells.len           (7B)
		//   mov  cellsCapOff(%rax), %rdx   ; rdx = cells.cap           (7B)
		//   cmp  %rdx, %rcx                ; flags = len - cap         (3B)
		//   jae  deopt_listgrow            ; len >= cap → deopt        (6B)
		//   mov  cellsOff(%rax), %rdx      ; rdx = cells.ptr           (7B)
		//   mov  xVal, [%rdx + %rcx*8]     ; cells[len] = raw xVal     (4B)
		//   movw $0xFFFA, 6(%rdx,%rcx,8)   ; overwrite tag bytes 6,7   (7B)
		//   inc  %rcx                      ; len++                     (3B)
		//   mov  %rcx, cellsLenOff(%rax)   ; commit cells.len          (7B)
		//   mov  %ecx, 4(%rax)             ; commit vmList.len (u32)   (3B)
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + 7 + 7 + 3 + 6 + 7 + 4 + 7 + 3 + 7 + 3, nil

	case vm3.OpNewList:
		// Phase 6.3.4.n.2.c: when fn.JITPreAllocList is set the JIT skips
		// fn.Code[0]'s OpNewList entirely; jitCall (Go side) writes the
		// pre-allocated list handle into jf.regsCell[A] before the
		// trampoline call. Same skip rule for the contiguous K-prefix.
		if idx == 0 && fn.JITPreAllocList {
			return 0, nil
		}
		if idx < int(fn.JITPreAllocListPrefix) {
			return 0, nil
		}
		return 0, fmt.Errorf("%w: opcode %d (inline NewList unsupported)", ErrNotImplemented, op.Code)

	case vm3.OpConstF64K:
		idx := int(uint16(op.C))
		if idx >= len(fn.Consts) {
			return 0, fmt.Errorf("%w: ConstF64K idx %d out of range", ErrNotImplemented, idx)
		}
		bits := int64(uint64(fn.Consts[idx]))
		// mov $imm, %rcx (7 or 10) ; movq xmm<A>, %rcx (5).
		return movImm64ByteCount(bits, xRCX) + 5, nil

	case vm3.OpMovF64:
		// movsd xmm<A>, xmm<B>: 4 bytes (xmm0..7 only, no REX needed).
		return 4, nil
	case vm3.OpAddF64, vm3.OpSubF64, vm3.OpMulF64, vm3.OpDivF64:
		// optional movsd xmm<A>, xmm<B> (4) ; <op>sd xmm<A>, xmm<C> (4).
		if op.A == op.B {
			return 4, nil
		}
		return 8, nil
	case vm3.OpNegF64:
		// mov $signbit, %rcx (10) ; movq xmm15, %rcx (5, REX.W+R) ;
		// optional movsd xmm<A>, xmm<B> (4) ; xorpd xmm<A>, xmm15 (5, REX.B).
		if op.A == op.B {
			return 10 + 5 + 5, nil
		}
		return 10 + 5 + 4 + 5, nil

	case vm3.OpFmaF64:
		// Phase 6.3.4.h.2 AMD64 lowering: VFMADDxxxSD selected so dst
		// stays in xmm<A> and the three FMA operands map to vm3's
		// semantics A = B*mul2 + addend without extra moves when one of
		// the operands already aliases A. All four code paths emit a
		// single 5-byte VEX-encoded instruction; only the catch-all
		// (no alias) needs a leading movsd (4 bytes) to set A := B
		// before the 132 form.
		mul2 := int(uint16(op.C) & 0xFF)
		addend := int((uint16(op.C) >> 8) & 0xFF)
		a, b := int(op.A), int(op.B)
		if a == b || a == addend || a == mul2 {
			return 5, nil
		}
		return 4 + 5, nil
	case vm3.OpSqrtF64:
		// optional movsd xmm<A>, xmm<B> (4) ; sqrtsd xmm<A>, xmm<A> (4).
		// (sqrtsd's source can match its dest, so we always operate
		// in-place after the optional copy.)
		if op.A == op.B {
			return 4, nil
		}
		return 8, nil

	case vm3.OpCmpEqF64Br, vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br:
		// ucomisd xmm<A>, xmm<B> (4) ; jp +6 (6) ; jcc rel32 (6).
		return 4 + 6 + 6, nil
	case vm3.OpCmpNeF64Br:
		// ucomisd (4) ; jp target (6) ; jne target (6).
		return 4 + 6 + 6, nil
	case vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br:
		// ucomisd (4) ; jcc rel32 (6).
		return 4 + 6, nil

	case vm3.OpI64ToF64:
		// cvtsi2sd xmm<A>, %r<B> (5, F2 REX.W 0F 2A /r — 5 bytes when
		// xmm0..7 and i64 reg needs REX.B for r >= 8).
		return cvtsi2sdByteCount(op.B), nil
	case vm3.OpF64ToI64:
		// cvttsd2si %r<A>, xmm<B>: F2 REX.W 0F 2C /r — 5 bytes.
		return cvttsd2siByteCount(op.A), nil
	case vm3.OpLookupI64KW:
		// Phase 6.4.c cold form: movabs %rax, &table[0] (10 bytes)
		// then mov %xDst, [%rax + %xIdx*8] (3..4 bytes depending on
		// dst/idx REX needs). Hoist support deferred; the table base
		// load is per-site for now.
		tableIdx := int(uint16(op.C))
		if tableIdx >= len(fn.I64Tables) || len(fn.I64Tables[tableIdx]) == 0 {
			return 0, fmt.Errorf("%w: LookupI64KW table %d out of range or empty",
				ErrNotImplemented, tableIdx)
		}
		base := int64(uintptr(unsafe.Pointer(&fn.I64Tables[tableIdx][0])))
		return movImm64ByteCount(base, xRAX) + mov64LoadIdxLsl3ByteCount(r2xAMD64(op.A), xRAX, r2xAMD64(op.B)), nil

	case vm3.OpF64ArrayGetF64:
		// Phase 6.3.4.n.3 cold form (AMD64 mirror of ARM64 OpF64ArrayGetF64).
		// regsF64[A] = arenas.F64Arrs[handleIdx(regsCell[B])].data[regsI64[C]].
		//   mov  disp32(%rbp), %eax       ; idx = low 32 of regsCell[B]   (6B)
		//   imul $stride, %rax, %rax      ; rax = idx * sizeof(vmF64Array) (7B)
		//   mov  f64ArrsBaseOff(%r14), %rcx ; rcx = arenas.F64Arrs base   (7B)
		//   add  %rcx, %rax               ; rax = &arenas.F64Arrs[idx]    (3B)
		//   mov  dataOff(%rax), %rax      ; rax = data.ptr                (7B)
		//   movsd  [%rax + xIdx*8], xmm<A> ; xmm<A> = data[regsI64[C]]    (6B SIB-scale3)
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + 7 + 6, nil
	case vm3.OpF64ArraySetF64:
		// Phase 6.3.4.n.3 cold form (AMD64 mirror of ARM64 OpF64ArraySetF64).
		// arenas.F64Arrs[handleIdx(regsCell[A])].data[regsI64[C]] = regsF64[B].
		// Same byte budget as Get: 6+7+7+3+7+6.
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + 7 + 6, nil
	case vm3.OpF64ArrayLenI64:
		// Phase 6.3.4.n.3 cold form: load the slab's u32 .len into a
		// dest GPR (zero-extend to 64 bits).
		//   mov  disp32(%rbp), %eax       ; idx = low 32 of regsCell[B] (6B)
		//   imul $stride, %rax, %rax      ;                              (7B)
		//   mov  f64ArrsBaseOff(%r14), %rcx ;                            (7B)
		//   add  %rcx, %rax               ;                              (3B)
		//   mov  lenOff(%rax), %eax       ; rax<-len (32-bit form zero-ext, 6B disp32 form)
		//   mov  %rax, x_A                ;                              (3B)
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + mov32LoadDisp32ByteCount(xRAX, xRAX) + 3, nil
	case vm3.OpI64ArrayGetI64:
		// Phase 6.3.4.n.3 cold form (mirror OpF64ArrayGetF64 but the
		// final load is mov64 SIB into the i64 dest reg, 4B).
		//   mov  disp32(%rbp), %eax       ; idx                          (6B)
		//   imul $stride, %rax, %rax      ;                              (7B)
		//   mov  i64ArrsBaseOff(%r14), %rcx ;                            (7B)
		//   add  %rcx, %rax               ;                              (3B)
		//   mov  dataOff(%rax), %rax      ; rax = data.ptr               (7B)
		//   mov  [%rax + xIdx*8], xA      ; xA = data[idx]               (4B)
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + 7 + 4, nil
	case vm3.OpI64ArraySetI64:
		// Phase 6.3.4.n.3 cold form: same shape as Get but final is a
		// 4-byte SIB store of xB through [rax + xIdx*8].
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + 7 + 4, nil
	case vm3.OpI64ArrayLenI64:
		return mov32LoadDisp32ByteCount(xRAX, xRBP) + 7 + 7 + 3 + mov32LoadDisp32ByteCount(xRAX, xRAX) + 3, nil
	case vm3.OpNewF64Array:
		// Phase 6.3.4.n.3: jitCall pre-allocates the slab and writes the
		// handle into regsCell[A]; the emit step writes zero bytes when
		// the op is in the pre-alloc K-prefix.
		if idx < int(fn.JITPreAllocF64ArrPrefix) {
			return 0, nil
		}
		return 0, fmt.Errorf("%w: opcode %d (inline NewF64Array unsupported)", ErrNotImplemented, op.Code)
	case vm3.OpNewI64Array:
		if idx < int(fn.JITPreAllocI64ArrPrefix) {
			return 0, nil
		}
		return 0, fmt.Errorf("%w: opcode %d (inline NewI64Array unsupported)", ErrNotImplemented, op.Code)

	case vm3.OpCallI64:
		if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx {
			return 0, fmt.Errorf("%w: CallI64 to non-self idx %d (SelfIdx=%d)",
				ErrNotImplemented, uint16(op.C), opts.SelfIdx)
		}
		nSpill := popcount32(spillSets[idx])
		nArgs := fn.NumI64Params()
		// Each spill store: mov xS, disp32(%rbx) = 7 bytes.
		// Each arg store: mov xS, disp32(%rbx) = 7 bytes.
		// lea N*8(%rbx), %rdi (7) — set RDI = callee window so the
		//   recursive callee's `mov %rdi, %rbx` prologue step receives
		//   a valid pointer rather than a stale slot-1 value.
		// mov %r15, %rsi (3) — propagate status pointer so the callee
		//   prologue's `mov %rsi, %r15` keeps R15 = *status.
		// call rel32 (5); mov %rax, xA (3).
		// Each spill reload: mov disp32(%rbx), xS = 7 bytes.
		return 7*nSpill + 7*nArgs + 7 + 3 + 5 + 3 + 7*nSpill, nil
	case vm3.OpCallMixed:
		// Phase 6.3.4.m.4c.5/.6: AMD64 cell-bank OpCallMixed (self and
		// cross-fn). Sequence:
		//   spill caller-saved i64 slots (7B each)
		//   write i64 args   (7B each)
		//   write cell args  (14B each: load via %rdx, store back)
		//   lea callerI64*8(%rbx), %rdi   (7B; 3B mov if callerI64=0)
		//   mov %r15, %rsi                (3B)
		//   lea callerCell*8(%rbp), %rcx  (7B; 3B mov if callerCell=0)
		//   mov %r14, %r8                 (3B)
		//   self:    call rel32 to byte 0         (5B)
		//   crossfn: movabs $callee.JITCode, %r10 (10B); call *%r10 (3B)
		//   reload spills                 (7B each)
		//   optional deopt check: cmpq $0,(%r15) (4B) + jne rel32 (6B)
		//   store result: mov %rax, xA (3B i64) or mov %rax, disp32(%rbp) (7B cell)
		calleeIdx := int(uint16(op.C))
		isSelf := opts.SelfIdx >= 0 && calleeIdx == opts.SelfIdx
		var callee *vm3.Function
		if isSelf {
			callee = fn
		} else {
			if opts.Prog == nil {
				return 0, fmt.Errorf("%w: CallMixed cross-fn needs opts.Prog", ErrNotImplemented)
			}
			if calleeIdx < 0 || calleeIdx >= len(opts.Prog.Funcs) {
				return 0, fmt.Errorf("%w: CallMixed callee idx %d out of range",
					ErrNotImplemented, calleeIdx)
			}
			callee = opts.Prog.Funcs[calleeIdx]
			if callee.JITCode == nil {
				return 0, fmt.Errorf("%w: CallMixed callee %s has no JITCode",
					ErrNotImplemented, callee.Name)
			}
		}
		nSpill := popcount32(spillSets[idx])
		callerI64 := int(fn.NumRegsI64)
		callerCell := int(fn.NumRegsCell)
		nI64Args, nF64Args, nCellArgs := countParamBanks(callee)
		if nF64Args != 0 {
			return 0, fmt.Errorf("%w: CallMixed with f64 args (AMD64 cell-bank rejects f64)",
				ErrNotImplemented)
		}
		argBytes := 7*nI64Args + 14*nCellArgs
		baseI64Bytes := 7
		if callerI64 == 0 {
			baseI64Bytes = 3
		}
		baseCellBytes := 7
		if callerCell == 0 {
			baseCellBytes = 3
		}
		// ABI setup: RDI = i64 base, RSI = status (mov %r15,%rsi 3B),
		// RCX = cell base, R8 = arena ctx (mov %r14,%r8 3B).
		abiBytes := baseI64Bytes + 3 + baseCellBytes + 3
		callBytes := 5
		if !isSelf {
			callBytes = 10 + 3 // movabs $imm, %r10 + call *%r10
		}
		deoptBytes := 0
		if isSelf && selfDeoptCalleeAMD64(fn) {
			deoptBytes = 4 + 6 // cmpq $0,(%r15) + jne rel32
		} else if !isSelf && crossFnDeoptCalleeAMD64(callee) {
			deoptBytes = 4 + 6
		}
		resultBytes := 3 // mov %rax, xA (BankI64)
		if vm3.Bank(op.BankFlags&0x3) == vm3.BankCell {
			resultBytes = 7 // mov %rax, disp32(%rbp)
		}
		return 7*nSpill + argBytes + abiBytes + callBytes + 7*nSpill + deoptBytes + resultBytes, nil
	default:
		return 0, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)
	}
}

// emitInstrAMD64 emits the x86_64 bytes for op at bytecode index idx.
// pcMap[i] is the byte offset where instruction i's lowering begins
// (pcMap[len(Code)] is also the byte offset of the fall-through past
// the last instruction and the deopt block start). deoptStart is the
// byte offset of the deopt block; guard sites compute a JZ rel32
// offset to it.
func emitInstrAMD64(fn *vm3.Function, op vm3.Op, idx int, pcMap []int, deoptStart int, opts Options, spillSets []uint32) ([]byte, error) {
	xA := r2xAMD64(op.A)
	xB := r2xAMD64(op.B)
	switch op.Code {
	case vm3.OpConstI64K:
		return movImm64(xA, int64(op.C)), nil
	case vm3.OpConstI64KW:
		v := fn.Consts[int(uint16(op.C))].Int()
		return movImm64(xA, v), nil
	case vm3.OpMovI64:
		return mov64RR(xB, xA), nil
	case vm3.OpAddI64:
		// Commutative; collapse A == C via commutativity to avoid
		// `mov xB, xA` clobbering xC when A and C share a register.
		xC := r2xAMD64(uint16(op.C))
		var out []byte
		switch {
		case op.A == op.B:
			out = append(out, add64RR(xC, xA)...)
		case op.A == uint16(op.C):
			out = append(out, add64RR(xB, xA)...)
		default:
			out = append(out, mov64RR(xB, xA)...)
			out = append(out, add64RR(xC, xA)...)
		}
		return out, nil
	case vm3.OpSubI64:
		// Non-commutative; A == C uses sub+neg so we don't lose the
		// original C value before the subtract: A = -(C - B) = B - C.
		xC := r2xAMD64(uint16(op.C))
		var out []byte
		switch {
		case op.A == op.B:
			out = append(out, sub64RR(xC, xA)...)
		case op.A == uint16(op.C):
			out = append(out, sub64RR(xB, xA)...)
			out = append(out, neg64R(xA)...)
		default:
			out = append(out, mov64RR(xB, xA)...)
			out = append(out, sub64RR(xC, xA)...)
		}
		return out, nil
	case vm3.OpMulI64:
		// imul is commutative; mirror OpAddI64.
		xC := r2xAMD64(uint16(op.C))
		var out []byte
		switch {
		case op.A == op.B:
			out = append(out, imul64RR(xC, xA)...)
		case op.A == uint16(op.C):
			out = append(out, imul64RR(xB, xA)...)
		default:
			out = append(out, mov64RR(xB, xA)...)
			out = append(out, imul64RR(xC, xA)...)
		}
		return out, nil
	case vm3.OpNegI64:
		var out []byte
		if op.A != op.B {
			out = append(out, mov64RR(xB, xA)...)
		}
		out = append(out, neg64R(xA)...)
		return out, nil
	case vm3.OpDivI64:
		xC := r2xAMD64(uint16(op.C))
		dz := deoptStartForStatusAMD64(fn, deoptStart, StatusDivByZero)
		return emitDivOrMod(fn, xA, xB, xC, pcMap[idx], dz, true), nil
	case vm3.OpModI64:
		xC := r2xAMD64(uint16(op.C))
		dz := deoptStartForStatusAMD64(fn, deoptStart, StatusDivByZero)
		return emitDivOrMod(fn, xA, xB, xC, pcMap[idx], dz, false), nil
	case vm3.OpAddI64K:
		var out []byte
		if op.A != op.B {
			out = append(out, mov64RR(xB, xA)...)
		}
		out = append(out, add64RImm32(xA, int32(op.C))...)
		return out, nil
	case vm3.OpSubI64K:
		var out []byte
		if op.A != op.B {
			out = append(out, mov64RR(xB, xA)...)
		}
		out = append(out, sub64RImm32(xA, int32(op.C))...)
		return out, nil
	case vm3.OpMulI64K:
		return imul64RRImm32(xA, xB, int32(op.C)), nil
	case vm3.OpDivI64K:
		return emitDivKOrModK(xA, xB, int32(op.C), true), nil
	case vm3.OpModI64K:
		return emitDivKOrModK(xA, xB, int32(op.C), false), nil
	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br:
		cc := condForCmpRegAMD64(op.Code)
		xCmpB := r2xAMD64(op.B)
		src := pcMap[idx] + 3 + 6
		dst := pcMap[int(uint16(op.C))]
		rel := int32(dst - src)
		out := cmp64RR(xCmpB, xA)
		out = append(out, jccRel32(cc, rel)...)
		return out, nil
	case vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		cc := condForCmpKImmAMD64(op.Code)
		imm := int32(int16(op.B))
		src := pcMap[idx] + 7 + 6
		dst := pcMap[int(uint16(op.C))]
		rel := int32(dst - src)
		out := cmp64RImm32(xA, imm)
		out = append(out, jccRel32(cc, rel)...)
		return out, nil
	case vm3.OpJump:
		src := pcMap[idx] + 5
		dst := pcMap[int(uint16(op.C))]
		return jmpRel32(int32(dst - src)), nil
	case vm3.OpReturnI64:
		out := mov64RR(xA, xRAX)
		out = emitEpilogueAMD64(out, fn)
		return out, nil
	case vm3.OpReturnConstK:
		out := movImm64(xRAX, int64(op.C))
		out = emitEpilogueAMD64(out, fn)
		return out, nil
	case vm3.OpReturnF64:
		// movq %rax, %xmm<A>: bit-cast f64 to int64 in RAX, then run
		// the epilogue (which preserves RAX through pops).
		out := movqR64FromXMM(xRAX, int(op.A))
		out = emitEpilogueAMD64(out, fn)
		return out, nil
	case vm3.OpReturnCell:
		// Phase 6.3.4.m.4c.2: cell-bank return. Load regsCell[A] from
		// RBP+disp32 into RAX, then run the epilogue. RBP is restored
		// by the epilogue's pop sequence; RAX carries the handle through.
		out := mov64LoadDisp32(xRAX, xRBP, int32(op.A)*8)
		out = emitEpilogueAMD64(out, fn)
		return out, nil

	case vm3.OpPairFst, vm3.OpPairSnd:
		// Phase 6.3.4.m.4c.3: regsCell[A] = arenas.Pairs[handleIdx(regsCell[B])].fst/snd.
		// RBP = regsCell base; R14 = *jitArenaCtx. The slab idx lives
		// in the low 32 bits of the Cell handle (ArenaPair never spans
		// more than 2^32 slots), so a 32-bit load is enough to mask off
		// the tag bits while zero-extending into RAX.
		stride := int64(vm3.JITPairSlabStride())
		var fieldOff int32
		if op.Code == vm3.OpPairFst {
			fieldOff = int32(vm3.JITPairFstOffset())
		} else {
			fieldOff = int32(vm3.JITPairSndOffset())
		}
		pairsBaseOff := int32(jitArenaCtxPairsBaseOff())
		out := mov32LoadDisp32(xRAX, xRBP, int32(op.B)*8)
		out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		out = append(out, mov64LoadDisp32(xRCX, xR14, pairsBaseOff)...)
		out = append(out, add64RR(xRCX, xRAX)...)
		out = append(out, mov64LoadDisp32(xRCX, xRAX, fieldOff)...)
		out = append(out, mov64StoreDisp32(xRCX, xRBP, int32(op.A)*8)...)
		return out, nil

	case vm3.OpNewPair:
		// Phase 6.3.4.m.4c.4: inline pair allocator. The JIT bumps a
		// snapshot of pairsLen kept in jitArenaCtx (R14) so make_tree-
		// style recursive allocators do not cross back into Go for
		// every pair. When pairsLen == pairsCap the JIT raises
		// StatusPairGrow; jitCall regrows the slab and retries the
		// trampoline. The free-list reuse path is skipped: every JIT-
		// allocated pair is appended at the slab tail with gen=0, so
		// the header write collapses to a single u32 store.
		stride := int64(vm3.JITPairSlabStride())
		fstOff := int32(vm3.JITPairFstOffset())
		sndOff := int32(vm3.JITPairSndOffset())
		pairsLenOff := int32(jitArenaCtxPairsLenOff())
		pairsCapOff := int32(jitArenaCtxPairsCapOff())
		pairsBaseOff := int32(jitArenaCtxPairsBaseOff())
		pgStart := deoptStartForStatusAMD64(fn, deoptStart, StatusPairGrow)
		// pairsLen → rax, pairsCap → rcx, cmp + jae deopt.
		out := mov64LoadDisp32(xRAX, xR14, pairsLenOff)
		out = append(out, mov64LoadDisp32(xRCX, xR14, pairsCapOff)...)
		out = append(out, cmp64RR(xRCX, xRAX)...) // cmp rax, rcx (sets flags from rax-rcx)
		afterJae := pcMap[idx] + len(out) + 6
		out = append(out, jccRel32(ccAE, int32(pgStart-afterJae))...)
		// pairsBase → rdx; slot ptr = pairsBase + pairsLen*stride in rcx.
		out = append(out, mov64LoadDisp32(xRDX, xR14, pairsBaseOff)...)
		out = append(out, imul64RRImm32(xRCX, xRAX, int32(stride))...)
		out = append(out, add64RR(xRDX, xRCX)...)
		// header u32 = flagAlive<<16 | gen=0 = 0x10000.
		out = append(out, movMemImm32Disp0(xRCX, 0x10000)...)
		// store fst (regsCell[B]).
		out = append(out, mov64LoadDisp32(xRDX, xRBP, int32(op.B)*8)...)
		out = append(out, mov64StoreDisp32(xRDX, xRCX, fstOff)...)
		// store snd (regsCell[uint16(C)]).
		out = append(out, mov64LoadDisp32(xRDX, xRBP, int32(uint16(op.C))*8)...)
		out = append(out, mov64StoreDisp32(xRDX, xRCX, sndOff)...)
		// build handle: rdx = idx; rcx = 0xFFFF800000000000; rdx |= rcx.
		out = append(out, mov32RR(xRAX, xRDX)...)
		// handleTag = (ArenaPair=8 << 44) | (tagHandle=0xFFFF << 48) = 0xFFFF_8000_0000_0000.
		// As signed int64 = -2^47.
		out = append(out, movRImm64(xRCX, -0x800000000000)...)
		out = append(out, or64RR(xRCX, xRDX)...)
		out = append(out, mov64StoreDisp32(xRDX, xRBP, int32(op.A)*8)...)
		// pairsLen++; commit.
		out = append(out, inc64R(xRAX)...)
		out = append(out, mov64StoreDisp32(xRAX, xR14, pairsLenOff)...)
		return out, nil

	case vm3.OpListGetI64:
		// Phase 6.3.4.n.2.a: regsI64[A] = arenas.Lists[handleIdx(regsCell[B])].cells[regsI64[C]].Int().
		// Cold form mirroring the ARM64 cold path (no hoisted slab base
		// or cells.ptr pin). RAX is the scratch slab-address/payload
		// register; RCX holds the listsBase load. xIdx is the pinned
		// register for regsI64[C]; xA is the pinned register for
		// regsI64[A]. The shl/sar pair is the AMD64 equivalent of ARM64
		// SBFX: it sign-extends the low 48 bits of the boxed Int48 cell.
		//
		// Phase 6.3.4.n.2.f: when hoistsCellsPtrAMD64(fn) the prologue
		// already cached cells.ptr at disp8(%rbx); skip the
		// slab/cells.ptr compute chain.
		xIdx := r2xAMD64(uint16(op.C))
		xA := r2xAMD64(op.A)
		var out []byte
		if hoistsCellsPtrAMD64(fn) {
			out = mov64LoadDisp8(xRAX, xRBX, int8(hoistDispAMD64(fn)))
		} else {
			stride := int64(vm3.JITListSlabStride())
			cellsOff := int32(vm3.JITListCellsOffset())
			listsBaseOff := int32(jitArenaCtxListsBaseOff())
			out = mov32LoadDisp32(xRAX, xRBP, int32(op.B)*8)
			out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
			out = append(out, mov64LoadDisp32(xRCX, xR14, listsBaseOff)...)
			out = append(out, add64RR(xRCX, xRAX)...)
			out = append(out, mov64LoadDisp32(xRAX, xRAX, cellsOff)...)
		}
		out = append(out, mov64LoadIdxLsl3(xRAX, xRAX, xIdx)...)
		out = append(out, shl64RImm8(xRAX, 16)...)
		out = append(out, sar64RImm8(xRAX, 16)...)
		out = append(out, mov64RR(xRAX, xA)...)
		return out, nil

	case vm3.OpListGetI64K:
		// Phase 6.3.4.n.2.e: constant-index list read. Mirrors OpListGetI64
		// but bakes idx*8 into the cells-array load's disp32, eliminating
		// the dependence on a live i64 register for the index. Frees one
		// i64 slot in the kernel; fannkuch_redux uses this to fit
		// NumRegsI64=8 under the AMD64 cell-bank cap.
		//
		// Phase 6.3.4.n.2.f: when hoistsCellsPtrAMD64(fn) the cells.ptr
		// is already cached at disp8(%rbx); we also shrink the
		// constant-indexed load to disp8 when idxK*8 fits in [-128, 127]
		// (the common case: small constant indices like 0..15).
		idxK := int32(uint16(op.C)) * 8
		xA := r2xAMD64(op.A)
		var out []byte
		if hoistsCellsPtrAMD64(fn) {
			out = mov64LoadDisp8(xRAX, xRBX, int8(hoistDispAMD64(fn)))
			if idxK >= -128 && idxK <= 127 {
				out = append(out, mov64LoadDisp8(xRAX, xRAX, int8(idxK))...)
			} else {
				out = append(out, mov64LoadDisp32(xRAX, xRAX, idxK)...)
			}
		} else {
			stride := int64(vm3.JITListSlabStride())
			cellsOff := int32(vm3.JITListCellsOffset())
			listsBaseOff := int32(jitArenaCtxListsBaseOff())
			out = mov32LoadDisp32(xRAX, xRBP, int32(op.B)*8)
			out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
			out = append(out, mov64LoadDisp32(xRCX, xR14, listsBaseOff)...)
			out = append(out, add64RR(xRCX, xRAX)...)
			out = append(out, mov64LoadDisp32(xRAX, xRAX, cellsOff)...)
			out = append(out, mov64LoadDisp32(xRAX, xRAX, idxK)...)
		}
		out = append(out, shl64RImm8(xRAX, 16)...)
		out = append(out, sar64RImm8(xRAX, 16)...)
		out = append(out, mov64RR(xRAX, xA)...)
		return out, nil

	case vm3.OpListSetI64:
		// Phase 6.3.4.n.2.b: arenas.Lists[handleIdx(regsCell[A])].cells[regsI64[C]] = CInt(regsI64[B]).
		// Cold form mirroring the ARM64 cold path (no hoisted slab base
		// or cells.ptr pin). RAX is the scratch slab-address/cells.ptr
		// register; RDX holds the packed payload; RCX is reused as
		// listsBase scratch and then as the Int48 tag immediate. xVal /
		// xIdx are pinned r2xAMD64 registers (slots 0..7 only, so they
		// never alias RAX/RCX/RDX). The shl/shr-logical pair masks the
		// low 48 bits of the value; the movabs supplies the high-16-bit
		// Int48 tag (0xFFFA in the top 16 of the 64-bit word). The
		// final SIB store mirrors the ARM64 STR with LSL #3 index.
		//
		// Phase 6.3.4.n.2.f: when hoistsCellsPtrAMD64(fn) the cells.ptr
		// is already cached at disp8(%rbx); skip the slab/cells.ptr
		// compute chain (RCX is then free up to the movabs tag load).
		xIdx := r2xAMD64(uint16(op.C))
		xVal := r2xAMD64(op.B)
		if hoistsCellsPtrAMD64(fn) {
			// Hot form: cells.ptr is pinned at disp8(%rbx). Use the
			// OpListPushI64 "raw SIB store + 16-bit tag overwrite"
			// trick: the 16-bit write at offset 6 overwrites bytes
			// 6..7 of the just-stored xVal with 0xFFFA. The low 48
			// bits of xVal already hold the signed payload (two's
			// complement preserves them), so OpListGetI64's shl/sar
			// sign-extend recovers the original i64 round-trip.
			out := mov64LoadDisp8(xRAX, xRBX, int8(hoistDispAMD64(fn)))
			out = append(out, mov64StoreIdxLsl3(xVal, xRAX, xIdx)...)
			out = append(out, mov16ImmStoreSIBDisp8(0xFFFA, xRAX, xIdx, 6)...)
			return out, nil
		}
		// Cold form: recompute slab + cells.ptr, then mask-and-OR pack.
		stride := int64(vm3.JITListSlabStride())
		cellsOff := int32(vm3.JITListCellsOffset())
		listsBaseOff := int32(jitArenaCtxListsBaseOff())
		tag := uint64(0xFFFA) << 48
		out := mov32LoadDisp32(xRAX, xRBP, int32(op.A)*8)
		out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		out = append(out, mov64LoadDisp32(xRCX, xR14, listsBaseOff)...)
		out = append(out, add64RR(xRCX, xRAX)...)
		out = append(out, mov64LoadDisp32(xRAX, xRAX, cellsOff)...)
		out = append(out, mov64RR(xVal, xRDX)...)
		out = append(out, shl64RImm8(xRDX, 16)...)
		out = append(out, shr64RImm8(xRDX, 16)...)
		out = append(out, movRImm64(xRCX, int64(tag))...)
		out = append(out, or64RR(xRCX, xRDX)...)
		out = append(out, mov64StoreIdxLsl3(xRDX, xRAX, xIdx)...)
		return out, nil

	case vm3.OpListPushI64:
		// Phase 6.3.4.n.2.c: arenas.Lists[handleIdx(regsCell[A])] appends
		// CInt(regsI64[B]) when cells.len < cells.cap; otherwise raises
		// StatusListGrow and the interpreter regrows + retries via jitCall.
		//
		// Cold form (no slab/cells.ptr/cells.cap pin). RAX holds the slab
		// address through the whole op; RCX is reused first as listsBase
		// scratch, then as cells.len (live until step 13/14), and RDX as
		// cells.cap then cells.ptr (live for the SIB store and the tag
		// overwrite). xVal is a pinned r2xAMD64 register (slot 0..7 only).
		//
		// The packed-Cell store exploits the layout: byte 0..5 of the
		// stored 8-byte xVal hold the low-48 payload (negative values are
		// represented in two's complement so bits 0..47 already encode the
		// signed payload), and the 16-bit immediate write at offset 6
		// overwrites bytes 6..7 with the 0xFFFA Int48 tag in one shot. No
		// fourth scratch is needed; the read-back via OpListGetI64 reverses
		// the sign-extend pair (shl/sar by 16) and recovers the original
		// i64 across the full int48 range.
		stride := int64(vm3.JITListSlabStride())
		cellsOff := int32(vm3.JITListCellsOffset())
		cellsLenOff := cellsOff + 8
		cellsCapOff := cellsOff + 16
		listsBaseOff := int32(jitArenaCtxListsBaseOff())
		xVal := r2xAMD64(op.B)
		lgStart := deoptStartForStatusAMD64(fn, deoptStart, StatusListGrow)
		out := mov32LoadDisp32(xRAX, xRBP, int32(op.A)*8)
		out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		out = append(out, mov64LoadDisp32(xRCX, xR14, listsBaseOff)...)
		out = append(out, add64RR(xRCX, xRAX)...)
		out = append(out, mov64LoadDisp32(xRCX, xRAX, cellsLenOff)...)
		out = append(out, mov64LoadDisp32(xRDX, xRAX, cellsCapOff)...)
		out = append(out, cmp64RR(xRDX, xRCX)...) // flags = rcx - rdx = len - cap
		afterJae := pcMap[idx] + len(out) + 6
		out = append(out, jccRel32(ccAE, int32(lgStart-afterJae))...)
		out = append(out, mov64LoadDisp32(xRDX, xRAX, cellsOff)...)
		out = append(out, mov64StoreIdxLsl3(xVal, xRDX, xRCX)...)
		out = append(out, mov16ImmStoreSIBDisp8(0xFFFA, xRDX, xRCX, 6)...)
		out = append(out, inc64R(xRCX)...)
		out = append(out, mov64StoreDisp32(xRCX, xRAX, cellsLenOff)...)
		out = append(out, mov32StoreDisp8(xRCX, xRAX, 4)...)
		return out, nil

	case vm3.OpNewList:
		// Phase 6.3.4.n.2.c: when fn.JITPreAllocList is set the JIT skips
		// fn.Code[0]'s OpNewList entirely; jitCall (Go side) writes the
		// pre-allocated list handle into jf.regsCell[A] before the
		// trampoline. Inline OpNewList outside the pre-alloc prefix is
		// still out of scope on AMD64 (would need an inline arena-alloc
		// kernel) and the admission gate rejects it.
		if idx == 0 && fn.JITPreAllocList {
			return []byte{}, nil
		}
		if idx < int(fn.JITPreAllocListPrefix) {
			return []byte{}, nil
		}
		return nil, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)

	case vm3.OpConstF64K:
		bits := int64(uint64(fn.Consts[int(uint16(op.C))]))
		out := movImm64(xRCX, bits)
		out = append(out, movqXMMFromR64(int(op.A), xRCX)...)
		return out, nil

	case vm3.OpMovF64:
		return movsdRR(int(op.A), int(op.B)), nil
	case vm3.OpAddF64:
		var out []byte
		if op.A != op.B {
			out = append(out, movsdRR(int(op.A), int(op.B))...)
		}
		out = append(out, addsdRR(int(op.A), int(uint16(op.C)))...)
		return out, nil
	case vm3.OpSubF64:
		var out []byte
		if op.A != op.B {
			out = append(out, movsdRR(int(op.A), int(op.B))...)
		}
		out = append(out, subsdRR(int(op.A), int(uint16(op.C)))...)
		return out, nil
	case vm3.OpMulF64:
		var out []byte
		if op.A != op.B {
			out = append(out, movsdRR(int(op.A), int(op.B))...)
		}
		out = append(out, mulsdRR(int(op.A), int(uint16(op.C)))...)
		return out, nil
	case vm3.OpDivF64:
		var out []byte
		if op.A != op.B {
			out = append(out, movsdRR(int(op.A), int(op.B))...)
		}
		out = append(out, divsdRR(int(op.A), int(uint16(op.C)))...)
		return out, nil
	case vm3.OpNegF64:
		// xmm15 = bit-pattern 0x8000000000000000 ; result = src ^ xmm15.
		signBit := uint64(1) << 63
		out := movImm64(xRCX, int64(signBit))
		out = append(out, movqXMMFromR64(15, xRCX)...)
		if op.A != op.B {
			out = append(out, movsdRR(int(op.A), int(op.B))...)
		}
		out = append(out, xorpdRR(int(op.A), 15)...)
		return out, nil

	case vm3.OpFmaF64:
		mul2 := int(uint16(op.C) & 0xFF)
		addend := int((uint16(op.C) >> 8) & 0xFF)
		a, b := int(op.A), int(op.B)
		switch {
		case a == b:
			return vfmaddSDRR(0x98, a, addend, mul2), nil
		case a == addend:
			return vfmaddSDRR(0xB8, a, b, mul2), nil
		case a == mul2:
			return vfmaddSDRR(0xA8, a, b, addend), nil
		default:
			out := movsdRR(a, b)
			return append(out, vfmaddSDRR(0x98, a, addend, mul2)...), nil
		}
	case vm3.OpSqrtF64:
		var out []byte
		if op.A != op.B {
			out = append(out, movsdRR(int(op.A), int(op.B))...)
		}
		return append(out, sqrtsdRR(int(op.A), int(op.A))...), nil

	case vm3.OpCmpEqF64Br, vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br:
		// ucomisd ; jp +6 (skip the je/jb/jbe) ; jcc target.
		dst := pcMap[int(uint16(op.C))]
		out := ucomisdRR(int(op.A), int(op.B))
		out = append(out, jccRel32(byte(0xA), 6)...) // JP rel32 over the next 6 bytes
		src := pcMap[idx] + len(out) + 6
		rel := int32(dst - src)
		cc := condForCmpF64AMD64(op.Code)
		out = append(out, jccRel32(cc, rel)...)
		return out, nil
	case vm3.OpCmpNeF64Br:
		// ucomisd ; jp target ; jne target.
		dst := pcMap[int(uint16(op.C))]
		out := ucomisdRR(int(op.A), int(op.B))
		src1 := pcMap[idx] + len(out) + 6
		out = append(out, jccRel32(byte(0xA), int32(dst-src1))...) // JP target
		src2 := pcMap[idx] + len(out) + 6
		out = append(out, jccRel32(byte(0x5), int32(dst-src2))...) // JNE target
		return out, nil
	case vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br:
		// ucomisd ; ja/jae target (single 6-byte branch).
		dst := pcMap[int(uint16(op.C))]
		out := ucomisdRR(int(op.A), int(op.B))
		src := pcMap[idx] + len(out) + 6
		cc := condForCmpF64AMD64(op.Code)
		out = append(out, jccRel32(cc, int32(dst-src))...)
		return out, nil

	case vm3.OpI64ToF64:
		return cvtsi2sd(int(op.A), int(r2xAMD64(op.B))), nil
	case vm3.OpF64ToI64:
		return cvttsd2si(int(r2xAMD64(op.A)), int(op.B)), nil
	case vm3.OpLookupI64KW:
		// regsI64[A] = fn.I64Tables[uint16(C)][regsI64[B]]
		//
		// Cold form (Phase 6.4.c): load the table base into a scratch
		// GPR (RAX, never in r2xAMD64's range) then indexed-load through
		// it. Caller pre-emits OpCmpGeI64KBr against the table length so
		// the load itself is unchecked (mirrors the interpreter and the
		// ARM64 lowering). Hoisting the base into a callee-saved reg is
		// the natural follow-up; deferred until the cold form lands.
		tableIdx := int(uint16(op.C))
		if tableIdx >= len(fn.I64Tables) || len(fn.I64Tables[tableIdx]) == 0 {
			return nil, fmt.Errorf("%w: LookupI64KW table %d out of range or empty",
				ErrNotImplemented, tableIdx)
		}
		base := int64(uintptr(unsafe.Pointer(&fn.I64Tables[tableIdx][0])))
		out := movImm64(xRAX, base)
		out = append(out, mov64LoadIdxLsl3(r2xAMD64(op.A), xRAX, r2xAMD64(op.B))...)
		return out, nil

	case vm3.OpF64ArrayGetF64:
		// Phase 6.3.4.n.3 cold form. Mirrors OpListGetI64's cold-form
		// slab compute (RAX as scratch for the slab address; RCX for the
		// f64ArrsBase load). The final movsd loads data[regsI64[C]] into
		// xmm<A>; raw f64 bits round-trip with no boxing since the slab
		// stores native float64 data.
		stride := int64(vm3.JITF64ArrSlabStride())
		dataOff := int32(vm3.JITF64ArrDataOffset())
		baseOff := int32(jitArenaCtxF64ArrsBaseOff())
		xIdx := r2xAMD64(uint16(op.C))
		out := mov32LoadDisp32(xRAX, xRBP, int32(op.B)*8)
		out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		out = append(out, mov64LoadDisp32(xRCX, xR14, baseOff)...)
		out = append(out, add64RR(xRCX, xRAX)...)
		out = append(out, mov64LoadDisp32(xRAX, xRAX, dataOff)...)
		out = append(out, movsdLoadXMMIdxLsl3(int(op.A), xRAX, xIdx)...)
		return out, nil

	case vm3.OpF64ArraySetF64:
		// Phase 6.3.4.n.3 cold form. arenas.F64Arrs[regsCell[A]].data[regsI64[C]] = regsF64[B].
		stride := int64(vm3.JITF64ArrSlabStride())
		dataOff := int32(vm3.JITF64ArrDataOffset())
		baseOff := int32(jitArenaCtxF64ArrsBaseOff())
		xIdx := r2xAMD64(uint16(op.C))
		out := mov32LoadDisp32(xRAX, xRBP, int32(op.A)*8)
		out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		out = append(out, mov64LoadDisp32(xRCX, xR14, baseOff)...)
		out = append(out, add64RR(xRCX, xRAX)...)
		out = append(out, mov64LoadDisp32(xRAX, xRAX, dataOff)...)
		out = append(out, movsdStoreXMMIdxLsl3(int(op.B), xRAX, xIdx)...)
		return out, nil

	case vm3.OpF64ArrayLenI64:
		// Phase 6.3.4.n.3 cold form. regsI64[A] = int64(F64Arrs[B].len).
		stride := int64(vm3.JITF64ArrSlabStride())
		lenOff := int32(vm3.JITF64ArrLenOffset())
		baseOff := int32(jitArenaCtxF64ArrsBaseOff())
		xA := r2xAMD64(op.A)
		out := mov32LoadDisp32(xRAX, xRBP, int32(op.B)*8)
		out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		out = append(out, mov64LoadDisp32(xRCX, xR14, baseOff)...)
		out = append(out, add64RR(xRCX, xRAX)...)
		out = append(out, mov32LoadDisp32(xRAX, xRAX, lenOff)...)
		out = append(out, mov64RR(xRAX, xA)...)
		return out, nil

	case vm3.OpI64ArrayGetI64:
		// Phase 6.3.4.n.3 cold form. regsI64[A] = I64Arrs[B].data[regsI64[C]].
		stride := int64(vm3.JITI64ArrSlabStride())
		dataOff := int32(vm3.JITI64ArrDataOffset())
		baseOff := int32(jitArenaCtxI64ArrsBaseOff())
		xIdx := r2xAMD64(uint16(op.C))
		xA := r2xAMD64(op.A)
		out := mov32LoadDisp32(xRAX, xRBP, int32(op.B)*8)
		out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		out = append(out, mov64LoadDisp32(xRCX, xR14, baseOff)...)
		out = append(out, add64RR(xRCX, xRAX)...)
		out = append(out, mov64LoadDisp32(xRAX, xRAX, dataOff)...)
		out = append(out, mov64LoadIdxLsl3(xA, xRAX, xIdx)...)
		return out, nil

	case vm3.OpI64ArraySetI64:
		// Phase 6.3.4.n.3 cold form. I64Arrs[A].data[regsI64[C]] = regsI64[B].
		stride := int64(vm3.JITI64ArrSlabStride())
		dataOff := int32(vm3.JITI64ArrDataOffset())
		baseOff := int32(jitArenaCtxI64ArrsBaseOff())
		xIdx := r2xAMD64(uint16(op.C))
		xB := r2xAMD64(op.B)
		out := mov32LoadDisp32(xRAX, xRBP, int32(op.A)*8)
		out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		out = append(out, mov64LoadDisp32(xRCX, xR14, baseOff)...)
		out = append(out, add64RR(xRCX, xRAX)...)
		out = append(out, mov64LoadDisp32(xRAX, xRAX, dataOff)...)
		out = append(out, mov64StoreIdxLsl3(xB, xRAX, xIdx)...)
		return out, nil

	case vm3.OpI64ArrayLenI64:
		stride := int64(vm3.JITI64ArrSlabStride())
		lenOff := int32(vm3.JITI64ArrLenOffset())
		baseOff := int32(jitArenaCtxI64ArrsBaseOff())
		xA := r2xAMD64(op.A)
		out := mov32LoadDisp32(xRAX, xRBP, int32(op.B)*8)
		out = append(out, imul64RRImm32(xRAX, xRAX, int32(stride))...)
		out = append(out, mov64LoadDisp32(xRCX, xR14, baseOff)...)
		out = append(out, add64RR(xRCX, xRAX)...)
		out = append(out, mov32LoadDisp32(xRAX, xRAX, lenOff)...)
		out = append(out, mov64RR(xRAX, xA)...)
		return out, nil

	case vm3.OpNewF64Array:
		// Phase 6.3.4.n.3: pre-allocated by jitCall; emit nothing.
		if idx < int(fn.JITPreAllocF64ArrPrefix) {
			return []byte{}, nil
		}
		return nil, fmt.Errorf("%w: opcode %d (inline NewF64Array unsupported)", ErrNotImplemented, op.Code)

	case vm3.OpNewI64Array:
		if idx < int(fn.JITPreAllocI64ArrPrefix) {
			return []byte{}, nil
		}
		return nil, fmt.Errorf("%w: opcode %d (inline NewI64Array unsupported)", ErrNotImplemented, op.Code)

	case vm3.OpCallI64:
		if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx {
			return nil, fmt.Errorf("%w: CallI64 to non-self idx %d (SelfIdx=%d)",
				ErrNotImplemented, uint16(op.C), opts.SelfIdx)
		}
		spillMask := spillSets[idx]
		nArgs := fn.NumI64Params()
		nRegsI64 := int(fn.NumRegsI64)
		out := make([]byte, 0, 64)
		// Spill live caller-saved slots (0..5).
		for r := uint16(0); r < 6; r++ {
			if spillMask&(1<<r) != 0 {
				out = append(out, mov64StoreDisp32(r2xAMD64(r), xRBX, int32(r)*8)...)
			}
		}
		// Write args into callee window at offset NumRegsI64*8.
		for k := 0; k < nArgs; k++ {
			src := r2xAMD64(op.B + uint16(k))
			out = append(out, mov64StoreDisp32(src, xRBX, int32(nRegsI64+k)*8)...)
		}
		// Re-establish the SysV first/second arg registers the prologue
		// expects (RDI = regs base, RSI = status). The callee prologue
		// runs `mov %rdi, %rbx` / `mov %rsi, %r15`, so leaving stale
		// slot-1 / slot-0 values in RDI/RSI would clobber RBX with
		// junk and segfault on the first pinned-slot load. Keep RBX
		// unchanged here — the callee derives its own window from RDI.
		out = append(out, lea64Disp32(xRDI, xRBX, int32(nRegsI64)*8)...)
		out = append(out, mov64RR(xR15, xRSI)...)
		// CALL rel32 to entry (byte 0).
		entry := 0
		callSite := pcMap[idx] + len(out) + 5
		rel := int32(entry - callSite)
		out = append(out, callRel32(rel)...)
		// Move result into destination slot (RAX → xA).
		out = append(out, mov64RR(xRAX, xA)...)
		// Reload spilled slots.
		for r := uint16(0); r < 6; r++ {
			if spillMask&(1<<r) != 0 {
				out = append(out, mov64LoadDisp32(r2xAMD64(r), xRBX, int32(r)*8)...)
			}
		}
		return out, nil
	case vm3.OpCallMixed:
		// Phase 6.3.4.m.4c.5/.6: AMD64 cell-bank OpCallMixed (self and
		// cross-fn). Mirrors the AArch64 lowering's contract: write i64
		// args into the i64 window via RBX-relative stores, cell args
		// into the cell window via RBP-relative stores (load-through-
		// RDX two-step since AMD64 has no mem-to-mem move). Then set
		// up the SysV ABI for the callee (RDI = callee regsI64 base,
		// RSI = status, RCX = callee regsCell base, R8 = arena ctx).
		// Self: CALL rel32 to byte 0. Cross-fn: movabs $callee.JITCode,
		// %r10 + CALL *%r10. After the CALL, optionally propagate the
		// callee's status code via cmpq $0,(%r15) + jne passthrough,
		// then route the result.
		calleeIdx := int(uint16(op.C))
		isSelf := opts.SelfIdx >= 0 && calleeIdx == opts.SelfIdx
		var callee *vm3.Function
		if isSelf {
			callee = fn
		} else {
			if opts.Prog == nil {
				return nil, fmt.Errorf("%w: CallMixed cross-fn needs opts.Prog", ErrNotImplemented)
			}
			if calleeIdx < 0 || calleeIdx >= len(opts.Prog.Funcs) {
				return nil, fmt.Errorf("%w: CallMixed callee idx %d out of range",
					ErrNotImplemented, calleeIdx)
			}
			callee = opts.Prog.Funcs[calleeIdx]
			if callee.JITCode == nil {
				return nil, fmt.Errorf("%w: CallMixed callee %s has no JITCode",
					ErrNotImplemented, callee.Name)
			}
		}
		spillMask := spillSets[idx]
		callerI64 := int(fn.NumRegsI64)
		callerCell := int(fn.NumRegsCell)
		out := make([]byte, 0, 96)
		// Spill live caller-saved i64 slots.
		for r := uint16(0); r < 6; r++ {
			if spillMask&(1<<r) != 0 {
				out = append(out, mov64StoreDisp32(r2xAMD64(r), xRBX, int32(r)*8)...)
			}
		}
		// Write args bank-by-bank using the callee's ParamBanks (the
		// cross-fn callee may have a different shape than the caller).
		// argReg = op.B + k follows the ARM64 convention (slot indices
		// are shared across banks; the bank-specific reg in the caller
		// is r2x*(argReg)).
		for k, b := range callee.ParamBanks {
			argReg := op.B + uint16(k)
			switch b {
			case vm3.BankI64:
				src := r2xAMD64(argReg)
				out = append(out, mov64StoreDisp32(src, xRBX, int32(callerI64+k)*8)...)
			case vm3.BankCell:
				// Cell args live at [%rbp + argReg*8]; load via RDX
				// (caller-saved scratch) and store to the callee
				// window at [%rbp + (callerCell+k)*8].
				out = append(out, mov64LoadDisp32(xRDX, xRBP, int32(argReg)*8)...)
				out = append(out, mov64StoreDisp32(xRDX, xRBP, int32(callerCell+k)*8)...)
			default:
				return nil, fmt.Errorf("%w: CallMixed param bank %d not supported on AMD64",
					ErrNotImplemented, b)
			}
		}
		// SysV ABI setup for the callee.
		if callerI64 == 0 {
			out = append(out, mov64RR(xRBX, xRDI)...)
		} else {
			out = append(out, lea64Disp32(xRDI, xRBX, int32(callerI64)*8)...)
		}
		out = append(out, mov64RR(xR15, xRSI)...)
		if callerCell == 0 {
			out = append(out, mov64RR(xRBP, xRCX)...)
		} else {
			out = append(out, lea64Disp32(xRCX, xRBP, int32(callerCell)*8)...)
		}
		out = append(out, mov64RR(xR14, xR8)...)
		// CALL: self uses rel32 to byte 0; cross-fn uses movabs+CALL r10.
		if isSelf {
			callSite := pcMap[idx] + len(out) + 5
			rel := int32(0 - callSite)
			out = append(out, callRel32(rel)...)
		} else {
			out = append(out, movRImm64(xR10, int64(uintptr(callee.JITCode)))...)
			out = append(out, callIndirectR10()...)
		}
		// Reload spilled slots. The result still sits in RAX which is
		// outside r2xAMD64's range, so spill reloads don't clobber it.
		for r := uint16(0); r < 6; r++ {
			if spillMask&(1<<r) != 0 {
				out = append(out, mov64LoadDisp32(r2xAMD64(r), xRBX, int32(r)*8)...)
			}
		}
		// Optional deopt propagation. If the callee can deopt, the
		// callee's deopt block writes a non-zero status to (%r15)
		// before returning. We do a memory-form cmp (no scratch reg
		// clobber) and JNE into the shared passthrough block.
		needsDeoptCheck := false
		if isSelf {
			needsDeoptCheck = selfDeoptCalleeAMD64(fn)
		} else {
			needsDeoptCheck = crossFnDeoptCalleeAMD64(callee)
		}
		if needsDeoptCheck {
			out = append(out, cmpMemImm8R15Indirect(0)...)
			ptStart := passthroughStartAMD64(fn, deoptStart)
			afterJNE := pcMap[idx] + len(out) + 6
			out = append(out, jccRel32(ccNZ, int32(ptStart-afterJNE))...)
		}
		// Route the result. BankI64 → mov %rax, xA. BankCell → store
		// %rax into regsCell[op.A] at [%rbp + op.A*8].
		switch vm3.Bank(op.BankFlags & 0x3) {
		case vm3.BankCell:
			out = append(out, mov64StoreDisp32(xRAX, xRBP, int32(op.A)*8)...)
		case vm3.BankI64:
			out = append(out, mov64RR(xRAX, xA)...)
		default:
			return nil, fmt.Errorf("%w: CallMixed result bank %d not supported on AMD64",
				ErrNotImplemented, op.BankFlags&0x3)
		}
		return out, nil
	default:
		return nil, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)
	}
}

// emitDivOrMod lowers reg-reg OpDivI64 / OpModI64.
// All clobbers (RAX, RDX) are scratch on this backend so no spill is
// needed. The deopt branch is a JZ rel32 to deoptStart.
//
//	test xC, xC                  ; 3
//	jz   deoptStart              ; 6
//	mov  %rXb, %rax              ; 3 (dividend low)
//	cqo                          ; 2 (sign-extend into RDX:RAX)
//	idiv %rXc                    ; 3 (RAX=quot, RDX=rem)
//	mov  %rax|%rdx, %rXa         ; 3 (quot for Div, rem for Mod)
func emitDivOrMod(fn *vm3.Function, xA, xB, xC, opOff, deoptStart int, isDiv bool) []byte {
	out := test64RR(xC, xC)
	afterJZ := opOff + len(out) + 6
	rel := int32(deoptStart - afterJZ)
	out = append(out, jccRel32(ccZ, rel)...)
	out = append(out, mov64RR(xB, xRAX)...)
	out = append(out, cqoBytes()...)
	out = append(out, idiv64R(xC)...)
	if isDiv {
		out = append(out, mov64RR(xRAX, xA)...)
	} else {
		out = append(out, mov64RR(xRDX, xA)...)
	}
	return out
}

// emitDivKOrModK lowers OpDivI64K / OpModI64K. The K is a non-zero
// immediate so there is no deopt path; RCX is used as the divisor.
//
//	mov  $imm, %rcx               ; 7
//	mov  %rXb, %rax               ; 3
//	cqo                           ; 2
//	idiv %rcx                     ; 3
//	mov  %rax|%rdx, %rXa          ; 3
func emitDivKOrModK(xA, xB int, imm int32, isDiv bool) []byte {
	out := movRImm32SignExt(xRCX, imm)
	out = append(out, mov64RR(xB, xRAX)...)
	out = append(out, cqoBytes()...)
	out = append(out, idiv64R(xRCX)...)
	if isDiv {
		out = append(out, mov64RR(xRAX, xA)...)
	} else {
		out = append(out, mov64RR(xRDX, xA)...)
	}
	return out
}

// AMD64 condition codes (jcc opcode low nibble).
const (
	ccZ  = 0x4 // JZ / JE: equal
	ccNZ = 0x5 // JNZ / JNE
	ccAE = 0x3 // JAE: unsigned >= (CF=0)
	ccBE = 0x6 // JBE: unsigned <= (CF=1 OR ZF=1)
	ccL  = 0xC // JL: signed less
	ccGE = 0xD // JGE: signed greater-equal
	ccLE = 0xE // JLE
	ccG  = 0xF // JG
)

// condForCmpRegAMD64 returns the AMD64 jcc condition for a reg-reg
// vm3 compare-and-branch opcode. The CMP form on x86_64 is
// `cmp r/m, r` (we emit `cmp xB, xA` which sets flags from `xA - xB`),
// so condition codes are the natural mapping.
func condForCmpRegAMD64(code vm3.OpCode) byte {
	switch code {
	case vm3.OpCmpEqI64Br:
		return ccZ
	case vm3.OpCmpNeI64Br:
		return ccNZ
	case vm3.OpCmpLtI64Br:
		return ccL
	case vm3.OpCmpLeI64Br:
		return ccLE
	case vm3.OpCmpGtI64Br:
		return ccG
	case vm3.OpCmpGeI64Br:
		return ccGE
	}
	return ccZ
}

// condForCmpKImmAMD64 mirrors condForCmpRegAMD64 for the K-form
// compare-and-branch opcodes.
func condForCmpKImmAMD64(code vm3.OpCode) byte {
	switch code {
	case vm3.OpCmpEqI64KBr:
		return ccZ
	case vm3.OpCmpNeI64KBr:
		return ccNZ
	case vm3.OpCmpLtI64KBr:
		return ccL
	case vm3.OpCmpLeI64KBr:
		return ccLE
	case vm3.OpCmpGtI64KBr:
		return ccG
	case vm3.OpCmpGeI64KBr:
		return ccGE
	}
	return ccZ
}

// --- AMD64 instruction encoders (Intel syntax in comments) ---
//
// All emit fixed-width forms (imm32, disp32, rel32) so pass 1 can
// predict the byte count without inspecting operand values.

// rex returns the REX prefix byte: 0x40 base, plus W (bit 3), R (bit
// 2), X (bit 1), B (bit 0). Caller composes the bits it needs.
func rex(w, r, x, b bool) byte {
	v := byte(0x40)
	if w {
		v |= 0x08
	}
	if r {
		v |= 0x04
	}
	if x {
		v |= 0x02
	}
	if b {
		v |= 0x01
	}
	return v
}

// modRM packs mod/reg/rm into the ModR/M byte. mod in [0, 3]; reg
// and rm in [0, 7] (high-bit extensions come from REX).
func modRM(mod, reg, rm byte) byte {
	return (mod << 6) | ((reg & 7) << 3) | (rm & 7)
}

// mov64RR emits `mov %rSrc, %rDst` (Intel: mov rDst, rSrc).
// Opcode REX.W 89 /r with src in reg field, dst in r/m field.
func mov64RR(src, dst int) []byte {
	if src == dst {
		return nil
	}
	return []byte{rex(true, src >= 8, false, dst >= 8), 0x89, modRM(3, byte(src), byte(dst))}
}

// add64RR emits `add %rSrc, %rDst` (Intel: add rDst, rSrc).
// Opcode REX.W 01 /r.
func add64RR(src, dst int) []byte {
	return []byte{rex(true, src >= 8, false, dst >= 8), 0x01, modRM(3, byte(src), byte(dst))}
}

// sub64RR emits `sub %rSrc, %rDst` (Intel: sub rDst, rSrc).
// Opcode REX.W 29 /r.
func sub64RR(src, dst int) []byte {
	return []byte{rex(true, src >= 8, false, dst >= 8), 0x29, modRM(3, byte(src), byte(dst))}
}

// cmp64RR emits `cmp %rSrc, %rDst` (Intel: cmp rDst, rSrc), setting
// flags from `rDst - rSrc`. Opcode REX.W 39 /r.
func cmp64RR(src, dst int) []byte {
	return []byte{rex(true, src >= 8, false, dst >= 8), 0x39, modRM(3, byte(src), byte(dst))}
}

// imul64RR emits `imul %rSrc, %rDst` (Intel: imul rDst, rSrc), i.e.
// rDst *= rSrc. Opcode REX.W 0F AF /r with rDst in reg field, rSrc in
// r/m field — direction is OPPOSITE to add/sub above.
func imul64RR(src, dst int) []byte {
	return []byte{rex(true, dst >= 8, false, src >= 8), 0x0F, 0xAF, modRM(3, byte(dst), byte(src))}
}

// imul64RRImm32 emits the three-operand form `imul rDst, rSrc, imm32`,
// i.e. rDst = rSrc * imm32. Opcode REX.W 69 /r imm32.
func imul64RRImm32(dst, src int, imm int32) []byte {
	out := []byte{rex(true, dst >= 8, false, src >= 8), 0x69, modRM(3, byte(dst), byte(src))}
	out = appendImm32(out, imm)
	return out
}

// add64RImm32 emits `add rDst, imm32` (sign-extended). Opcode REX.W
// 81 /0 imm32.
func add64RImm32(dst int, imm int32) []byte {
	out := []byte{rex(true, false, false, dst >= 8), 0x81, modRM(3, 0, byte(dst))}
	out = appendImm32(out, imm)
	return out
}

// sub64RImm32 emits `sub rDst, imm32`. Opcode REX.W 81 /5 imm32.
func sub64RImm32(dst int, imm int32) []byte {
	out := []byte{rex(true, false, false, dst >= 8), 0x81, modRM(3, 5, byte(dst))}
	out = appendImm32(out, imm)
	return out
}

// cmp64RImm32 emits `cmp rDst, imm32`. Opcode REX.W 81 /7 imm32.
func cmp64RImm32(dst int, imm int32) []byte {
	out := []byte{rex(true, false, false, dst >= 8), 0x81, modRM(3, 7, byte(dst))}
	out = appendImm32(out, imm)
	return out
}

// neg64R emits `neg %rDst` (rDst = -rDst). Opcode REX.W F7 /3.
func neg64R(dst int) []byte {
	return []byte{rex(true, false, false, dst >= 8), 0xF7, modRM(3, 3, byte(dst))}
}

// shl64RImm8 emits `shl %rDst, imm8`. Opcode REX.W C1 /4 ib. Phase
// 6.3.4.n.2.a uses this with sar64RImm8 (16-bit shift pair) as the
// AMD64 SBFX-equivalent that sign-extends the low 48 bits of a NaN-
// boxed int48 payload from arenas.Lists[i].cells[j]. 4 bytes.
func shl64RImm8(dst int, imm uint8) []byte {
	return []byte{rex(true, false, false, dst >= 8), 0xC1, modRM(3, 4, byte(dst&7)), imm}
}

// sar64RImm8 emits `sar %rDst, imm8`. Opcode REX.W C1 /7 ib. Paired
// with shl64RImm8 to sign-extend an N-bit signed value packed in the
// low N bits of a register: shl by (64-N), sar by (64-N). 4 bytes.
func sar64RImm8(dst int, imm uint8) []byte {
	return []byte{rex(true, false, false, dst >= 8), 0xC1, modRM(3, 7, byte(dst&7)), imm}
}

// shr64RImm8 emits `shr %rDst, imm8` (logical shift right). Opcode
// REX.W C1 /5 ib. Phase 6.3.4.n.2.b pairs it with shl64RImm8 (shl 16
// then shr 16 logical) to mask a 64-bit value to its low 48 bits
// before OR-ing in the Int48 tag for the cell store. 4 bytes.
func shr64RImm8(dst int, imm uint8) []byte {
	return []byte{rex(true, false, false, dst >= 8), 0xC1, modRM(3, 5, byte(dst&7)), imm}
}

// idiv64R emits `idiv %rDivisor` (signed). Opcode REX.W F7 /7. RDX:RAX
// is the dividend; RAX = quotient, RDX = remainder on return.
func idiv64R(divisor int) []byte {
	return []byte{rex(true, false, false, divisor >= 8), 0xF7, modRM(3, 7, byte(divisor))}
}

// test64RR emits `test %rSrc, %rDst` (Intel: test rDst, rSrc).
// Opcode REX.W 85 /r.
func test64RR(src, dst int) []byte {
	return []byte{rex(true, src >= 8, false, dst >= 8), 0x85, modRM(3, byte(src), byte(dst))}
}

// cqoBytes returns `cqo`: sign-extend RAX into RDX:RAX before IDIV.
// Opcode REX.W 99.
func cqoBytes() []byte { return []byte{0x48, 0x99} }

// mov64LoadDisp32 emits `mov disp32(rBase), rDst`. Opcode REX.W 8B /r
// with mod=10 (disp32). Caller pre-checks that rBase is not RSP/R12
// (would require a SIB byte).
func mov64LoadDisp32(dst, base int, disp int32) []byte {
	out := []byte{rex(true, dst >= 8, false, base >= 8), 0x8B, modRM(2, byte(dst), byte(base))}
	out = appendImm32(out, disp)
	return out
}

// mov32LoadDisp32 emits `mov disp32(rBase), %eDst`, a 32-bit load that
// zero-extends to the full 64-bit destination per AMD64 semantics.
// Used by OpPairFst/OpPairSnd (Phase 6.3.4.m.4c.3) to extract the low
// 32 bits of a Cell handle (the slab idx) without a separate UXTW.
// Encoding: optional REX (no W) + 8B /r mod=10 + disp32. 6 bytes when
// neither dst nor base needs REX, 7 bytes when one does.
func mov32LoadDisp32(dst, base int, disp int32) []byte {
	var out []byte
	if dst >= 8 || base >= 8 {
		var r byte = 0x40
		if dst >= 8 {
			r |= 0x04
		}
		if base >= 8 {
			r |= 0x01
		}
		out = append(out, r)
	}
	out = append(out, 0x8B, modRM(2, byte(dst&7), byte(base&7)))
	out = appendImm32(out, disp)
	return out
}

// mov32LoadDisp32ByteCount mirrors mov32LoadDisp32 for byte-count
// prediction.
func mov32LoadDisp32ByteCount(dst, base int) int {
	if dst >= 8 || base >= 8 {
		return 7
	}
	return 6
}

// mov32RR emits `mov %eSrc, %eDst`, a 32-bit reg-reg copy that
// zero-extends to the full 64-bit destination per AMD64 semantics.
// Opcode 89 /r mod=11. 2 bytes when neither reg needs REX, 3 bytes
// otherwise. Used by OpNewPair (Phase 6.3.4.m.4c.4) to copy the
// low-32 pairsLen idx into a separate handle-building register.
func mov32RR(src, dst int) []byte {
	var out []byte
	if src >= 8 || dst >= 8 {
		var r byte = 0x40
		if src >= 8 {
			r |= 0x04 // REX.R
		}
		if dst >= 8 {
			r |= 0x01 // REX.B
		}
		out = append(out, r)
	}
	out = append(out, 0x89, modRM(3, byte(src&7), byte(dst&7)))
	return out
}

// or64RR emits `or %rSrc, %rDst` (Intel: or rDst, rSrc). Opcode REX.W
// 09 /r mod=11.
func or64RR(src, dst int) []byte {
	return []byte{rex(true, src >= 8, false, dst >= 8), 0x09, modRM(3, byte(src), byte(dst))}
}

// inc64R emits `inc %rDst`. Opcode REX.W FF /0 mod=11. 3 bytes.
// Phase 6.3.4.m.4c.4 uses it to bump pairsLen by one before the
// commit-store.
func inc64R(dst int) []byte {
	return []byte{rex(true, false, false, dst >= 8), 0xFF, modRM(3, 0, byte(dst))}
}

// movMemImm32Disp0 emits `movl $imm32, (rBase)` — a 32-bit store of
// imm32 to memory at rBase with no displacement. Opcode C7 /0 imm32
// with mod=00. Caller must pre-check rBase != RBP/R13 (those need the
// disp8=0 mod=01 form to avoid the [disp32] decoding) and rBase != RSP
// (would require a SIB byte). Used by OpNewPair to write the vmPair
// header u32 = flagAlive<<16 | gen=0 in a single 6-byte store.
func movMemImm32Disp0(base int, imm int32) []byte {
	var out []byte
	if base >= 8 {
		out = append(out, 0x41) // REX.B
	}
	out = append(out, 0xC7, modRM(0, 0, byte(base&7)))
	out = appendImm32(out, imm)
	return out
}

// mov64LoadIdxLsl3 emits `mov rDst, [rBase + rIdx*8]`. Opcode REX.W [REX.R]
// [REX.X] [REX.B] 8B ModR/M(mod=00, reg=dst, rm=100=SIB) SIB(scale=11,
// index, base). Phase 6.4.c uses this for the AMD64 OpLookupI64KW cold
// lowering: load `fn.I64Tables[c][regsI64[B]]` into the destination
// register after a scratch register holds the table base.
//
// Caller pre-checks that rBase is not RBP or R13 (those need mod=01
// with disp8=0 to disambiguate from the [disp32] form) and that rIdx is
// not RSP (RSP as SIB.index means "no index"). The vm3 i64 reg
// allocator never produces those clashes: r2xAMD64 maps slots 0..8 to
// RSI/RDI/R8/R9/R10/R11/R12/R13/R14, and this helper is only invoked
// with rBase=RAX (scratch). R13 as index is fine, the [disp32]
// exception applies only to the SIB.base field.
func mov64LoadIdxLsl3(dst, base, idx int) []byte {
	rexW := byte(0x48)
	if dst >= 8 {
		rexW |= 0x04 // REX.R
	}
	if idx >= 8 {
		rexW |= 0x02 // REX.X
	}
	if base >= 8 {
		rexW |= 0x01 // REX.B
	}
	mod := modRM(0, byte(dst&7), 4) // rm=100 = SIB follows
	sib := byte(3<<6) | byte((idx&7)<<3) | byte(base&7)
	return []byte{rexW, 0x8B, mod, sib}
}

// mov64LoadIdxLsl3ByteCount mirrors mov64LoadIdxLsl3. The encoding is
// always 4 bytes for any combination of dst/base/idx since REX, opcode,
// ModR/M, and SIB are each one byte.
func mov64LoadIdxLsl3ByteCount(dst, base, idx int) int { return 4 }

// mov64StoreIdxLsl3 emits `mov rSrc, [rBase + rIdx*8]`. Opcode REX.W
// 89 /r with mod=00 and SIB(scale=3, index=rIdx, base=rBase). Mirrors
// mov64LoadIdxLsl3 for the write direction. Caller must pre-check that
// rBase is not RBP/R13 (those need mod=01/10+disp under the SIB.base
// quirk); for Phase 6.3.4.n.2.b the base is RAX (cells.ptr from a
// prior load), so the quirk is avoided.
func mov64StoreIdxLsl3(src, base, idx int) []byte {
	rexW := byte(0x48)
	if src >= 8 {
		rexW |= 0x04 // REX.R
	}
	if idx >= 8 {
		rexW |= 0x02 // REX.X
	}
	if base >= 8 {
		rexW |= 0x01 // REX.B
	}
	mod := modRM(0, byte(src&7), 4) // rm=100 = SIB follows
	sib := byte(3<<6) | byte((idx&7)<<3) | byte(base&7)
	return []byte{rexW, 0x89, mod, sib}
}

// mov64StoreDisp32 emits `mov rSrc, disp32(rBase)`. Opcode REX.W 89 /r
// with mod=10 (disp32). Same SIB restriction as the load form.
func mov64StoreDisp32(src, base int, disp int32) []byte {
	out := []byte{rex(true, src >= 8, false, base >= 8), 0x89, modRM(2, byte(src), byte(base))}
	out = appendImm32(out, disp)
	return out
}

// mov64LoadDisp8 emits `mov disp8(rBase), rDst`, a 64-bit load with an
// 8-bit signed displacement. Opcode REX.W 8B /r mod=01 disp8. 4 bytes.
// Used by the Phase 6.3.4.n.2.f cells.ptr hoist hot form to read the
// stashed cells.ptr at &regsI64[NumRegsI64] (disp = NumRegsI64*8, which
// is in 0..64 since the AMD64 cell-bank caps NumRegsI64 at 8). Caller
// must pre-check that rBase != RSP/R12 (would require a SIB byte).
func mov64LoadDisp8(dst, base int, disp int8) []byte {
	return []byte{rex(true, dst >= 8, false, base >= 8), 0x8B, modRM(1, byte(dst&7), byte(base&7)), byte(disp)}
}

// mov64StoreDisp8 emits `mov rSrc, disp8(rBase)`, the store mirror of
// mov64LoadDisp8. Opcode REX.W 89 /r mod=01 disp8. 4 bytes. Used by
// the Phase 6.3.4.n.2.f hoist prologue to stash cells.ptr at
// &regsI64[NumRegsI64].
func mov64StoreDisp8(src, base int, disp int8) []byte {
	return []byte{rex(true, src >= 8, false, base >= 8), 0x89, modRM(1, byte(src&7), byte(base&7)), byte(disp)}
}

// mov32StoreDisp8 emits `mov %eSrc, disp8(rBase)`, a 32-bit reg-to-mem
// store with an 8-bit signed displacement. Opcode 89 /r with mod=01
// disp8. No REX is emitted when neither register needs the high bit.
// Used by Phase 6.3.4.n.2.c to write vmList.len (a u32 at byte offset
// 4 of the slab) as a side-effect of OpListPushI64. 3 bytes when
// neither reg needs REX, 4 bytes otherwise. Caller pre-checks that
// rBase is not RSP/R12 (would require a SIB byte) and not RBP/R13 in
// mod=00, which is moot here since mod=01 forces a disp8.
func mov32StoreDisp8(src, base int, disp int8) []byte {
	var out []byte
	if src >= 8 || base >= 8 {
		var r byte = 0x40
		if src >= 8 {
			r |= 0x04 // REX.R
		}
		if base >= 8 {
			r |= 0x01 // REX.B
		}
		out = append(out, r)
	}
	out = append(out, 0x89, modRM(1, byte(src&7), byte(base&7)), byte(disp))
	return out
}

// mov16ImmStoreSIBDisp8 emits `movw $imm16, disp8(rBase, rIdx, 8)`, a
// 16-bit immediate store at the address `rBase + rIdx*8 + disp8`.
// Encoding: 66 (operand-size prefix) C7 /0 with mod=01, rm=100 (SIB
// follows), SIB(scale=11, index=rIdx, base=rBase), disp8, imm16. Phase
// 6.3.4.n.2.c uses this to overwrite the top 16 bits of a just-stored
// 8-byte Cell with the Int48 tag (0xFFFA) at byte offset 6 of the
// slot, packing the 48-bit payload with the tag in a single 7-byte
// instruction. Caller pre-checks rIdx != RSP and rBase != RBP/R13
// w/ mod=00 (here mod=01 so the [disp32]-quirk does not apply, but
// rBase=RBP would still need a different SIB encoding; we only call
// this with rBase=RDX). 7 bytes when neither reg needs REX, 8 bytes
// otherwise.
func mov16ImmStoreSIBDisp8(imm uint16, base, idx int, disp int8) []byte {
	// Prefix order: 0x66 (operand-size, group 3) must precede REX, since
	// REX must immediately precede the opcode byte. The earlier ordering
	// (REX then 0x66) caused the CPU to ignore the REX prefix entirely,
	// dropping REX.X and silently demoting an R8..R15 index to RAX..RDI,
	// producing wild addresses when the SIB index was a high register.
	out := []byte{0x66}
	if base >= 8 || idx >= 8 {
		var r byte = 0x40
		if idx >= 8 {
			r |= 0x02 // REX.X
		}
		if base >= 8 {
			r |= 0x01 // REX.B
		}
		out = append(out, r)
	}
	sib := byte(3<<6) | byte((idx&7)<<3) | byte(base&7)
	out = append(out, 0xC7, modRM(1, 0, 4), sib, byte(disp))
	var b [2]byte
	binary.LittleEndian.PutUint16(b[:], imm)
	return append(out, b[:]...)
}

// movMemImm32R15Indirect emits `movq $imm32, (%r15)` (sign-extended).
// Opcode REX.W|REX.B C7 /0 ModR/M(mod=00, reg=0, rm=111) imm32. Used by
// the deopt block to write *status without going through RAX.
func movMemImm32R15Indirect(imm int32) []byte {
	out := []byte{0x49, 0xC7, modRM(0, 0, 7)}
	out = appendImm32(out, imm)
	return out
}

// cmpMemImm8R15Indirect emits `cmpq $imm8, (%r15)` (sign-extended).
// Opcode REX.W|REX.B 83 /7 ModR/M(mod=00, reg=7, rm=111) imm8. Used by
// self-recursive OpCallMixed (Phase 6.3.4.m.4c.5) to test the callee's
// status word without clobbering RAX (which carries the call result).
// 4 bytes total.
func cmpMemImm8R15Indirect(imm int8) []byte {
	return []byte{0x49, 0x83, modRM(0, 7, 7), byte(imm)}
}

// movRImm32SignExt emits `mov rDst, imm32` (sign-extended to 64-bit).
// Opcode REX.W C7 /0 imm32 — 7 bytes total. Used for the K-form Div/Mod
// divisor load.
func movRImm32SignExt(dst int, imm int32) []byte {
	out := []byte{rex(true, false, false, dst >= 8), 0xC7, modRM(3, 0, byte(dst))}
	out = appendImm32(out, imm)
	return out
}

// movRImm64 emits `mov rDst, imm64`. Opcode REX.W B8+rd imm64 — 10
// bytes total. Used when imm does not fit in int32.
func movRImm64(dst int, imm int64) []byte {
	out := []byte{rex(true, false, false, dst >= 8), 0xB8 | byte(dst&7)}
	var b [8]byte
	binary.LittleEndian.PutUint64(b[:], uint64(imm))
	return append(out, b[:]...)
}

// movImm64 emits the shortest mov-imm sequence for v into dst: imm32
// sign-extended (7 bytes) when v fits in int32, otherwise the full
// imm64 form (10 bytes).
func movImm64(dst int, v int64) []byte {
	if v >= -(1<<31) && v <= (1<<31)-1 {
		return movRImm32SignExt(dst, int32(v))
	}
	return movRImm64(dst, v)
}

// movImm64ByteCount mirrors movImm64. dst is consulted only for parity
// with the AArch64 signature; on AMD64 the encoding depends only on v.
func movImm64ByteCount(v int64, dst int) int {
	if v >= -(1<<31) && v <= (1<<31)-1 {
		return 7
	}
	return 10
}

// push64 emits `push rSrc` (1 or 2 bytes depending on REX.B need).
func push64(r int) []byte {
	if r >= 8 {
		return []byte{0x41, 0x50 | byte(r&7)}
	}
	return []byte{0x50 | byte(r&7)}
}

// pop64 emits `pop rDst`.
func pop64(r int) []byte {
	if r >= 8 {
		return []byte{0x41, 0x58 | byte(r&7)}
	}
	return []byte{0x58 | byte(r&7)}
}

// subRSPImm8 emits `sub rsp, imm8` (4 bytes). Used for stack alignment.
func subRSPImm8(imm int8) []byte {
	return []byte{0x48, 0x83, modRM(3, 5, xRSP), byte(imm)}
}

// lea64Disp32 emits `lea rDst, [rBase + disp32]`. Opcode REX.W 8D /r
// with mod=10 (disp32). Used at the self-recursive OpCallI64 site to
// load RDI with the callee's regs-window base before the CALL so the
// callee prologue's `mov %rdi, %rbx` lands on a valid pointer.
func lea64Disp32(dst, base int, disp int32) []byte {
	out := []byte{rex(true, dst >= 8, false, base >= 8), 0x8D, modRM(2, byte(dst), byte(base))}
	out = appendImm32(out, disp)
	return out
}

// addRSPImm8 emits `add rsp, imm8` (4 bytes).
func addRSPImm8(imm int8) []byte {
	return []byte{0x48, 0x83, modRM(3, 0, xRSP), byte(imm)}
}

// retByte returns the single-byte RET opcode.
func retByte() byte { return 0xC3 }

// jccRel32 emits `jcc rel32` (6 bytes). cc is the low nibble of the
// conditional jump opcode (see cc* constants above).
func jccRel32(cc byte, rel int32) []byte {
	out := []byte{0x0F, 0x80 | cc}
	return appendImm32(out, rel)
}

// jmpRel32 emits `jmp rel32` (5 bytes).
func jmpRel32(rel int32) []byte {
	out := []byte{0xE9}
	return appendImm32(out, rel)
}

// callRel32 emits `call rel32` (5 bytes). Used for self-recursive
// OpCallI64.
func callRel32(rel int32) []byte {
	out := []byte{0xE8}
	return appendImm32(out, rel)
}

// callIndirectR10 emits `call *%r10` (3 bytes: 41 FF D2). Used by
// cross-fn OpCallMixed (Phase 6.3.4.m.4c.6) after a `movabs $callee,
// %r10` loads the callee's entry pointer. R10 is caller-saved and not
// pinned to any vm3 i64 slot at the call site (the spill/reload step
// runs before the movabs, so any live i64 in slot-4=R10 has been
// saved). Opcode FF /2 (mod=11, reg=2, rm=R10 with REX.B).
func callIndirectR10() []byte {
	return []byte{0x41, 0xFF, modRM(3, 2, byte(xR10&7))}
}

// appendImm32 appends a little-endian 32-bit integer to out.
func appendImm32(out []byte, v int32) []byte {
	var b [4]byte
	binary.LittleEndian.PutUint32(b[:], uint32(v))
	return append(out, b[:]...)
}

// --- SSE2 / XMM encoders for the f64 bank ---
//
// All forms here keep REX optional (4-byte encodings) when both operands
// are xmm0..7 to match byteCountAMD64's predictions. When either side is
// xmm8..15 a REX prefix is added, growing the encoding by one byte. The
// 5-byte form is used by xorpd against xmm15 in OpNegF64 and by the
// MOVQ GPR<->XMM bit-cast ops where REX.W is always present.

// sseRR2 emits `<F2-prefix>sse op xmmDst, xmmSrc` for ADDSD/SUBSD/
// MULSD/DIVSD/MOVSD. Opcodes are F2 [REX] 0F xx /r.
func sseRR2(opc byte, dst, src int) []byte {
	if dst < 8 && src < 8 {
		return []byte{0xF2, 0x0F, opc, modRM(3, byte(dst), byte(src))}
	}
	return []byte{0xF2, rex(false, dst >= 8, false, src >= 8), 0x0F, opc, modRM(3, byte(dst&7), byte(src&7))}
}

// movsdRR emits `movsd xmmDst, xmmSrc` (low-64-bit register copy).
// Opcode F2 [REX] 0F 10 /r. Always emits the instruction even when
// dst==src so byteCount predictions stay deterministic.
func movsdRR(dst, src int) []byte { return sseRR2(0x10, dst, src) }

// addsdRR emits `addsd xmmDst, xmmSrc`. Opcode F2 [REX] 0F 58 /r.
func addsdRR(dst, src int) []byte { return sseRR2(0x58, dst, src) }

// subsdRR emits `subsd xmmDst, xmmSrc`. Opcode F2 [REX] 0F 5C /r.
func subsdRR(dst, src int) []byte { return sseRR2(0x5C, dst, src) }

// mulsdRR emits `mulsd xmmDst, xmmSrc`. Opcode F2 [REX] 0F 59 /r.
func mulsdRR(dst, src int) []byte { return sseRR2(0x59, dst, src) }

// divsdRR emits `divsd xmmDst, xmmSrc`. Opcode F2 [REX] 0F 5E /r.
func divsdRR(dst, src int) []byte { return sseRR2(0x5E, dst, src) }

// sqrtsdRR emits `sqrtsd xmmDst, xmmSrc`. Opcode F2 [REX] 0F 51 /r.
// IEEE 754 correctly-rounded scalar square root; bit-identical to Go's
// math.Sqrt on AMD64 (which emits the same SQRTSD). Phase 6.3.4.h.2
// (AMD64 catch-up for OpSqrtF64, mirror of ARM64 FSQRT in lower_arm64).
func sqrtsdRR(dst, src int) []byte { return sseRR2(0x51, dst, src) }

// vfmaddSDRR emits a 3-byte-VEX-encoded VFMADDxxxSD xmmDst, xmmSrc1, xmmSrc2.
// opc selects the FMA3 variant:
//
//	0x98 = VFMADD132SD: dst = dst*src2 + src1
//	0xA8 = VFMADD213SD: dst = src1*dst + src2
//	0xB8 = VFMADD231SD: dst = src1*src2 + dst
//
// All operands must live in xmm0..7 (vm3 caps f64 regs at 8). Encoding:
//
//	C4 byte1 byte2 opc modrm   (5 bytes total)
//	  byte1 = 11100010b = 0xE2  (R̄=X̄=B̄=1, mmmmm=00010 for 0F 38 escape)
//	  byte2 = 1 vvvv 0 01b      (W=1, ~src1 in vvvv, L=0, pp=01 for 66)
//	  modrm = 11 dst src2       (register-register, ModRM.r/m = src2)
//
// Used by Phase 6.3.4.h.2 (AMD64 catch-up for OpFmaF64, mirror of ARM64
// FMADD in lower_arm64). Bit-identical to math.FMA per IEEE 754-2008.
func vfmaddSDRR(opc byte, dst, src1, src2 int) []byte {
	vvvv := byte(0xF) ^ byte(src1&0xF)
	byte2 := byte(0x80) | (vvvv << 3) | 0x01
	return []byte{0xC4, 0xE2, byte2, opc, modRM(3, byte(dst&7), byte(src2&7))}
}

// movsdLoadXMMFromGPR emits `movsd xmm<dst>, disp32(%baseGPR)`.
// Opcode F2 [REX] 0F 10 /r disp32. REX is emitted when dst or base
// needs the high bit (R8..R15, including R12/R14 used as the f64
// base). When base&7 == 4 (RSP or R12) the SIB-follows quirk forces
// an extra SIB byte (0x24 = scale=0/index=none/base=4) between the
// ModR/M and the disp32. Phase 6.3.4.n.3 generalizes the original
// movsdLoadXMMFromR14 so cell-bank+f64 fns can pin the f64 base in
// R12; without the SIB byte the decoder reads the first byte of
// disp32 as SIB and the whole prologue desyncs.
func movsdLoadXMMFromGPR(dst int, base int, disp int32) []byte {
	var out []byte
	if dst >= 8 || base >= 8 {
		out = []byte{0xF2, rex(false, dst >= 8, false, base >= 8), 0x0F, 0x10, modRM(2, byte(dst&7), byte(base&7))}
	} else {
		out = []byte{0xF2, 0x0F, 0x10, modRM(2, byte(dst), byte(base))}
	}
	if base&7 == 4 {
		out = append(out, 0x24)
	}
	return appendImm32(out, disp)
}

// movsdLoadXMMIdxLsl3 emits `movsd xmm<dst>, [r<base> + r<idx>*8]`.
// Opcode F2 [REX] 0F 10 /r with mod=00, ModR/M.r/m=100 (SIB follows),
// SIB(scale=3, index=idx, base=base). Used by Phase 6.3.4.n.3 to load
// an F64Array element after the cells.ptr has been computed into the
// base GPR. Caller must pre-check rBase is not RBP/R13 (those need
// mod=01+disp under the SIB.base quirk). Returns 5 bytes; the F2
// prefix is one byte, REX is one byte (always emitted because we may
// have base >= 8 or idx >= 8), then opcode + ModR/M + SIB. When dst,
// base, and idx are all xmm0..7 / RAX..RDI the REX byte still gets
// emitted (0x40) for predictability of the byte count.
func movsdLoadXMMIdxLsl3(dst, base, idx int) []byte {
	rexB := byte(0x40)
	if dst >= 8 {
		rexB |= 0x04 // REX.R
	}
	if idx >= 8 {
		rexB |= 0x02 // REX.X
	}
	if base >= 8 {
		rexB |= 0x01 // REX.B
	}
	mod := modRM(0, byte(dst&7), 4) // rm=100 = SIB follows
	sib := byte(3<<6) | byte((idx&7)<<3) | byte(base&7)
	return []byte{0xF2, rexB, 0x0F, 0x10, mod, sib}
}

// movsdStoreXMMIdxLsl3 emits `movsd [r<base> + r<idx>*8], xmm<src>`.
// Opcode F2 [REX] 0F 11 /r SIB. Mirrors movsdLoadXMMIdxLsl3 for the
// store side. Same RBP/R13 base caveat as the load. Returns 6 bytes
// like the load.
func movsdStoreXMMIdxLsl3(src, base, idx int) []byte {
	rexB := byte(0x40)
	if src >= 8 {
		rexB |= 0x04 // REX.R
	}
	if idx >= 8 {
		rexB |= 0x02 // REX.X
	}
	if base >= 8 {
		rexB |= 0x01 // REX.B
	}
	mod := modRM(0, byte(src&7), 4)
	sib := byte(3<<6) | byte((idx&7)<<3) | byte(base&7)
	return []byte{0xF2, rexB, 0x0F, 0x11, mod, sib}
}

// xorpdRR emits `xorpd xmmDst, xmmSrc`. Opcode 66 [REX] 0F 57 /r. Used
// by OpNegF64 to flip the sign bit by XOR with xmm15 holding
// 0x8000000000000000.
func xorpdRR(dst, src int) []byte {
	if dst < 8 && src < 8 {
		return []byte{0x66, 0x0F, 0x57, modRM(3, byte(dst), byte(src))}
	}
	return []byte{0x66, rex(false, dst >= 8, false, src >= 8), 0x0F, 0x57, modRM(3, byte(dst&7), byte(src&7))}
}

// ucomisdRR emits `ucomisd xmmLhs, xmmRhs` (unordered compare, sets
// EFLAGS). Opcode 66 [REX] 0F 2E /r. The f64 compare-and-branch lowers
// to a UCOMISD followed by a JP-guarded JCC pair (or single JA/JAE for
// Gt/Ge).
func ucomisdRR(lhs, rhs int) []byte {
	if lhs < 8 && rhs < 8 {
		return []byte{0x66, 0x0F, 0x2E, modRM(3, byte(lhs), byte(rhs))}
	}
	return []byte{0x66, rex(false, lhs >= 8, false, rhs >= 8), 0x0F, 0x2E, modRM(3, byte(lhs&7), byte(rhs&7))}
}

// movqXMMFromR64 emits `movq xmmDst, rSrc64` (GPR-to-XMM bit-cast).
// Opcode 66 REX.W 0F 6E /r. xmm goes in reg field, r64 in r/m field.
// REX.W is always set so the encoding is always 5 bytes.
func movqXMMFromR64(xmm, r64 int) []byte {
	return []byte{0x66, rex(true, xmm >= 8, false, r64 >= 8), 0x0F, 0x6E, modRM(3, byte(xmm&7), byte(r64&7))}
}

// movqR64FromXMM emits `movq rDst64, xmmSrc` (XMM-to-GPR bit-cast).
// Opcode 66 REX.W 0F 7E /r. Used by OpReturnF64 to deliver the f64 in
// RAX (where the trampoline picks it up as a uint64 and the Go caller
// reads with math.Float64frombits).
func movqR64FromXMM(r64, xmm int) []byte {
	return []byte{0x66, rex(true, xmm >= 8, false, r64 >= 8), 0x0F, 0x7E, modRM(3, byte(xmm&7), byte(r64&7))}
}

// cvtsi2sd emits `cvtsi2sd xmmDst, rSrc64` (signed int64 to double).
// Opcode F2 REX.W 0F 2A /r. Always 5 bytes.
func cvtsi2sd(xmm, r64 int) []byte {
	return []byte{0xF2, rex(true, xmm >= 8, false, r64 >= 8), 0x0F, 0x2A, modRM(3, byte(xmm&7), byte(r64&7))}
}

// cvttsd2si emits `cvttsd2si rDst64, xmmSrc` (truncating double to
// signed int64). Opcode F2 REX.W 0F 2C /r. Always 5 bytes.
func cvttsd2si(r64, xmm int) []byte {
	return []byte{0xF2, rex(true, r64 >= 8, false, xmm >= 8), 0x0F, 0x2C, modRM(3, byte(r64&7), byte(xmm&7))}
}

// cvtsi2sdByteCount mirrors cvtsi2sd; REX.W is always present so the
// encoding is always 5 bytes regardless of operand register selection.
func cvtsi2sdByteCount(r uint16) int { return 5 }

// cvttsd2siByteCount mirrors cvttsd2si; always 5 bytes.
func cvttsd2siByteCount(r uint16) int { return 5 }

// condForCmpF64AMD64 returns the JCC condition for an f64 compare-and-
// branch. UCOMISD sets EFLAGS for `lhs RELATION rhs`:
//
//	greater: CF=0, ZF=0, PF=0
//	less:    CF=1, ZF=0, PF=0
//	equal:   CF=0, ZF=1, PF=0
//	unordered (NaN): CF=1, ZF=1, PF=1
//
// Eq/Lt/Le are emitted as a JP-skip-+6 + JCC pair so the JCC only fires
// on the ordered case. Gt/Ge are JA/JAE, which require CF=0 and so
// naturally exclude the NaN path. Ne is special-cased inline as
// JP-to-target + JNE-to-target so NaN propagates to a branch.
func condForCmpF64AMD64(code vm3.OpCode) byte {
	switch code {
	case vm3.OpCmpEqF64Br:
		return 0x4 // JE  (ZF=1)
	case vm3.OpCmpLtF64Br:
		return 0x2 // JB  (CF=1)
	case vm3.OpCmpLeF64Br:
		return 0x6 // JBE (CF=1 OR ZF=1)
	case vm3.OpCmpGtF64Br:
		return 0x7 // JA  (CF=0 AND ZF=0)
	case vm3.OpCmpGeF64Br:
		return 0x3 // JAE (CF=0)
	}
	return 0x4
}
