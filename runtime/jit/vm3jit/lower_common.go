package vm3jit

import "mochi/runtime/vm3"

// Status codes the JIT writes through *(status word ptr) on a deopt
// path. Caller checks this word after a trampoline.CallStatus return
// and routes to the corresponding vm3 error if non-zero.
const (
	StatusOK        int64 = 0
	StatusDivByZero int64 = 1
	// StatusListGrow is the deopt code written by OpListPushI64 when
	// cells.len >= cells.cap, signalling the JIT cannot append in
	// place. The deopt block also spills all pinned regs back to the
	// regsX base arrays so the interpreter can resume from PC=0 with
	// the JIT's final state via the deopt-scratch protocol in vm3.VM
	// (Phase 6.2d.2.c). Kernels admitted under the OpListPushI64
	// whitelist have a single loop entry at PC=0, so restart at PC=0
	// with the spilled regs reproduces the next iteration of the
	// fill loop without losing state.
	StatusListGrow int64 = 2
	// StatusMapGrow is the deopt code written by OpMapSetI64I64 when
	// the load-factor 0.5 trigger (2*(nLive+1) > cap) fires. Like
	// StatusListGrow it spills pinned regs and routes back through
	// the interpreter at PC=0 (Phase 6.2d.2.d step 4). OpMapGetI64I64
	// never grows the table so it does not emit a deopt for grow,
	// only for an empty-table miss (also routed via StatusMapGrow).
	StatusMapGrow int64 = 3
)

// hasRegRegDivMod reports whether fn contains any reg-reg OpDivI64 or
// OpModI64 (the K-form variants reject /0 at Compile time and need no
// runtime guard, so they do not contribute to the deopt block).
func hasRegRegDivMod(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpDivI64 || op.Code == vm3.OpModI64 {
			return true
		}
	}
	return false
}

// hasListPushI64 reports whether fn contains any OpListPushI64. Each
// site emits an inline cap check + conditional branch to the
// StatusListGrow deopt block (Phase 6.2d.2.c).
func hasListPushI64(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpListPushI64 {
			return true
		}
	}
	return false
}

// hasListGetI64 reports whether fn contains any OpListGetI64.
func hasListGetI64(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpListGetI64 {
			return true
		}
	}
	return false
}

// hasMapSetI64I64 reports whether fn contains any OpMapSetI64I64. Each
// site emits inline splitmix64 + probe loop + 3 stores + nLive bump,
// gated on a load-factor 0.5 grow check that deopts via StatusMapGrow
// (Phase 6.2d.2.d step 4).
func hasMapSetI64I64(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpMapSetI64I64 {
			return true
		}
	}
	return false
}

// hasMapGetI64I64 reports whether fn contains any OpMapGetI64I64.
func hasMapGetI64I64(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpMapGetI64I64 {
			return true
		}
	}
	return false
}

// hasMapOpI64 reports whether fn touches the map slab via either of
// the JIT-lowered map ops. Used by the prologue to decide whether to
// load mapsBase (vs listsBase) into the slab-base pin and whether to
// reserve the auxiliary scratch pair holding {mask, table.ptr}.
func hasMapOpI64(fn *vm3.Function) bool {
	return hasMapSetI64I64(fn) || hasMapGetI64I64(fn)
}

// popcount32 counts set bits in v. Used to size OpCallI64 spill loops
// on every backend.
func popcount32(v uint32) int {
	v = v - ((v >> 1) & 0x55555555)
	v = (v & 0x33333333) + ((v >> 2) & 0x33333333)
	v = (v + (v >> 4)) & 0x0F0F0F0F
	return int((v * 0x01010101) >> 24)
}

// liveSuccUnion is the live-in union across the successors of pc i.
// Branch opcodes have two successors (fall-through and op.C target);
// Jump has one (op.C); Return* have none; everything else falls
// through to i+1. Used by both lower_arm64 and lower_amd64 backward
// liveness passes.
func liveSuccUnion(fn *vm3.Function, i int, liveIn []uint32) uint32 {
	op := fn.Code[i]
	switch op.Code {
	case vm3.OpReturnI64, vm3.OpReturnConstK, vm3.OpReturnF64:
		return 0
	case vm3.OpJump:
		t := int(uint16(op.C))
		if t < 0 || t > len(fn.Code) {
			return 0
		}
		return liveIn[t]
	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br,
		vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		t := int(uint16(op.C))
		if t < 0 || t > len(fn.Code) {
			return liveIn[i+1]
		}
		return liveIn[i+1] | liveIn[t]
	default:
		if i+1 <= len(fn.Code) {
			return liveIn[i+1]
		}
		return 0
	}
}

// fmaFusion describes a JIT-level peephole that absorbs the OpMulF64
// at mulIdx into the OpAddF64 or OpSubF64 at mulIdx+1, emitting a
// single ARM64 FMADD or FMSUB (or AMD64 VFMADD/VFMSUB) in place of
// the two-instruction sequence. Phase 6.3.4.j.4b.
//
// FMADD encodes Dd = Da + Dn*Dm and FMSUB encodes Dd = Da - Dn*Dm.
// For OpAddF64 the fusion is FMADD with Da = the non-mul-result
// addend. For OpSubF64 the fusion is FMSUB only when the subtrahend
// (op.C) equals the mul's destination; otherwise the shape would
// require FNMSUB-style restructuring and is left unfused.
//
// All five register indices live in fn's f64 register bank (8 entries,
// MaxF64Regs).
type fmaFusion struct {
	Kind byte   // 'a' for FMADD, 's' for FMSUB
	Dd   uint16 // dest reg (op.A of the consuming Add/SubF64)
	Dn   uint16 // first mul source (mul.B)
	Dm   uint16 // second mul source (uint16(mul.C))
	Da   uint16 // the non-mul-result addend / minuend
}

// fmaFusionAt reports whether the OpAddF64 / OpSubF64 at idx fuses
// with the preceding OpMulF64 at idx-1 into a single FMADD or FMSUB.
// Used by both lower_arm64 and lower_amd64.
//
// Safety conditions:
//   - idx >= 1 and fn.Code[idx-1].Code == OpMulF64.
//   - fn.Code[idx].Code is OpAddF64 (Add fuses to FMADD) or OpSubF64
//     with op.C == mul.A (Sub fuses to FMSUB).
//   - mul.A is not live past idx (next access of mul.A in fn.Code
//     is either a re-definition or end-of-function).
//   - No branch in fn.Code targets idx (forbids landing on the
//     consumer without the absorbed MUL having executed).
//   - Both ops sit clear of any cells.ptr refresh PC the lowering
//     might inject; that check is backend-local and goes in the
//     caller (the refresh prefix only touches x-regs, so a fusion
//     straddling refreshPC is actually safe, but we filter it out
//     here to keep the analysis self-contained).
func fmaFusionAt(fn *vm3.Function, idx int) (fmaFusion, bool) {
	if idx < 1 || idx >= len(fn.Code) {
		return fmaFusion{}, false
	}
	mul := fn.Code[idx-1]
	op := fn.Code[idx]
	if mul.Code != vm3.OpMulF64 {
		return fmaFusion{}, false
	}
	var f fmaFusion
	f.Dd = op.A
	f.Dn = mul.B
	f.Dm = uint16(mul.C)
	switch op.Code {
	case vm3.OpAddF64:
		switch {
		case op.B == mul.A && uint16(op.C) != mul.A:
			f.Da = uint16(op.C)
		case uint16(op.C) == mul.A && op.B != mul.A:
			f.Da = op.B
		default:
			return fmaFusion{}, false
		}
		f.Kind = 'a'
	case vm3.OpSubF64:
		if uint16(op.C) != mul.A || op.B == mul.A {
			return fmaFusion{}, false
		}
		f.Da = op.B
		f.Kind = 's'
	default:
		return fmaFusion{}, false
	}
	if isF64LiveAfter(fn, idx, mul.A) {
		return fmaFusion{}, false
	}
	if branchTargetsContain(fn, idx) {
		return fmaFusion{}, false
	}
	return f, true
}

// fmaFusionAbsorbed reports whether the op at idx is an OpMulF64 that
// gets absorbed into a fusion at idx+1. Used by lowering to emit zero
// words for the absorbed MUL.
func fmaFusionAbsorbed(fn *vm3.Function, idx int) bool {
	if idx < 0 || idx+1 >= len(fn.Code) {
		return false
	}
	if fn.Code[idx].Code != vm3.OpMulF64 {
		return false
	}
	_, ok := fmaFusionAt(fn, idx+1)
	return ok
}

// isF64LiveAfter returns true if some op after idx reads f64 register r
// before any op redefines it. End-of-function counts as not-live.
func isF64LiveAfter(fn *vm3.Function, idx int, r uint16) bool {
	for j := idx + 1; j < len(fn.Code); j++ {
		d, u := defUseF64(fn.Code[j])
		bit := uint16(1) << r
		if u&bit != 0 {
			return true
		}
		if d&bit != 0 {
			return false
		}
	}
	return false
}

// defUseF64 returns (defs, uses) bitmasks over fn's f64 register bank
// for op. Bit r is set iff f64 register r is defined / used. Used by
// the FMA fusion liveness check.
func defUseF64(op vm3.Op) (uint16, uint16) {
	a := uint16(1) << op.A
	b := uint16(1) << op.B
	c := uint16(1) << uint16(op.C)
	switch op.Code {
	case vm3.OpMovF64, vm3.OpNegF64, vm3.OpSqrtF64:
		return a, b
	case vm3.OpConstF64K:
		return a, 0
	case vm3.OpAddF64, vm3.OpSubF64, vm3.OpMulF64, vm3.OpDivF64:
		return a, b | c
	case vm3.OpFmaF64:
		mul2 := uint16(1) << (uint16(op.C) & 0xFF)
		addend := uint16(1) << ((uint16(op.C) >> 8) & 0xFF)
		return a, b | mul2 | addend
	case vm3.OpCmpEqF64Br, vm3.OpCmpNeF64Br,
		vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br,
		vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br:
		return 0, a | b
	case vm3.OpListGetF64:
		return a, 0
	case vm3.OpListSetF64:
		return 0, b
	case vm3.OpI64ToF64:
		return a, 0
	case vm3.OpF64ToI64:
		return 0, b
	case vm3.OpReturnF64:
		return 0, a
	case vm3.OpCallF64:
		return a, 0
	}
	return 0, 0
}

// branchTargetsContain reports whether any branch or jump op in fn
// targets bytecode index target. Used by FMA fusion to forbid landing
// on a consumer that would otherwise execute without its absorbed MUL.
func branchTargetsContain(fn *vm3.Function, target int) bool {
	for _, op := range fn.Code {
		switch op.Code {
		case vm3.OpJump,
			vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
			vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
			vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br,
			vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
			vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
			vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr,
			vm3.OpCmpEqF64Br, vm3.OpCmpNeF64Br,
			vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br,
			vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br:
			if int(uint16(op.C)) == target {
				return true
			}
		}
	}
	return false
}

// defUseI64 returns (defs, uses) bitmasks for op over fn's i64 register
// bank. Bit r is set iff register r is defined/used. For OpCallI64 the
// uses cover op.B..op.B+nArgs-1.
func defUseI64(fn *vm3.Function, op vm3.Op) (uint32, uint32) {
	a := uint32(1) << op.A
	b := uint32(1) << op.B
	c := uint32(1) << uint16(op.C)
	switch op.Code {
	case vm3.OpConstI64K, vm3.OpConstI64KW:
		return a, 0
	case vm3.OpMovI64:
		return a, b
	case vm3.OpAddI64, vm3.OpSubI64, vm3.OpMulI64,
		vm3.OpDivI64, vm3.OpModI64:
		return a, b | c
	case vm3.OpNegI64:
		return a, b
	case vm3.OpAddI64K, vm3.OpSubI64K, vm3.OpMulI64K,
		vm3.OpDivI64K, vm3.OpModI64K:
		return a, b
	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br:
		return 0, a | b
	case vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		return 0, a
	case vm3.OpJump:
		return 0, 0
	case vm3.OpReturnI64:
		return 0, a
	case vm3.OpReturnConstK:
		return 0, 0
	case vm3.OpCallI64:
		var uses uint32
		n := fn.NumI64Params()
		for k := 0; k < n; k++ {
			uses |= 1 << (op.B + uint16(k))
		}
		return a, uses
	case vm3.OpCallMixed:
		// Conservative liveness for the cross-fn path: the callee's
		// ParamBanks live in another Function record we do not thread
		// through this helper, so over-approximate uses to the 8-slot
		// window starting at op.B (ParamBanks length is bounded by 8
		// in the snapshot arrays in vm3.VM.OpCallMixed). Over-stating
		// uses at the call site only feeds back into predecessor
		// liveness, which produces correct (possibly larger) spill
		// sets at upstream call sites; the call site's own spill mask
		// derives from liveOut, which is unaffected. defs follow the
		// retBank: only the i64 bank reuses op.A as an i64 reg index.
		var defs uint32
		if vm3.Bank(op.BankFlags&0x3) == vm3.BankI64 {
			defs = a
		}
		uses := uint32(0xFF) << op.B
		return defs, uses
	}
	return 0, 0
}
