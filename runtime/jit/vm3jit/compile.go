package vm3jit

import (
	"encoding/binary"
	"errors"
	"fmt"
	"unsafe"

	"mochi/runtime/vm3"
)

// ErrUnsupported is returned by Compile on architectures that have no
// vm3jit backend.
var ErrUnsupported = errors.New("vm3jit: no backend for this architecture")

// ErrNotImplemented is returned by Compile when fn uses an opcode,
// register count, or bank shape outside the current backend's scope.
// Callers fall back to the vm3 interpreter.
var ErrNotImplemented = errors.New("vm3jit: not implemented")

// maxI64Regs is the cap on simultaneously live i64 registers in the
// AArch64 JIT. Slots 0..6 land in x9..x15 (caller-saved, free); slots
// 7..16 land in x19..x28 (callee-saved, requires STP/LDP pairs in the
// prologue/epilogue). Mirrors vm2jit MEP-39 §6.14 Phase B.
const maxI64Regs = 17

// maxI64RegsAMD64 is the cap on the AMD64 backend. Slots 0..5 land in
// RSI, RDI, R8, R9, R10, R11 (caller-saved); slots 6..8 land in R12,
// R13, R14 (callee-saved, pushed/popped in prologue/epilogue); slot 9
// lands in RBP (callee-saved, pushed/popped). The remaining x86_64
// GPRs are reserved: RAX is the return register and IDIV quotient;
// RCX is scratch; RDX is IDIV remainder; RBX holds the regsI64 base
// pointer (preserved by the SysV ABI across our internal CALL); R15
// holds the *int64 status word pointer; RSP is the stack pointer.
// See lower_amd64.go.
//
// When fn.NumRegsF64 > 0 the AMD64 backend additionally steals R14 to
// hold the regsF64 base pointer, dropping the effective i64 cap to 8.
// When fn.NumRegsCell > 0 the backend steals R14 (arenaCtx) and RBP
// (regsCell base), dropping the effective i64 cap to 8 as well.
const maxI64RegsAMD64 = 10

// maxF64RegsARM64 is the cap on simultaneously-live f64 registers in
// the AArch64 backend. Slots 0..7 land in v0..v7 (caller-saved SIMD/FP).
// Phase 6.2b kernels are f64-only inside the loop body, no calls cross
// the f64 slots, so we do not yet need v8..v15 (callee-saved). The
// regsF64 base pointer is pinned in x2 by the CallStatusFF trampoline
// and copied into x3 in the prologue (x3 is otherwise unused; freeing
// x2 for scratch is unnecessary because the prologue keeps it live).
const maxF64RegsARM64 = 8

// maxF64RegsAMD64 is the AMD64 cap. Slots 0..7 land in xmm0..xmm7
// (caller-saved on SysV). The regsF64 base pointer arrives in RDX and
// is parked in R14 by the prologue. When NumRegsF64 > 0 the i64 cap
// drops to 8 because R14 is stolen.
const maxF64RegsAMD64 = 8

// CompiledFunc is a handle to a vm3jit-compiled function. It owns the
// executable page and must be freed via Free when the function is
// unloaded.
type CompiledFunc struct {
	fn   *vm3.Function
	code []byte
}

// CodeLen returns the size of the JIT'd code in bytes.
func (c *CompiledFunc) CodeLen() int { return len(c.code) }

// Entry returns the executable entry pointer to be passed as the
// first argument of trampoline.Call.
func (c *CompiledFunc) Entry() unsafe.Pointer { return pageEntry(c.code) }

// MaxI64Regs is the cap on simultaneously-live i64 registers the
// AArch64 backend supports. Exported so tests and callers can size
// their reg scratch buffers correctly. The AMD64 backend caps lower
// (see maxI64RegsAMD64); tests that target both architectures must
// stay within the smaller value.
const MaxI64Regs = maxI64Regs

// MaxF64Regs is the cap on simultaneously-live f64 registers the JIT
// supports on either architecture (currently 8 on both AArch64 and
// AMD64; see maxF64RegsARM64 / maxF64RegsAMD64).
const MaxF64Regs = maxF64RegsARM64

// Free releases the executable page.
func (c *CompiledFunc) Free() error {
	if c.code == nil {
		return nil
	}
	err := pageFree(c.code)
	c.code = nil
	return err
}

// Options tunes Compile's behavior. Zero value disables features that
// need extra context from the caller.
type Options struct {
	// SelfIdx is fn's index in its containing Program. When set
	// (>= 0), the JIT lowers self-recursive OpCallI64 (op.C ==
	// SelfIdx) to a native BL inside the same code page. -1
	// disables self-recursion: any OpCallI64 returns ErrNotImplemented
	// and the caller falls back to the interpreter.
	SelfIdx int

	// Prog is fn's containing Program. When set, the JIT can resolve
	// callees in cross-fn OpCallMixed sites (Phase 6.2d.2.b) and
	// inspect their JITCode + ParamBanks at compile time. nil
	// disables cross-fn admission: any OpCallMixed to a non-self
	// callee returns ErrNotImplemented and the caller falls back to
	// the interpreter.
	Prog *vm3.Program
}

// DefaultOptions returns the conservative defaults for callers that
// build a fn standalone (no Program context).
func DefaultOptions() Options { return Options{SelfIdx: -1} }

// Compile lowers fn to native code for the host architecture and
// returns the handle. Equivalent to CompileWithOptions(fn,
// DefaultOptions()).
func Compile(fn *vm3.Function) (*CompiledFunc, error) {
	return CompileWithOptions(fn, DefaultOptions())
}

// CompileInProgram is the convenience form for Programs: it looks up
// fn = prog.Funcs[idx] and threads idx through as opts.SelfIdx so the
// JIT can lower self-recursive OpCallI64.
func CompileInProgram(prog *vm3.Program, idx uint32) (*CompiledFunc, error) {
	if int(idx) >= len(prog.Funcs) {
		return nil, fmt.Errorf("vm3jit: fn idx %d out of range (Funcs=%d)", idx, len(prog.Funcs))
	}
	return CompileWithOptions(prog.Funcs[idx], Options{SelfIdx: int(idx), Prog: prog})
}

// CompileWithOptions is the explicit-options form. Phase 6.0..6.2b
// accepts i64-only and i64+f64 functions whose opcode set is covered
// by the host backend (lower_arm64 / lower_amd64). Phase 6.2d.2.a
// step 2 admits Cell-bank fns on ARM64 whose opcode set is a strict
// subset of {OpListGetI64, OpAddI64*, OpCmp*Br, OpTailCallMixed self-tail,
// OpReturnI64, OpMovI64, OpConstI64K} (the lists_fill_sum "sum" shape).
// AMD64 still rejects Cell-bank pending its own lowering (Phase 6.2d.2.e).
func CompileWithOptions(fn *vm3.Function, opts Options) (*CompiledFunc, error) {
	if fn.NumRegsCell != 0 {
		if err := checkCellBankAdmissible(fn, opts); err != nil {
			return nil, err
		}
	}
	i64Cap, f64Cap, archOK := archCaps(fn)
	if !archOK {
		return nil, ErrUnsupported
	}
	if int(fn.NumRegsI64) > i64Cap {
		return nil, fmt.Errorf("%w: %s uses %d i64 regs (max %d on this arch%s)",
			ErrNotImplemented, fn.Name, fn.NumRegsI64, i64Cap, capNote(fn))
	}
	if int(fn.NumRegsF64) > f64Cap {
		return nil, fmt.Errorf("%w: %s uses %d f64 regs (max %d on this arch)",
			ErrNotImplemented, fn.Name, fn.NumRegsF64, f64Cap)
	}
	if fn.NumRegsF64 > 0 && fn.ResultBank != vm3.BankF64 && fn.ResultBank != vm3.BankI64 {
		return nil, fmt.Errorf("%w: %s f64 fn returns non-{i64,f64} bank",
			ErrNotImplemented, fn.Name)
	}
	raw, err := lowerHost(fn, opts)
	if err != nil {
		return nil, err
	}
	page, err := pageAlloc(len(raw))
	if err != nil {
		return nil, err
	}
	if err := pageWrite(page, raw); err != nil {
		_ = pageFree(page)
		return nil, err
	}
	return &CompiledFunc{fn: fn, code: page}, nil
}

// lowerHost dispatches to the per-arch lowerer and returns the raw
// little-endian byte stream for the executable page.
func lowerHost(fn *vm3.Function, opts Options) ([]byte, error) {
	switch hostArch {
	case ArchARM64:
		words, err := lowerARM64(fn, opts)
		if err != nil {
			return nil, err
		}
		buf := make([]byte, len(words)*4)
		for i, w := range words {
			binary.LittleEndian.PutUint32(buf[i*4:], w)
		}
		return buf, nil
	case ArchAMD64:
		return lowerAMD64(fn, opts)
	default:
		return nil, ErrUnsupported
	}
}

// maxI64RegsCellARM64 is the i64 cap on ARM64 when fn carries a Cell
// bank: callee-saved slots 7..10 land in x21..x24 (4 max), with x19/x20
// stolen for cached arena base pointers (listsBase / future mapsBase).
// Caller-saved slots 0..6 in x9..x15 are unchanged.
const maxI64RegsCellARM64 = 11

// checkCellBankAdmissible enforces the sum-shape whitelist for Cell-bank
// fns on ARM64 (Phase 6.2d.2.a step 2). The whitelist is intentionally
// narrow: only the opcodes the sum kernel actually uses are admitted,
// so unrelated Cell-bank fns (fill, map workloads, ...) keep falling
// back to the interpreter until their own sub-phases land. AMD64 admits
// the narrow scaffold opcode set (i64-only + OpReturnCell) starting in
// Phase 6.3.4.m.4c.1/.2; pair ops, list/map ops, and OpCallMixed are
// added in m.4c.3+.
func checkCellBankAdmissible(fn *vm3.Function, opts Options) error {
	if int(fn.NumRegsCell) > maxCellRegs {
		return fmt.Errorf("%w: %s uses %d Cell regs (max %d on this arch)",
			ErrNotImplemented, fn.Name, fn.NumRegsCell, maxCellRegs)
	}
	if hostArch == ArchAMD64 {
		return checkCellBankAdmissibleAMD64(fn, opts)
	}
	if hostArch != ArchARM64 {
		return fmt.Errorf("%w: %s has Cell bank usage (Cell=%d) on unsupported arch",
			ErrNotImplemented, fn.Name, fn.NumRegsCell)
	}
	for i, op := range fn.Code {
		switch op.Code {
		case vm3.OpConstI64K, vm3.OpConstI64KW, vm3.OpMovI64,
			vm3.OpAddI64, vm3.OpSubI64, vm3.OpMulI64,
			vm3.OpDivI64, vm3.OpModI64,
			vm3.OpAddI64K, vm3.OpSubI64K, vm3.OpMulI64K,
			vm3.OpDivI64K, vm3.OpModI64K,
			vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
			vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
			vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br,
			vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
			vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
			vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr,
			vm3.OpJump, vm3.OpReturnI64, vm3.OpReturnConstK, vm3.OpReturnCell,
			vm3.OpListGetI64, vm3.OpListPushI64, vm3.OpListSetI64,
			vm3.OpListGetF64, vm3.OpListSetF64,
			vm3.OpMapSetI64I64, vm3.OpMapGetI64I64,
			vm3.OpLookupI64KW,
			vm3.OpF64ArrayGetF64, vm3.OpF64ArraySetF64, vm3.OpF64ArrayLenI64,
			vm3.OpI64ArrayGetI64, vm3.OpI64ArraySetI64, vm3.OpI64ArrayPushI64, vm3.OpI64ArrayLenI64,
			vm3.OpPairFst, vm3.OpPairSnd, vm3.OpNewPair,
			vm3.OpConstF64K, vm3.OpMovF64,
			vm3.OpAddF64, vm3.OpSubF64, vm3.OpMulF64, vm3.OpDivF64,
			vm3.OpNegF64, vm3.OpFmaF64, vm3.OpSqrtF64,
			vm3.OpCmpEqF64Br, vm3.OpCmpNeF64Br,
			vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br,
			vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br,
			vm3.OpI64ToF64, vm3.OpF64ToI64,
			vm3.OpReturnF64:
			continue
		case vm3.OpNewList:
			// Phase 6.2d.2.b step 2: admit at pc=0 when the lowerer can
			// skip its emission (jitCall pre-allocates the list on the
			// Go side). canPreAllocList further requires no other op in
			// fn overwrites the cell.
			//
			// Phase 6.3.4.j.3 generalizes the skip to a contiguous prefix
			// of K OpNewList ops at pc 0..K-1. Admit when the op falls
			// inside the prefix (preAllocListPrefix returns K>0 and
			// i<K).
			if i == 0 && canPreAllocList(fn) {
				continue
			}
			if k := int(preAllocListPrefix(fn)); k > 0 && i < k {
				continue
			}
			return fmt.Errorf("%w: %s pc %d Cell-bank fn uses inline OpNewList (only pre-alloc prefix is admitted)",
				ErrNotImplemented, fn.Name, i)
		case vm3.OpNewMap:
			// Phase 6.3.4.f.2: admit at pc=0 when the OpNewMap can be
			// lifted into jitCall (canPreAllocMap). The lowerer emits
			// zero words; the prologue picks up the pre-seeded handle
			// from jf.regsCell[A].
			if i == 0 && canPreAllocMap(fn) {
				continue
			}
			return fmt.Errorf("%w: %s pc %d Cell-bank fn uses inline OpNewMap (only pre-alloc at pc=0 is admitted)",
				ErrNotImplemented, fn.Name, i)
		case vm3.OpNewF64Array:
			// Phase 6.3.4.j.5.b: admit at i < K where K is the contiguous
			// OpNewF64Array prefix length the JIT lifted into jitCall.
			// The lowerer emits zero words for each pre-allocated PC; the
			// prologue's LDR x_cell, [x3, #A*8] picks up the seeded
			// handle. Inline OpNewF64Array outside the prefix routes back
			// to the interpreter (it would need an inline arena-alloc
			// kernel that we have not lowered yet).
			if k := int(preAllocF64ArrPrefix(fn)); k > 0 && i < k {
				continue
			}
			return fmt.Errorf("%w: %s pc %d Cell-bank fn uses inline OpNewF64Array (only pre-alloc prefix is admitted)",
				ErrNotImplemented, fn.Name, i)
		case vm3.OpNewI64Array:
			// Phase 6.3.4.l.4 mirror of OpNewF64Array: admit at i < K
			// where K is the contiguous OpNewI64Array prefix length the
			// JIT lifted into jitCall.
			if k := int(preAllocI64ArrPrefix(fn)); k > 0 && i < k {
				continue
			}
			return fmt.Errorf("%w: %s pc %d Cell-bank fn uses inline OpNewI64Array (only pre-alloc prefix is admitted)",
				ErrNotImplemented, fn.Name, i)
		case vm3.OpTailCallMixed:
			if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx || op.B != 0 {
				return fmt.Errorf("%w: %s pc %d TailCallMixed not a self-tail with argBase=0",
					ErrNotImplemented, fn.Name, i)
			}
		case vm3.OpCallMixed:
			if opts.SelfIdx >= 0 && int(uint16(op.C)) == opts.SelfIdx {
				if err := checkSelfCallMixedAdmissible(fn, op, i, opts); err != nil {
					return err
				}
				continue
			}
			if err := checkCrossFnCallMixedAdmissible(fn, op, i, opts); err != nil {
				return err
			}
		default:
			return fmt.Errorf("%w: %s pc %d Cell-bank fn uses opcode %d outside the sum-shape whitelist",
				ErrNotImplemented, fn.Name, i, op.Code)
		}
	}
	return nil
}

// checkCellBankAdmissibleAMD64 is the AMD64 cell-bank whitelist (Phase
// 6.3.4.m.4c.1 .. m.4c.6, extended in n.2.a, n.2.b, and n.2.c). The
// scaffold admits:
//
//   - the i64 arithmetic / compare-and-branch / control-flow set the
//     existing lower_amd64 supports,
//   - OpReturnCell (m.4c.2),
//   - OpPairFst / OpPairSnd (m.4c.3), the read-only pair access pair,
//   - OpNewPair (m.4c.4), the inline allocator with StatusPairGrow deopt,
//   - OpCallMixed self-recursive (m.4c.5),
//   - OpCallMixed cross-fn (m.4c.6), provided the callee is JIT-compiled,
//     has no f64 regs, and has no f64 params,
//   - OpListGetI64 (n.2.a), the read-only list access op,
//   - OpListSetI64 (n.2.b), the write side of the list access pair, and
//   - OpListPushI64 + OpNewList (n.2.c). OpListPushI64 uses the inline
//     cap-check + StatusListGrow deopt fast path; OpNewList is admitted
//     only at the pre-alloc prefix (jitCall pre-allocates the list slab,
//     the emit step writes zero bytes). All cold form; hoisted slab-base
//     optimizations come in later sub-phases.
//
// Map ops on cell-bank AMD64 are still out of scope. f64 banks remain
// rejected because cell-bank repurposes R14 for *jitArenaCtx and R14 is
// the f64 base on AMD64.
func checkCellBankAdmissibleAMD64(fn *vm3.Function, opts Options) error {
	if fn.NumRegsF64 > 0 {
		return fmt.Errorf("%w: %s has both Cell and f64 banks (AMD64 m.4c.1 admits Cell+I64 only; R14 shared)",
			ErrNotImplemented, fn.Name)
	}
	for i, op := range fn.Code {
		switch op.Code {
		case vm3.OpConstI64K, vm3.OpConstI64KW, vm3.OpMovI64,
			vm3.OpAddI64, vm3.OpSubI64, vm3.OpMulI64,
			vm3.OpDivI64, vm3.OpModI64,
			vm3.OpAddI64K, vm3.OpSubI64K, vm3.OpMulI64K,
			vm3.OpDivI64K, vm3.OpModI64K,
			vm3.OpNegI64,
			vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
			vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
			vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br,
			vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
			vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
			vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr,
			vm3.OpJump,
			vm3.OpReturnI64, vm3.OpReturnConstK, vm3.OpReturnCell,
			vm3.OpPairFst, vm3.OpPairSnd, vm3.OpNewPair,
			vm3.OpListGetI64, vm3.OpListSetI64, vm3.OpListPushI64,
			vm3.OpLookupI64KW:
			continue
		case vm3.OpNewList:
			// Phase 6.3.4.n.2.c: admit at pc=0 when the lowerer can skip
			// emission (jitCall pre-allocates the list). Inline OpNewList
			// outside the pre-alloc prefix routes back to the interpreter.
			if i == 0 && canPreAllocList(fn) {
				continue
			}
			if k := int(preAllocListPrefix(fn)); k > 0 && i < k {
				continue
			}
			return fmt.Errorf("%w: %s pc %d Cell-bank fn uses inline OpNewList (only pre-alloc prefix is admitted on AMD64)",
				ErrNotImplemented, fn.Name, i)
		case vm3.OpCallMixed:
			idx := int(uint16(op.C))
			isSelf := opts.SelfIdx >= 0 && idx == opts.SelfIdx
			if !isSelf {
				// Cross-fn OpCallMixed (Phase 6.3.4.m.4c.6). The callee
				// must already be JIT-compiled (the 2-pass CompileProgram
				// orders cell-bank fns topologically so leaves compile
				// before their callers). f64 params/regs remain rejected
				// because R14 is shared between cell-bank arena ctx and
				// the f64 base.
				if opts.Prog == nil {
					return fmt.Errorf("%w: %s pc %d CallMixed cross-fn needs opts.Prog",
						ErrNotImplemented, fn.Name, i)
				}
				if idx < 0 || idx >= len(opts.Prog.Funcs) {
					return fmt.Errorf("%w: %s pc %d CallMixed callee idx %d out of range",
						ErrNotImplemented, fn.Name, i, idx)
				}
				callee := opts.Prog.Funcs[idx]
				if callee.JITCode == nil {
					return fmt.Errorf("%w: %s pc %d CallMixed callee %s has no JITCode (not compiled yet)",
						ErrNotImplemented, fn.Name, i, callee.Name)
				}
				if callee.NumRegsF64 > 0 {
					return fmt.Errorf("%w: %s pc %d CallMixed cross-fn callee %s has f64 regs (R14 shared on AMD64 cell-bank)",
						ErrNotImplemented, fn.Name, i, callee.Name)
				}
				for k, b := range callee.ParamBanks {
					if b == vm3.BankF64 {
						return fmt.Errorf("%w: %s pc %d CallMixed cross-fn callee %s has f64 param at %d",
							ErrNotImplemented, fn.Name, i, callee.Name, k)
					}
				}
				continue
			}
			// Self-recursive: reject f64 params for the same R14-sharing
			// reason that rejects cell-bank fns with NumRegsF64 > 0.
			for k, b := range fn.ParamBanks {
				if b == vm3.BankF64 {
					return fmt.Errorf("%w: %s pc %d Cell-bank OpCallMixed has f64 param at %d",
						ErrNotImplemented, fn.Name, i, k)
				}
			}
			continue
		default:
			return fmt.Errorf("%w: %s pc %d Cell-bank fn uses opcode %d (AMD64 cell-bank scaffold m.4c.1..m.4c.6)",
				ErrNotImplemented, fn.Name, i, op.Code)
		}
	}
	return nil
}

// checkCrossFnCallMixedAdmissible enforces the cross-fn OpCallMixed
// whitelist. Step 1 (deopt-free callees only) was the initial wedge;
// step 2 (Phase 6.2d.2.b) extends admission to deopt-capable callees
// (OpListPushI64, reg-reg Div/Mod) by having the caller's BLR site
// load *(x1) into x16 right after the call, then CBNZ to a single
// per-fn passthrough deopt block that spills the caller's pinned regs
// and runs the frame epilogue without writing the caller's own
// status. The callee already wrote the status before its own deopt
// block returned, so the propagated value reaches the trampoline
// unchanged and jitCall surfaces it as deopt=true.
//
// Resource budget: total live regs across the caller+callee frame
// must fit in the jitFrame3 buffer. I64 has 4096 slots so any sane
// pair fits; F64 caps at MaxF64Regs and Cell at MaxCellRegs, so the
// sum is enforced explicitly.
func checkCrossFnCallMixedAdmissible(fn *vm3.Function, op vm3.Op, pc int, opts Options) error {
	if opts.Prog == nil {
		return fmt.Errorf("%w: %s pc %d CallMixed needs opts.Prog (use CompileInProgram)",
			ErrNotImplemented, fn.Name, pc)
	}
	idx := int(uint16(op.C))
	if idx >= len(opts.Prog.Funcs) {
		return fmt.Errorf("%w: %s pc %d CallMixed callee idx %d out of range (Funcs=%d)",
			ErrNotImplemented, fn.Name, pc, idx, len(opts.Prog.Funcs))
	}
	if opts.SelfIdx >= 0 && idx == opts.SelfIdx {
		return fmt.Errorf("%w: %s pc %d CallMixed to self (idx %d) routed to self-call admission",
			ErrNotImplemented, fn.Name, pc, idx)
	}
	callee := opts.Prog.Funcs[idx]
	if callee.JITCode == nil {
		return fmt.Errorf("%w: %s pc %d CallMixed callee %s has no JITCode (not compiled yet)",
			ErrNotImplemented, fn.Name, pc, callee.Name)
	}
	// Frame budget: caller and callee share the same jitFrame3 buffer;
	// each frame's regs<bank> window must be disjoint, so the union
	// must fit in MaxF64Regs / MaxCellRegs respectively.
	if int(fn.NumRegsF64)+int(callee.NumRegsF64) > maxF64RegsARM64 {
		return fmt.Errorf("%w: %s pc %d CallMixed total f64 regs %d+%d > %d",
			ErrNotImplemented, fn.Name, pc,
			fn.NumRegsF64, callee.NumRegsF64, maxF64RegsARM64)
	}
	if int(fn.NumRegsCell)+int(callee.NumRegsCell) > maxCellRegs {
		return fmt.Errorf("%w: %s pc %d CallMixed total cell regs %d+%d > %d",
			ErrNotImplemented, fn.Name, pc,
			fn.NumRegsCell, callee.NumRegsCell, maxCellRegs)
	}
	// Caller restrictions to keep the BLR site self-contained:
	//   - no F64 bank in the caller (would need v0..v7 spill across BLR)
	//   - no list ops in the caller body (would conflict with the x20
	//     arena-ctx stash this site relies on)
	if fn.NumRegsF64 > 0 {
		return fmt.Errorf("%w: %s pc %d CallMixed caller has %d f64 regs (admits only i64+cell callers)",
			ErrNotImplemented, fn.Name, pc, fn.NumRegsF64)
	}
	if hasListGetI64(fn) || hasListPushI64(fn) {
		return fmt.Errorf("%w: %s pc %d CallMixed caller has list ops in body (reserves x20 for arena ctx)",
			ErrNotImplemented, fn.Name, pc)
	}
	return nil
}

// checkSelfCallMixedAdmissible enforces the self-OpCallMixed whitelist
// (Phase 6.3.4.m.3). Self-recursion via a PC-relative BL inside the same
// JIT page is the cell-bank mirror of the existing OpCallI64 self path:
// the trampoline-pinned x4 = &jitArenaCtx is stashed in x20 at prologue
// end (same as cross-fn CallMixed) and reloaded into x4 right before each
// BL site so the recursive prologue's LDR x19, [x4, #...] sees the same
// arena ctx pointer.
//
// Resource constraints:
//   - No f64 regs in the caller (mirrors cross-fn restriction; self-call
//     would otherwise need v0..v7 spill across BL).
//   - No list ops in the caller body (would conflict with the x20 arena-
//     ctx stash, same as cross-fn).
//   - NumRegsCell must fit in MaxCellRegs (already enforced by the global
//     cell-bank reg cap). Per-fn slot use is NumRegsCell; recursion depth
//     is bounded by jitFrame3RegsCellWords / NumRegsCell at runtime.
//
// op.B is the arg base on the caller side (offset into the caller's regs
// window where args[0] sits); for self-call we require op.B==0 only when
// the recursive call's argv matches the leaf shape. Currently we admit
// any op.B as long as the caller's window holds the args at the expected
// slots, since the BL site writes args at callerN+k offsets which are
// always valid relative to the bumped base.
func checkSelfCallMixedAdmissible(fn *vm3.Function, op vm3.Op, pc int, opts Options) error {
	if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx {
		return fmt.Errorf("%w: %s pc %d checkSelfCallMixedAdmissible called for non-self CallMixed",
			ErrNotImplemented, fn.Name, pc)
	}
	if fn.NumRegsF64 > 0 {
		return fmt.Errorf("%w: %s pc %d self-CallMixed caller has %d f64 regs (admits only i64+cell callers)",
			ErrNotImplemented, fn.Name, pc, fn.NumRegsF64)
	}
	if hasListGetI64(fn) || hasListPushI64(fn) {
		return fmt.Errorf("%w: %s pc %d self-CallMixed caller has list ops in body (reserves x20 for arena ctx)",
			ErrNotImplemented, fn.Name, pc)
	}
	return nil
}

// archMaxI64Regs returns the host architecture's cap on simultaneously
// live i64 registers, and whether the architecture is supported at
// all. AArch64 supports 17 (x9..x15 caller-saved + x19..x28
// callee-saved). AMD64 supports 10 (RSI/RDI/R8/R9/R10/R11 caller-saved
// + R12..R14 + RBP callee-saved; RBX is reserved for the regsI64 base
// pointer and R15 for the *status word).
//
// Deprecated: kept for callers outside the JIT itself. Internal call
// sites should use archCaps(fn) which folds in the F64-driven i64-cap
// reduction on AMD64.
func archMaxI64Regs() (int, bool) {
	switch hostArch {
	case ArchARM64:
		return maxI64Regs, true
	case ArchAMD64:
		return maxI64RegsAMD64, true
	default:
		return 0, false
	}
}

// archCaps returns the host architecture's caps on simultaneously-live
// (i64, f64) registers given fn's bank shape, along with whether the
// architecture is supported. On AMD64, when fn.NumRegsF64 > 0 the i64
// cap drops to 8 because R14 is repurposed to hold the regsF64 base
// pointer (the regsI64 base lives in RBX, *status in R15). AArch64 is
// unaffected: the regsF64 base lives in x2, which is already free.
func archCaps(fn *vm3.Function) (int, int, bool) {
	switch hostArch {
	case ArchARM64:
		i64Cap := maxI64Regs
		if fn.NumRegsCell > 0 {
			i64Cap = maxI64RegsCellARM64
		}
		// Phase 6.3.4.j.3: when NumRegsCell > 4 the lower-cell range
		// (cells 4..7) overlaps the i64 callee-saved lane (x21..x24),
		// so callers must keep i64 in the caller-saved bank (regs 0..6
		// at x9..x15).
		if fn.NumRegsCell > 4 {
			i64Cap = 7
		}
		return i64Cap, maxF64RegsARM64, true
	case ArchAMD64:
		i64Cap := maxI64RegsAMD64
		if fn.NumRegsF64 > 0 {
			// Phase 6.3.4.n.1: slot 8 (R14) is stolen for the regsF64
			// base, so cap drops by two from the new maxI64RegsAMD64=10
			// (slot 9 RBP is technically free but the allocator is dense,
			// so we cannot skip slot 8). Effective cap = 8.
			i64Cap = maxI64RegsAMD64 - 2
		}
		if fn.NumRegsCell > 0 {
			// Phase 6.3.4.m.4c.1 + 6.3.4.n.1: cell-bank steals R14
			// (arenaCtx) and RBP (regsCell). Slot 8 and slot 9 are both
			// unavailable, so the cap drops by two: effective cap = 8.
			i64Cap = maxI64RegsAMD64 - 2
		}
		return i64Cap, maxF64RegsAMD64, true
	default:
		return 0, 0, false
	}
}

// capNote explains the AMD64 i64-cap reduction for the error message
// when fn would otherwise have fit but the F64 bank steals R14.
func capNote(fn *vm3.Function) string {
	if hostArch == ArchAMD64 && fn.NumRegsF64 > 0 {
		return " with f64 regs in use; R14 is stolen for the regsF64 base pointer"
	}
	return ""
}
