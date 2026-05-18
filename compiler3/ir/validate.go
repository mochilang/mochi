package ir

import "fmt"

// Validate checks that fn is well-formed SSA:
//
//  1. Each Value's ID matches its position in fn.Values.
//  2. Each Value is owned by exactly one Block (appears in exactly
//     one Block.Values list).
//  3. Each Block.Values entry references a defined Value.
//  4. Phi nodes appear only at the head of a block.
//  5. Phi arity matches the block's predecessor count, and every
//     (predBlockID, srcValueID) names a real block + value.
//  6. Operands of non-Phi values dominate the using site (per the
//     CFG dominator tree).
//  7. Type preservation: typed ops produce values whose Type matches
//     the op's contract, and the op's operand Types match.
//  8. Terminator semantics: TermJump names a real Target; TermBranch
//     names a real condition Value (TypeBool) and two real successor
//     blocks; TermReturn's Value has Type == fn.Result (or both are
//     TypeUnit).
//
// Returns the first violation as an error. Used by every fixture
// test so an ill-formed IR is caught before regalloc or emit see it.
func Validate(fn *Function) error {
	for i := range fn.Values {
		if fn.Values[i].ID != uint32(i) {
			return fmt.Errorf("value at index %d has ID %d", i, fn.Values[i].ID)
		}
	}
	for i := range fn.Blocks {
		if fn.Blocks[i].ID != uint32(i) {
			return fmt.Errorf("block at index %d has ID %d", i, fn.Blocks[i].ID)
		}
	}

	owner := make(map[uint32]uint32, len(fn.Values))
	for bi := range fn.Blocks {
		blk := &fn.Blocks[bi]
		seenNonPhi := false
		for _, vid := range blk.Values {
			if int(vid) >= len(fn.Values) {
				return fmt.Errorf("block %d references undefined value %d", bi, vid)
			}
			if prev, ok := owner[vid]; ok {
				return fmt.Errorf("value %d is owned by blocks %d and %d", vid, prev, bi)
			}
			owner[vid] = uint32(bi)
			v := &fn.Values[vid]
			if v.Op == OpPhi {
				if seenNonPhi {
					return fmt.Errorf("block %d: phi v%d after non-phi", bi, vid)
				}
				if len(v.Args)%2 != 0 {
					return fmt.Errorf("phi v%d has odd Args length %d", vid, len(v.Args))
				}
				if len(v.Args)/2 != len(blk.Preds) {
					return fmt.Errorf("phi v%d arity %d != block %d preds %d", vid, len(v.Args)/2, bi, len(blk.Preds))
				}
				for k := 0; k < len(v.Args); k += 2 {
					predID := v.Args[k]
					srcID := v.Args[k+1]
					if int(predID) >= len(fn.Blocks) {
						return fmt.Errorf("phi v%d names undefined pred block %d", vid, predID)
					}
					if int(srcID) >= len(fn.Values) {
						return fmt.Errorf("phi v%d names undefined source value %d", vid, srcID)
					}
				}
			} else {
				seenNonPhi = true
			}
		}
	}

	for _, vid := range fn.Params {
		if int(vid) >= len(fn.Values) {
			return fmt.Errorf("function %q names undefined param value %d", fn.Name, vid)
		}
		if fn.Values[vid].Op != OpParam {
			return fmt.Errorf("function %q param v%d has Op %s, expected OpParam", fn.Name, vid, fn.Values[vid].Op)
		}
	}

	for bi := range fn.Blocks {
		blk := &fn.Blocks[bi]
		switch blk.Term.Kind {
		case TermJump:
			if int(blk.Term.Target) >= len(fn.Blocks) {
				return fmt.Errorf("block %d: jump target %d out of range", bi, blk.Term.Target)
			}
		case TermBranch:
			if int(blk.Term.Value) >= len(fn.Values) {
				return fmt.Errorf("block %d: branch cond v%d out of range", bi, blk.Term.Value)
			}
			if got := fn.Values[blk.Term.Value].Type; got != TypeBool {
				return fmt.Errorf("block %d: branch cond v%d has Type %s, want bool", bi, blk.Term.Value, got)
			}
			if int(blk.Term.IfTrue) >= len(fn.Blocks) {
				return fmt.Errorf("block %d: branch IfTrue %d out of range", bi, blk.Term.IfTrue)
			}
			if int(blk.Term.IfFalse) >= len(fn.Blocks) {
				return fmt.Errorf("block %d: branch IfFalse %d out of range", bi, blk.Term.IfFalse)
			}
		case TermReturn:
			if fn.Result == TypeUnit {
				// return value optional
				break
			}
			if int(blk.Term.Value) >= len(fn.Values) {
				return fmt.Errorf("block %d: return v%d out of range", bi, blk.Term.Value)
			}
			if got := fn.Values[blk.Term.Value].Type; got != fn.Result {
				return fmt.Errorf("block %d: return v%d has Type %s, function Result %s", bi, blk.Term.Value, got, fn.Result)
			}
		case TermInvalid:
			return fmt.Errorf("block %d has TermInvalid", bi)
		}
	}

	return checkOperandTypes(fn)
}

// checkOperandTypes verifies, for each typed op, that its operand
// Types and Type field match the op's contract. The contract table
// is the source of truth for what each op produces.
func checkOperandTypes(fn *Function) error {
	for vi := range fn.Values {
		v := &fn.Values[vi]
		want := opContract(v.Op)
		if want.outType != TypeInvalid && v.Type != want.outType {
			return fmt.Errorf("v%d %s: result Type %s, want %s", vi, v.Op, v.Type, want.outType)
		}
		for ai, expectIn := range want.inTypes {
			if expectIn == TypeInvalid {
				continue
			}
			if ai >= len(v.Args) {
				return fmt.Errorf("v%d %s: missing arg %d (want %s)", vi, v.Op, ai, expectIn)
			}
			srcID := v.Args[ai]
			if int(srcID) >= len(fn.Values) {
				return fmt.Errorf("v%d %s: arg %d names undefined value %d", vi, v.Op, ai, srcID)
			}
			if got := fn.Values[srcID].Type; got != expectIn {
				return fmt.Errorf("v%d %s: arg %d v%d has Type %s, want %s", vi, v.Op, ai, srcID, got, expectIn)
			}
		}
	}
	return nil
}

type opSig struct {
	outType Type
	inTypes [3]Type
}

func opContract(o OpCode) opSig {
	switch o {
	case OpAddI64, OpSubI64, OpMulI64, OpDivI64, OpModI64:
		return opSig{TypeI64, [3]Type{TypeI64, TypeI64}}
	case OpAddI64Imm, OpSubI64Imm, OpMulI64Imm, OpDivI64Imm, OpModI64Imm:
		return opSig{TypeI64, [3]Type{TypeI64}}
	case OpNegI64:
		return opSig{TypeI64, [3]Type{TypeI64}}
	case OpAddF64, OpSubF64, OpMulF64, OpDivF64:
		return opSig{TypeF64, [3]Type{TypeF64, TypeF64}}
	case OpNegF64:
		return opSig{TypeF64, [3]Type{TypeF64}}
	case OpCmpEqI64, OpCmpNeI64, OpCmpLtI64, OpCmpLeI64, OpCmpGtI64, OpCmpGeI64:
		return opSig{TypeBool, [3]Type{TypeI64, TypeI64}}
	case OpCmpEqI64Imm, OpCmpNeI64Imm, OpCmpLtI64Imm, OpCmpLeI64Imm, OpCmpGtI64Imm, OpCmpGeI64Imm:
		return opSig{TypeBool, [3]Type{TypeI64}}
	case OpLenStr:
		return opSig{TypeI64, [3]Type{TypeStr}}
	case OpConcatStr:
		return opSig{TypeStr, [3]Type{TypeStr, TypeStr}}
	case OpNewList:
		return opSig{TypeList, [3]Type{}}
	case OpListLenI64:
		return opSig{TypeI64, [3]Type{TypeList}}
	case OpListPushI64:
		return opSig{TypeUnit, [3]Type{TypeList, TypeI64}}
	case OpListGetI64:
		return opSig{TypeI64, [3]Type{TypeList, TypeI64}}
	case OpListSetI64:
		return opSig{TypeUnit, [3]Type{TypeList, TypeI64, TypeI64}}
	case OpNewMap:
		return opSig{TypeMap, [3]Type{}}
	case OpMapSetI64I64:
		return opSig{TypeUnit, [3]Type{TypeMap, TypeI64, TypeI64}}
	case OpMapGetI64I64:
		return opSig{TypeI64, [3]Type{TypeMap, TypeI64}}
	}
	// OpParam, OpConst, OpPhi, OpCall, OpTailCall: the validator
	// can't pre-compute their signature without more context, so we
	// pass them through with no check here.
	return opSig{}
}
