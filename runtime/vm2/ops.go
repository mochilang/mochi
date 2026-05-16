package vm2

// Op is the vm2 opcode. The set is intentionally small: just enough
// to run the MEP-17 benchmark programs hand-coded in bench/. Adding
// opcodes is cheap (one case in the dispatch loop), so the set will
// grow as more programs are ported.
type Op uint8

const (
	OpHalt Op = iota
	OpConst   // A = ins.Val
	OpMove    // A = B
	OpAdd     // A = B + C (generic; quickens to AddInt/AddFloat)
	OpSub     // A = B - C
	OpMul     // A = B * C
	OpAddInt  // A = B + C (int fast path with overflow check)
	OpSubInt
	OpMulInt
	OpLess    // A = B < C
	OpLessInt
	OpLessEq
	OpEqual
	OpEqualInt
	OpJump        // ip = A
	OpJumpIfFalse // if !B: ip = A
	OpJumpIfTrue
	OpCall   // A = call(fn=B, args=regs[D..D+C])
	OpCallV  // A = call(funcVal=B, args=regs[D..D+C])
	OpReturn // return regs[A]
	OpPrint  // print regs[A]
	OpMakeList // A = list of regs[C..C+B]
	OpAppend   // A = append(B, C) — list result
	OpIndex    // A = B[C]
	OpLen      // A = len(B)
	OpMakeClosure // A = closure(fn=B, captures=regs[D..D+C])

	// Quickened (never emitted directly; patched by dispatcher).
	OpIndex_List
	OpAdd_II
	OpSub_II
	OpMul_II
	OpLess_II
)

func (op Op) String() string {
	switch op {
	case OpHalt:
		return "Halt"
	case OpConst:
		return "Const"
	case OpMove:
		return "Move"
	case OpAdd:
		return "Add"
	case OpSub:
		return "Sub"
	case OpMul:
		return "Mul"
	case OpAddInt:
		return "AddInt"
	case OpSubInt:
		return "SubInt"
	case OpMulInt:
		return "MulInt"
	case OpLess:
		return "Less"
	case OpLessInt:
		return "LessInt"
	case OpLessEq:
		return "LessEq"
	case OpEqual:
		return "Equal"
	case OpEqualInt:
		return "EqualInt"
	case OpJump:
		return "Jump"
	case OpJumpIfFalse:
		return "JumpIfFalse"
	case OpJumpIfTrue:
		return "JumpIfTrue"
	case OpCall:
		return "Call"
	case OpCallV:
		return "CallV"
	case OpReturn:
		return "Return"
	case OpPrint:
		return "Print"
	case OpMakeList:
		return "MakeList"
	case OpAppend:
		return "Append"
	case OpIndex:
		return "Index"
	case OpLen:
		return "Len"
	case OpMakeClosure:
		return "MakeClosure"
	case OpIndex_List:
		return "Index_List"
	case OpAdd_II:
		return "Add_II"
	case OpSub_II:
		return "Sub_II"
	case OpMul_II:
		return "Mul_II"
	case OpLess_II:
		return "Less_II"
	default:
		return "?"
	}
}
