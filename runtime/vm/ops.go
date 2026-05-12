package vm

type Op uint8

const (
	OpConst Op = iota
	OpMove
	OpAdd
	OpSub
	OpMul
	OpDiv
	OpMod
	OpEqual
	OpNotEqual
	OpLess
	OpLessEq
	OpIn
	OpJump
	OpJumpIfFalse
	OpLen
	OpIndex
	OpSlice
	OpSetIndex
	OpMakeList
	OpMakeMap
	OpPrint
	OpPrint2
	OpPrintN
	OpCall2
	OpCall
	OpCallV
	OpReturn
	OpNot
	OpJumpIfTrue
	OpNow
	OpRealNow
	OpMem
	OpJSON
	OpAppend
	OpStr
	OpUpper
	OpLower
	OpReverse
	OpPadStart
	OpInput
	OpFirst
	OpCount
	OpExists
	OpSHA256
	OpAvg
	OpSum
	OpMin
	OpMax
	OpValues
	OpCollect
	OpCast
	OpIterPrep
	OpLoad
	OpSave
	OpEval
	OpFetch

	// Global variable operations
	OpGetGlobal
	OpSetGlobal

	// Closure creation
	OpMakeClosure

	// Specialized numeric ops
	OpAddInt
	OpAddFloat
	OpSubInt
	OpSubFloat
	OpMulInt
	OpMulFloat
	OpDivInt
	OpDivFloat
	OpModInt
	OpModFloat
	OpPow
	OpRound
	OpEqualInt
	OpEqualFloat
	OpLessInt
	OpLessFloat
	OpLessEqInt
	OpLessEqFloat

	// Unary numeric ops
	OpNeg
	OpNegInt
	OpNegFloat

	// List operations
	OpUnionAll
	OpUnion
	OpExcept
	OpIntersect
	OpSort
	OpDistinct
	OpExpect
	OpSelect

	// Foreign function interface operations
	OpGoCall
	OpGoAutoCall
	OpPyCall
)

func (op Op) String() string {
	switch op {
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
	case OpDiv:
		return "Div"
	case OpMod:
		return "Mod"
	case OpEqual:
		return "Equal"
	case OpNotEqual:
		return "NotEqual"
	case OpLess:
		return "Less"
	case OpLessEq:
		return "LessEq"
	case OpIn:
		return "In"
	case OpJump:
		return "Jump"
	case OpJumpIfFalse:
		return "JumpIfFalse"
	case OpLen:
		return "Len"
	case OpIndex:
		return "Index"
	case OpSlice:
		return "Slice"
	case OpSetIndex:
		return "SetIndex"
	case OpMakeList:
		return "MakeList"
	case OpMakeMap:
		return "MakeMap"
	case OpPrint:
		return "Print"
	case OpPrint2:
		return "Print2"
	case OpPrintN:
		return "PrintN"
	case OpCall2:
		return "Call2"
	case OpCall:
		return "Call"
	case OpCallV:
		return "CallV"
	case OpReturn:
		return "Return"
	case OpNot:
		return "Not"
	case OpJumpIfTrue:
		return "JumpIfTrue"
	case OpNow:
		return "Now"
	case OpRealNow:
		return "RealNow"
	case OpMem:
		return "Mem"
	case OpJSON:
		return "JSON"
	case OpAppend:
		return "Append"
	case OpStr:
		return "Str"
	case OpUpper:
		return "Upper"
	case OpLower:
		return "Lower"
	case OpReverse:
		return "Reverse"
	case OpPadStart:
		return "PadStart"
	case OpInput:
		return "Input"
	case OpFirst:
		return "First"
	case OpCount:
		return "Count"
	case OpExists:
		return "Exists"
	case OpSHA256:
		return "SHA256"
	case OpAvg:
		return "Avg"
	case OpSum:
		return "Sum"
	case OpMin:
		return "Min"
	case OpMax:
		return "Max"
	case OpValues:
		return "Values"
	case OpCollect:
		return "Collect"
	case OpCast:
		return "Cast"
	case OpIterPrep:
		return "IterPrep"
	case OpLoad:
		return "Load"
	case OpSave:
		return "Save"
	case OpEval:
		return "Eval"
	case OpFetch:
		return "Fetch"
	case OpGetGlobal:
		return "GetGlobal"
	case OpSetGlobal:
		return "SetGlobal"
	case OpMakeClosure:
		return "MakeClosure"
	case OpAddInt:
		return "AddInt"
	case OpAddFloat:
		return "AddFloat"
	case OpSubInt:
		return "SubInt"
	case OpSubFloat:
		return "SubFloat"
	case OpMulInt:
		return "MulInt"
	case OpMulFloat:
		return "MulFloat"
	case OpDivInt:
		return "DivInt"
	case OpDivFloat:
		return "DivFloat"
	case OpModInt:
		return "ModInt"
	case OpModFloat:
		return "ModFloat"
	case OpPow:
		return "Pow"
	case OpEqualInt:
		return "EqualInt"
	case OpEqualFloat:
		return "EqualFloat"
	case OpLessInt:
		return "LessInt"
	case OpLessFloat:
		return "LessFloat"
	case OpLessEqInt:
		return "LessEqInt"
	case OpLessEqFloat:
		return "LessEqFloat"
	case OpNeg:
		return "Neg"
	case OpNegInt:
		return "NegInt"
	case OpNegFloat:
		return "NegFloat"
	case OpUnionAll:
		return "UnionAll"
	case OpUnion:
		return "Union"
	case OpExcept:
		return "Except"
	case OpIntersect:
		return "Intersect"
	case OpSort:
		return "Sort"
	case OpDistinct:
		return "Distinct"
	case OpExpect:
		return "Expect"
	case OpSelect:
		return "Select"
	case OpGoCall:
		return "GoCall"
	case OpGoAutoCall:
		return "GoAutoCall"
	case OpPyCall:
		return "PyCall"
	default:
		return "?"
	}
}

