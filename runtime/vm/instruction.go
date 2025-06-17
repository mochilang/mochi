package vm

// OpCode represents a single instruction.
type OpCode int

const (
	OpNop OpCode = iota
	OpLoadConst
	OpAdd
	OpSub
	OpMul
	OpDiv
	OpJump
	OpJumpIf
	OpCall
	OpRet
)

// Instr is a single SSA instruction operating on registers.
type Instr struct {
	Op  OpCode
	A   int // first operand
	B   int // second operand or end of arg slice for call
	C   int // destination register
	Arg any // literal value, jump target or callee
}
