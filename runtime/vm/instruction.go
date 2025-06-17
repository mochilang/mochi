package vm

// Instruction represents a single VM instruction.
type Instruction struct {
	Op    Opcode
	Value any    // for OpConst
	Str   string // for variable name
}
