package vm

// Opcode represents a single instruction code in the virtual machine.
type Opcode uint8

const (
	OpConst Opcode = iota
	OpAdd
	OpSub
	OpMul
	OpDiv
	OpNeg
	OpLoadVar
	OpStoreVar
	OpPrint
	OpReturn
	OpStop
)
