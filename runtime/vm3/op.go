package vm3

// OpCode is the vm3 bytecode opcode. The bank is encoded in the opcode
// mnemonic itself (e.g. OpAddI64 vs OpAddF64), so the interpreter
// never decides at runtime which bank to read.
type OpCode uint8

const (
	OpNop OpCode = iota

	// Typed arithmetic (Phase 2).
	OpMovI64
	OpMovF64
	OpAddI64
	OpSubI64
	OpMulI64
	OpDivI64
	OpAddF64
	OpSubF64
	OpMulF64
	OpDivF64

	// Constant load (Phase 2).
	OpConstI64
	OpConstF64

	// Control flow (Phase 2).
	OpJump
	OpJumpIfI64
	OpJumpIfNotI64
	OpCmpEqI64
	OpCmpLtI64
	OpCmpEqF64
	OpCmpLtF64

	// Call / return (Phase 2).
	OpCall
	OpTailCall
	OpReturnI64
	OpReturnF64
	OpReturnCell

	// Container ops (Phase 3, placeholder).
	OpListGetI64
	OpListGetF64
	OpListGetCell
	OpListSetI64
	OpListSetF64
	OpListSetCell
)

// Op is a single 8-byte vm3 bytecode word. compiler3 emits these into
// Function.Code. Banks for A and B are encoded in BankFlags; C may be
// either a third register (bank inferred from the opcode) or a 16-bit
// signed immediate.
type Op struct {
	Code      OpCode
	BankFlags uint8
	A         uint16
	B         uint16
	C         int16
}
