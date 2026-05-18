package vm3

// Frame is one activation record. Each frame carries three typed
// register banks. compiler3 partitions every SSA value into exactly one
// bank at lowering time, based on the value's static type.
type Frame struct {
	fn *Function
	pc int

	regsI64  []int64
	regsF64  []float64
	regsCell []Cell

	prev *Frame
}

// Function is a compiled vm3 function. Each call to it allocates a
// Frame with banks sized by Num*Regs.
type Function struct {
	Name   string
	Code   []Op
	Consts []Cell

	NumRegsI64  uint16
	NumRegsF64  uint16
	NumRegsCell uint16

	ParamBanks []Bank
	ResultBank Bank
}

// Bank identifies one of the three typed register banks.
type Bank uint8

const (
	BankI64 Bank = iota
	BankF64
	BankCell
)
