package vm3

// Frame is one activation record. Each frame holds a base index into
// each of the VM's three typed register stacks; the live window for
// this activation is stack[base : base + fn.NumRegs*]. Storing only
// indices keeps the Frame record small and lets the call path avoid
// per-call register-slice allocation, which dominates recursive
// workloads.
type Frame struct {
	fn *Function
	pc int

	baseI64  int
	baseF64  int
	baseCell int

	// retReg names the caller register that receives this frame's
	// return value. Encoded in the call op's A field.
	retReg uint16
	// retBank tags which bank retReg lives in.
	retBank Bank
}

// Function is a compiled vm3 function. Each activation reserves
// NumRegs* slots in each typed register stack.
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

// NumI64Params reports how many of f's parameters are in regsI64.
func (f *Function) NumI64Params() int {
	n := 0
	for _, b := range f.ParamBanks {
		if b == BankI64 {
			n++
		}
	}
	return n
}

// NumF64Params reports how many of f's parameters are in regsF64.
func (f *Function) NumF64Params() int {
	n := 0
	for _, b := range f.ParamBanks {
		if b == BankF64 {
			n++
		}
	}
	return n
}

// NumCellParams reports how many of f's parameters are in regsCell.
func (f *Function) NumCellParams() int {
	n := 0
	for _, b := range f.ParamBanks {
		if b == BankCell {
			n++
		}
	}
	return n
}
