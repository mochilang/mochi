package vm2

// Instr is a single bytecode instruction. Fixed 20 bytes (one cache
// line holds three). Variable-length encoding is a later MEP-21 v2
// item; the fixed form keeps step 3 minimal.
type Instr struct {
	Op      Op
	_       [3]byte
	A, B, C int32
	D       int32
}

// Function is a compiled top-level function or method body.
type Function struct {
	Name      string
	NumParams int
	NumRegs   int
	Code      []Instr
	Consts    []Cell
}

// Program is the unit of execution. Main names the entry function.
type Program struct {
	Funcs []*Function
	Main  int
}
