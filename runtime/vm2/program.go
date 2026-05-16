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
	// StrConsts holds the raw bytes of string-typed constants. The
	// emit-time index in StrConsts is the operand B value of an
	// OpLoadStrK; the dispatch loop materializes a fresh vmString on
	// first use via newString. Carrying bytes rather than pre-allocated
	// *vmStrings keeps Function trivially serializable later.
	StrConsts [][]byte
}

// Program is the unit of execution. Main names the entry function.
type Program struct {
	Funcs []*Function
	Main  int
}
