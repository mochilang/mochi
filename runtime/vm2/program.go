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
	// OpLoadStrK. Carrying bytes (rather than pre-allocated *vmStrings)
	// keeps Function trivially serializable later.
	StrConsts [][]byte
	// StrCells is the per-Cell view of StrConsts used by the dispatch
	// loop. Entries with len <= MaxInlineStr are packed inline at
	// runtime materialization (no allocation); longer entries are
	// allocated once into the running VM's Objects table and the Cell
	// is cached here. This field is populated by VM.Run; not part of
	// the on-disk Program shape.
	StrCells []Cell
}

// Program is the unit of execution. Main names the entry function.
type Program struct {
	Funcs []*Function
	Main  int
}
