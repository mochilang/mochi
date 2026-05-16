package vm2

// Instr is the vm2 instruction word. The Quick byte (MEP-18 Phase C)
// lets the dispatcher rewrite a generic op to a tag-specialized one
// in place. A,B,C,D are register operands; Val holds the inline
// constant for OpConst.
type Instr struct {
	Op    Op
	Quick uint8
	A     int32
	B     int32
	C     int32
	D     int32
	Val   Value
}

// Function is a compiled function in vm2. NumRegs is the size of the
// register slab the dispatcher allocates per call. callSites and
// quickMisses are the per-IP IC and deopt-counter tables; allocated
// lazily on first observation.
type Function struct {
	Name      string
	NumParams int
	NumRegs   int
	Code      []Instr

	callSites   []*callSite
	quickMisses []uint8
}

// Closure is a function value with captured arguments. Stored as a
// concrete *Closure pointer inside Value.Ref so the GC sees a real
// pointer and the interpreter pays one type assertion per call
// rather than a pair of interface descriptor reads.
type Closure struct {
	Fn   int
	Args []Value
}

// Program is the compiled unit vm2 executes.
type Program struct {
	Funcs      []Function
	NumGlobals int
}
