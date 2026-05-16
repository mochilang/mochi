package tmpljit

// Op is a 6-opcode register VM tuned to express the canonical
// fill_sum-shaped workload. Registers are int64; register 0 holds
// the call argument on entry.
type Op uint8

const (
	OpMovImm Op = iota // dst = imm
	OpAdd              // dst = a + b
	OpMul              // dst = a * b
	OpLt               // dst = (a < b) ? 1 : 0
	OpJnz              // pc += imm (signed instruction count) if regs[a] != 0
	OpRet              // return regs[a]
)

// NumRegs is the size of the register file visible to bytecode.
// The JIT pins each VM register to a fixed AArch64 caller-saved
// GPR (r0..r6 -> x9..x15), see emit_arm64.go for the mapping. The
// interpreter does not have this limit but the corpus and tests
// stay within 7 registers so the two backends are interchangeable.
const NumRegs = 7

// Instr is the fixed-width bytecode word.
type Instr struct {
	Op  Op
	Dst uint8 // destination register (or condition register for Jnz)
	A   uint8 // source register A
	B   uint8 // source register B
	Imm int32 // immediate (MovImm value, or Jnz signed instruction offset)
}

// Program is a linear array of instructions. The JIT compiles a
// Program once and returns a callable function pointer; the
// interpreter reads it in place.
type Program []Instr
