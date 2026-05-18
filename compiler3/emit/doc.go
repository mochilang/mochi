// Package emit is compiler3's bytecode generator.
//
// The emitter walks blocks in reverse postorder and emits the
// fixed-width 8-byte opcodes described in MEP-40 §6.6. Constants are
// pooled per function; strings live in the global string arena at
// compile time.
//
// Phase 0 ships only the scaffold.
package emit

import (
	"mochi/compiler3/ir"
	"mochi/compiler3/regalloc"
	"mochi/runtime/vm3"
)

// Compile lowers fn to a runtime/vm3 Function. Phase 0 returns an
// empty Function so callers can wire end-to-end before Phase 2 fills
// in the opcode bodies.
func Compile(fn *ir.Function, alloc regalloc.Result) (*vm3.Function, error) {
	_ = fn
	return &vm3.Function{
		Name:        fn.Name,
		NumRegsI64:  alloc.NumI64,
		NumRegsF64:  alloc.NumF64,
		NumRegsCell: alloc.NumCell,
	}, nil
}
