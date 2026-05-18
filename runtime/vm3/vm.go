package vm3

import "errors"

// VM is a vm3 interpreter instance. Each VM owns one Arenas struct,
// rooted from this field; Go's GC reaches every arena slab and every
// backing slice through normal traversal.
type VM struct {
	arenas Arenas
	frame  *Frame
}

// New constructs an empty VM. Phase 0 ships only the scaffold; later
// phases wire Run and the opcode bodies.
func New() *VM {
	return &VM{}
}

// Arenas returns the VM's arena state. Tests use this to inspect slot
// counts and free-list lengths.
func (vm *VM) Arenas() *Arenas { return &vm.arenas }

// ErrNotImplemented is returned by VM.Run until Phase 2 lands the
// interpreter loop.
var ErrNotImplemented = errors.New("vm3: VM.Run not yet implemented (Phase 2)")

// Run executes fn until it returns or traps. Phase 0 returns
// ErrNotImplemented unconditionally.
func (vm *VM) Run(fn *Function) (Cell, error) {
	_ = fn
	return CNull(), ErrNotImplemented
}
