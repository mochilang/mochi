package vm3

import "testing"

func TestNewVMHasEmptyArenas(t *testing.T) {
	vm := New()
	a := vm.Arenas()
	if len(a.Strings)+len(a.Lists)+len(a.Maps)+len(a.Sets)+
		len(a.Structs)+len(a.Closures)+len(a.Bignums)+len(a.Bytes)+
		len(a.Pairs)+len(a.F64Arrs)+len(a.I64Arrs)+len(a.U8Arrs) != 0 {
		t.Fatal("new VM has non-empty arena")
	}
}

// Phase 2 ships a working math-subset interpreter. End-to-end tests
// live alongside the corpus programs that exercise each opcode group.
