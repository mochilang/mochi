package vm3

import (
	"errors"
	"testing"
)

func TestNewVMHasEmptyArenas(t *testing.T) {
	vm := New()
	a := vm.Arenas()
	if len(a.Strings)+len(a.Lists)+len(a.Maps)+len(a.Sets)+
		len(a.Structs)+len(a.Closures)+len(a.Bignums)+len(a.Bytes)+
		len(a.Pairs)+len(a.F64Arrs)+len(a.I64Arrs)+len(a.U8Arrs) != 0 {
		t.Fatal("new VM has non-empty arena")
	}
}

func TestRunReturnsNotImplemented(t *testing.T) {
	vm := New()
	if _, err := vm.Run(&Function{}); !errors.Is(err, ErrNotImplemented) {
		t.Fatalf("Run: got %v want ErrNotImplemented", err)
	}
}
