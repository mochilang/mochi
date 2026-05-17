package vm2

import (
	"bytes"
	"testing"
)

// TestBytesNewAndSet exercises the owning-view fast path: allocate a
// fresh view, write each byte, read them back.
func TestBytesNewAndSet(t *testing.T) {
	vm := &VM{}
	c := vm.newBytes(8)
	for i := int64(0); i < 8; i++ {
		vm.bytesSet(c, i, i+'A')
	}
	for i := int64(0); i < 8; i++ {
		if g := vm.bytesGet(c, i).Int(); g != i+'A' {
			t.Errorf("bytesGet(%d) = %d, want %d", i, g, i+'A')
		}
	}
	if l := vm.bytesAt(c).n; l != 8 {
		t.Errorf("len = %d, want 8", l)
	}
}

// TestBytesSliceReadOnly verifies the §3.1.4 ownership contract:
// OpBytesSlice produces a non-owning view, and OpBytesSet on it must
// trap rather than mutate the parent.
func TestBytesSliceReadOnly(t *testing.T) {
	vm := &VM{}
	src := vm.newBytes(8)
	for i := int64(0); i < 8; i++ {
		vm.bytesSet(src, i, i+'a')
	}
	view := vm.bytesSlice(src, 2, 4)
	if l := vm.bytesAt(view).n; l != 4 {
		t.Errorf("slice len = %d, want 4", l)
	}
	if g := vm.bytesGet(view, 0).Int(); g != 'c' {
		t.Errorf("slice[0] = %d, want 'c'", g)
	}
	if g := vm.bytesGet(view, 3).Int(); g != 'f' {
		t.Errorf("slice[3] = %d, want 'f'", g)
	}
	defer func() {
		if r := recover(); r == nil {
			t.Fatalf("bytesSet on non-owning view did not panic")
		}
	}()
	vm.bytesSet(view, 0, 'Z')
}

// TestBytesFromU8ArrayAliases verifies the view shares storage with
// the U8Array, so writes through the array are visible through the
// view.
func TestBytesFromU8ArrayAliases(t *testing.T) {
	vm := &VM{}
	arr := vm.newU8Array(4)
	a := vm.u8ArrAt(arr)
	copy(a.data, []byte("ABCD"))
	v := vm.bytesFromU8Array(arr)
	if g := vm.bytesGet(v, 1).Int(); g != 'B' {
		t.Errorf("view[1] = %d, want 'B'", g)
	}
	a.data[1] = 'X'
	if g := vm.bytesGet(v, 1).Int(); g != 'X' {
		t.Errorf("view[1] after array mutation = %d, want 'X'", g)
	}
}

// TestBytesEqualAndHash verifies two views with the same content but
// different (off, n) coordinates compare equal and hash identically.
func TestBytesEqualAndHash(t *testing.T) {
	vm := &VM{}
	a := vm.newBytes(6)
	for i := int64(0); i < 6; i++ {
		vm.bytesSet(a, i, i+'X')
	}
	b := vm.newBytes(4)
	for i := int64(0); i < 4; i++ {
		vm.bytesSet(b, i, i+'Y')
	}
	av := vm.bytesSlice(a, 1, 4)
	bv := vm.bytesSlice(b, 0, 4)
	if vm.bytesEqual(av, bv).Bool() != true {
		t.Errorf("equal mismatch")
	}
	if vm.bytesHash(av).Int() != vm.bytesHash(bv).Int() {
		t.Errorf("hash mismatch")
	}
}

// TestStdinReadAllAndStdoutWriteBytes round-trips the I/O ops through
// bytes.Buffer.
func TestStdinReadAllAndStdoutWriteBytes(t *testing.T) {
	in := bytes.NewBufferString("hello world")
	out := new(bytes.Buffer)
	vm := &VM{Stdin: in, Stdout: out}
	// Simulate the dispatcher's OpStdinReadAll + OpStdoutWriteBytes.
	data, err := readAllForTest(vm)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if _, err := vm.Stdout.Write(data); err != nil {
		t.Fatalf("write: %v", err)
	}
	if got := out.String(); got != "hello world" {
		t.Errorf("round trip = %q, want %q", got, "hello world")
	}
}

func readAllForTest(vm *VM) ([]byte, error) {
	buf := make([]byte, 0, 64)
	tmp := make([]byte, 64)
	for {
		n, err := vm.Stdin.Read(tmp)
		buf = append(buf, tmp[:n]...)
		if err != nil {
			if err.Error() == "EOF" {
				return buf, nil
			}
			return buf, nil
		}
	}
}
