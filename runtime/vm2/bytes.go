package vm2

import (
	"hash/fnv"
	"unsafe"
)

// vmBytes is a (buf, off, n) view onto a backing byte buffer (MEP-38
// §3.1.1). Views are logically immutable except through OpBytesSet on
// an owning view (§3.1.4). Slicing produces a fresh *vmBytes that
// aliases the same buf with different (off, n) coordinates; the parent
// buffer stays alive as long as any slice reaches it.
//
// Layout is held to 48 bytes (half a cache line on M4) so a view fits
// comfortably alongside one Cell in the register file:
//
//	buf  []byte 24 B (ptr/len/cap)
//	off  int     8 B
//	n    int     8 B
//	owns bool    1 B + 7 B padding
type vmBytes struct {
	buf  []byte
	off  int
	n    int
	owns bool
}

// newBytes allocates a fresh buffer of length n and returns an owning
// view of it. The view is the only reference to the backing slice when
// it is returned; OpBytesSet is legal on it until the view is sliced.
func (vm *VM) newBytes(n int) Cell {
	if n < 0 {
		n = 0
	}
	b := &vmBytes{buf: make([]byte, n), off: 0, n: n, owns: true}
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(b)}
}

// bytesAt fetches the *vmBytes backing a tagPtr cell.
func (vm *VM) bytesAt(c Cell) *vmBytes { return (*vmBytes)(c.PtrTo()) }

// bytesSlice produces a fresh non-owning view of src[off:off+n].
// Traps on OOB. The new view's owns=false so OpBytesSet on it traps,
// preserving the contract that only the OpBytesNew allocator can
// mint a writable view.
func (vm *VM) bytesSlice(src Cell, off, n int64) Cell {
	s := vm.bytesAt(src)
	if off < 0 || n < 0 || off+n > int64(s.n) {
		panic("vm2/bytes: slice out of range")
	}
	v := &vmBytes{buf: s.buf, off: s.off + int(off), n: int(n), owns: false}
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(v)}
}

// bytesFromU8Array constructs a non-owning view over the whole U8Array
// backing slice. The view shares storage with the array: writes
// through the array (via OpU8ArrSet) are visible through the view.
func (vm *VM) bytesFromU8Array(src Cell) Cell {
	a := vm.u8ArrAt(src)
	v := &vmBytes{buf: a.data, off: 0, n: len(a.data), owns: false}
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(v)}
}

// bytesFromStr constructs a non-owning view over the string's backing
// bytes. The view shares storage with the *vmString: vm2 strings are
// immutable so the parent's contents cannot change under the view.
func (vm *VM) bytesFromStr(src Cell) Cell {
	s := vm.stringAt(src)
	v := &vmBytes{buf: s.bytes, off: 0, n: len(s.bytes), owns: false}
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(v)}
}

// bytesGet returns CInt(buf[off+i]). Traps on OOB.
func (vm *VM) bytesGet(src Cell, i int64) Cell {
	b := vm.bytesAt(src)
	if i < 0 || i >= int64(b.n) {
		panic("vm2/bytes: index out of range")
	}
	return CInt(int64(b.buf[b.off+int(i)]))
}

// bytesSet writes byte(v) to buf[off+i]. Traps on OOB. Traps if the
// view does not own its backing slice (§3.1.4).
func (vm *VM) bytesSet(dst Cell, i int64, v int64) {
	b := vm.bytesAt(dst)
	if !b.owns {
		panic("vm2/bytes: write to non-owning view")
	}
	if i < 0 || i >= int64(b.n) {
		panic("vm2/bytes: index out of range")
	}
	b.buf[b.off+int(i)] = byte(v)
}

// bytesEqual returns a bool Cell. Two views are equal iff they project
// the same byte sequence (length + content), regardless of backing
// buffer identity.
func (vm *VM) bytesEqual(a, b Cell) Cell {
	x := vm.bytesAt(a)
	y := vm.bytesAt(b)
	if x.n != y.n {
		return CBool(false)
	}
	xs := x.buf[x.off : x.off+x.n]
	ys := y.buf[y.off : y.off+y.n]
	for i := range xs {
		if xs[i] != ys[i] {
			return CBool(false)
		}
	}
	return CBool(true)
}

// bytesHash returns CInt(FNV-1a 64-bit hash) of the view's bytes.
// Matches the *vmString hash family so map keys can mix the two
// without spurious collisions, see MEP-38 §3.1.2.
func (vm *VM) bytesHash(src Cell) Cell {
	b := vm.bytesAt(src)
	h := fnv.New64a()
	h.Write(b.buf[b.off : b.off+b.n])
	return CInt(int64(h.Sum64()))
}
