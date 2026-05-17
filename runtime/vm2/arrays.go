package vm2

import "unsafe"

// vmF64Array is the heap representation of a `[]float64`-typed array
// (MEP-37 §3.3). Storage is a flat Go slice so the inner loop reads
// 8 bytes per element instead of the 16 bytes a Cell takes; for the BG
// FP workloads (n_body, spectral_norm, mandelbrot) this halves the
// working set and removes the Cell decode on every load.
type vmF64Array struct {
	data []float64
}

// vmI64Array mirrors vmF64Array for int64-typed arrays. Used by
// fannkuch_redux (which is dominated by `[]int8` permutation work; we
// widen to int64 because vm2 has no narrower scalar type today but
// keep storage homogeneous so the cache hit pattern stays flat).
type vmI64Array struct {
	data []int64
}

// vmU8Array mirrors the above for byte arrays. Used by the byte-array
// BG workloads (fasta, reverse_complement, k_nucleotide) and by the
// mandelbrot output bitmap.
type vmU8Array struct {
	data []byte
}

func (vm *VM) newF64Array(n int) Cell {
	if n < 0 {
		n = 0
	}
	a := &vmF64Array{data: make([]float64, n)}
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(a)}
}

func (vm *VM) newI64Array(n int) Cell {
	if n < 0 {
		n = 0
	}
	a := &vmI64Array{data: make([]int64, n)}
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(a)}
}

func (vm *VM) newU8Array(n int) Cell {
	if n < 0 {
		n = 0
	}
	a := &vmU8Array{data: make([]byte, n)}
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(a)}
}

func (vm *VM) f64ArrAt(c Cell) *vmF64Array { return (*vmF64Array)(c.PtrTo()) }
func (vm *VM) i64ArrAt(c Cell) *vmI64Array { return (*vmI64Array)(c.PtrTo()) }
func (vm *VM) u8ArrAt(c Cell) *vmU8Array   { return (*vmU8Array)(c.PtrTo()) }
