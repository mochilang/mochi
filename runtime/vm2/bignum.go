package vm2

import (
	"math/big"
	"unsafe"
)

// Bignum (MEP-39 §4.2.3). Arbitrary-precision signed integers are
// reference-typed: a bignum Cell is tagPtr whose Obj field carries a
// *big.Int so the host GC reaches the pointee directly. We reuse the
// existing tagPtr encoding (no new tag bit) because vm2 already does
// type-driven dispatch through the IR: bignum opcodes never run on a
// list/map/string operand and vice versa.
//
// The tagPtrStrFlag bit is cleared for bignums so mapKeyOf treats them
// as identity-keyed pointers (consistent with *vmList / *vmMap), which
// is fine for pidigits, our motivating workload (no bignum-keyed maps).
// A later subsystem that wants value-equality keys can add a normalised
// hash without changing this op family.

// CBigInt boxes a *big.Int into a tagPtr Cell. b must not be nil.
func CBigInt(b *big.Int) Cell {
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(b)}
}

// BigInt returns the *big.Int carried by c. Caller must already know
// the cell holds a bignum (via the IR-driven dispatch); the runtime
// does not tag-distinguish bignums from other tagPtr pointees.
func (c Cell) BigInt() *big.Int {
	return (*big.Int)(c.PtrTo())
}

// JIT slow-path accessors — called by runtime/jit/vm2jit when a bignum
// opcode is encountered. Bignum ops always deopt to the interpreter in
// Phase 1, so these helpers exist for symmetry with the list/map JIT
// surface; the interpreter's eval arms do the real work.

// JITAddBigInt returns CBigInt(a + b).
func JITAddBigInt(a, b Cell) Cell {
	r := new(big.Int).Add(a.BigInt(), b.BigInt())
	return CBigInt(r)
}

// JITSubBigInt returns CBigInt(a - b).
func JITSubBigInt(a, b Cell) Cell {
	r := new(big.Int).Sub(a.BigInt(), b.BigInt())
	return CBigInt(r)
}

// JITMulBigInt returns CBigInt(a * b).
func JITMulBigInt(a, b Cell) Cell {
	r := new(big.Int).Mul(a.BigInt(), b.BigInt())
	return CBigInt(r)
}

// JITDivBigInt returns CBigInt(a / b) using Euclidean Quo (truncated
// toward zero). Panics on divide-by-zero, matching the i64 contract.
func JITDivBigInt(a, b Cell) Cell {
	bv := b.BigInt()
	if bv.Sign() == 0 {
		panic("vm2: bignum division by zero")
	}
	r := new(big.Int).Quo(a.BigInt(), bv)
	return CBigInt(r)
}

// JITModBigInt returns CBigInt(a % b) using Rem (sign of dividend),
// matching OpModI64.
func JITModBigInt(a, b Cell) Cell {
	bv := b.BigInt()
	if bv.Sign() == 0 {
		panic("vm2: bignum mod by zero")
	}
	r := new(big.Int).Rem(a.BigInt(), bv)
	return CBigInt(r)
}

// bigIntAt is the mirror of listAt/mapAt for code that wants the
// concrete pointer without going through Cell.BigInt() (handy for
// readability inside this package).
func (vm *VM) bigIntAt(c Cell) *big.Int {
	return (*big.Int)(c.PtrTo())
}
