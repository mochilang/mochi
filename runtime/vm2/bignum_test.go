package vm2

import (
	"math/big"
	"testing"
)

func runBignumProg(t *testing.T, numRegs int, consts []Cell, code []Instr) (Cell, *VM) {
	t.Helper()
	fn := &Function{
		Name:    "test",
		NumRegs: numRegs,
		Code:    code,
		Consts:  consts,
	}
	prog := &Program{Funcs: []*Function{fn}, Main: 0}
	vm := New(prog)
	got, err := vm.Run()
	if err != nil {
		t.Fatalf("vm.Run: %v", err)
	}
	return got, vm
}

func TestBignumLoadConst(t *testing.T) {
	want, _ := new(big.Int).SetString("12345678901234567890", 10)
	got, _ := runBignumProg(t, 1, []Cell{CBigInt(want)}, []Instr{
		{Op: OpLoadConstI, A: 0, B: 0},
		{Op: OpReturn, A: 0},
	})
	if got.BigInt().Cmp(want) != 0 {
		t.Fatalf("want %s, got %s", want, got.BigInt())
	}
}

func TestBignumAddSubMul(t *testing.T) {
	a, _ := new(big.Int).SetString("1000000000000000000", 10)
	b, _ := new(big.Int).SetString("2000000000000000000", 10)
	got, _ := runBignumProg(t, 4, []Cell{CBigInt(a), CBigInt(b)}, []Instr{
		{Op: OpLoadConstI, A: 0, B: 0},
		{Op: OpLoadConstI, A: 1, B: 1},
		{Op: OpAddBigInt, A: 2, B: 0, C: 1}, // 3e18
		{Op: OpMulBigInt, A: 3, B: 2, C: 0}, // 3e36
		{Op: OpSubBigInt, A: 3, B: 3, C: 1}, // 3e36 - 2e18
		{Op: OpReturn, A: 3},
	})
	sum := new(big.Int).Add(a, b)
	prod := new(big.Int).Mul(sum, a)
	want := new(big.Int).Sub(prod, b)
	if got.BigInt().Cmp(want) != 0 {
		t.Fatalf("want %s, got %s", want, got.BigInt())
	}
}

func TestBignumDivMod(t *testing.T) {
	// (3^50) / (3^20) = 3^30; (3^50) % (3^20) = 0.
	a := new(big.Int).Exp(big.NewInt(3), big.NewInt(50), nil)
	b := new(big.Int).Exp(big.NewInt(3), big.NewInt(20), nil)
	got, _ := runBignumProg(t, 4, []Cell{CBigInt(a), CBigInt(b)}, []Instr{
		{Op: OpLoadConstI, A: 0, B: 0},
		{Op: OpLoadConstI, A: 1, B: 1},
		{Op: OpDivBigInt, A: 2, B: 0, C: 1},
		{Op: OpModBigInt, A: 3, B: 0, C: 1},
		{Op: OpAddBigInt, A: 2, B: 2, C: 3}, // 3^30 + 0
		{Op: OpReturn, A: 2},
	})
	want := new(big.Int).Exp(big.NewInt(3), big.NewInt(30), nil)
	if got.BigInt().Cmp(want) != 0 {
		t.Fatalf("want %s, got %s", want, got.BigInt())
	}
}

func TestBignumDivByZeroTraps(t *testing.T) {
	fn := &Function{
		Name:    "test",
		NumRegs: 3,
		Code: []Instr{
			{Op: OpLoadConstI, A: 0, B: 0},
			{Op: OpLoadConstI, A: 1, B: 1},
			{Op: OpDivBigInt, A: 2, B: 0, C: 1},
			{Op: OpReturn, A: 2},
		},
		Consts: []Cell{
			CBigInt(big.NewInt(42)),
			CBigInt(big.NewInt(0)),
		},
	}
	prog := &Program{Funcs: []*Function{fn}, Main: 0}
	vm := New(prog)
	if _, err := vm.Run(); err == nil {
		t.Fatal("want error for bignum div by zero, got nil")
	}
}

func TestBignumLessEqual(t *testing.T) {
	a := big.NewInt(100)
	b := big.NewInt(200)
	got, _ := runBignumProg(t, 4, []Cell{CBigInt(a), CBigInt(b)}, []Instr{
		{Op: OpLoadConstI, A: 0, B: 0},
		{Op: OpLoadConstI, A: 1, B: 1},
		{Op: OpLessBigInt, A: 2, B: 0, C: 1},   // 100 < 200 -> true
		{Op: OpEqualBigInt, A: 3, B: 0, C: 0},  // a == a -> true
		{Op: OpReturn, A: 2},
	})
	if !got.Bool() {
		t.Fatalf("want true for 100<200, got %v", got)
	}
}

func TestBignumBigIntToI64(t *testing.T) {
	got, _ := runBignumProg(t, 2, []Cell{CBigInt(big.NewInt(-1234567890))}, []Instr{
		{Op: OpLoadConstI, A: 0, B: 0},
		{Op: OpBigIntToI64, A: 1, B: 0},
		{Op: OpReturn, A: 1},
	})
	if got.Int() != -1234567890 {
		t.Fatalf("want -1234567890, got %d", got.Int())
	}
}

func TestBignumI64ToBigIntAndToStr(t *testing.T) {
	got, _ := runBignumProg(t, 3, []Cell{CInt(123456789)}, []Instr{
		{Op: OpLoadConstI, A: 0, B: 0},
		{Op: OpI64ToBigInt, A: 1, B: 0},
		{Op: OpBigIntToStr, A: 2, B: 1},
		{Op: OpReturn, A: 2},
	})
	vm := New(&Program{}) // unused, for makeStr decode parity
	_ = vm
	// Decode the resulting string Cell. Heap string > 5 bytes.
	var buf [MaxInlineStr]byte
	var bytes []byte
	if got.IsSStr() {
		bytes = got.SStrBytes(&buf)
	} else if got.IsHeapStr() {
		s := (*vmString)(got.PtrTo())
		bytes = s.bytes
	} else {
		t.Fatalf("want string cell, got %x", got.Bits)
	}
	if string(bytes) != "123456789" {
		t.Fatalf("want \"123456789\", got %q", string(bytes))
	}
}
