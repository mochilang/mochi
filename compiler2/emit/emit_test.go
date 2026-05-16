package emit

import (
	"testing"

	"mochi/compiler2/ir"
	vm2 "mochi/runtime/vm2"
)

// buildFibIR builds the IR for fib(n) as described in
// compiler2/ir TestBuildFib.
func buildFibIR() *ir.Module {
	const fibIdx = 1 // module index 1; main is index 0

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	n := bMain.ConstI64(20)
	r := bMain.Call(fibIdx, ir.TI64, n)
	bMain.Ret(r)

	bFib := ir.NewBuilder("fib", []ir.Type{ir.TI64}, ir.TI64)
	param := bFib.Param(0)
	thenB := bFib.NewBlock()
	elseB := bFib.NewBlock()

	c2 := bFib.ConstI64(2)
	cond := bFib.LessI64(param, c2)
	bFib.CondBr(cond, thenB, elseB)

	bFib.SwitchTo(thenB)
	bFib.Ret(param)

	bFib.SwitchTo(elseB)
	c1 := bFib.ConstI64(1)
	n1 := bFib.SubI64(param, c1)
	r1 := bFib.Call(fibIdx, ir.TI64, n1)
	n2 := bFib.SubI64(param, c2)
	r2 := bFib.Call(fibIdx, ir.TI64, n2)
	s := bFib.AddI64(r1, r2)
	bFib.Ret(s)

	return &ir.Module{
		Funcs: []*ir.Function{bMain.Function(), bFib.Function()},
		Main:  0,
	}
}

func TestEmitFibCompilesAndRuns(t *testing.T) {
	m := buildFibIR()
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	v := vm2.New(p)
	got, err := v.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if !got.IsInt() || got.Int() != 6765 {
		t.Fatalf("fib(20) = %v, want 6765", got)
	}
}

func TestEmitConstFold(t *testing.T) {
	// main() = 2 + 3 (folds to nothing without opt; emit still works)
	b := ir.NewBuilder("main", nil, ir.TI64)
	x := b.ConstI64(2)
	y := b.ConstI64(3)
	z := b.AddI64(x, y)
	b.Ret(z)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	got, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got.Int() != 5 {
		t.Fatalf("2+3 = %d, want 5", got.Int())
	}
}
