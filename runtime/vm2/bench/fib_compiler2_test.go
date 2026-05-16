package bench

import (
	"testing"

	"mochi/compiler2/emit"
	"mochi/compiler2/ir"
	vm2 "mochi/runtime/vm2"
)

// buildFibCompiler2 constructs the same fib program via the compiler2
// pipeline (IR -> regalloc -> emit) instead of hand-coding bytecode.
// This exercises the full from-scratch stack end-to-end.
func buildFibCompiler2(n int64) *vm2.Program {
	const fibIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	x := bMain.ConstI64(n)
	r := bMain.Call(fibIdx, ir.TI64, x)
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

	m := &ir.Module{
		Funcs: []*ir.Function{bMain.Function(), bFib.Function()},
		Main:  0,
	}
	p, err := emit.Compile(m)
	if err != nil {
		panic(err)
	}
	return p
}

func TestFib10Compiler2(t *testing.T) {
	v := vm2.New(buildFibCompiler2(10))
	got, err := v.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got.Int() != fibGold(10) {
		t.Fatalf("fib(10) = %d, want %d", got.Int(), fibGold(10))
	}
}

func TestFib20Compiler2(t *testing.T) {
	v := vm2.New(buildFibCompiler2(20))
	got, err := v.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got.Int() != fibGold(20) {
		t.Fatalf("fib(20) = %d, want %d", got.Int(), fibGold(20))
	}
}

func BenchmarkFib25Compiler2(b *testing.B) {
	p := buildFibCompiler2(25)
	v := vm2.New(p)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = v.Run()
	}
}
