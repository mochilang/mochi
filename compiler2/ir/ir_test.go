package ir

import "testing"

// TestBuildFib constructs the typed IR for fib(n) and verifies it.
//
//	fib(n: i64) -> i64:
//	  b0:
//	    c2 = const 2
//	    cond = n < c2
//	    condbr cond, b1, b2
//	  b1:
//	    ret n
//	  b2:
//	    c1 = const 1
//	    n1 = n - c1
//	    r1 = call fib(n1)
//	    n2 = n - c2
//	    r2 = call fib(n2)
//	    s = r1 + r2
//	    ret s
func TestBuildFib(t *testing.T) {
	const fibIdx = 0
	b := NewBuilder("fib", []Type{TI64}, TI64)
	n := b.Param(0)

	b1 := b.NewBlock()
	b2 := b.NewBlock()

	// b0
	c2 := b.ConstI64(2)
	cond := b.LessI64(n, c2)
	b.CondBr(cond, b1, b2)

	// b1
	b.SwitchTo(b1)
	b.Ret(n)

	// b2
	b.SwitchTo(b2)
	c1 := b.ConstI64(1)
	n1 := b.SubI64(n, c1)
	r1 := b.Call(fibIdx, TI64, n1)
	n2 := b.SubI64(n, c2)
	r2 := b.Call(fibIdx, TI64, n2)
	s := b.AddI64(r1, r2)
	b.Ret(s)

	fn := b.Function()
	m := &Module{Funcs: []*Function{fn}, Main: 0}
	if err := Verify(m, fn); err != nil {
		t.Fatalf("Verify: %v", err)
	}

	if len(fn.Blocks) != 3 {
		t.Errorf("blocks = %d, want 3", len(fn.Blocks))
	}
	// b0 has 4 values: param(n), c2, cond, terminator
	if got, want := len(fn.Blocks[0].Insts), 4; got != want {
		t.Errorf("b0 insts = %d, want %d", got, want)
	}
}

func TestVerifyRejectsDoubleTerminator(t *testing.T) {
	b := NewBuilder("f", nil, TUnit)
	b.Br(b.Current())
	defer func() {
		if r := recover(); r == nil {
			t.Fatal("appending after terminator should panic")
		}
	}()
	b.Br(b.Current()) // should panic via emit
}

func TestVerifyRejectsEmptyBlock(t *testing.T) {
	b := NewBuilder("f", nil, TUnit)
	b.NewBlock() // unreachable empty block
	b.Br(b.Current())
	if err := Verify(nil, b.Function()); err == nil {
		t.Fatal("Verify accepted empty block")
	}
}

func TestVerifyRejectsBadCallIdx(t *testing.T) {
	b := NewBuilder("f", nil, TI64)
	b.Call(99, TI64) // out of range relative to a 1-func module
	b.Ret(ValueID(0))
	fn := b.Function()
	m := &Module{Funcs: []*Function{fn}}
	if err := Verify(m, fn); err == nil {
		t.Fatal("Verify accepted out-of-range call")
	}
}
