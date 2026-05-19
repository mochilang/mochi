package vm3jit_test

import (
	"testing"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestDeoptCountListsFillSumParity locks the Phase 6.2d.2.b step 2.F
// regrow-and-retry behavior: across 100 parity-perturbed iters the JIT
// pays at most one StatusListGrow deopt (the first odd iter that
// exceeds the static OpNewList capHint), the retry succeeds against the
// regrown warm cache, and every subsequent iter takes the no-deopt
// fast path. A regression in the retry path would surface here as a
// 100x jump in deopt count.
func TestDeoptCountListsFillSumParity(t *testing.T) {
	prog := corpus.ListsFillSum.Build(128)
	cfs := vm3jit.CompileProgram(prog)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()
	fn := prog.Funcs[prog.Entry]
	if fn.JITCode == nil {
		t.Skip("main not JIT-compiled on this host")
	}
	vm := vm3.NewWithProgram(prog)
	args := []int64{128}
	startTotal := vm3jit.DeoptCount
	startPre := vm3jit.DeoptCountPreAlloc
	startRetry := vm3jit.DeoptCountPreAllocRetry
	startGen := vm3jit.DeoptCountGeneral
	for i := 0; i < 100; i++ {
		args[0] = 128 + int64(i&1)
		got, err := vm.RunWithArgs(fn, args)
		if err != nil {
			t.Fatalf("iter %d: %v", i, err)
		}
		n := args[0]
		want := n * (n - 1) / 2
		if got.Int() != want {
			t.Fatalf("iter %d: got %d want %d", i, got.Int(), want)
		}
	}
	gotTotal := vm3jit.DeoptCount - startTotal
	gotPre := vm3jit.DeoptCountPreAlloc - startPre
	gotRetry := vm3jit.DeoptCountPreAllocRetry - startRetry
	gotGen := vm3jit.DeoptCountGeneral - startGen
	t.Logf("100 parity iters: total=%d preAlloc=%d retrySucc=%d general=%d",
		gotTotal, gotPre, gotRetry, gotGen)
	if gotTotal > 2 {
		t.Fatalf("regrow-and-retry regressed: total deopts=%d want <=2 (1 expected)", gotTotal)
	}
	if gotRetry != gotPre {
		t.Fatalf("retry must succeed on every PreAlloc deopt: preAlloc=%d retrySucc=%d", gotPre, gotRetry)
	}
	if gotGen != 0 {
		t.Fatalf("general deopt path must not fire under regrow-and-retry: general=%d", gotGen)
	}

	// Steady-state n=128 only: warm cache stays sized, zero deopts.
	startTotal = vm3jit.DeoptCount
	startPre = vm3jit.DeoptCountPreAlloc
	startGen = vm3jit.DeoptCountGeneral
	for i := 0; i < 100; i++ {
		args[0] = 128
		_, err := vm.RunWithArgs(fn, args)
		if err != nil {
			t.Fatalf("n=128 iter %d: %v", i, err)
		}
	}
	t.Logf("100 n=128 iters: total=%d preAlloc=%d general=%d",
		vm3jit.DeoptCount-startTotal,
		vm3jit.DeoptCountPreAlloc-startPre,
		vm3jit.DeoptCountGeneral-startGen)
	if d := vm3jit.DeoptCount - startTotal; d != 0 {
		t.Fatalf("steady-state n=128 must not deopt: total=%d", d)
	}
}
