package ir

import "testing"

// TestFixturesValidate asserts every hand-built corpus fixture passes
// Validate. Phase 4.2 (opt), 4.3 (regalloc), and 4.4 (emit) consume
// these as their golden SSA inputs, so an ill-formed fixture here
// would propagate as a silent bug through the rest of the pipeline.
func TestFixturesValidate(t *testing.T) {
	cases := []struct {
		name string
		fn   func() *Function
	}{
		{"fib_iter", FixtureFibIter},
		{"sum_loop", FixtureSumLoop},
		{"fact_rec", FixtureFactRec},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			fn := tc.fn()
			if err := Validate(fn); err != nil {
				t.Fatalf("%s: Validate: %v", tc.name, err)
			}
		})
	}
}

// TestFixtureFibIterShape pins the structural invariants Phase 4.2+
// rely on: a 4-block CFG with a 3-phi loop head.
func TestFixtureFibIterShape(t *testing.T) {
	fn := FixtureFibIter()
	if got, want := len(fn.Blocks), 4; got != want {
		t.Fatalf("blocks: got %d, want %d", got, want)
	}
	if fn.Result != TypeI64 {
		t.Fatalf("result: got %s, want i64", fn.Result)
	}
	if len(fn.Params) != 1 {
		t.Fatalf("params: got %d, want 1", len(fn.Params))
	}
	head := &fn.Blocks[1]
	phis := 0
	for _, vid := range head.Values {
		if fn.Values[vid].Op == OpPhi {
			phis++
		}
	}
	if phis != 3 {
		t.Fatalf("loop_head phis: got %d, want 3", phis)
	}
	if head.Term.Kind != TermBranch {
		t.Fatalf("loop_head terminator: got %v, want branch", head.Term.Kind)
	}
}

// TestFixtureFactRecCallShape pins the recursive call site: an OpCall
// with Const == 0 (self-call) and Args[0] supplying the n-1 argument.
func TestFixtureFactRecCallShape(t *testing.T) {
	fn := FixtureFactRec()
	var found bool
	for vi := range fn.Values {
		v := &fn.Values[vi]
		if v.Op != OpCall {
			continue
		}
		if v.Const != 0 {
			t.Fatalf("call v%d: Const %d, want 0 (self-call)", vi, v.Const)
		}
		if len(v.Args) != 1 {
			t.Fatalf("call v%d: %d args, want 1", vi, len(v.Args))
		}
		found = true
	}
	if !found {
		t.Fatal("no OpCall value found in fact_rec fixture")
	}
}

// TestValidateRejectsBadPhi confirms Validate flags a phi whose arity
// disagrees with its block's predecessor count. This is the single
// most common shape bug a future AST-to-IR frontend could introduce.
func TestValidateRejectsBadPhi(t *testing.T) {
	fn := FixtureSumLoop()
	// SumLoop's loop head is Blocks[1] with two phis at indices 0 and 1.
	head := &fn.Blocks[1]
	phiID := head.Values[0]
	// Truncate the phi's Args so its arity becomes 1, but Preds is 2.
	fn.Values[phiID].Args = fn.Values[phiID].Args[:2]
	if err := Validate(fn); err == nil {
		t.Fatal("Validate accepted phi with wrong arity")
	}
}
