package corpus

import "mochi/runtime/vm3"

// Nsieve: Sieve of Eratosthenes, count primes p where 2 <= p <= n.
// Matches compiler2/corpus.ExpectNsieve and the cross-language
// nsieve template at bench/template/bg/nsieve. Bench parameters are
// N=1000 and N=10000 to match the cross-lang baseline.
//
// vm2's encoding uses four mutually-tail-calling functions (fill,
// mark, outer, main). vm3 has unconditional jumps so the entire
// kernel collapses to one function with three nested while loops.
// This is the natural shape for the JIT: no cross-function calls in
// the hot loop, no parameter shuffling per iteration.
//
//	fun nsieve(n) {
//	  var xs = []
//	  var i = 0
//	  while i <= n { xs.push(0); i++ }       // fill: n+1 zero slots
//
//	  var count = 0
//	  var i = 2
//	  while i <= n {
//	    if xs[i] == 0 {                       // unmarked = prime
//	      count++
//	      var j = i*i
//	      while j <= n { xs[j] = 1; j += i }  // mark multiples
//	    }
//	    i++
//	  }
//	  return count
//	}
//
// Banks: i64 (n, count, i, j, val, one_lit) + cell (xs).
// NumRegsI64 = 5 (r0=n, r1=count, r2=i, r3=j, r4=val).
// NumRegsCell = 1 (r0=xs).
//
// Bytecode (22 ops):
//
//	 0  NewList      A=0c, 0, 0           ; xs = []
//	 1  ConstI64K    A=2, 0, 0            ; i = 0 (fill counter)
//	    fill_test (PC=2):
//	 2  CmpGtI64Br   A=2, B=0, C=7        ; if i > n -> fill_done
//	 3  ConstI64K    A=4, 0, 0            ; val = 0
//	 4  ListPushI64  A=0c, B=4, 0         ; xs.push(0)
//	 5  AddI64K      A=2, B=2, C=1        ; i++
//	 6  Jump         0, 0, 2              ; back to fill_test
//	    fill_done (PC=7):
//	 7  ConstI64K    A=1, 0, 0            ; count = 0
//	 8  ConstI64K    A=2, 0, 2            ; i = 2
//	    outer_test (PC=9):
//	 9  CmpGtI64Br   A=2, B=0, C=21       ; if i > n -> end
//	10  ListGetI64   A=4, B=0c, C=2       ; val = xs[i]
//	11  CmpNeI64KBr  A=4, B=0, C=19       ; if val != 0 -> skip
//	12  AddI64K      A=1, B=1, C=1        ; count++
//	13  MulI64       A=3, B=2, C=2        ; j = i*i
//	    inner_test (PC=14):
//	14  CmpGtI64Br   A=3, B=0, C=19       ; if j > n -> skip
//	15  ConstI64K    A=4, 0, 1            ; one = 1
//	16  ListSetI64   A=0c, B=4, C=3       ; xs[j] = 1
//	17  AddI64       A=3, B=3, C=2        ; j += i
//	18  Jump         0, 0, 14             ; back to inner_test
//	    skip (PC=19):
//	19  AddI64K      A=2, B=2, C=1        ; i++
//	20  Jump         0, 0, 9              ; back to outer_test
//	    end (PC=21):
//	21  ReturnI64    A=1, 0, 0
var Nsieve = &Program{
	Name: "nsieve",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:        "nsieve",
			NumRegsI64:  5,
			NumRegsCell: 1,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpNewList, 0, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),
				vm3.MakeOp(vm3.OpCmpGtI64Br, 2, 0, 7),
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 0),
				vm3.MakeOp(vm3.OpListPushI64, 0, 4, 0),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 2),
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 2),
				vm3.MakeOp(vm3.OpCmpGtI64Br, 2, 0, 21),
				vm3.MakeOp(vm3.OpListGetI64, 4, 0, 2),
				vm3.MakeOp(vm3.OpCmpNeI64KBr, 4, 0, 19),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpMulI64, 3, 2, 2),
				vm3.MakeOp(vm3.OpCmpGtI64Br, 3, 0, 19),
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 1),
				vm3.MakeOp(vm3.OpListSetI64, 0, 4, 3),
				vm3.MakeOp(vm3.OpAddI64, 3, 3, 2),
				vm3.MakeOp(vm3.OpJump, 0, 0, 14),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 9),
				vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
