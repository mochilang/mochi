package corpus

import "mochi/runtime/vm3"

// PrimeCount: count primes in [2, n) by trial division.
// Matches compiler2/corpus.ExpectPrimeCount.
//
//	fun primes(n) {
//	  var count = 0
//	  var i = 2
//	  while i < n {
//	    var isPrime = 1
//	    var j = 2
//	    while j * j <= i {
//	      if i % j == 0 { isPrime = 0; break }
//	      j = j + 1
//	    }
//	    if isPrime != 0 { count = count + 1 }
//	    i = i + 1
//	  }
//	  return count
//	}
//
// Bank: i64. NumRegsI64 = 6 (n, count, i, isPrime, j, jsq).
//
// Bytecode:
//
//	 0  ConstI64K   A=1, C=0          ; count = 0
//	 1  ConstI64K   A=2, C=2          ; i = 2
//	 2  CmpGtI64Br  A=2, B=0, C=17    ; outer: if i > n jump to end
//	 3  ConstI64K   A=3, C=1          ; isPrime = 1
//	 4  ConstI64K   A=4, C=2          ; j = 2
//	 5  MulI64      A=5, B=4, C=4     ; inner: jsq = j*j
//	 6  CmpGtI64Br  A=5, B=2, C=12    ; if jsq > i jump to after-inner
//	 7  ModI64      A=5, B=2, C=4     ; rem = i % j (reuse jsq slot)
//	 8  CmpNeI64KBr A=5, B=0, C=10    ; if rem != 0 skip break body
//	 9  ConstI64K   A=3, C=0          ; isPrime = 0
//	10  Jump                   C=12   ; break out of inner
//	11  AddI64K     A=4, B=4, C=1     ; j = j + 1
//	12  Jump                   C=5    ; back to inner loop test
//	wait, this is wrong because op 12 is after op 11 but we wanted op 11
//	to be the loop tail and op 12 to be after-inner.
//
// Cleaner layout:
//
//	 0  ConstI64K   A=1, C=0          ; count = 0
//	 1  ConstI64K   A=2, C=2          ; i = 2
//	 2  CmpGtI64Br  A=2, B=0, C=15    ; outer test
//	 3  ConstI64K   A=3, C=1          ; isPrime = 1
//	 4  ConstI64K   A=4, C=2          ; j = 2
//	 5  MulI64      A=5, B=4, C=4     ; inner: jsq = j*j
//	 6  CmpGtI64Br  A=5, B=2, C=12    ; if jsq > i jump to after-inner
//	 7  ModI64      A=5, B=2, C=4     ; rem = i % j
//	 8  CmpNeI64KBr A=5, B=0, C=10    ; if rem != 0 skip break
//	 9  ConstI64K   A=3, C=0          ; isPrime = 0; will jump out
//	10  Jump                   C=12   ; goto after-inner
//	11  AddI64K     A=4, B=4, C=1     ; j = j + 1; fall through
//	    Jump back to inner test (op 5) is needed; insert here.
//	... rearrange:
//
//	 0  ConstI64K   A=1, C=0          ; count = 0
//	 1  ConstI64K   A=2, C=2          ; i = 2
//	    outer_test: PC=2
//	 2  CmpGtI64Br  A=2, B=0, C=16    ; outer: if i > n jump to end
//	 3  ConstI64K   A=3, C=1          ; isPrime = 1
//	 4  ConstI64K   A=4, C=2          ; j = 2
//	    inner_test: PC=5
//	 5  MulI64      A=5, B=4, C=4     ; jsq = j*j
//	 6  CmpGtI64Br  A=5, B=2, C=12    ; after-inner if jsq > i
//	 7  ModI64      A=5, B=2, C=4     ; rem = i % j
//	 8  CmpEqI64KBr A=5, B=0, C=11    ; if rem == 0 jump to mark-composite
//	 9  AddI64K     A=4, B=4, C=1     ; j = j + 1
//	10  Jump                   C=5    ; back to inner_test
//	    mark_composite: PC=11
//	11  ConstI64K   A=3, C=0          ; isPrime = 0; fall through to after-inner
//	    after_inner: PC=12
//	12  CmpEqI64KBr A=3, B=0, C=14    ; if isPrime == 0 skip increment
//	13  AddI64K     A=1, B=1, C=1     ; count = count + 1
//	    advance: PC=14
//	14  AddI64K     A=2, B=2, C=1     ; i = i + 1
//	15  Jump                   C=2    ; back to outer_test
//	    end: PC=16
//	16  ReturnI64   A=1
var PrimeCount = &Program{
	Name: "prime_count",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:       "prime_count",
			NumRegsI64: 6,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 2),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 0, 16),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 1),
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 2),
				vm3.MakeOp(vm3.OpMulI64, 5, 4, 4),
				vm3.MakeOp(vm3.OpCmpGtI64Br, 5, 2, 12),
				vm3.MakeOp(vm3.OpModI64, 5, 2, 4),
				vm3.MakeOp(vm3.OpCmpEqI64KBr, 5, 0, 11),
				vm3.MakeOp(vm3.OpAddI64K, 4, 4, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 5),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				vm3.MakeOp(vm3.OpCmpEqI64KBr, 3, 0, 14),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 2),
				vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
