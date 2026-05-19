package corpus

import "mochi/runtime/vm3"

// SwitchLookup8CmpChain and SwitchLookup8Table are the Phase 6.4 micro-
// bench corpus for the switch-to-lookup-table optimization (port of
// Go CL 756340 / golang/go#78203). Both kernels loop N times, generate
// an unpredictable key in [0, 8) via a small bounded-state LCG, and
// dispatch a per-key i64 result that is accumulated into the running
// sum. The only difference is dispatch shape:
//
//   - CmpChain emits a cascade of OpCmpEqI64KBr (one per case) followed
//     by per-case OpConstI64K + OpJump bodies. This is what a naive
//     switch lowering looks like, and is what vm3 emits today.
//
//   - Table emits a single OpCmpGeI64KBr bounds check followed by one
//     OpLookupI64KW that loads the per-case constant from an i64
//     compile-time table on the Function.
//
// The keys come from a 32749-period LCG (state = state*17 + 12345 mod
// 32749, then key = state mod 8). The period is far larger than any
// branch-predictor history, so the cmp-chain pays a serial chain of
// mispredictions per dispatch, while the table form is one always-
// taken cmp + one indexed load. CL 756340 measures -62.65% on the
// equivalent Go bench (SwitchLookup8Unpredictable).
//
// Both variants are constructed so that their inputs and outputs are
// bit-identical for the same N; the test harness asserts this.
//
// The per-case values [111, 222, ..., 888] are arbitrary, chosen so
// that ConstI64K (int16) immediates fit and so the sum is unique per
// key sequence (verifies dispatch went to the right case).

// switchLookupValues is the 8-entry case-value table, shared across
// both corpus variants and the test harness.
var switchLookupValues = [8]int64{111, 222, 333, 444, 555, 666, 777, 888}

// SwitchLookup8CmpChain dispatches via a cmp-and-branch cascade.
//
// NumRegsI64 = 6 (n=r0, sum=r1, i=r2, state=r3, key=r4, result=r5).
//
// PC layout:
//
//	 0  ConstI64K  1, 0, 0       ; sum = 0
//	 1  ConstI64K  2, 0, 0       ; i = 0
//	 2  ConstI64K  3, 0, 1       ; state = 1
//	    loop_test (PC=3):
//	 3  CmpGeI64Br 2, 0, end     ; if i >= n goto end
//	    LCG (PC=4..7):
//	 4  MulI64K    3, 3, 17      ; state *= 17
//	 5  AddI64K    3, 3, 12345   ; state += 12345
//	 6  ModI64K    3, 3, 32749   ; state %= 32749
//	 7  ModI64K    4, 3, 8       ; key = state % 8
//	    cmp-chain dispatch (PC=8..15):
//	 8  CmpEqI64KBr 4, 0, case0
//	 9  CmpEqI64KBr 4, 1, case1
//	10  CmpEqI64KBr 4, 2, case2
//	11  CmpEqI64KBr 4, 3, case3
//	12  CmpEqI64KBr 4, 4, case4
//	13  CmpEqI64KBr 4, 5, case5
//	14  CmpEqI64KBr 4, 6, case6
//	15  CmpEqI64KBr 4, 7, case7
//	    default (unreachable but valid):
//	16  ConstI64K  5, 0, 0
//	17  Jump       0, 0, join
//	    cases:
//	18  ConstI64K  5, 0, 111
//	19  Jump       0, 0, join
//	20  ConstI64K  5, 0, 222
//	21  Jump       0, 0, join
//	22  ConstI64K  5, 0, 333
//	23  Jump       0, 0, join
//	24  ConstI64K  5, 0, 444
//	25  Jump       0, 0, join
//	26  ConstI64K  5, 0, 555
//	27  Jump       0, 0, join
//	28  ConstI64K  5, 0, 666
//	29  Jump       0, 0, join
//	30  ConstI64K  5, 0, 777
//	31  Jump       0, 0, join
//	32  ConstI64K  5, 0, 888
//	33  Jump       0, 0, join
//	    join (PC=34):
//	34  AddI64     1, 1, 5       ; sum += result
//	35  AddI64K    2, 2, 1       ; i++
//	36  Jump       0, 0, 3       ; back to loop_test
//	    end (PC=37):
//	37  ReturnI64  1, 0, 0
var SwitchLookup8CmpChain = &Program{
	Name: "switch_lookup8_cmp_chain",
	Build: func(_ int64) *vm3.Program {
		const join = 34
		const end = 37
		code := []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
			vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),
			vm3.MakeOp(vm3.OpConstI64K, 3, 0, 1),
			vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 0, end),
			vm3.MakeOp(vm3.OpMulI64K, 3, 3, 17),
			vm3.MakeOp(vm3.OpAddI64K, 3, 3, 12345),
			vm3.MakeOp(vm3.OpModI64K, 3, 3, 32749),
			vm3.MakeOp(vm3.OpModI64K, 4, 3, 8),
			vm3.MakeOp(vm3.OpCmpEqI64KBr, 4, 0, 18),
			vm3.MakeOp(vm3.OpCmpEqI64KBr, 4, 1, 20),
			vm3.MakeOp(vm3.OpCmpEqI64KBr, 4, 2, 22),
			vm3.MakeOp(vm3.OpCmpEqI64KBr, 4, 3, 24),
			vm3.MakeOp(vm3.OpCmpEqI64KBr, 4, 4, 26),
			vm3.MakeOp(vm3.OpCmpEqI64KBr, 4, 5, 28),
			vm3.MakeOp(vm3.OpCmpEqI64KBr, 4, 6, 30),
			vm3.MakeOp(vm3.OpCmpEqI64KBr, 4, 7, 32),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, 0),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, int16(switchLookupValues[0])),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, int16(switchLookupValues[1])),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, int16(switchLookupValues[2])),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, int16(switchLookupValues[3])),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, int16(switchLookupValues[4])),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, int16(switchLookupValues[5])),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, int16(switchLookupValues[6])),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, int16(switchLookupValues[7])),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpAddI64, 1, 1, 5),
			vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
			vm3.MakeOp(vm3.OpJump, 0, 0, 3),
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
		}
		fn := &vm3.Function{
			Name:       "switch_lookup8_cmp_chain",
			NumRegsI64: 6,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Code:       code,
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}

// SwitchLookup8Table dispatches via OpLookupI64KW backed by an i64
// table on the Function.
//
// NumRegsI64 = 6 (n=r0, sum=r1, i=r2, state=r3, key=r4, result=r5).
//
// PC layout:
//
//	 0  ConstI64K     1, 0, 0      ; sum = 0
//	 1  ConstI64K     2, 0, 0      ; i = 0
//	 2  ConstI64K     3, 0, 1      ; state = 1
//	    loop_test (PC=3):
//	 3  CmpGeI64Br    2, 0, end    ; if i >= n goto end
//	    LCG (PC=4..7):
//	 4  MulI64K       3, 3, 17
//	 5  AddI64K       3, 3, 12345
//	 6  ModI64K       3, 3, 32749
//	 7  ModI64K       4, 3, 8
//	    bounds + table lookup (PC=8..10):
//	 8  CmpGeI64KBr   4, 8, default
//	 9  LookupI64KW   5, 4, 0      ; result = I64Tables[0][key]
//	10  Jump          0, 0, join
//	    default (unreachable):
//	11  ConstI64K     5, 0, 0
//	    join (PC=12):
//	12  AddI64        1, 1, 5
//	13  AddI64K       2, 2, 1
//	14  Jump          0, 0, 3
//	    end (PC=15):
//	15  ReturnI64     1, 0, 0
var SwitchLookup8Table = &Program{
	Name: "switch_lookup8_table",
	Build: func(_ int64) *vm3.Program {
		const join = 12
		const end = 15
		table := make([]int64, 8)
		for i, v := range switchLookupValues {
			table[i] = v
		}
		code := []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
			vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),
			vm3.MakeOp(vm3.OpConstI64K, 3, 0, 1),
			vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 0, end),
			vm3.MakeOp(vm3.OpMulI64K, 3, 3, 17),
			vm3.MakeOp(vm3.OpAddI64K, 3, 3, 12345),
			vm3.MakeOp(vm3.OpModI64K, 3, 3, 32749),
			vm3.MakeOp(vm3.OpModI64K, 4, 3, 8),
			vm3.MakeOp(vm3.OpCmpGeI64KBr, 4, 8, 11),
			vm3.MakeOp(vm3.OpLookupI64KW, 5, 4, 0),
			vm3.MakeOp(vm3.OpJump, 0, 0, join),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, 0),
			vm3.MakeOp(vm3.OpAddI64, 1, 1, 5),
			vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
			vm3.MakeOp(vm3.OpJump, 0, 0, 3),
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
		}
		fn := &vm3.Function{
			Name:       "switch_lookup8_table",
			NumRegsI64: 6,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Code:       code,
			I64Tables:  [][]int64{table},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}

// ExpectSwitchLookup8 is the Go-native reference. It must return the
// same value as both bytecode variants for any non-negative N. The
// LCG and per-key table are an exact mirror of the bytecode above.
func ExpectSwitchLookup8(n int64) int64 {
	var sum int64
	state := int64(1)
	for i := int64(0); i < n; i++ {
		state = (state*17 + 12345) % 32749
		key := state % 8
		sum += switchLookupValues[key]
	}
	return sum
}
