package corpus

import (
	"mochi/runtime/vm3"
)

// ExpectFannkuchRedux is the Go oracle that mirrors the FannkuchRedux
// kernel bit-for-bit. It runs n trials of init+countFlips on a fixed
// 7-element permutation `perm[i] = ((i+k) % 7) + 1` and returns the
// sum of per-trial flip counts. Matches the cross-lang
// `fannkuch_redux.go.tmpl` template peer used by the BG suite.
func ExpectFannkuchRedux(n int64) int64 {
	var perm [7]int64
	var total int64
	for k := int64(0); k < n; k++ {
		for i := int64(0); i < 7; i++ {
			perm[i] = ((i+k)%7 + 1)
		}
		var flips int64
		head := perm[0]
		for head != 1 {
			lo, hi := int64(0), head-1
			for lo < hi {
				perm[lo], perm[hi] = perm[hi], perm[lo]
				lo++
				hi--
			}
			head = perm[0]
			flips++
		}
		total += flips
	}
	return total
}

// FannkuchRedux: BG `fannkuch_redux` cross-lang shape port. The
// canonical BG kernel generates every permutation of length n and
// finds the max flip count; the cross-lang template (and so this port)
// simplifies to a fixed 7-element permutation rotated by trial index k
// and accumulates the per-trial flip totals. n is the outer trial
// count.
//
// Single vm3 function with three nested loops:
//
//  1. outer trial loop k in [0, n)
//  2. init loop seeding `perm[i] = ((i+k) % 7) + 1` for i in [0, 7)
//  3. countFlips inner loop: while head != 1, reverse perm[0..head-1]
//     and increment flips
//
// Storage is a single generic list of length 7 (`OpNewList` followed
// by 7 `OpListPushI64`s of 0 to grow it to len 7). After the prefix
// the kernel uses only `OpListGetI64`/`OpListSetI64` so the JIT
// classifies the slab as `slabKindList`, matching nsieve's admit path.
//
// Register banks (NumRegsI64=8, NumRegsCell=1):
//
//	I64: 0 n_in, 1 k, 2 total, 3 i_or_lo, 4 hi,
//	     5 head_or_swap_b, 6 flips,
//	     7 tmp_a (also pre-loaded with 0 to source the init-prefix pushes)
//	Cell: 0 perm
//
// Phase 6.3.4.n.2.e squeeze: slot 5 holds `head` from pc=21 through
// pc=24 (where `hi = head - 1` is the last read) and is then reused as
// `swap_b` inside rev_loop (pc=27 writes it, pc=28 reads it). The two
// live ranges are disjoint. The constant-0 `zero_idx` slot is gone:
// perm[0] reads at pc=21 and pc=33 use `OpListGetI64K` (idx baked as
// imm), and the init pushes (pc=3..9) read 0 from slot 7 because pc=1
// seeds it (and the init_loop later overwrites slot 7 with the rotated
// element value at pc=14..17). NumRegsI64=8 fits the AMD64 cell-bank
// admission cap (R14/RBP are repurposed as arenaCtx / regsCell base,
// effective cap = 8).
//
// PC map (40 ops):
//
//	 0       NewList
//	 1..2    zero_idx = 0; total = 0
//	 3..9    push 7 zeros (perm.len -> 7)
//	10..11   k = 0; outer_loop
//	12..19   init loop (seed perm with rotation)
//	20..21   flips = 0; head = perm[0]
//	22..35   flip_loop -> reverse_loop -> reload head, flips++
//	36..38   total += flips; k++; jump to outer_loop
//	39       ReturnI64 total
//
// Branch targets (uint16(C)):
//
//	outer_loop pc=11 -> END        pc=39
//	init_loop  pc=13 -> INIT_END   pc=20
//	flip_loop  pc=22 -> FLIP_END   pc=36
//	rev_loop   pc=25 -> REV_END    pc=33
var FannkuchRedux = &Program{
	Name: "fannkuch_redux",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:        "fannkuch_redux",
			NumRegsI64:  8,
			NumRegsF64:  0,
			NumRegsCell: 1,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpNewList, 0, 0, 0),     // pc=0: perm = NewList
				vm3.MakeOp(vm3.OpConstI64K, 7, 0, 0),   // pc=1: tmp_a = 0 (zero source for pc=3..9)
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),   // pc=2: total = 0
				vm3.MakeOp(vm3.OpListPushI64, 0, 7, 0), // pc=3..9: push 7 zeros (from tmp_a)
				vm3.MakeOp(vm3.OpListPushI64, 0, 7, 0),
				vm3.MakeOp(vm3.OpListPushI64, 0, 7, 0),
				vm3.MakeOp(vm3.OpListPushI64, 0, 7, 0),
				vm3.MakeOp(vm3.OpListPushI64, 0, 7, 0),
				vm3.MakeOp(vm3.OpListPushI64, 0, 7, 0),
				vm3.MakeOp(vm3.OpListPushI64, 0, 7, 0),
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),    // pc=10: k = 0
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 39),  // pc=11: if k>=n -> END
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),    // pc=12: i = 0
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 3, 7, 20), // pc=13: if i>=7 -> INIT_END
				vm3.MakeOp(vm3.OpAddI64, 7, 3, 1),       // pc=14: tmp = i + k
				vm3.MakeOp(vm3.OpModI64K, 7, 7, 7),      // pc=15: tmp %= 7
				vm3.MakeOp(vm3.OpAddI64K, 7, 7, 1),      // pc=16: tmp += 1
				vm3.MakeOp(vm3.OpListSetI64, 0, 7, 3),   // pc=17: perm[i] = tmp
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, 1),      // pc=18: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 13),        // pc=19 -> init_loop
				vm3.MakeOp(vm3.OpConstI64K, 6, 0, 0),    // pc=20: flips = 0
				vm3.MakeOp(vm3.OpListGetI64K, 5, 0, 0),  // pc=21: head = perm[0] (idx baked)
				vm3.MakeOp(vm3.OpCmpEqI64KBr, 5, 1, 36), // pc=22: if head==1 -> FLIP_END
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),    // pc=23: lo = 0
				vm3.MakeOp(vm3.OpAddI64K, 4, 5, -1),     // pc=24: hi = head - 1 (last read of head)
				vm3.MakeOp(vm3.OpCmpGeI64Br, 3, 4, 33),  // pc=25: if lo>=hi -> REV_END
				vm3.MakeOp(vm3.OpListGetI64, 7, 0, 3),   // pc=26: a = perm[lo]
				vm3.MakeOp(vm3.OpListGetI64, 5, 0, 4),   // pc=27: swap_b = perm[hi] (slot 5 reused)
				vm3.MakeOp(vm3.OpListSetI64, 0, 5, 3),   // pc=28: perm[lo] = swap_b
				vm3.MakeOp(vm3.OpListSetI64, 0, 7, 4),   // pc=29: perm[hi] = a
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, 1),      // pc=30: lo++
				vm3.MakeOp(vm3.OpAddI64K, 4, 4, -1),     // pc=31: hi--
				vm3.MakeOp(vm3.OpJump, 0, 0, 25),        // pc=32 -> rev_loop
				vm3.MakeOp(vm3.OpListGetI64K, 5, 0, 0),  // pc=33: head = perm[0] (idx baked)
				vm3.MakeOp(vm3.OpAddI64K, 6, 6, 1),      // pc=34: flips++
				vm3.MakeOp(vm3.OpJump, 0, 0, 22),        // pc=35 -> flip_loop
				vm3.MakeOp(vm3.OpAddI64, 2, 2, 6),       // pc=36: total += flips
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),      // pc=37: k++
				vm3.MakeOp(vm3.OpJump, 0, 0, 11),        // pc=38 -> outer_loop
				vm3.MakeOp(vm3.OpReturnI64, 2, 0, 0),    // pc=39: return total
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
