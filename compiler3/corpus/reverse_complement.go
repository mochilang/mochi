package corpus

import (
	"mochi/runtime/vm3"
)

// ExpectReverseComplement mirrors the cross-lang reverse_complement
// template: fill an n-entry buffer with the repeating ACGT pattern,
// reverse-complement into a second buffer (A<->T, C<->G), then return
// the sum of the output byte values as int64. For n a multiple of 4
// the result is (n/4) * 287 because the complemented byte set per
// 4-cycle is {T, G, C, A} = {84, 71, 67, 65}, summing to 287.
func ExpectReverseComplement(n int64) int64 {
	if n <= 0 {
		return 0
	}
	in := make([]int64, n)
	out := make([]int64, n)
	bases := [4]int64{'A', 'C', 'G', 'T'}
	for i := int64(0); i < n; i++ {
		in[i] = bases[i%4]
	}
	for i := int64(0); i < n; i++ {
		c := in[i]
		var cc int64
		switch c {
		case 'A':
			cc = 'T'
		case 'C':
			cc = 'G'
		case 'G':
			cc = 'C'
		case 'T':
			cc = 'A'
		default:
			cc = c
		}
		out[n-1-i] = cc
	}
	var sum int64
	for i := int64(0); i < n; i++ {
		sum += out[i]
	}
	return sum
}

// ReverseComplement: BG `reverse_complement` cross-lang shape port. The
// template fills `in` with `bases[i%4]` for `bases = "ACGT"`, then
// builds `out[n-1-i] = complement(in[i])` (A<->T, C<->G), then returns
// `sum(out[i])` as int64. We mirror that exactly so the JIT path is
// exercised end-to-end across three sequential phases (fill, revcomp,
// sum). The Go template peer lives at bench/template/bg/reverse_complement.
//
// Single vm3 function with three sequential loops over two cell-bank
// lists:
//
//  1. fill loop (pc 5..11): in.push(bases[i%4]) and out.push(0) for
//     i in [0, n). Combining both pushes per iteration keeps the loop
//     count at n rather than 2n; the second push grows `out` to len n
//     so the revcomp loop can use OpListSetI64 by index.
//  2. revcomp loop (pc 14..20): for i in [0, n), val = in[i],
//     val = complement[val], out[dst_idx] = val with dst_idx = n-1-i
//     maintained by a parallel decrement (saves an OpSubI64 per iter).
//  3. sum loop (pc 22..26): sum += out[i] for i in [0, n).
//
// Two i64 lookup tables baked at Build time live in Function.I64Tables:
//
//	I64Tables[0] = bases    (4 entries: 'A'=65, 'C'=67, 'G'=71, 'T'=84)
//	I64Tables[1] = complement (256 entries: complement[c] = ACGT-comp(c),
//	              identity for other byte values)
//
// Both are unchecked OpLookupI64KW reads. Table 0's index `i%4` is
// produced by OpModI64K(2, 1, 4) so it lives in [0, 4). Table 1's
// index is `in[i]` which is one of {65, 67, 71, 84} by construction,
// always in [0, 256). The 256-entry table is 2KB (fits in one L1 line
// group) and avoids a 4-way OpCmpEqI64KBr cascade per element.
//
// Storage shape: two `OpNewList` with capHint = int16(n) so the JIT
// cell-bank path can keep the slab pinned in x19 without a grow-deopt
// (Phase 6.2d.2.b step 2.F regrow-and-retry covers the > 32767 case
// at interp speed). For n bench sizes (1000, 10000) capHint is exact.
//
// Banks: NumRegsI64=6 (0:n, 1:i, 2:val, 3:dst_idx, 4:sum, 5:zero),
// NumRegsCell=2 (0:in, 1:out).
//
// PC map (28 ops):
//
//	 0       NewList in (capHint=n)
//	 1       NewList out (capHint=n)
//	 2..4    zero=0; sum=0; i=0
//	 5..11   fill loop: in.push(bases[i%4]); out.push(0)
//	12..13   i=0; dst_idx = n-1
//	14..20   revcomp loop: out[dst_idx--] = complement(in[i++])
//	21..26   i=0; sum loop: sum += out[i]
//	27       ReturnI64 sum
var ReverseComplement = &Program{
	Name: "reverse_complement",
	Build: func(n int64) *vm3.Program {
		capHint := int16(0)
		if n > 0 && n <= 0x7FFF {
			capHint = int16(n)
		}
		bases := []int64{'A', 'C', 'G', 'T'}
		complement := make([]int64, 256)
		for i := range complement {
			complement[i] = int64(i)
		}
		complement['A'] = 'T'
		complement['T'] = 'A'
		complement['C'] = 'G'
		complement['G'] = 'C'
		fn := &vm3.Function{
			Name:        "reverse_complement",
			NumRegsI64:  6,
			NumRegsF64:  0,
			NumRegsCell: 2,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			I64Tables:   [][]int64{bases, complement},
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpNewList, 0, 0, capHint),  // pc=0: in = NewList
				vm3.MakeOp(vm3.OpNewList, 1, 0, capHint),  // pc=1: out = NewList
				vm3.MakeOp(vm3.OpConstI64K, 5, 0, 0),      // pc=2: zero = 0
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 0),      // pc=3: sum = 0
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),      // pc=4: i = 0
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 12),    // pc=5: if i>=n -> after_fill
				vm3.MakeOp(vm3.OpModI64K, 2, 1, 4),        // pc=6: val = i % 4
				vm3.MakeOp(vm3.OpLookupI64KW, 2, 2, 0),    // pc=7: val = bases[val]
				vm3.MakeOp(vm3.OpListPushI64, 0, 2, 0),    // pc=8: in.push(val)
				vm3.MakeOp(vm3.OpListPushI64, 1, 5, 0),    // pc=9: out.push(0)
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),        // pc=10: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 5),           // pc=11: -> fill_loop
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),      // pc=12: i = 0
				vm3.MakeOp(vm3.OpAddI64K, 3, 0, -1),       // pc=13: dst_idx = n - 1
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 21),    // pc=14: if i>=n -> after_rev
				vm3.MakeOp(vm3.OpListGetI64, 2, 0, 1),     // pc=15: val = in[i]
				vm3.MakeOp(vm3.OpLookupI64KW, 2, 2, 1),    // pc=16: val = complement[val]
				vm3.MakeOp(vm3.OpListSetI64, 1, 2, 3),     // pc=17: out[dst_idx] = val
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),        // pc=18: i++
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, -1),       // pc=19: dst_idx--
				vm3.MakeOp(vm3.OpJump, 0, 0, 14),          // pc=20: -> rev_loop
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),      // pc=21: i = 0
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 27),    // pc=22: if i>=n -> end
				vm3.MakeOp(vm3.OpListGetI64, 2, 1, 1),     // pc=23: val = out[i]
				vm3.MakeOp(vm3.OpAddI64, 4, 4, 2),         // pc=24: sum += val
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),        // pc=25: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 22),          // pc=26: -> sum_loop
				vm3.MakeOp(vm3.OpReturnI64, 4, 0, 0),      // pc=27: return sum
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
