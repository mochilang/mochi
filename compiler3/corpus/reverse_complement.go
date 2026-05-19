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
// typed-i64 arrays (Phase 6.3.4.l.4):
//
//  1. fill loop (pc 4..9): in[i] = bases[i%4] for i in [0, n). Both
//     arrays are pre-sized to length n by OpNewI64Array, so the fill
//     loop just Sets `in` (and `out` is left at its initial all-zero
//     state, which the revcomp loop will overwrite).
//  2. revcomp loop (pc 12..18): for i in [0, n), val = in[i],
//     val = complement[val], out[dst_idx] = val with dst_idx = n-1-i
//     maintained by a parallel decrement (saves an OpSubI64 per iter).
//  3. sum loop (pc 20..24): sum += out[i] for i in [0, n).
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
// Storage shape: two `OpNewI64Array` with capHint = int16(n) lifted into
// jitCall's pre-alloc prefix (Phase 6.3.4.l.4), so the JIT lowerer emits
// zero words for the two NewI64Array PCs and the prologue picks up the
// pre-seeded handles. AllocI64Arr(n) pre-fills the data slice with n
// zeros, so no Push is needed. Per-access cost drops to ~6 inst (UXTW
// + MOV stride + MUL + ADD x19 + LDR data.ptr + LDR/STR Xd) with no
// 48-bit CInt tag-box/unbox versus the OpList* path's 14+ inst (extra
// BFI on push/set, SBFX on get). Closes the read/write loop under 2x
// of Go.
//
// Banks: NumRegsI64=5 (0:n, 1:i, 2:val, 3:dst_idx, 4:sum),
// NumRegsCell=2 (0:in, 1:out).
//
// PC map (26 ops):
//
//	 0       NewI64Array in (capHint=n)
//	 1       NewI64Array out (capHint=n)
//	 2..3    sum=0; i=0
//	 4..9    fill loop: in[i] = bases[i%4]
//	10..11   i=0; dst_idx = n-1
//	12..18   revcomp loop: out[dst_idx--] = complement(in[i++])
//	19..24   i=0; sum loop: sum += out[i]
//	25       ReturnI64 sum
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
			NumRegsI64:  5,
			NumRegsF64:  0,
			NumRegsCell: 2,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			I64Tables:   [][]int64{bases, complement},
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpNewI64Array, 0, 0, capHint), // pc=0: in = NewI64Array(n)
				vm3.MakeOp(vm3.OpNewI64Array, 1, 0, capHint), // pc=1: out = NewI64Array(n)
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 0),         // pc=2: sum = 0
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),         // pc=3: i = 0
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 10),       // pc=4: if i>=n -> after_fill
				vm3.MakeOp(vm3.OpModI64K, 2, 1, 4),           // pc=5: val = i % 4
				vm3.MakeOp(vm3.OpLookupI64KW, 2, 2, 0),       // pc=6: val = bases[val]
				vm3.MakeOp(vm3.OpI64ArraySetI64, 0, 2, 1),    // pc=7: in[i] = val
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),           // pc=8: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 4),              // pc=9: -> fill_loop
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),         // pc=10: i = 0
				vm3.MakeOp(vm3.OpAddI64K, 3, 0, -1),          // pc=11: dst_idx = n - 1
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 19),       // pc=12: if i>=n -> after_rev
				vm3.MakeOp(vm3.OpI64ArrayGetI64, 2, 0, 1),    // pc=13: val = in[i]
				vm3.MakeOp(vm3.OpLookupI64KW, 2, 2, 1),       // pc=14: val = complement[val]
				vm3.MakeOp(vm3.OpI64ArraySetI64, 1, 2, 3),    // pc=15: out[dst_idx] = val
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),           // pc=16: i++
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, -1),          // pc=17: dst_idx--
				vm3.MakeOp(vm3.OpJump, 0, 0, 12),             // pc=18: -> rev_loop
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),         // pc=19: i = 0
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 25),       // pc=20: if i>=n -> end
				vm3.MakeOp(vm3.OpI64ArrayGetI64, 2, 1, 1),    // pc=21: val = out[i]
				vm3.MakeOp(vm3.OpAddI64, 4, 4, 2),            // pc=22: sum += val
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),           // pc=23: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 20),             // pc=24: -> sum_loop
				vm3.MakeOp(vm3.OpReturnI64, 4, 0, 0),         // pc=25: return sum
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
