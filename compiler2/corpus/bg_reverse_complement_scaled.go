package corpus

import "mochi/compiler2/ir"

// BuildReverseComplement is the scaled cross-lang companion to
// BuildReverseComplementKernel. The kernel form uses a hard-coded
// N=1024 buffer; this form takes N as a parameter so the cross-lang
// harness can sweep buffer sizes.
//
// The arithmetic boils down to:
//
//	in := bytes[0..N), filled with the repeating "ACGT" pattern
//	out[N-1-i] := complement(in[i]) for every i in [0, N)
//	return sum(out)
//
// When N is a multiple of 4, the sum is deterministic and equals
// (N/4) * (65+67+71+84) = (N/4) * 287; the cross-lang harness uses
// this single i64 to integer-compare every peer.
//
// MEP-39 §6.5 iter 7: the body is lowered to three single-dispatch
// bulk byte-array super-ops (OpU8FillACGT, OpU8ReverseComplementDNA,
// OpU8SumI64) instead of three per-byte tail-recursive helpers. The
// vm2 interpreter pays ~5 dispatches per run, not ~7N. The earlier
// six-function structure is preserved in git history for reference;
// the per-byte form still benches under the BG kernel test
// (runtime/vm2/bench/bg_reverse_complement_test.go) via the kernel
// builder.
func BuildReverseComplement(n int64) *ir.Module {
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	nv := bMain.ConstI64(n)
	in := bMain.NewU8Array(nv)
	out := bMain.NewU8Array(nv)
	bMain.U8FillACGT(in, nv)
	bMain.U8ReverseComplementDNA(in, out, nv)
	sum := bMain.U8SumI64(out, nv)
	bMain.Ret(sum)
	return &ir.Module{Funcs: []*ir.Function{bMain.Function()}, Main: 0}
}

// ExpectReverseComplement runs the same algorithm in plain Go so the
// oracle test can assert vm2 produces the bit-identical int64 result.
func ExpectReverseComplement(n int64) int64 {
	in := make([]byte, n)
	bases := [4]byte{'A', 'C', 'G', 'T'}
	for i := int64(0); i < n; i++ {
		in[i] = bases[i%4]
	}
	out := make([]byte, n)
	for i := int64(0); i < n; i++ {
		var c byte
		switch in[i] {
		case 'A':
			c = 'T'
		case 'T':
			c = 'A'
		case 'C':
			c = 'G'
		case 'G':
			c = 'C'
		default:
			c = in[i]
		}
		out[n-1-i] = c
	}
	var sum int64
	for i := int64(0); i < n; i++ {
		sum += int64(out[i])
	}
	return sum
}
