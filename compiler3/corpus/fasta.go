package corpus

import (
	"mochi/runtime/vm3"
)

// Fasta: BG `fasta` kernel scaled-down. Mirrors compiler2/corpus.BuildFasta
// and the cross-language template at bench/template/bg/fasta. Bench
// parameters are N=10000 and N=100000.
//
// Inner loop, expressed in Mochi-ish pseudocode:
//
//	seed = 42; hash = 0
//	for i in 0..n {
//	  seed = (seed*3877 + 29573) % 139968          // canonical BG LCG
//	  if      seed < thrA: b = 97  ('a')           // i64-threshold lookup
//	  else if seed < thrC: b = 99  ('c')
//	  else if seed < thrG: b = 103 ('g')
//	  else:                b = 116 ('t')
//	  hash = (hash*1009 + b) % 2147483647          // Mersenne-mod rolling hash
//	}
//	return hash
//
// vm2's BuildFasta uses 2 functions (main + tail-recursive loop with 4
// tail-call leaves). vm3 collapses to one function with an inline
// cascade and a back-jump. Both shapes admit pure-i64 JIT lowering;
// the single-function shape is what vm3jit expects (no frame setup
// per iter, no parameter shuffle).
//
// Constants too wide for OpAddI64K's int16 immediate live in fn.Consts:
//
//	Consts[0] = 139968       (LCG modulus)
//	Consts[1] = 2147483647   (rolling hash modulus, 2^31 - 1)
//	Consts[2] = fastaThrA    (~42404; smallest seed where seed/139968.0 >= 0.302954...)
//	Consts[3] = fastaThrC    (~70117)
//	Consts[4] = fastaThrG    (~97767)
//
// These thresholds make the i64 cascade bit-identical to the float
// cascade in compiler2/corpus.ExpectFasta for every seed in [0, 139968).
//
// Banks: i64 only. NumRegsI64 = 10
// (r0=n, r1=seed, r2=hash, r3=i, r4=MOD_LCG, r5=HASH_MOD,
//  r6=thrA, r7=thrC, r8=thrG, r9=b).
//
// Bytecode (29 ops):
//
//	 0  ConstI64K   1, 0, 42         ; seed = 42
//	 1  ConstI64K   2, 0, 0          ; hash = 0
//	 2  ConstI64K   3, 0, 0          ; i = 0
//	 3  ConstI64KW  4, 0, 0          ; MOD_LCG  = 139968
//	 4  ConstI64KW  5, 0, 1          ; HASH_MOD = 2147483647
//	 5  ConstI64KW  6, 0, 2          ; thrA
//	 6  ConstI64KW  7, 0, 3          ; thrC
//	 7  ConstI64KW  8, 0, 4          ; thrG
//	    loop_test (PC=8):
//	 8  CmpGeI64Br  3, 0, 28         ; if i >= n -> end
//	 9  MulI64K     1, 1, 3877       ; seed *= 3877
//	10  AddI64K     1, 1, 29573      ; seed += 29573
//	11  ModI64      1, 1, 4          ; seed %= MOD_LCG
//	    cascade:
//	12  CmpLtI64Br  1, 6, 17         ; if seed < thrA -> byteA
//	13  CmpLtI64Br  1, 7, 19         ; if seed < thrC -> byteC
//	14  CmpLtI64Br  1, 8, 21         ; if seed < thrG -> byteG
//	    byteT (fall through, PC=15):
//	15  ConstI64K   9, 0, 116        ; b = 't'
//	16  Jump        0, 0, 23
//	    byteA (PC=17):
//	17  ConstI64K   9, 0, 97         ; b = 'a'
//	18  Jump        0, 0, 23
//	    byteC (PC=19):
//	19  ConstI64K   9, 0, 99         ; b = 'c'
//	20  Jump        0, 0, 23
//	    byteG (PC=21):
//	21  ConstI64K   9, 0, 103        ; b = 'g'
//	22  Jump        0, 0, 23
//	    hash_step (PC=23):
//	23  MulI64K     2, 2, 1009       ; hash *= 1009
//	24  AddI64      2, 2, 9          ; hash += b
//	25  ModI64      2, 2, 5          ; hash %= HASH_MOD
//	26  AddI64K     3, 3, 1          ; i++
//	27  Jump        0, 0, 8          ; back to loop_test
//	    end (PC=28):
//	28  ReturnI64   2, 0, 0

// fastaThresholds caches the precomputed i64 cutoffs that make the
// integer cascade bit-identical to the float-prob cascade in
// compiler2/corpus.ExpectFasta for every seed in [0, 139968).
var fastaThrA, fastaThrC, fastaThrG = computeFastaThresholdsVM3()

func computeFastaThresholdsVM3() (int64, int64, int64) {
	var a, c, g int64
	for s := int64(0); s < 139968; s++ {
		p := float64(s) / 139968.0
		if p < 0.3029549426680 {
			a = s + 1
		}
		if p < 0.5009432431601 {
			c = s + 1
		}
		if p < 0.6984905497992 {
			g = s + 1
		}
	}
	return a, c, g
}

var Fasta = &Program{
	Name: "fasta",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:       "fasta",
			NumRegsI64: 10,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Consts: []vm3.Cell{
				vm3.CInt(139968),
				vm3.CInt(2147483647),
				vm3.CInt(fastaThrA),
				vm3.CInt(fastaThrC),
				vm3.CInt(fastaThrG),
			},
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 42),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				vm3.MakeOp(vm3.OpConstI64KW, 4, 0, 0),
				vm3.MakeOp(vm3.OpConstI64KW, 5, 0, 1),
				vm3.MakeOp(vm3.OpConstI64KW, 6, 0, 2),
				vm3.MakeOp(vm3.OpConstI64KW, 7, 0, 3),
				vm3.MakeOp(vm3.OpConstI64KW, 8, 0, 4),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 3, 0, 28),
				vm3.MakeOp(vm3.OpMulI64K, 1, 1, 3877),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 29573),
				vm3.MakeOp(vm3.OpModI64, 1, 1, 4),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 6, 17),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 7, 19),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 8, 21),
				vm3.MakeOp(vm3.OpConstI64K, 9, 0, 116),
				vm3.MakeOp(vm3.OpJump, 0, 0, 23),
				vm3.MakeOp(vm3.OpConstI64K, 9, 0, 97),
				vm3.MakeOp(vm3.OpJump, 0, 0, 23),
				vm3.MakeOp(vm3.OpConstI64K, 9, 0, 99),
				vm3.MakeOp(vm3.OpJump, 0, 0, 23),
				vm3.MakeOp(vm3.OpConstI64K, 9, 0, 103),
				vm3.MakeOp(vm3.OpJump, 0, 0, 23),
				vm3.MakeOp(vm3.OpMulI64K, 2, 2, 1009),
				vm3.MakeOp(vm3.OpAddI64, 2, 2, 9),
				vm3.MakeOp(vm3.OpModI64, 2, 2, 5),
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 8),
				vm3.MakeOp(vm3.OpReturnI64, 2, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
