package corpus

import "mochi/runtime/vm3"

// KNucleotide: Phase 6.3.4.f single-function port of the BG k_nucleotide
// kernel. Mirrors compiler2/corpus.BuildKNucleotide (4-fn loop/summ/lookup
// shape) but collapses to one vm3 function with inline LCG, i64-threshold
// cascade, and back-jump, so the hot loop carries no cross-fn call. This
// is the same shape choice we made for fasta in Phase 6.3.4.d: drop the
// per-iter dispatch + parameter shuffle and let the JIT see a flat
// loop-body once the Cell-bank admission grows to cover this opcode mix.
//
// Algorithm:
//
//	seed = 42; prev = lookup_int(LCG(seed))
//	m[prev] += 1                                 // bootstrap iter 0
//	for i in 1..n {
//	  seed = LCG(seed)                            // (seed*3877+29573)%139968
//	  code = lookup_int(seed)                     // 4-way int-threshold
//	  m[code] += 1                                // 1-mer count
//	  m[4 + prev*4 + code] += 1                   // 2-mer count
//	  prev = code
//	}
//	h = 0
//	for k in 0..20 { h = (h*1009 + m[k]) % 2147483647 }
//	return h
//
// Bit-identical to compiler2/corpus.ExpectKNucleotide because the i64
// thresholds in fastaThr{A,C,G} were chosen to make the integer cascade
// equivalent to the float-prob cascade for every seed in [0, 139968).
//
// Bank: i64 + Cell. NumRegsI64 = 14:
//
//	r0=n      r4=MOD_LCG  r6=thrA  r9=code   r11=v   r13=k
//	r1=seed   r5=HASH_MOD r7=thrC  r10=key2  r12=h
//	r2=i                  r8=thrG
//	r3=prev
//
// NumRegsCell = 1 (regsCell[0] = m).
//
// Bytecode (60 ops):
//
//	 0  ConstI64KW  4, 0, 0       ; MOD_LCG  = 139968
//	 1  ConstI64KW  5, 0, 1       ; HASH_MOD = 2147483647
//	 2  ConstI64KW  6, 0, 2       ; thrA
//	 3  ConstI64KW  7, 0, 3       ; thrC
//	 4  ConstI64KW  8, 0, 4       ; thrG
//	 5  NewMap      0, 0, 20      ; m = AllocMap(capHint=20)
//	 6  ConstI64K   1, 0, 42      ; seed = 42
//	 7  MulI64K     1, 1, 3877
//	 8  AddI64K     1, 1, 29573
//	 9  ModI64      1, 1, 4       ; seed %= MOD_LCG
//	10  CmpLtI64Br  1, 6, 15      ; if seed < thrA -> BOOT_A
//	11  CmpLtI64Br  1, 7, 17      ; if seed < thrC -> BOOT_C
//	12  CmpLtI64Br  1, 8, 19      ; if seed < thrG -> BOOT_G
//	13  ConstI64K   3, 0, 3       ; prev = 3 (t fall-through)
//	14  Jump              20
//	15  ConstI64K   3, 0, 0       ; BOOT_A: prev = 0 (a)
//	16  Jump              20
//	17  ConstI64K   3, 0, 1       ; BOOT_C: prev = 1 (c)
//	18  Jump              20
//	19  ConstI64K   3, 0, 2       ; BOOT_G: prev = 2 (g); fall through
//	20  MapGetI64I64 11, 0, 3     ; v = m[prev]
//	21  AddI64K     11, 11, 1
//	22  MapSetI64I64 0, 3, 11     ; m[prev] = v
//	23  ConstI64K   2, 0, 1       ; i = 1
//	    loop_top (PC=24):
//	24  CmpGeI64Br  2, 0, 50      ; if i >= n -> loop_end
//	25  MulI64K     1, 1, 3877
//	26  AddI64K     1, 1, 29573
//	27  ModI64      1, 1, 4
//	28  CmpLtI64Br  1, 6, 33      ; ITER_A
//	29  CmpLtI64Br  1, 7, 35      ; ITER_C
//	30  CmpLtI64Br  1, 8, 37      ; ITER_G
//	31  ConstI64K   9, 0, 3       ; code = 3 (t)
//	32  Jump              38
//	33  ConstI64K   9, 0, 0       ; ITER_A
//	34  Jump              38
//	35  ConstI64K   9, 0, 1       ; ITER_C
//	36  Jump              38
//	37  ConstI64K   9, 0, 2       ; ITER_G; fall through
//	38  MapGetI64I64 11, 0, 9     ; v = m[code]
//	39  AddI64K     11, 11, 1
//	40  MapSetI64I64 0, 9, 11     ; m[code] = v
//	41  MulI64K     10, 3, 4      ; key2 = prev*4
//	42  AddI64      10, 10, 9     ; key2 += code
//	43  AddI64K     10, 10, 4     ; key2 += 4
//	44  MapGetI64I64 11, 0, 10    ; v = m[key2]
//	45  AddI64K     11, 11, 1
//	46  MapSetI64I64 0, 10, 11
//	47  MovI64      3, 9, 0       ; prev = code
//	48  AddI64K     2, 2, 1       ; i++
//	49  Jump              24
//	    loop_end (PC=50):
//	50  ConstI64K   12, 0, 0      ; h = 0
//	51  ConstI64K   13, 0, 0      ; k = 0
//	    summ_top (PC=52):
//	52  CmpGeI64KBr 13, 20, 59    ; if k >= 20 -> summ_end
//	53  MapGetI64I64 11, 0, 13    ; v = m[k]
//	54  MulI64K     12, 12, 1009  ; h *= 1009
//	55  AddI64      12, 12, 11    ; h += v
//	56  ModI64      12, 12, 5     ; h %= HASH_MOD
//	57  AddI64K     13, 13, 1     ; k++
//	58  Jump              52
//	    summ_end (PC=59):
//	59  ReturnI64   12, 0, 0
var KNucleotide = &Program{
	Name: "k_nucleotide",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:        "k_nucleotide",
			NumRegsI64:  14,
			NumRegsCell: 1,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Consts: []vm3.Cell{
				vm3.CInt(139968),
				vm3.CInt(2147483647),
				vm3.CInt(fastaThrA),
				vm3.CInt(fastaThrC),
				vm3.CInt(fastaThrG),
			},
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstI64KW, 4, 0, 0),
				vm3.MakeOp(vm3.OpConstI64KW, 5, 0, 1),
				vm3.MakeOp(vm3.OpConstI64KW, 6, 0, 2),
				vm3.MakeOp(vm3.OpConstI64KW, 7, 0, 3),
				vm3.MakeOp(vm3.OpConstI64KW, 8, 0, 4),
				vm3.MakeOp(vm3.OpNewMap, 0, 0, 20),
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 42),
				vm3.MakeOp(vm3.OpMulI64K, 1, 1, 3877),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 29573),
				vm3.MakeOp(vm3.OpModI64, 1, 1, 4),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 6, 15),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 7, 17),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 8, 19),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 3),
				vm3.MakeOp(vm3.OpJump, 0, 0, 20),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				vm3.MakeOp(vm3.OpJump, 0, 0, 20),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 20),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 2),
				vm3.MakeOp(vm3.OpMapGetI64I64, 11, 0, 3),
				vm3.MakeOp(vm3.OpAddI64K, 11, 11, 1),
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 3, 11),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 1),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 0, 50),
				vm3.MakeOp(vm3.OpMulI64K, 1, 1, 3877),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 29573),
				vm3.MakeOp(vm3.OpModI64, 1, 1, 4),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 6, 33),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 7, 35),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 8, 37),
				vm3.MakeOp(vm3.OpConstI64K, 9, 0, 3),
				vm3.MakeOp(vm3.OpJump, 0, 0, 38),
				vm3.MakeOp(vm3.OpConstI64K, 9, 0, 0),
				vm3.MakeOp(vm3.OpJump, 0, 0, 38),
				vm3.MakeOp(vm3.OpConstI64K, 9, 0, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 38),
				vm3.MakeOp(vm3.OpConstI64K, 9, 0, 2),
				vm3.MakeOp(vm3.OpMapGetI64I64, 11, 0, 9),
				vm3.MakeOp(vm3.OpAddI64K, 11, 11, 1),
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 9, 11),
				vm3.MakeOp(vm3.OpMulI64K, 10, 3, 4),
				vm3.MakeOp(vm3.OpAddI64, 10, 10, 9),
				vm3.MakeOp(vm3.OpAddI64K, 10, 10, 4),
				vm3.MakeOp(vm3.OpMapGetI64I64, 11, 0, 10),
				vm3.MakeOp(vm3.OpAddI64K, 11, 11, 1),
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 10, 11),
				vm3.MakeOp(vm3.OpMovI64, 3, 9, 0),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 24),
				vm3.MakeOp(vm3.OpConstI64K, 12, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 13, 0, 0),
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 13, 20, 59),
				vm3.MakeOp(vm3.OpMapGetI64I64, 11, 0, 13),
				vm3.MakeOp(vm3.OpMulI64K, 12, 12, 1009),
				vm3.MakeOp(vm3.OpAddI64, 12, 12, 11),
				vm3.MakeOp(vm3.OpModI64, 12, 12, 5),
				vm3.MakeOp(vm3.OpAddI64K, 13, 13, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 52),
				vm3.MakeOp(vm3.OpReturnI64, 12, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
