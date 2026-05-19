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
// Bank: i64 + Cell. NumRegsI64 = 11 (fits the cell-bank ARM64 cap so the
// JIT admits the entire function). Slot reuse: r0/r1/r2 carry n/seed/i
// in the inner loop, then h/HASH_MOD/k in the summ loop after the inner
// loop ends. The map kernel clobbers x13/x14/x15 (= r4/r5/r6 in the
// cell-bank layout), so the constants in r4..r6 are spilled+restored by
// the kernel; map keys/values are kept out of r4..r6 so xKey/xVal are
// never read post-clobber:
//
//	r0=n→h    r4=MOD_LCG  r7=thrG    r10=v
//	r1=seed→HASH_MOD r5=thrA  r8=code
//	r2=i→k    r6=thrC   r9=key2
//	r3=prev
//
// NumRegsCell = 1 (regsCell[0] = m).
//
// Bytecode (60 ops):
//
//	 0  NewMap      0, 0, 20      ; m = AllocMap(capHint=20) (pc=0 so JIT can pre-alloc)
//	 1  ConstI64KW  4, 0, 0       ; MOD_LCG  = 139968
//	 2  ConstI64KW  5, 0, 2       ; thrA
//	 3  ConstI64KW  6, 0, 3       ; thrC
//	 4  ConstI64KW  7, 0, 4       ; thrG
//	 5  ConstI64K   1, 0, 42      ; seed = 42
//	 6  MulI64K     1, 1, 3877
//	 7  AddI64K     1, 1, 29573
//	 8  ModI64      1, 1, 4       ; seed %= MOD_LCG
//	 9  CmpLtI64Br  1, 5, 14      ; if seed < thrA -> BOOT_A
//	10  CmpLtI64Br  1, 6, 16      ; if seed < thrC -> BOOT_C
//	11  CmpLtI64Br  1, 7, 18      ; if seed < thrG -> BOOT_G
//	12  ConstI64K   3, 0, 3       ; prev = 3 (t fall-through)
//	13  Jump              19
//	14  ConstI64K   3, 0, 0       ; BOOT_A: prev = 0 (a)
//	15  Jump              19
//	16  ConstI64K   3, 0, 1       ; BOOT_C: prev = 1 (c)
//	17  Jump              19
//	18  ConstI64K   3, 0, 2       ; BOOT_G: prev = 2 (g); fall through
//	19  MapGetI64I64 10, 0, 3     ; v = m[prev]
//	20  AddI64K     10, 10, 1
//	21  MapSetI64I64 0, 3, 10     ; m[prev] = v
//	22  ConstI64K   2, 0, 1       ; i = 1
//	    loop_top (PC=23):
//	23  CmpGeI64Br  2, 0, 49      ; if i >= n -> loop_end
//	24  MulI64K     1, 1, 3877
//	25  AddI64K     1, 1, 29573
//	26  ModI64      1, 1, 4
//	27  CmpLtI64Br  1, 5, 32      ; ITER_A
//	28  CmpLtI64Br  1, 6, 34      ; ITER_C
//	29  CmpLtI64Br  1, 7, 36      ; ITER_G
//	30  ConstI64K   8, 0, 3       ; code = 3 (t)
//	31  Jump              37
//	32  ConstI64K   8, 0, 0       ; ITER_A
//	33  Jump              37
//	34  ConstI64K   8, 0, 1       ; ITER_C
//	35  Jump              37
//	36  ConstI64K   8, 0, 2       ; ITER_G; fall through
//	37  MapGetI64I64 10, 0, 8     ; v = m[code]
//	38  AddI64K     10, 10, 1
//	39  MapSetI64I64 0, 8, 10     ; m[code] = v
//	40  MulI64K     9, 3, 4       ; key2 = prev*4
//	41  AddI64      9, 9, 8       ; key2 += code
//	42  AddI64K     9, 9, 4       ; key2 += 4
//	43  MapGetI64I64 10, 0, 9     ; v = m[key2]
//	44  AddI64K     10, 10, 1
//	45  MapSetI64I64 0, 9, 10
//	46  MovI64      3, 8, 0       ; prev = code
//	47  AddI64K     2, 2, 1       ; i++
//	48  Jump              23
//	    loop_end (PC=49):
//	49  ConstI64K   0, 0, 0       ; h = 0 (reuse r0; n is dead post-loop)
//	50  ConstI64KW  1, 0, 1       ; HASH_MOD = 2147483647 (reuse r1)
//	51  ConstI64K   2, 0, 0       ; k = 0 (reuse r2)
//	    summ_top (PC=52):
//	52  CmpGeI64KBr 2, 20, 59     ; if k >= 20 -> summ_end
//	53  MapGetI64I64 10, 0, 2     ; v = m[k]
//	54  MulI64K     0, 0, 1009    ; h *= 1009
//	55  AddI64      0, 0, 10      ; h += v
//	56  ModI64      0, 0, 1       ; h %= HASH_MOD
//	57  AddI64K     2, 2, 1       ; k++
//	58  Jump              52
//	    summ_end (PC=59):
//	59  ReturnI64   0, 0, 0
var KNucleotide = &Program{
	Name: "k_nucleotide",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:        "k_nucleotide",
			NumRegsI64:  11,
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
				vm3.MakeOp(vm3.OpNewMap, 0, 0, 20),
				vm3.MakeOp(vm3.OpConstI64KW, 4, 0, 0),
				vm3.MakeOp(vm3.OpConstI64KW, 5, 0, 2),
				vm3.MakeOp(vm3.OpConstI64KW, 6, 0, 3),
				vm3.MakeOp(vm3.OpConstI64KW, 7, 0, 4),
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 42),
				vm3.MakeOp(vm3.OpMulI64K, 1, 1, 3877),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 29573),
				vm3.MakeOp(vm3.OpModI64, 1, 1, 4),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 5, 14),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 6, 16),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 7, 18),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 3),
				vm3.MakeOp(vm3.OpJump, 0, 0, 19),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				vm3.MakeOp(vm3.OpJump, 0, 0, 19),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 19),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 2),
				vm3.MakeOp(vm3.OpMapGetI64I64, 10, 0, 3),
				vm3.MakeOp(vm3.OpAddI64K, 10, 10, 1),
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 3, 10),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 1),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 0, 49),
				vm3.MakeOp(vm3.OpMulI64K, 1, 1, 3877),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 29573),
				vm3.MakeOp(vm3.OpModI64, 1, 1, 4),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 5, 32),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 6, 34),
				vm3.MakeOp(vm3.OpCmpLtI64Br, 1, 7, 36),
				vm3.MakeOp(vm3.OpConstI64K, 8, 0, 3),
				vm3.MakeOp(vm3.OpJump, 0, 0, 37),
				vm3.MakeOp(vm3.OpConstI64K, 8, 0, 0),
				vm3.MakeOp(vm3.OpJump, 0, 0, 37),
				vm3.MakeOp(vm3.OpConstI64K, 8, 0, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 37),
				vm3.MakeOp(vm3.OpConstI64K, 8, 0, 2),
				vm3.MakeOp(vm3.OpMapGetI64I64, 10, 0, 8),
				vm3.MakeOp(vm3.OpAddI64K, 10, 10, 1),
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 8, 10),
				vm3.MakeOp(vm3.OpMulI64K, 9, 3, 4),
				vm3.MakeOp(vm3.OpAddI64, 9, 9, 8),
				vm3.MakeOp(vm3.OpAddI64K, 9, 9, 4),
				vm3.MakeOp(vm3.OpMapGetI64I64, 10, 0, 9),
				vm3.MakeOp(vm3.OpAddI64K, 10, 10, 1),
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 9, 10),
				vm3.MakeOp(vm3.OpMovI64, 3, 8, 0),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 23),
				vm3.MakeOp(vm3.OpConstI64K, 0, 0, 0),
				vm3.MakeOp(vm3.OpConstI64KW, 1, 0, 1),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 2, 20, 59),
				vm3.MakeOp(vm3.OpMapGetI64I64, 10, 0, 2),
				vm3.MakeOp(vm3.OpMulI64K, 0, 0, 1009),
				vm3.MakeOp(vm3.OpAddI64, 0, 0, 10),
				vm3.MakeOp(vm3.OpModI64, 0, 0, 1),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 52),
				vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
