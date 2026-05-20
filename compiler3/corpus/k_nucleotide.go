package corpus

import "mochi/runtime/vm3"

// KNucleotide: Phase 6.3.4.f single-function port of the BG k_nucleotide
// kernel. Mirrors compiler2/corpus.BuildKNucleotide (4-fn loop/summ/lookup
// shape) but collapses to one vm3 function with inline LCG, i64-threshold
// cascade, and back-jump, so the hot loop carries no cross-fn call.
//
// Phase 6.3.4.n.8.e refactor: MOD_LCG, HASH_MOD, and the three fastaThr
// thresholds previously lived in dedicated i64 registers (r4..r7, plus
// r1 reused for HASH_MOD in the summ loop). Each of those is constant
// across the kernel, so they fold into instruction immediates via the
// wide-K opcodes OpModI64KW / OpCmpLtI64KWBr (B = Consts pool index).
// That drops NumRegsI64 from 11 to 7, which fits both the ARM64 cell-bank
// cap of 11 and the AMD64 cell-bank effective cap of 8 (maxI64=10 minus
// RBP/R14 stolen for regsCell / arenaCtx). The transform is textbook
// constant-folding-into-immediates; on AMD64 the LCG modulus and the
// HASH_MOD constant both fit in int32, so the wide-K lowering reduces
// to the same 7-byte IMM32 sequence used by OpCmpLtI64K / OpModI64K
// with no extra cost. Bit-identical output.
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
// Bank: i64 + Cell. NumRegsI64 = 7 (fits both ARM64 and AMD64 cell-bank
// caps). The ARM64 map kernel uses x13/x14/x15 (= vm3 i64 slots 4..6)
// as scratch/loop-invariant pins and explicitly forbids any map-op
// key/val/dst operand from sitting in slots 4..6. Layout therefore
// places the four map-op operands (code, key2, v, prev) in slots 0..3,
// and pushes the three loop-carried non-operands (n, seed, i) into
// slots 4..6, where the map kernel's entry-spill/exit-restore pair
// preserves them across each map call:
//
//	r0=code→h    r4=i        r6=n
//	r1=key2      r5=seed
//	r2=v         (r6 receives n via OpMovI64 at PC 1; param arrives at r0)
//	r3=prev→k
//
// In the summ loop, code/key2/prev slots are reused: r0=h (was code),
// r3=k (was prev), r2=v (unchanged).
//
// NumRegsCell = 1 (regsCell[0] = m).
//
// Consts:
//	[0] = 139968    (MOD_LCG; OpModI64KW)
//	[1] = 2147483647 (HASH_MOD; OpModI64KW)
//	[2] = fastaThrA (OpCmpLtI64KWBr)
//	[3] = fastaThrC (OpCmpLtI64KWBr)
//	[4] = fastaThrG (OpCmpLtI64KWBr)
//
// Bytecode (56 ops):
//
//	 0  NewMap        0, 0, 20       ; regsCell[0] = m (cap=20)
//	 1  MovI64        6, 0, 0        ; r6 = r0 (param n moves out of r0)
//	 2  ConstI64K     5, 0, 42       ; seed(r5) = 42
//	 3  MulI64K       5, 5, 3877
//	 4  AddI64K       5, 5, 29573
//	 5  ModI64KW      5, 5, 0        ; seed %= Consts[0]
//	 6  CmpLtI64KWBr  5, 2, 11       ; if seed<thrA -> BOOT_A
//	 7  CmpLtI64KWBr  5, 3, 13       ; if seed<thrC -> BOOT_C
//	 8  CmpLtI64KWBr  5, 4, 15       ; if seed<thrG -> BOOT_G
//	 9  ConstI64K     3, 0, 3        ; prev = 3 (T fall-through)
//	10  Jump                16
//	11  ConstI64K     3, 0, 0        ; BOOT_A
//	12  Jump                16
//	13  ConstI64K     3, 0, 1        ; BOOT_C
//	14  Jump                16
//	15  ConstI64K     3, 0, 2        ; BOOT_G
//	16  MapGetI64I64  2, 0, 3        ; v(r2) = m[prev(r3)]
//	17  AddI64K       2, 2, 1
//	18  MapSetI64I64  0, 3, 2        ; m[prev] = v
//	19  ConstI64K     4, 0, 1        ; i = 1
//	    loop_top (PC=20):
//	20  CmpGeI64Br    4, 6, 46       ; if i >= n -> loop_end
//	21  MulI64K       5, 5, 3877
//	22  AddI64K       5, 5, 29573
//	23  ModI64KW      5, 5, 0        ; seed %= Consts[0]
//	24  CmpLtI64KWBr  5, 2, 29       ; ITER_A
//	25  CmpLtI64KWBr  5, 3, 31       ; ITER_C
//	26  CmpLtI64KWBr  5, 4, 33       ; ITER_G
//	27  ConstI64K     0, 0, 3        ; code(r0) = 3 (T)
//	28  Jump                34
//	29  ConstI64K     0, 0, 0        ; ITER_A
//	30  Jump                34
//	31  ConstI64K     0, 0, 1        ; ITER_C
//	32  Jump                34
//	33  ConstI64K     0, 0, 2        ; ITER_G
//	34  MapGetI64I64  2, 0, 0        ; v = m[code]
//	35  AddI64K       2, 2, 1
//	36  MapSetI64I64  0, 0, 2        ; m[code] = v
//	37  MulI64K       1, 3, 4        ; key2(r1) = prev*4
//	38  AddI64        1, 1, 0        ; key2 += code
//	39  AddI64K       1, 1, 4        ; key2 += 4
//	40  MapGetI64I64  2, 0, 1        ; v = m[key2]
//	41  AddI64K       2, 2, 1
//	42  MapSetI64I64  0, 1, 2        ; m[key2] = v
//	43  MovI64        3, 0, 0        ; prev = code
//	44  AddI64K       4, 4, 1        ; i++
//	45  Jump                20
//	    loop_end (PC=46):
//	46  ConstI64K     0, 0, 0        ; h(r0) = 0
//	47  ConstI64K     3, 0, 0        ; k(r3) = 0
//	    summ_top (PC=48):
//	48  CmpGeI64KBr   3, 20, 55      ; if k >= 20 -> summ_end
//	49  MapGetI64I64  2, 0, 3        ; v = m[k]
//	50  MulI64K       0, 0, 1009     ; h *= 1009
//	51  AddI64        0, 0, 2        ; h += v
//	52  ModI64KW      0, 0, 1        ; h %= Consts[1] (HASH_MOD)
//	53  AddI64K       3, 3, 1        ; k++
//	54  Jump                48
//	    summ_end (PC=55):
//	55  ReturnI64     0, 0, 0
var KNucleotide = &Program{
	Name: "k_nucleotide",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:        "k_nucleotide",
			NumRegsI64:  7,
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
				vm3.MakeOp(vm3.OpMovI64, 6, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 5, 0, 42),
				vm3.MakeOp(vm3.OpMulI64K, 5, 5, 3877),
				vm3.MakeOp(vm3.OpAddI64K, 5, 5, 29573),
				vm3.MakeOp(vm3.OpModI64KW, 5, 5, 0),
				vm3.MakeOp(vm3.OpCmpLtI64KWBr, 5, 2, 11),
				vm3.MakeOp(vm3.OpCmpLtI64KWBr, 5, 3, 13),
				vm3.MakeOp(vm3.OpCmpLtI64KWBr, 5, 4, 15),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 3),
				vm3.MakeOp(vm3.OpJump, 0, 0, 16),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				vm3.MakeOp(vm3.OpJump, 0, 0, 16),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 16),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 2),
				vm3.MakeOp(vm3.OpMapGetI64I64, 2, 0, 3),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 3, 2),
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 1),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 4, 6, 46),
				vm3.MakeOp(vm3.OpMulI64K, 5, 5, 3877),
				vm3.MakeOp(vm3.OpAddI64K, 5, 5, 29573),
				vm3.MakeOp(vm3.OpModI64KW, 5, 5, 0),
				vm3.MakeOp(vm3.OpCmpLtI64KWBr, 5, 2, 29),
				vm3.MakeOp(vm3.OpCmpLtI64KWBr, 5, 3, 31),
				vm3.MakeOp(vm3.OpCmpLtI64KWBr, 5, 4, 33),
				vm3.MakeOp(vm3.OpConstI64K, 0, 0, 3),
				vm3.MakeOp(vm3.OpJump, 0, 0, 34),
				vm3.MakeOp(vm3.OpConstI64K, 0, 0, 0),
				vm3.MakeOp(vm3.OpJump, 0, 0, 34),
				vm3.MakeOp(vm3.OpConstI64K, 0, 0, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 34),
				vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),
				vm3.MakeOp(vm3.OpMapGetI64I64, 2, 0, 0),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 2),
				vm3.MakeOp(vm3.OpMulI64K, 1, 3, 4),
				vm3.MakeOp(vm3.OpAddI64, 1, 1, 0),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 4),
				vm3.MakeOp(vm3.OpMapGetI64I64, 2, 0, 1),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 1, 2),
				vm3.MakeOp(vm3.OpMovI64, 3, 0, 0),
				vm3.MakeOp(vm3.OpAddI64K, 4, 4, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 20),
				vm3.MakeOp(vm3.OpConstI64K, 0, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 3, 20, 55),
				vm3.MakeOp(vm3.OpMapGetI64I64, 2, 0, 3),
				vm3.MakeOp(vm3.OpMulI64K, 0, 0, 1009),
				vm3.MakeOp(vm3.OpAddI64, 0, 0, 2),
				vm3.MakeOp(vm3.OpModI64KW, 0, 0, 1),
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 48),
				vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
