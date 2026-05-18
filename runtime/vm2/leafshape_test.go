package vm2

import "testing"

// TestAnalyzeLeafShape_BinaryTreesShape verifies the analyzer recognises
// the two MEP-39 §6.10 leaf shapes: OpReturnI64K (used by check_tree)
// and OpReturnNewPairKK (used by make_tree).
func TestAnalyzeLeafShape_BinaryTreesShape(t *testing.T) {
	cases := []struct {
		name string
		code []Instr
		want Function
	}{
		{
			name: "make_tree leaf",
			code: []Instr{
				{Op: OpJumpIfNotEqualI64K, A: 0, B: 0, C: 2},
				{Op: OpReturnNewPairKK, A: 0, B: 0, C: 0},
				{Op: OpSubI64K, A: 3, B: 0, C: 1},
				{Op: OpReturn, A: 0},
			},
			want: Function{
				LeafKind:     LeafKindReturnNewPairKK,
				LeafGuardReg: 0,
				LeafGuardK:   0,
				LeafReturnB:  0,
				LeafReturnC:  0,
			},
		},
		{
			name: "check_tree leaf",
			code: []Instr{
				{Op: OpJumpIfNotEqualI64K, A: 1, B: 0, C: 2},
				{Op: OpReturnI64K, A: 1},
				{Op: OpSubI64K, A: 5, B: 1, C: 1},
				{Op: OpReturn, A: 1},
			},
			want: Function{
				LeafKind:     LeafKindReturnI64K,
				LeafGuardReg: 1,
				LeafGuardK:   0,
				LeafReturnA:  1,
			},
		},
		{
			name: "non-leaf (jump first not equal-K)",
			code: []Instr{
				{Op: OpJumpIfLessI64K, A: 0, B: 5, C: 2},
				{Op: OpReturnI64K, A: 1},
				{Op: OpReturn, A: 0},
			},
			want: Function{LeafKind: LeafKindNone},
		},
		{
			name: "non-leaf (return is not constant op)",
			code: []Instr{
				{Op: OpJumpIfNotEqualI64K, A: 0, B: 0, C: 2},
				{Op: OpReturn, A: 0},
				{Op: OpReturn, A: 0},
			},
			want: Function{LeafKind: LeafKindNone},
		},
		{
			name: "non-leaf (jump target is not pc=2)",
			code: []Instr{
				{Op: OpJumpIfNotEqualI64K, A: 0, B: 0, C: 5},
				{Op: OpReturnI64K, A: 1},
				{Op: OpReturn, A: 0},
			},
			want: Function{LeafKind: LeafKindNone},
		},
		{
			name: "non-leaf (guard reg out of params)",
			code: []Instr{
				{Op: OpJumpIfNotEqualI64K, A: 3, B: 0, C: 2},
				{Op: OpReturnI64K, A: 1},
				{Op: OpReturn, A: 0},
			},
			want: Function{LeafKind: LeafKindNone},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			fn := &Function{NumParams: 2, NumRegs: 8, Code: tc.code}
			AnalyzeLeafShape(fn)
			if fn.LeafKind != tc.want.LeafKind {
				t.Fatalf("LeafKind = %d, want %d", fn.LeafKind, tc.want.LeafKind)
			}
			if fn.LeafKind == LeafKindNone {
				return
			}
			if fn.LeafGuardReg != tc.want.LeafGuardReg {
				t.Errorf("LeafGuardReg = %d, want %d", fn.LeafGuardReg, tc.want.LeafGuardReg)
			}
			if fn.LeafGuardK != tc.want.LeafGuardK {
				t.Errorf("LeafGuardK = %d, want %d", fn.LeafGuardK, tc.want.LeafGuardK)
			}
			if tc.want.LeafKind == LeafKindReturnI64K && fn.LeafReturnA != tc.want.LeafReturnA {
				t.Errorf("LeafReturnA = %d, want %d", fn.LeafReturnA, tc.want.LeafReturnA)
			}
			if tc.want.LeafKind == LeafKindReturnNewPairKK {
				if fn.LeafReturnB != tc.want.LeafReturnB {
					t.Errorf("LeafReturnB = %d, want %d", fn.LeafReturnB, tc.want.LeafReturnB)
				}
				if fn.LeafReturnC != tc.want.LeafReturnC {
					t.Errorf("LeafReturnC = %d, want %d", fn.LeafReturnC, tc.want.LeafReturnC)
				}
			}
		})
	}
}
