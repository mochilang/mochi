package types

import (
	"mochi/parser"
	"testing"
)

// Pin the MEP 4 P6 fix to inferBinaryType for `list + list`.
// Before P6 the inference path used equalTypes, which is strict, so
// `[1, 2] + []` (right-hand elem AnyType from an empty-list literal)
// would be silently widened to `[any]` even though applyBinaryType
// accepted it as `[int]`. Heterogeneous concatenations like
// `[1] + ["x"]` would also be inferred to `[any]` rather than the
// honest AnyType that downstream tools can recognise.
//
// The new contract:
//   - `[T] + []` infers to `[T]` (the more concrete elem wins).
//   - `[T] + [U]` with T != U infers to `AnyType{}`.
//   - applyBinaryType remains the authoritative error path.
func TestInferListConcatElemTypes(t *testing.T) {
	cases := []struct {
		name   string
		src    string
		expect Type
	}{
		{
			name:   "empty list keeps concrete elem",
			src:    "[1, 2] + []",
			expect: ListType{Elem: IntType{}},
		},
		{
			name:   "empty list on left also keeps concrete elem",
			src:    "[] + [1, 2]",
			expect: ListType{Elem: IntType{}},
		},
		{
			name:   "matching elem types preserved",
			src:    "[1] + [2]",
			expect: ListType{Elem: IntType{}},
		},
		{
			name:   "heterogeneous elem types infer to AnyType",
			src:    "[1] + [\"x\"]",
			expect: AnyType{},
		},
	}

	env := NewEnv(nil)
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			prog, err := parser.ParseString("let _ = " + tc.src)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			got := ExprType(prog.Statements[0].Let.Value, env)
			if !equalTypes(got, tc.expect) {
				t.Fatalf("ExprType(%s) = %v, want %v", tc.src, got, tc.expect)
			}
		})
	}
}
