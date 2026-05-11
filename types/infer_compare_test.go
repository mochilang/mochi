package types_test

import (
	"mochi/parser"
	"mochi/types"
	"testing"
)

// inferRHS parses `let x = <expr>` and returns the inferred type of
// the right-hand side under a fresh env.
func inferRHS(t *testing.T, expr string) types.Type {
	t.Helper()
	src := "let x = " + expr + "\n"
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse %q: %v", src, err)
	}
	if len(prog.Statements) == 0 || prog.Statements[0].Let == nil {
		t.Fatalf("expected a let stmt in %q", src)
	}
	return types.ExprType(prog.Statements[0].Let.Value, types.NewEnv(nil))
}

// TestInferComparisonHonest pins MEP-5 P16: comparisons and equality are
// always bool-typed at the inference layer (the operator's principal
// result kind is `bool`); cross-kind operand mismatches are reported by
// the checker as T030, not by widening the inferrer's result to `any`.
// This supersedes the previous MEP-4 P9 fallback that returned `any` so
// downstream tools would not consume a wrong type: under MEP-5 the
// inferrer never returns `any` as a fallback at all.
func TestInferComparisonHonest(t *testing.T) {
	cases := []struct {
		name string
		expr string
		want string
	}{
		{"int_eq_int", `1 == 2`, "bool"},
		{"int_lt_int", `1 < 2`, "bool"},
		{"int_eq_string", `1 == "x"`, "bool"},
		{"struct_eq_int", `{a: 1} == 2`, "bool"},
		{"bool_eq_int", `true == 1`, "bool"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := inferRHS(t, tc.expr)
			if got == nil || got.String() != tc.want {
				t.Fatalf("ExprType(%q) = %v, want %v", tc.expr, got, tc.want)
			}
		})
	}
}
