package emit

import (
	"strings"
	"testing"

	"mochi/transpiler3/c/aotir"
)

// TestEmitDispatch covers Phase 2.0 expression rendering.
// The test inspects substrings (not whole-source equality) so a
// later cosmetic change to the prologue does not require a
// snapshot update.
func TestEmitDispatch(t *testing.T) {
	cases := []struct {
		name string
		expr aotir.Expr
		want string
	}{
		{
			name: "int_literal_positive",
			expr: &aotir.IntLit{Value: 42},
			want: "mochi_print_i64(INT64_C(42));",
		},
		{
			name: "int_literal_negative",
			expr: &aotir.IntLit{Value: -17},
			want: "mochi_print_i64(-INT64_C(17));",
		},
		{
			name: "int_literal_min",
			expr: &aotir.IntLit{Value: -1 << 63},
			want: "(-INT64_C(9223372036854775807) - INT64_C(1))",
		},
		{
			name: "bool_true",
			expr: &aotir.BoolLit{Value: true},
			want: "mochi_print_bool(1);",
		},
		{
			name: "float_with_trailing_zero",
			expr: &aotir.FloatLit{Value: 1.0},
			want: "(double)(1.0)",
		},
		{
			name: "binary_add_int",
			expr: &aotir.BinaryExpr{
				Op:     aotir.BinAddI64,
				Left:   &aotir.IntLit{Value: 1},
				Right:  &aotir.IntLit{Value: 2},
				Result: aotir.TypeInt,
			},
			want: "(INT64_C(1) + INT64_C(2))",
		},
		{
			name: "binary_and_bool",
			expr: &aotir.BinaryExpr{
				Op:     aotir.BinAndBool,
				Left:   &aotir.BoolLit{Value: true},
				Right:  &aotir.BoolLit{Value: false},
				Result: aotir.TypeBool,
			},
			want: "(1 && 0)",
		},
		{
			name: "unary_not_bool",
			expr: &aotir.UnaryExpr{
				Op:      aotir.UnNotBool,
				Operand: &aotir.BoolLit{Value: false},
				Result:  aotir.TypeBool,
			},
			want: "(!0)",
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			callee := printCalleeFor(c.expr.Type())
			prog := &aotir.Program{
				Functions: []*aotir.Function{{
					Name:       "main",
					ReturnType: aotir.TypeUnit,
					Body: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.CallStmt{Func: callee, Args: []aotir.Expr{c.expr}},
					}},
				}},
			}
			src, err := Emit(prog)
			if err != nil {
				t.Fatalf("Emit: %v", err)
			}
			if !strings.Contains(src, c.want) {
				t.Fatalf("emitted source missing %q:\n%s", c.want, src)
			}
		})
	}
}

func printCalleeFor(t aotir.Type) string {
	switch t {
	case aotir.TypeString:
		return "mochi_print_str"
	case aotir.TypeInt:
		return "mochi_print_i64"
	case aotir.TypeFloat:
		return "mochi_print_f64"
	case aotir.TypeBool:
		return "mochi_print_bool"
	}
	return "?"
}
