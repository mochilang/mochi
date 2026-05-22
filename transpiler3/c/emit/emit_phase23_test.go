package emit

import (
	"strings"
	"testing"

	"mochi/transpiler3/c/aotir"
)

// TestEmitPhase23Divzero pins the Phase 2.3 emit policy: integer
// `/` and `%` lower to the libmochi `mochi_div_i64` / `mochi_mod_i64`
// runtime helpers (which trap rhs == 0), not to raw C infix
// operators. Float division and the other arithmetic ops keep the
// infix form, and the prologue gains the `mochi/errors.h` include.
func TestEmitPhase23Divzero(t *testing.T) {
	// mkMain wraps an integer-typed expression in a print call so
	// each fixture corresponds to a self-contained program that
	// also exercises the prologue. The integer print builtin is
	// fine for div/mod cases (the result is int); for the float
	// case we swap to mochi_print_f64.
	mkMain := func(callee string, value aotir.Expr) *aotir.Program {
		return &aotir.Program{
			Functions: []*aotir.Function{
				{
					Name:       "main",
					ReturnType: aotir.TypeUnit,
					Body: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.CallStmt{Func: callee, Args: []aotir.Expr{value}},
					}},
				},
			},
			Main: 0,
		}
	}

	intLit := func(v int64) aotir.Expr { return &aotir.IntLit{Value: v} }
	floatLit := func(v float64) aotir.Expr { return &aotir.FloatLit{Value: v} }

	cases := []struct {
		name    string
		prog    *aotir.Program
		want    []string
		notWant []string
	}{
		{
			name: "div_int_uses_runtime_helper",
			prog: mkMain("mochi_print_i64", &aotir.BinaryExpr{
				Op:     aotir.BinDivI64,
				Left:   intLit(20),
				Right:  intLit(4),
				Result: aotir.TypeInt,
			}),
			want: []string{
				"#include \"mochi/errors.h\"",
				"mochi_div_i64(INT64_C(20), INT64_C(4))",
			},
			notWant: []string{
				"(INT64_C(20) / INT64_C(4))",
			},
		},
		{
			name: "mod_int_uses_runtime_helper",
			prog: mkMain("mochi_print_i64", &aotir.BinaryExpr{
				Op:     aotir.BinModI64,
				Left:   intLit(17),
				Right:  intLit(5),
				Result: aotir.TypeInt,
			}),
			want: []string{
				"#include \"mochi/errors.h\"",
				"mochi_mod_i64(INT64_C(17), INT64_C(5))",
			},
			notWant: []string{
				"(INT64_C(17) % INT64_C(5))",
			},
		},
		{
			name: "div_float_stays_infix",
			prog: mkMain("mochi_print_f64", &aotir.BinaryExpr{
				Op:     aotir.BinDivF64,
				Left:   floatLit(1.0),
				Right:  floatLit(2.0),
				Result: aotir.TypeFloat,
			}),
			want: []string{
				"((double)(1.0) / (double)(2.0))",
			},
			notWant: []string{
				"mochi_div_i64",
				"mochi_mod_i64",
			},
		},
		{
			name: "add_int_stays_infix",
			prog: mkMain("mochi_print_i64", &aotir.BinaryExpr{
				Op:     aotir.BinAddI64,
				Left:   intLit(1),
				Right:  intLit(2),
				Result: aotir.TypeInt,
			}),
			want: []string{
				"(INT64_C(1) + INT64_C(2))",
			},
			notWant: []string{
				"mochi_div_i64",
				"mochi_mod_i64",
			},
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			got, err := Emit(c.prog)
			if err != nil {
				t.Fatalf("Emit error: %v", err)
			}
			for _, w := range c.want {
				if !strings.Contains(got, w) {
					t.Errorf("missing %q in emit output:\n%s", w, got)
				}
			}
			for _, w := range c.notWant {
				if strings.Contains(got, w) {
					t.Errorf("unexpected %q in emit output:\n%s", w, got)
				}
			}
		})
	}
}
