package emit

import (
	"strings"
	"testing"

	"mochi/transpiler3/c/aotir"
)

// TestEmitPhase21Stmts covers each new Phase 2.1 statement shape:
// let / var / assign / if / while / break / continue / return.
// Substring assertions keep these resilient to prologue changes.
func TestEmitPhase21Stmts(t *testing.T) {
	cases := []struct {
		name string
		body []aotir.Stmt
		want []string
	}{
		{
			name: "let_int_immutable",
			body: []aotir.Stmt{
				&aotir.LetStmt{
					Name: "x", VarType: aotir.TypeInt,
					Init: &aotir.IntLit{Value: 7},
				},
			},
			want: []string{"const int64_t x = INT64_C(7);"},
		},
		{
			name: "var_int_mutable",
			body: []aotir.Stmt{
				&aotir.LetStmt{
					Name: "x", VarType: aotir.TypeInt, Mutable: true,
					Init: &aotir.IntLit{Value: 1},
				},
			},
			want: []string{"int64_t x = INT64_C(1);", "    int64_t"},
		},
		{
			name: "let_float",
			body: []aotir.Stmt{
				&aotir.LetStmt{
					Name: "f", VarType: aotir.TypeFloat,
					Init: &aotir.FloatLit{Value: 0.5},
				},
			},
			want: []string{"const double f = (double)(0.5);"},
		},
		{
			name: "let_bool_uses_int",
			body: []aotir.Stmt{
				&aotir.LetStmt{
					Name: "b", VarType: aotir.TypeBool,
					Init: &aotir.BoolLit{Value: true},
				},
			},
			want: []string{"const int b = 1;"},
		},
		{
			name: "assign_var",
			body: []aotir.Stmt{
				&aotir.LetStmt{
					Name: "x", VarType: aotir.TypeInt, Mutable: true,
					Init: &aotir.IntLit{Value: 1},
				},
				&aotir.AssignStmt{
					Name: "x",
					Value: &aotir.BinaryExpr{
						Op:     aotir.BinAddI64,
						Left:   &aotir.VarRef{Name: "x", VarType: aotir.TypeInt},
						Right:  &aotir.IntLit{Value: 2},
						Result: aotir.TypeInt,
					},
				},
			},
			want: []string{"x = (x + INT64_C(2));"},
		},
		{
			name: "if_then_only",
			body: []aotir.Stmt{
				&aotir.IfStmt{
					Cond: &aotir.BoolLit{Value: true},
					Then: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.CallStmt{Func: "mochi_print_i64", Args: []aotir.Expr{&aotir.IntLit{Value: 1}}},
					}},
				},
			},
			want: []string{"if (1) {", "        mochi_print_i64(INT64_C(1));", "    }"},
		},
		{
			name: "if_then_else",
			body: []aotir.Stmt{
				&aotir.IfStmt{
					Cond: &aotir.BoolLit{Value: false},
					Then: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.CallStmt{Func: "mochi_print_i64", Args: []aotir.Expr{&aotir.IntLit{Value: 1}}},
					}},
					Else: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.CallStmt{Func: "mochi_print_i64", Args: []aotir.Expr{&aotir.IntLit{Value: 2}}},
					}},
				},
			},
			want: []string{"if (0) {", "} else {", "    }"},
		},
		{
			name: "while_break_continue",
			body: []aotir.Stmt{
				&aotir.WhileStmt{
					Cond: &aotir.BoolLit{Value: true},
					Body: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.BreakStmt{},
						&aotir.ContinueStmt{},
					}},
				},
			},
			want: []string{"while (1) {", "        break;", "        continue;"},
		},
		{
			name: "early_return",
			body: []aotir.Stmt{
				&aotir.ReturnStmt{},
			},
			want: []string{"    return 0;"},
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			prog := &aotir.Program{
				Functions: []*aotir.Function{{
					Name:       "main",
					ReturnType: aotir.TypeUnit,
					Body:       &aotir.Block{Statements: c.body},
				}},
			}
			src, err := Emit(prog)
			if err != nil {
				t.Fatalf("Emit: %v", err)
			}
			for _, w := range c.want {
				if !strings.Contains(src, w) {
					t.Fatalf("emitted source missing %q:\n%s", w, src)
				}
			}
		})
	}
}
