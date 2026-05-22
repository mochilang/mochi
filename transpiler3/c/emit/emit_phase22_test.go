package emit

import (
	"strings"
	"testing"

	"mochi/transpiler3/c/aotir"
)

// TestEmitPhase22Functions exercises the file-scope emit shape for
// user-defined functions: forward declaration before main and full
// body afterwards, with the right parameter list and return type.
func TestEmitPhase22Functions(t *testing.T) {
	cases := []struct {
		name    string
		prog    *aotir.Program
		want    []string
		notWant []string
	}{
		{
			name: "no_args_returns_int",
			prog: &aotir.Program{
				Functions: []*aotir.Function{
					{
						Name:       "answer",
						ReturnType: aotir.TypeInt,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.ReturnStmt{Value: &aotir.IntLit{Value: 42}},
						}},
					},
					{
						Name:       "main",
						ReturnType: aotir.TypeUnit,
						Body:       &aotir.Block{},
					},
				},
				Main: 1,
			},
			want: []string{
				"static int64_t answer(void);",
				"static int64_t answer(void) {",
				"    return INT64_C(42);",
				"int main(void) {",
				"    return 0;",
			},
		},
		{
			name: "two_args_returns_int",
			prog: &aotir.Program{
				Functions: []*aotir.Function{
					{
						Name:       "add",
						Params:     []aotir.Param{{Name: "a", Type: aotir.TypeInt}, {Name: "b", Type: aotir.TypeInt}},
						ReturnType: aotir.TypeInt,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.ReturnStmt{Value: &aotir.BinaryExpr{
								Op:     aotir.BinAddI64,
								Left:   &aotir.VarRef{Name: "a", VarType: aotir.TypeInt},
								Right:  &aotir.VarRef{Name: "b", VarType: aotir.TypeInt},
								Result: aotir.TypeInt,
							}},
						}},
					},
					{Name: "main", ReturnType: aotir.TypeUnit, Body: &aotir.Block{}},
				},
				Main: 1,
			},
			want: []string{
				"static int64_t add(int64_t a, int64_t b);",
				"static int64_t add(int64_t a, int64_t b) {",
				"    return (a + b);",
			},
		},
		{
			name: "callexpr_in_print",
			prog: &aotir.Program{
				Functions: []*aotir.Function{
					{
						Name:       "id",
						Params:     []aotir.Param{{Name: "x", Type: aotir.TypeInt}},
						ReturnType: aotir.TypeInt,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.ReturnStmt{Value: &aotir.VarRef{Name: "x", VarType: aotir.TypeInt}},
						}},
					},
					{
						Name:       "main",
						ReturnType: aotir.TypeUnit,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.CallStmt{Func: "mochi_print_i64", Args: []aotir.Expr{
								&aotir.CallExpr{
									Func:   "id",
									Args:   []aotir.Expr{&aotir.IntLit{Value: 5}},
									Result: aotir.TypeInt,
								},
							}},
						}},
					},
				},
				Main: 1,
			},
			want: []string{
				"static int64_t id(int64_t x);",
				"    mochi_print_i64(id(INT64_C(5)));",
			},
		},
		{
			name: "callstmt_discards_result",
			prog: &aotir.Program{
				Functions: []*aotir.Function{
					{
						Name:       "noisy",
						Params:     []aotir.Param{{Name: "n", Type: aotir.TypeInt}},
						ReturnType: aotir.TypeInt,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.ReturnStmt{Value: &aotir.VarRef{Name: "n", VarType: aotir.TypeInt}},
						}},
					},
					{
						Name:       "main",
						ReturnType: aotir.TypeUnit,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.CallStmt{Func: "noisy", Args: []aotir.Expr{&aotir.IntLit{Value: 9}}},
						}},
					},
				},
				Main: 1,
			},
			want: []string{
				"    noisy(INT64_C(9));",
			},
		},
		{
			name: "for_range_simple",
			prog: &aotir.Program{
				Functions: []*aotir.Function{
					{
						Name:       "main",
						ReturnType: aotir.TypeUnit,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.ForRangeStmt{
								Var:   "i",
								Start: &aotir.IntLit{Value: 0},
								End:   &aotir.IntLit{Value: 3},
								Body: &aotir.Block{Statements: []aotir.Stmt{
									&aotir.CallStmt{Func: "mochi_print_i64", Args: []aotir.Expr{
										&aotir.VarRef{Name: "i", VarType: aotir.TypeInt},
									}},
								}},
							},
						}},
					},
				},
				Main: 0,
			},
			want: []string{
				"const int64_t __mochi_end_i = INT64_C(3);",
				"for (int64_t i = INT64_C(0); i < __mochi_end_i; i++) {",
				"mochi_print_i64(i);",
			},
		},
		{
			name: "return_with_value",
			prog: &aotir.Program{
				Functions: []*aotir.Function{
					{
						Name:       "k",
						ReturnType: aotir.TypeBool,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.ReturnStmt{Value: &aotir.BoolLit{Value: true}},
						}},
					},
					{Name: "main", ReturnType: aotir.TypeUnit, Body: &aotir.Block{}},
				},
				Main: 1,
			},
			want: []string{
				"static int k(void);",
				"    return 1;",
				// k() body ends with the user return; only main()
				// should ever get an auto-inserted `return 0;`.
				"    return 1;\n}\nint main(void) {",
			},
		},
		{
			name: "forward_decls_sorted_by_name",
			prog: &aotir.Program{
				Functions: []*aotir.Function{
					{
						Name:       "main",
						ReturnType: aotir.TypeUnit,
						Body:       &aotir.Block{},
					},
					{
						Name:       "zeta",
						ReturnType: aotir.TypeInt,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.ReturnStmt{Value: &aotir.IntLit{Value: 0}},
						}},
					},
					{
						Name:       "alpha",
						ReturnType: aotir.TypeInt,
						Body: &aotir.Block{Statements: []aotir.Stmt{
							&aotir.ReturnStmt{Value: &aotir.IntLit{Value: 0}},
						}},
					},
				},
				Main: 0,
			},
			want: []string{
				"static int64_t alpha(void);\nstatic int64_t zeta(void);",
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
