package aotir

import "testing"

// TestVerifyPhase22 covers the new Phase 2.2 surface:
// user-function signatures (multi-arg, non-unit return), CallExpr
// (value-producing user-fn call), ForRangeStmt, and value-returning
// ReturnStmt. Positive cases pin the happy path; negative cases pin
// the invariants the lowerer and the emitter rely on.
func TestVerifyPhase22(t *testing.T) {
	// Helpers ---------------------------------------------------------

	mkProg := func(fns ...*Function) *Program {
		var main int
		for i, fn := range fns {
			if fn.Name == "main" {
				main = i
			}
		}
		return &Program{Functions: fns, Main: main}
	}

	mkMain := func(stmts ...Stmt) *Function {
		return &Function{
			Name:       "main",
			ReturnType: TypeUnit,
			Body:       &Block{Statements: stmts},
		}
	}

	cases := []struct {
		name    string
		program *Program
		wantErr bool
	}{
		// Positive ---------------------------------------------------
		{
			name: "fn_no_args_returns_int",
			program: mkProg(
				&Function{
					Name:       "answer",
					ReturnType: TypeInt,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &IntLit{Value: 42}},
					}},
				},
				mkMain(
					&CallStmt{Func: "mochi_print_i64", Args: []Expr{
						&CallExpr{Func: "answer", Result: TypeInt},
					}},
				),
			),
		},
		{
			name: "fn_two_args_returns_int",
			program: mkProg(
				&Function{
					Name:       "add",
					Params:     []Param{{Name: "a", Type: TypeInt}, {Name: "b", Type: TypeInt}},
					ReturnType: TypeInt,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &BinaryExpr{
							Op:     BinAddI64,
							Left:   &VarRef{Name: "a", VarType: TypeInt},
							Right:  &VarRef{Name: "b", VarType: TypeInt},
							Result: TypeInt,
						}},
					}},
				},
				mkMain(
					&CallStmt{Func: "mochi_print_i64", Args: []Expr{
						&CallExpr{
							Func:   "add",
							Args:   []Expr{&IntLit{Value: 3}, &IntLit{Value: 4}},
							Result: TypeInt,
						},
					}},
				),
			),
		},
		{
			name: "for_range_int",
			program: mkProg(mkMain(
				&ForRangeStmt{
					Var:   "i",
					Start: &IntLit{Value: 0},
					End:   &IntLit{Value: 3},
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_i64", Args: []Expr{
							&VarRef{Name: "i", VarType: TypeInt},
						}},
					}},
				},
			)),
		},
		{
			name: "for_range_with_break_continue",
			program: mkProg(mkMain(
				&ForRangeStmt{
					Var:   "i",
					Start: &IntLit{Value: 0},
					End:   &IntLit{Value: 5},
					Body: &Block{Statements: []Stmt{
						&BreakStmt{},
						&ContinueStmt{},
					}},
				},
			)),
		},
		{
			name: "discard_result_user_call",
			program: mkProg(
				&Function{
					Name:       "shout",
					Params:     []Param{{Name: "n", Type: TypeInt}},
					ReturnType: TypeInt,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &VarRef{Name: "n", VarType: TypeInt}},
					}},
				},
				mkMain(
					&CallStmt{
						Func: "shout",
						Args: []Expr{&IntLit{Value: 7}},
					},
				),
			),
		},
		{
			name: "mutual_recursion_signatures",
			program: mkProg(
				&Function{
					Name:       "even",
					Params:     []Param{{Name: "n", Type: TypeInt}},
					ReturnType: TypeBool,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &CallExpr{
							Func:   "odd",
							Args:   []Expr{&VarRef{Name: "n", VarType: TypeInt}},
							Result: TypeBool,
						}},
					}},
				},
				&Function{
					Name:       "odd",
					Params:     []Param{{Name: "n", Type: TypeInt}},
					ReturnType: TypeBool,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &CallExpr{
							Func:   "even",
							Args:   []Expr{&VarRef{Name: "n", VarType: TypeInt}},
							Result: TypeBool,
						}},
					}},
				},
				mkMain(),
			),
		},

		// Negative ---------------------------------------------------
		{
			name: "main_must_be_unit_returning",
			program: mkProg(&Function{
				Name:       "main",
				ReturnType: TypeInt,
				Body:       &Block{},
			}),
			wantErr: true,
		},
		{
			name: "main_must_have_no_params",
			program: mkProg(&Function{
				Name:       "main",
				Params:     []Param{{Name: "x", Type: TypeInt}},
				ReturnType: TypeUnit,
				Body:       &Block{},
			}),
			wantErr: true,
		},
		{
			name: "call_arity_mismatch",
			program: mkProg(
				&Function{
					Name:       "add",
					Params:     []Param{{Name: "a", Type: TypeInt}, {Name: "b", Type: TypeInt}},
					ReturnType: TypeInt,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &IntLit{Value: 0}},
					}},
				},
				mkMain(
					&CallStmt{Func: "mochi_print_i64", Args: []Expr{
						&CallExpr{
							Func:   "add",
							Args:   []Expr{&IntLit{Value: 1}},
							Result: TypeInt,
						},
					}},
				),
			),
			wantErr: true,
		},
		{
			name: "call_arg_type_mismatch",
			program: mkProg(
				&Function{
					Name:       "id",
					Params:     []Param{{Name: "x", Type: TypeInt}},
					ReturnType: TypeInt,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &VarRef{Name: "x", VarType: TypeInt}},
					}},
				},
				mkMain(
					&CallStmt{Func: "mochi_print_i64", Args: []Expr{
						&CallExpr{
							Func:   "id",
							Args:   []Expr{&FloatLit{Value: 1.0}},
							Result: TypeInt,
						},
					}},
				),
			),
			wantErr: true,
		},
		{
			name: "call_result_type_mismatch_with_decl",
			program: mkProg(
				&Function{
					Name:       "id",
					Params:     []Param{{Name: "x", Type: TypeInt}},
					ReturnType: TypeInt,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &VarRef{Name: "x", VarType: TypeInt}},
					}},
				},
				mkMain(
					&CallStmt{Func: "mochi_print_i64", Args: []Expr{
						&CallExpr{
							Func:   "id",
							Args:   []Expr{&IntLit{Value: 1}},
							Result: TypeFloat,
						},
					}},
				),
			),
			wantErr: true,
		},
		{
			name: "return_type_mismatch",
			program: mkProg(
				&Function{
					Name:       "id",
					Params:     []Param{{Name: "x", Type: TypeInt}},
					ReturnType: TypeInt,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &FloatLit{Value: 1.0}},
					}},
				},
				mkMain(),
			),
			wantErr: true,
		},
		{
			name: "return_missing_value",
			program: mkProg(
				&Function{
					Name:       "id",
					Params:     []Param{{Name: "x", Type: TypeInt}},
					ReturnType: TypeInt,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: nil},
					}},
				},
				mkMain(),
			),
			wantErr: true,
		},
		{
			name: "for_range_start_not_int",
			program: mkProg(mkMain(
				&ForRangeStmt{
					Var:   "i",
					Start: &FloatLit{Value: 0.0},
					End:   &IntLit{Value: 3},
					Body:  &Block{},
				},
			)),
			wantErr: true,
		},
		{
			name: "for_range_var_immutable",
			program: mkProg(mkMain(
				&ForRangeStmt{
					Var:   "i",
					Start: &IntLit{Value: 0},
					End:   &IntLit{Value: 3},
					Body: &Block{Statements: []Stmt{
						&AssignStmt{Name: "i", Value: &IntLit{Value: 9}},
					}},
				},
			)),
			wantErr: true,
		},
		{
			name: "for_range_var_scoped_to_body",
			program: mkProg(mkMain(
				&ForRangeStmt{
					Var:   "i",
					Start: &IntLit{Value: 0},
					End:   &IntLit{Value: 3},
					Body:  &Block{},
				},
				&CallStmt{Func: "mochi_print_i64", Args: []Expr{
					&VarRef{Name: "i", VarType: TypeInt},
				}},
			)),
			wantErr: true,
		},
		{
			name: "unresolved_callee",
			program: mkProg(mkMain(
				&CallStmt{Func: "mochi_print_i64", Args: []Expr{
					&CallExpr{Func: "bogus", Result: TypeInt},
				}},
			)),
			wantErr: true,
		},
		{
			name: "duplicate_function_name",
			program: &Program{
				Functions: []*Function{
					{
						Name:       "f",
						ReturnType: TypeInt,
						Body:       &Block{Statements: []Stmt{&ReturnStmt{Value: &IntLit{Value: 1}}}},
					},
					{
						Name:       "f",
						ReturnType: TypeInt,
						Body:       &Block{Statements: []Stmt{&ReturnStmt{Value: &IntLit{Value: 2}}}},
					},
					{Name: "main", ReturnType: TypeUnit, Body: &Block{}},
				},
				Main: 2,
			},
			wantErr: true,
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			err := Verify(c.program)
			if c.wantErr && err == nil {
				t.Fatalf("expected Verify to reject, got nil error")
			}
			if !c.wantErr && err != nil {
				t.Fatalf("Verify rejected valid program: %v", err)
			}
		})
	}
}
