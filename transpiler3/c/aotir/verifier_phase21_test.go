package aotir

import "testing"

// TestVerifyPhase21 covers the new Phase 2.1 statement kinds:
// LetStmt, AssignStmt, IfStmt, WhileStmt, BreakStmt, ContinueStmt,
// ReturnStmt, and VarRef. Positive cases check the happy path;
// negative cases pin the scoping / mutability / typing invariants
// the lower pass relies on.
func TestVerifyPhase21(t *testing.T) {
	mkMain := func(stmts ...Stmt) *Program {
		return &Program{
			Functions: []*Function{{
				Name:       "main",
				ReturnType: TypeUnit,
				Body:       &Block{Statements: stmts},
			}},
		}
	}

	cases := []struct {
		name    string
		program *Program
		wantErr bool
	}{
		{
			name: "let_then_use",
			program: mkMain(
				&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 1}},
				&CallStmt{Func: "mochi_print_i64", Args: []Expr{
					&VarRef{Name: "x", VarType: TypeInt},
				}},
			),
		},
		{
			name: "var_assign_then_use",
			program: mkMain(
				&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 1}, Mutable: true},
				&AssignStmt{Name: "x", Value: &IntLit{Value: 2}},
			),
		},
		{
			name: "if_then_only",
			program: mkMain(
				&IfStmt{
					Cond: &BoolLit{Value: true},
					Then: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_i64", Args: []Expr{&IntLit{Value: 1}}},
					}},
				},
			),
		},
		{
			name: "while_with_break_continue",
			program: mkMain(
				&WhileStmt{
					Cond: &BoolLit{Value: true},
					Body: &Block{Statements: []Stmt{
						&BreakStmt{},
						&ContinueStmt{},
					}},
				},
			),
		},
		{
			name: "bare_return_from_main",
			program: mkMain(
				&ReturnStmt{},
			),
		},
		{
			name: "let_type_mismatch",
			program: mkMain(
				&LetStmt{Name: "x", VarType: TypeInt, Init: &FloatLit{Value: 1.0}},
			),
			wantErr: true,
		},
		{
			name: "assign_to_undeclared",
			program: mkMain(
				&AssignStmt{Name: "x", Value: &IntLit{Value: 1}},
			),
			wantErr: true,
		},
		{
			name: "assign_to_immutable",
			program: mkMain(
				&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 1}},
				&AssignStmt{Name: "x", Value: &IntLit{Value: 2}},
			),
			wantErr: true,
		},
		{
			name: "assign_type_mismatch",
			program: mkMain(
				&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 1}, Mutable: true},
				&AssignStmt{Name: "x", Value: &FloatLit{Value: 1.0}},
			),
			wantErr: true,
		},
		{
			name: "if_cond_not_bool",
			program: mkMain(
				&IfStmt{Cond: &IntLit{Value: 1}, Then: &Block{}},
			),
			wantErr: true,
		},
		{
			name: "while_cond_not_bool",
			program: mkMain(
				&WhileStmt{Cond: &IntLit{Value: 0}, Body: &Block{}},
			),
			wantErr: true,
		},
		{
			name: "break_outside_loop",
			program: mkMain(
				&BreakStmt{},
			),
			wantErr: true,
		},
		{
			name: "continue_outside_loop",
			program: mkMain(
				&ContinueStmt{},
			),
			wantErr: true,
		},
		{
			name: "varref_unresolved",
			program: mkMain(
				&CallStmt{Func: "mochi_print_i64", Args: []Expr{
					&VarRef{Name: "x", VarType: TypeInt},
				}},
			),
			wantErr: true,
		},
		{
			name: "varref_wrong_type",
			program: mkMain(
				&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 1}},
				&CallStmt{Func: "mochi_print_bool", Args: []Expr{
					&VarRef{Name: "x", VarType: TypeBool},
				}},
			),
			wantErr: true,
		},
		{
			name: "varref_out_of_scope",
			program: mkMain(
				&IfStmt{
					Cond: &BoolLit{Value: true},
					Then: &Block{Statements: []Stmt{
						&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 1}},
					}},
				},
				// Reference outside the if-then body where x was declared.
				&CallStmt{Func: "mochi_print_i64", Args: []Expr{
					&VarRef{Name: "x", VarType: TypeInt},
				}},
			),
			wantErr: true,
		},
		{
			name: "redeclare_same_scope",
			program: mkMain(
				&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 1}},
				&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 2}},
			),
			wantErr: true,
		},
		{
			name: "shadow_in_nested_scope_ok",
			program: mkMain(
				&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 1}},
				&IfStmt{
					Cond: &BoolLit{Value: true},
					Then: &Block{Statements: []Stmt{
						&LetStmt{Name: "x", VarType: TypeInt, Init: &IntLit{Value: 2}},
					}},
				},
			),
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			err := Verify(c.program)
			if c.wantErr {
				if err == nil {
					t.Fatalf("expected Verify error, got nil")
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected Verify error: %v", err)
			}
		})
	}
}
