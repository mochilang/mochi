package aotir

import "testing"

// TestVerifyPrimitives covers the Phase 2.0 surface: literal
// expressions, typed binary/unary operators, and the new
// print builtins.
func TestVerifyPrimitives(t *testing.T) {
	cases := []struct {
		name    string
		program *Program
		wantErr bool
	}{
		{
			name: "print_int_literal",
			program: &Program{
				Functions: []*Function{{
					Name:       "main",
					ReturnType: TypeUnit,
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_i64", Args: []Expr{&IntLit{Value: 42}}},
					}},
				}},
			},
		},
		{
			name: "print_float_literal",
			program: &Program{
				Functions: []*Function{{
					Name:       "main",
					ReturnType: TypeUnit,
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_f64", Args: []Expr{&FloatLit{Value: 1.5}}},
					}},
				}},
			},
		},
		{
			name: "print_bool_literal",
			program: &Program{
				Functions: []*Function{{
					Name:       "main",
					ReturnType: TypeUnit,
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_bool", Args: []Expr{&BoolLit{Value: true}}},
					}},
				}},
			},
		},
		{
			name: "int_add",
			program: &Program{
				Functions: []*Function{{
					Name:       "main",
					ReturnType: TypeUnit,
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_i64", Args: []Expr{
							&BinaryExpr{
								Op:     BinAddI64,
								Left:   &IntLit{Value: 1},
								Right:  &IntLit{Value: 2},
								Result: TypeInt,
							},
						}},
					}},
				}},
			},
		},
		{
			name: "bool_short_circuit",
			program: &Program{
				Functions: []*Function{{
					Name:       "main",
					ReturnType: TypeUnit,
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_bool", Args: []Expr{
							&BinaryExpr{
								Op:     BinAndBool,
								Left:   &BoolLit{Value: true},
								Right:  &BoolLit{Value: false},
								Result: TypeBool,
							},
						}},
					}},
				}},
			},
		},
		{
			name: "unary_negate_int",
			program: &Program{
				Functions: []*Function{{
					Name:       "main",
					ReturnType: TypeUnit,
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_i64", Args: []Expr{
							&UnaryExpr{
								Op:      UnNegI64,
								Operand: &IntLit{Value: 7},
								Result:  TypeInt,
							},
						}},
					}},
				}},
			},
		},
		{
			name: "type_mismatch_int_op_float",
			program: &Program{
				Functions: []*Function{{
					Name:       "main",
					ReturnType: TypeUnit,
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_i64", Args: []Expr{
							&BinaryExpr{
								Op:     BinAddI64,
								Left:   &IntLit{Value: 1},
								Right:  &FloatLit{Value: 2.0},
								Result: TypeInt,
							},
						}},
					}},
				}},
			},
			wantErr: true,
		},
		{
			name: "wrong_result_type",
			program: &Program{
				Functions: []*Function{{
					Name:       "main",
					ReturnType: TypeUnit,
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_bool", Args: []Expr{
							&BinaryExpr{
								Op:     BinEqI64,
								Left:   &IntLit{Value: 1},
								Right:  &IntLit{Value: 1},
								Result: TypeInt,
							},
						}},
					}},
				}},
			},
			wantErr: true,
		},
		{
			name: "wrong_builtin_arg_type",
			program: &Program{
				Functions: []*Function{{
					Name:       "main",
					ReturnType: TypeUnit,
					Body: &Block{Statements: []Stmt{
						&CallStmt{Func: "mochi_print_i64", Args: []Expr{&BoolLit{Value: true}}},
					}},
				}},
			},
			wantErr: true,
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

// TestTypeStringRoundTrip pins the textual identifiers used by the
// emit-time mangler. Changing these names without bumping the
// transpiler version would silently invalidate every cache entry
// the moment a new build runs.
func TestTypeStringRoundTrip(t *testing.T) {
	cases := []struct {
		t    Type
		want string
	}{
		{TypeUnit, "unit"},
		{TypeString, "string"},
		{TypeInt, "int"},
		{TypeFloat, "float"},
		{TypeBool, "bool"},
	}
	for _, c := range cases {
		if got := c.t.String(); got != c.want {
			t.Errorf("Type(%d).String() = %q, want %q", c.t, got, c.want)
		}
	}
}
