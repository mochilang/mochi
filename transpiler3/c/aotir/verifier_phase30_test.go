package aotir

import "testing"

// TestVerifyPhase30 covers the Phase 3.0 surface: record-type
// declarations (RecordDecl), record literals (RecordLit) with full
// field coverage in declaration order, field access (FieldAccess),
// and structural equality (BinEqRec / BinNeRec). Positive cases pin
// the happy path; negative cases pin the invariants the lowerer and
// the emitter rely on.
func TestVerifyPhase30(t *testing.T) {
	mkProg := func(recs []*RecordDecl, fns ...*Function) *Program {
		var main int
		for i, fn := range fns {
			if fn.Name == "main" {
				main = i
			}
		}
		return &Program{Records: recs, Functions: fns, Main: main}
	}
	mkMain := func(stmts ...Stmt) *Function {
		return &Function{
			Name:       "main",
			ReturnType: TypeUnit,
			Body:       &Block{Statements: stmts},
		}
	}
	pt := &RecordDecl{
		Name: "Pt",
		Fields: []RecordField{
			{Name: "x", Type: TypeInt},
			{Name: "y", Type: TypeInt},
		},
	}

	cases := []struct {
		name    string
		program *Program
		wantErr bool
	}{
		// Positive ---------------------------------------------------
		{
			name: "record_literal_in_decl_order",
			program: mkProg([]*RecordDecl{pt}, mkMain(
				&LetStmt{
					Name:       "p",
					VarType:    TypeRecord,
					RecordName: "Pt",
					Init: &RecordLit{
						TypeName: "Pt",
						Fields: []RecordLitArg{
							{Name: "x", Value: &IntLit{Value: 1}},
							{Name: "y", Value: &IntLit{Value: 2}},
						},
					},
				},
				&CallStmt{Func: "mochi_print_i64", Args: []Expr{
					&FieldAccess{
						Receiver: &VarRef{Name: "p", VarType: TypeRecord, RecordName: "Pt"},
						RecordName: "Pt", FieldName: "x", Result: TypeInt,
					},
				}},
			)),
		},
		{
			name: "record_equality_same_type",
			program: mkProg([]*RecordDecl{pt}, mkMain(
				&LetStmt{Name: "a", VarType: TypeRecord, RecordName: "Pt",
					Init: &RecordLit{TypeName: "Pt", Fields: []RecordLitArg{
						{Name: "x", Value: &IntLit{Value: 1}},
						{Name: "y", Value: &IntLit{Value: 2}},
					}},
				},
				&LetStmt{Name: "b", VarType: TypeRecord, RecordName: "Pt",
					Init: &RecordLit{TypeName: "Pt", Fields: []RecordLitArg{
						{Name: "x", Value: &IntLit{Value: 1}},
						{Name: "y", Value: &IntLit{Value: 2}},
					}},
				},
				&CallStmt{Func: "mochi_print_bool", Args: []Expr{
					&BinaryExpr{Op: BinEqRec,
						Left:   &VarRef{Name: "a", VarType: TypeRecord, RecordName: "Pt"},
						Right:  &VarRef{Name: "b", VarType: TypeRecord, RecordName: "Pt"},
						Result: TypeBool},
				}},
			)),
		},
		{
			name: "record_param_and_return",
			program: mkProg([]*RecordDecl{pt},
				&Function{
					Name:             "mk",
					ReturnType:       TypeRecord,
					ReturnRecordName: "Pt",
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &RecordLit{TypeName: "Pt", Fields: []RecordLitArg{
							{Name: "x", Value: &IntLit{Value: 1}},
							{Name: "y", Value: &IntLit{Value: 2}},
						}}},
					}},
				},
				&Function{
					Name:       "sumxy",
					Params:     []Param{{Name: "p", Type: TypeRecord, RecordName: "Pt"}},
					ReturnType: TypeInt,
					Body: &Block{Statements: []Stmt{
						&ReturnStmt{Value: &BinaryExpr{Op: BinAddI64,
							Left: &FieldAccess{
								Receiver:   &VarRef{Name: "p", VarType: TypeRecord, RecordName: "Pt"},
								RecordName: "Pt", FieldName: "x", Result: TypeInt,
							},
							Right: &FieldAccess{
								Receiver:   &VarRef{Name: "p", VarType: TypeRecord, RecordName: "Pt"},
								RecordName: "Pt", FieldName: "y", Result: TypeInt,
							},
							Result: TypeInt,
						}},
					}},
				},
				mkMain(),
			),
		},

		// Negative ---------------------------------------------------
		{
			name: "duplicate_record_name",
			program: mkProg([]*RecordDecl{pt, pt}, mkMain()),
			wantErr: true,
		},
		{
			name: "duplicate_field_in_record",
			program: mkProg([]*RecordDecl{{
				Name: "Bad",
				Fields: []RecordField{
					{Name: "x", Type: TypeInt},
					{Name: "x", Type: TypeInt},
				},
			}}, mkMain()),
			wantErr: true,
		},
		{
			name: "record_literal_unknown_record",
			program: mkProg([]*RecordDecl{pt}, mkMain(
				&LetStmt{Name: "p", VarType: TypeRecord, RecordName: "Ghost",
					Init: &RecordLit{TypeName: "Ghost"},
				},
			)),
			wantErr: true,
		},
		{
			name: "record_literal_missing_field",
			program: mkProg([]*RecordDecl{pt}, mkMain(
				&LetStmt{Name: "p", VarType: TypeRecord, RecordName: "Pt",
					Init: &RecordLit{TypeName: "Pt", Fields: []RecordLitArg{
						{Name: "x", Value: &IntLit{Value: 1}},
					}},
				},
			)),
			wantErr: true,
		},
		{
			name: "record_literal_field_out_of_order",
			program: mkProg([]*RecordDecl{pt}, mkMain(
				&LetStmt{Name: "p", VarType: TypeRecord, RecordName: "Pt",
					Init: &RecordLit{TypeName: "Pt", Fields: []RecordLitArg{
						{Name: "y", Value: &IntLit{Value: 2}},
						{Name: "x", Value: &IntLit{Value: 1}},
					}},
				},
			)),
			wantErr: true,
		},
		{
			name: "record_literal_unknown_field",
			program: mkProg([]*RecordDecl{pt}, mkMain(
				&LetStmt{Name: "p", VarType: TypeRecord, RecordName: "Pt",
					Init: &RecordLit{TypeName: "Pt", Fields: []RecordLitArg{
						{Name: "x", Value: &IntLit{Value: 1}},
						{Name: "z", Value: &IntLit{Value: 2}},
					}},
				},
			)),
			wantErr: true,
		},
		{
			name: "record_literal_wrong_field_type",
			program: mkProg([]*RecordDecl{pt}, mkMain(
				&LetStmt{Name: "p", VarType: TypeRecord, RecordName: "Pt",
					Init: &RecordLit{TypeName: "Pt", Fields: []RecordLitArg{
						{Name: "x", Value: &FloatLit{Value: 1.0}},
						{Name: "y", Value: &IntLit{Value: 2}},
					}},
				},
			)),
			wantErr: true,
		},
		{
			name: "field_access_unknown_field",
			program: mkProg([]*RecordDecl{pt}, mkMain(
				&LetStmt{Name: "p", VarType: TypeRecord, RecordName: "Pt",
					Init: &RecordLit{TypeName: "Pt", Fields: []RecordLitArg{
						{Name: "x", Value: &IntLit{Value: 1}},
						{Name: "y", Value: &IntLit{Value: 2}},
					}},
				},
				&CallStmt{Func: "mochi_print_i64", Args: []Expr{
					&FieldAccess{
						Receiver:   &VarRef{Name: "p", VarType: TypeRecord, RecordName: "Pt"},
						RecordName: "Pt", FieldName: "z", Result: TypeInt,
					},
				}},
			)),
			wantErr: true,
		},
		{
			name: "record_eq_cross_type",
			program: mkProg([]*RecordDecl{
				pt,
				{Name: "Sz", Fields: []RecordField{{Name: "w", Type: TypeInt}}},
			}, mkMain(
				&LetStmt{Name: "a", VarType: TypeRecord, RecordName: "Pt",
					Init: &RecordLit{TypeName: "Pt", Fields: []RecordLitArg{
						{Name: "x", Value: &IntLit{Value: 1}},
						{Name: "y", Value: &IntLit{Value: 2}},
					}},
				},
				&LetStmt{Name: "b", VarType: TypeRecord, RecordName: "Sz",
					Init: &RecordLit{TypeName: "Sz", Fields: []RecordLitArg{
						{Name: "w", Value: &IntLit{Value: 1}},
					}},
				},
				&CallStmt{Func: "mochi_print_bool", Args: []Expr{
					&BinaryExpr{Op: BinEqRec,
						Left:   &VarRef{Name: "a", VarType: TypeRecord, RecordName: "Pt"},
						Right:  &VarRef{Name: "b", VarType: TypeRecord, RecordName: "Sz"},
						Result: TypeBool},
				}},
			)),
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
