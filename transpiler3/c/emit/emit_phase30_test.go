package emit

import (
	"strings"
	"testing"

	"mochi/transpiler3/c/aotir"
)

// TestEmitPhase30Records pins the Phase 3.0 emit policy: every
// RecordDecl renders a `struct mochi_<Name>` definition plus a
// per-record `mochi_eq_<Name>` helper in the prologue, record
// literals render as C99 designated-init compound literals, field
// access renders as `(receiver).field`, and BinEqRec / BinNeRec
// lower to a helper call rather than the (illegal in C) `==` on
// structs. BinEqStr / BinNeStr lower to `strcmp(...) == 0` / `!= 0`.
func TestEmitPhase30Records(t *testing.T) {
	pt := &aotir.RecordDecl{
		Name: "Pt",
		Fields: []aotir.RecordField{
			{Name: "x", Type: aotir.TypeInt},
			{Name: "y", Type: aotir.TypeInt},
		},
	}
	mkProg := func(stmts ...aotir.Stmt) *aotir.Program {
		return &aotir.Program{
			Records: []*aotir.RecordDecl{pt},
			Functions: []*aotir.Function{{
				Name:       "main",
				ReturnType: aotir.TypeUnit,
				Body:       &aotir.Block{Statements: stmts},
			}},
			Main: 0,
		}
	}

	cases := []struct {
		name    string
		prog    *aotir.Program
		want    []string
		notWant []string
	}{
		{
			name: "struct_decl_in_prologue",
			prog: mkProg(
				&aotir.LetStmt{Name: "p", VarType: aotir.TypeRecord, RecordName: "Pt",
					Init: &aotir.RecordLit{TypeName: "Pt", Fields: []aotir.RecordLitArg{
						{Name: "x", Value: &aotir.IntLit{Value: 1}},
						{Name: "y", Value: &aotir.IntLit{Value: 2}},
					}},
				},
			),
			want: []string{
				"struct mochi_Pt {",
				"int64_t x;",
				"int64_t y;",
				"static int mochi_eq_Pt(struct mochi_Pt a, struct mochi_Pt b)",
				"return ((a).x == (b).x) && ((a).y == (b).y);",
			},
		},
		{
			name: "record_literal_designated_init",
			prog: mkProg(
				&aotir.LetStmt{Name: "p", VarType: aotir.TypeRecord, RecordName: "Pt",
					Init: &aotir.RecordLit{TypeName: "Pt", Fields: []aotir.RecordLitArg{
						{Name: "x", Value: &aotir.IntLit{Value: 11}},
						{Name: "y", Value: &aotir.IntLit{Value: 22}},
					}},
				},
			),
			want: []string{
				"const struct mochi_Pt p = (struct mochi_Pt){.x = INT64_C(11), .y = INT64_C(22)};",
			},
		},
		{
			name: "field_access_render",
			prog: mkProg(
				&aotir.LetStmt{Name: "p", VarType: aotir.TypeRecord, RecordName: "Pt",
					Init: &aotir.RecordLit{TypeName: "Pt", Fields: []aotir.RecordLitArg{
						{Name: "x", Value: &aotir.IntLit{Value: 1}},
						{Name: "y", Value: &aotir.IntLit{Value: 2}},
					}},
				},
				&aotir.CallStmt{Func: "mochi_print_i64", Args: []aotir.Expr{
					&aotir.FieldAccess{
						Receiver:   &aotir.VarRef{Name: "p", VarType: aotir.TypeRecord, RecordName: "Pt"},
						RecordName: "Pt", FieldName: "x", Result: aotir.TypeInt,
					},
				}},
			),
			want: []string{
				"mochi_print_i64((p).x);",
			},
		},
		{
			name: "record_eq_uses_helper_not_infix",
			prog: mkProg(
				&aotir.LetStmt{Name: "a", VarType: aotir.TypeRecord, RecordName: "Pt",
					Init: &aotir.RecordLit{TypeName: "Pt", Fields: []aotir.RecordLitArg{
						{Name: "x", Value: &aotir.IntLit{Value: 1}},
						{Name: "y", Value: &aotir.IntLit{Value: 2}},
					}},
				},
				&aotir.LetStmt{Name: "b", VarType: aotir.TypeRecord, RecordName: "Pt",
					Init: &aotir.RecordLit{TypeName: "Pt", Fields: []aotir.RecordLitArg{
						{Name: "x", Value: &aotir.IntLit{Value: 1}},
						{Name: "y", Value: &aotir.IntLit{Value: 2}},
					}},
				},
				&aotir.CallStmt{Func: "mochi_print_bool", Args: []aotir.Expr{
					&aotir.BinaryExpr{Op: aotir.BinEqRec,
						Left:   &aotir.VarRef{Name: "a", VarType: aotir.TypeRecord, RecordName: "Pt"},
						Right:  &aotir.VarRef{Name: "b", VarType: aotir.TypeRecord, RecordName: "Pt"},
						Result: aotir.TypeBool},
				}},
			),
			want: []string{
				"mochi_print_bool(mochi_eq_Pt(a, b));",
			},
			notWant: []string{
				"(a == b)",
			},
		},
		{
			name: "record_ne_negates_helper",
			prog: mkProg(
				&aotir.LetStmt{Name: "a", VarType: aotir.TypeRecord, RecordName: "Pt",
					Init: &aotir.RecordLit{TypeName: "Pt", Fields: []aotir.RecordLitArg{
						{Name: "x", Value: &aotir.IntLit{Value: 1}},
						{Name: "y", Value: &aotir.IntLit{Value: 2}},
					}},
				},
				&aotir.LetStmt{Name: "b", VarType: aotir.TypeRecord, RecordName: "Pt",
					Init: &aotir.RecordLit{TypeName: "Pt", Fields: []aotir.RecordLitArg{
						{Name: "x", Value: &aotir.IntLit{Value: 1}},
						{Name: "y", Value: &aotir.IntLit{Value: 3}},
					}},
				},
				&aotir.CallStmt{Func: "mochi_print_bool", Args: []aotir.Expr{
					&aotir.BinaryExpr{Op: aotir.BinNeRec,
						Left:   &aotir.VarRef{Name: "a", VarType: aotir.TypeRecord, RecordName: "Pt"},
						Right:  &aotir.VarRef{Name: "b", VarType: aotir.TypeRecord, RecordName: "Pt"},
						Result: aotir.TypeBool},
				}},
			),
			want: []string{
				"mochi_print_bool((!mochi_eq_Pt(a, b)));",
			},
		},
		{
			name: "string_eq_uses_strcmp",
			prog: &aotir.Program{
				Functions: []*aotir.Function{{
					Name:       "main",
					ReturnType: aotir.TypeUnit,
					Body: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.CallStmt{Func: "mochi_print_bool", Args: []aotir.Expr{
							&aotir.BinaryExpr{Op: aotir.BinEqStr,
								Left:   &aotir.StringLit{Value: "a"},
								Right:  &aotir.StringLit{Value: "b"},
								Result: aotir.TypeBool},
						}},
					}},
				}},
				Main: 0,
			},
			want: []string{
				`(strcmp("a", "b") == 0)`,
				"#include <string.h>",
			},
			notWant: []string{
				`("a" == "b")`,
			},
		},
		{
			name: "string_ne_uses_strcmp",
			prog: &aotir.Program{
				Functions: []*aotir.Function{{
					Name:       "main",
					ReturnType: aotir.TypeUnit,
					Body: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.CallStmt{Func: "mochi_print_bool", Args: []aotir.Expr{
							&aotir.BinaryExpr{Op: aotir.BinNeStr,
								Left:   &aotir.StringLit{Value: "a"},
								Right:  &aotir.StringLit{Value: "b"},
								Result: aotir.TypeBool},
						}},
					}},
				}},
				Main: 0,
			},
			want: []string{
				`(strcmp("a", "b") != 0)`,
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
