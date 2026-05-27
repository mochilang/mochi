package javasrc

import (
	"strings"
	"testing"
)

// TestJavaSrcNodes verifies that each node type produces valid Java source fragments.
func TestJavaSrcNodes(t *testing.T) {
	t.Run("class_decl", func(t *testing.T) {
		c := &ClassDecl{
			Modifiers: []string{"public", "final"},
			Name:      "Foo",
			Members: []Member{
				&FieldDecl{
					Modifiers: []string{"private", "final"},
					Type:      TypeLong,
					Name:      "x",
				},
			},
		}
		got := c.javaString(0)
		if !strings.Contains(got, "public final class Foo") {
			t.Errorf("missing class header: %s", got)
		}
		if !strings.Contains(got, "private final long x;") {
			t.Errorf("missing field: %s", got)
		}
	})

	t.Run("record_decl", func(t *testing.T) {
		r := &RecordDecl{
			Modifiers:  []string{"public"},
			Name:       "Point",
			Components: []RecordComponent{{Type: TypeLong, Name: "x"}, {Type: TypeLong, Name: "y"}},
		}
		got := r.javaString(0)
		if !strings.Contains(got, "public record Point(long x, long y)") {
			t.Errorf("unexpected: %s", got)
		}
	})

	t.Run("sealed_interface", func(t *testing.T) {
		s := &SealedInterfaceDecl{
			Modifiers: []string{"public"},
			Name:      "Shape",
			Permits:   []string{"Shape.Circle", "Shape.Square"},
		}
		got := s.javaString(0)
		if !strings.Contains(got, "sealed interface Shape permits Shape.Circle, Shape.Square") {
			t.Errorf("unexpected: %s", got)
		}
	})

	t.Run("method_decl", func(t *testing.T) {
		m := &MethodDecl{
			Modifiers:  []string{"public", "static"},
			ReturnType: TypeVoid,
			Name:       "main",
			Params:     []Param{{Type: &TypeRef{Name: "String", Array: true}, Name: "args"}},
			Body: &Block{Stmts: []Stmt{
				&ExprStmt{X: &StaticCallExpr{
					Class:  "System.out",
					Method: "println",
					Args:   []Expr{StringLit("hello")},
				}},
			}},
		}
		got := m.memberString(0)
		if !strings.Contains(got, "public static void main(String[] args)") {
			t.Errorf("unexpected: %s", got)
		}
		if !strings.Contains(got, `System.out.println("hello")`) {
			t.Errorf("missing println: %s", got)
		}
	})

	t.Run("compilation_unit", func(t *testing.T) {
		cu := &CompilationUnit{
			Package: "dev.mochi.user",
			Imports: []string{"java.util.List"},
			Types: []TypeDecl{
				&ClassDecl{
					Modifiers: []string{"public"},
					Name:      "HelloMochi",
					Members: []Member{
						&MethodDecl{
							Modifiers:  []string{"public", "static"},
							ReturnType: TypeVoid,
							Name:       "main",
							Params:     []Param{{Type: &TypeRef{Name: "String", Array: true}, Name: "args"}},
							Body:       &Block{},
						},
					},
				},
			},
		}
		got := cu.JavaString()
		if !strings.Contains(got, "package dev.mochi.user;") {
			t.Errorf("missing package: %s", got)
		}
		if !strings.Contains(got, "import java.util.List;") {
			t.Errorf("missing import: %s", got)
		}
		if !strings.Contains(got, "public class HelloMochi") {
			t.Errorf("missing class: %s", got)
		}
	})

	t.Run("switch_expr", func(t *testing.T) {
		se := &SwitchExpr{
			Tag: &NameExpr{Name: "x"},
			Cases: []SwitchExprCase{
				{Pattern: "Integer i", Body: &NameExpr{Name: "i"}},
				{Default: true, Body: LongLit(0)},
			},
		}
		got := se.ExprString()
		if !strings.Contains(got, "switch (x)") {
			t.Errorf("unexpected: %s", got)
		}
	})

	t.Run("literals", func(t *testing.T) {
		if LongLit(42).ExprString() != "42L" {
			t.Errorf("long lit: %s", LongLit(42).ExprString())
		}
		if BoolLit(true).ExprString() != "true" {
			t.Errorf("bool lit")
		}
		if StringLit("hello\nworld").ExprString() != `"hello\nworld"` {
			t.Errorf("string lit: %s", StringLit("hello\nworld").ExprString())
		}
	})
}
