package rtree

import (
	"strings"
	"testing"
)

func TestHelloWorldRender(t *testing.T) {
	sf := &SourceFile{
		Name:                "hello",
		FrozenStringLiteral: true,
		Requires:            []string{"mochi/runtime"},
		Decls: []Decl{
			&ModuleDecl{
				Name: "Program",
				Decls: []Decl{
					&ModuleDecl{
						Name: "Main",
						Decls: []Decl{
							&MethodDecl{
								Receiver: "self",
								Name:     "run",
								Params:   []MethodParam{{Name: "argv"}},
								Body: []Stmt{
									&ExprStmt{X: &MethodCall{
										Method: "puts",
										Args:   []Expr{&StringLit{Value: "Hello, world"}},
									}},
								},
							},
						},
					},
				},
			},
		},
	}

	got := sf.RubySource()
	want := `# frozen_string_literal: true

require "mochi/runtime"

module Program
  module Main
    def self.run(argv)
      puts "Hello, world"
    end
  end
end
`
	if got != want {
		t.Fatalf("RubySource() mismatch:\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}

func TestDataDeclRender(t *testing.T) {
	d := &DataDecl{Name: "User", Fields: []string{"id", "name"}}
	got := d.RubyString(0)
	want := `User = Data.define(:id, :name)`
	if got != want {
		t.Fatalf("DataDecl mismatch: got %q want %q", got, want)
	}
}

func TestStringEscape(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"hi", `"hi"`},
		{`a"b`, `"a\"b"`},
		{"a\nb", `"a\nb"`},
		{`x\y`, `"x\\y"`},
		{`#{x}`, `"\#{x}"`},
	}
	for _, c := range cases {
		got := (&StringLit{Value: c.in}).RubyExprString()
		if got != c.want {
			t.Errorf("escape(%q) = %s; want %s", c.in, got, c.want)
		}
	}
}

func TestBinaryOpAndIf(t *testing.T) {
	st := &IfStmt{
		Cond: &BinaryOp{Op: ">", Lhs: &Ident{Name: "x"}, Rhs: &IntLit{Value: 0}},
		Then: []Stmt{&ExprStmt{X: &MethodCall{Method: "puts", Args: []Expr{&StringLit{Value: "pos"}}}}},
		Else: []Stmt{&ExprStmt{X: &MethodCall{Method: "puts", Args: []Expr{&StringLit{Value: "neg"}}}}},
	}
	got := st.RubyString(0)
	if !strings.Contains(got, "if (x > 0)") {
		t.Errorf("expected 'if (x > 0)' in %q", got)
	}
	if !strings.Contains(got, "else") || !strings.Contains(got, "end") {
		t.Errorf("expected else/end in %q", got)
	}
}
