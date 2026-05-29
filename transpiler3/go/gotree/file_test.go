package gotree

import (
	"go/parser"
	"go/token"
	"strings"
	"testing"
)

// TestEmptyFile verifies File.Render produces a parseable
// Go source file for a minimal package + import + decl set.
func TestEmptyFile(t *testing.T) {
	f := &File{PackageName: "main"}
	src, err := f.Render()
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	if !strings.Contains(string(src), "package main") {
		t.Fatalf("missing package clause:\n%s", src)
	}
	if _, err := parser.ParseFile(token.NewFileSet(), "", src, 0); err != nil {
		t.Fatalf("output is not valid Go: %v\n---\n%s", err, src)
	}
}

// TestHelloFile verifies File.Render produces a valid
// `package main; import "fmt"; func main() { fmt.Println("hi") }`
// when assembled from gotree nodes. This is the smoke test for
// the entire shadow AST surface.
func TestHelloFile(t *testing.T) {
	f := &File{
		PackageName: "main",
		Imports:     []ImportSpec{{Path: "fmt"}},
		Decls: []Decl{
			&FuncDecl{
				Name: "main",
				Type: &FuncType{},
				Body: &BlockStmt{
					List: []Stmt{
						&ExprStmt{X: &CallExpr{
							Fun: &SelectorExpr{X: &Ident{Name: "fmt"}, Sel: "Println"},
							Args: []Expr{
								&BasicLit{Kind: StringLit, Value: "hi"},
							},
						}},
					},
				},
			},
		},
	}
	src, err := f.Render()
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	s := string(src)
	for _, want := range []string{`package main`, `"fmt"`, `func main()`, `fmt.Println("hi")`} {
		if !strings.Contains(s, want) {
			t.Fatalf("missing %q in output:\n%s", want, s)
		}
	}
	if _, err := parser.ParseFile(token.NewFileSet(), "", src, 0); err != nil {
		t.Fatalf("output is not valid Go: %v\n---\n%s", err, src)
	}
}

// TestRenderIsIdempotent verifies that Render's output, when
// fed back through Format, yields the same bytes. This is the
// Phase 0 reproducibility gate for the writer.
func TestRenderIsIdempotent(t *testing.T) {
	f := &File{
		PackageName: "main",
		Imports:     []ImportSpec{{Path: "fmt"}},
		Decls: []Decl{
			&FuncDecl{
				Name: "main",
				Type: &FuncType{},
				Body: &BlockStmt{
					List: []Stmt{
						&ExprStmt{X: &CallExpr{
							Fun: &SelectorExpr{X: &Ident{Name: "fmt"}, Sel: "Println"},
							Args: []Expr{
								&BasicLit{Kind: StringLit, Value: "hi"},
							},
						}},
					},
				},
			},
		},
	}
	src, err := f.Render()
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	again, err := Format(src)
	if err != nil {
		t.Fatalf("Format pass 2: %v", err)
	}
	if string(again) != string(src) {
		t.Fatalf("not idempotent:\n--- first ---\n%s\n--- second ---\n%s", src, again)
	}
}
