package golang

import "testing"

func TestParseASTInfo(t *testing.T) {
	src := "package foo\nimport \"fmt\"\nfunc main(){}"
	ast, err := ParseAST(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if ast.Package != "foo" {
		t.Fatalf("package name: %q", ast.Package)
	}
	if len(ast.Imports) != 1 || ast.Imports[0] != "fmt" {
		t.Fatalf("imports: %v", ast.Imports)
	}
}
