//go:build slow

package gox

import (
	"go/ast"
	"go/parser"
	"go/token"
)

// Program holds the parsed structure of a Go file.
type Program struct {
	File *ast.File `json:"file"`
}

// Inspect parses Go source code and returns its Program representation.
func Inspect(src string) (*Program, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "", src, parser.ParseComments)
	if err != nil {
		return nil, err
	}
	sanitize(file)
	return &Program{File: file}, nil
}

// sanitize removes references that make the AST cyclic and
// therefore unsuitable for JSON marshaling.
func sanitize(f *ast.File) {
	if f == nil {
		return
	}
	f.Scope = nil
	ast.Inspect(f, func(n ast.Node) bool {
		if id, ok := n.(*ast.Ident); ok {
			id.Obj = nil
		}
		return true
	})
}
