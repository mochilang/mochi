package mochi

import (
	"mochi/ast"
	"mochi/parser"
)

// Inspect parses Mochi source code using the official parser and returns a
// simplified AST representation.
func Inspect(src string) (*Program, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	root := ast.FromProgram(prog)
	return &Program{File: toNode(root)}, nil
}
