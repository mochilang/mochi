//go:build slow

package ocaml

import (
	"fmt"

	"mochi/ast"
	"mochi/parser"
)

// Transform parses the translated Mochi code and returns its AST representation.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	prog, err := parser.ParseString(p.Mochi)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}
