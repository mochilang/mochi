//go:build slow

package zig

import (
	"fmt"
	"os"

	"mochi/ast"
)

// node is a small helper to create an AST node.
func node(kind string, value any, children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: kind, Value: value, Children: children}
}

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	// Translation not implemented yet; return an empty program node.
	return node("program", nil), nil
}

// TransformFile reads the Zig file at path and converts it to a Mochi AST node.
func TransformFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	prog, err := Parse(string(data))
	if err != nil {
		return nil, err
	}
	return Transform(prog)
}
