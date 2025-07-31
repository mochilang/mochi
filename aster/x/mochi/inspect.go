package mochi

import (
	"mochi/ast"
	"mochi/parser"
)

// Inspect parses Mochi source code using the official parser and returns a
// simplified AST representation.
// Inspect parses Mochi source code using the official parser and returns a
// simplified Program. Positional information is omitted unless opts specifies
// otherwise.
func Inspect(src string, opts ...Option) (*Program, error) {
	var withPos bool
	if len(opts) > 0 {
		withPos = opts[0].WithPositions
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	root := ast.FromProgram(prog)
	n := convert(root, withPos)
	if n == nil {
		n = &Node{}
	}
	return &Program{File: &ProgramNode{Node: *n}}, nil
}
