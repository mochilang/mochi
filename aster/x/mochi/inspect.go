package mochi

import (
	"fmt"

	"mochi/ast"
	"mochi/parser"
)

// Inspect parses Mochi source code using the official parser and returns a
// simplified AST representation.
// Inspect parses Mochi source code using the official parser and returns a
// simplified Program. Positional information is omitted unless opts specifies
// otherwise.
func Inspect(src string, opts ...Option) (p *Program, err error) {
	var (
		withPos  bool
		filename string
	)
	if len(opts) > 0 {
		withPos = opts[0].WithPositions
		filename = opts[0].Filename
	}
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("%v", r)
			p = nil
		}
	}()
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	root := ast.FromProgram(prog)
	n := convert(root, withPos)
	if n == nil {
		n = &Node{}
	}
	return &Program{File: &ProgramNode{Node: *n}, Source: src, Filename: filename}, nil
}
