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
        var withPos, withSrc bool
        if len(opts) > 0 {
                withPos = opts[0].WithPositions
                withSrc = opts[0].WithSource
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
        p := &Program{File: &ProgramNode{Node: *n}}
        if withSrc {
                p.Source = src
        }
        return p, nil
}
