package scala

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tscala "github.com/tree-sitter/tree-sitter-scala/bindings/go"
)

// Inspect parses Scala source code using tree-sitter and returns its AST.  By
// default positional information is omitted from the resulting Program. Pass an
// Option with WithPositions set to true to include it.
func Inspect(src string, opts ...Option) (*Program, error) {
	var withPos bool
	if len(opts) > 0 {
		withPos = opts[0].WithPositions
	}
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tscala.Language()))
	data := []byte(src)
	tree := parser.Parse(data, nil)
	n := convert(tree.RootNode(), data, withPos)
	if n == nil {
		return &Program{}, nil
	}
	return &Program{Root: CompilationUnit{Node: *n}}, nil
}
