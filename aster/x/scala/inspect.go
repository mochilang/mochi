package scala

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tscala "github.com/tree-sitter/tree-sitter-scala/bindings/go"
)

// Inspect parses Scala source code using tree-sitter and returns its AST. By
// default positional information is omitted. Pass an Options value with
// Positions=true to include it.
func Inspect(src string, opts ...Options) (*Program, error) {
	var withPos bool
	if len(opts) > 0 {
		withPos = opts[0].Positions
	}
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tscala.Language()))
	data := []byte(src)
	tree := parser.Parse(data, nil)
	n := convert(tree.RootNode(), data, withPos)
	if n == nil {
		return &Program{}, nil
	}
	cu := &CompilationUnit{Node: *n}
	return &Program{Root: cu}, nil
}
