package scala

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tscala "github.com/tree-sitter/tree-sitter-scala/bindings/go"
)

// Inspect parses Scala source code using tree-sitter and returns its AST.
func Inspect(src string, opts ...bool) (*Program, error) {
	includePos := false
	if len(opts) > 0 {
		includePos = opts[0]
	}
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tscala.Language()))
	tree := parser.Parse([]byte(src), nil)
	n := convert(tree.RootNode(), []byte(src), includePos)
	if n == nil {
		return &Program{}, nil
	}
	cu := &CompilationUnit{*n}
	return &Program{Root: cu}, nil
}
