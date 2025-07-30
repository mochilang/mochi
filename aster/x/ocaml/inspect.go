package ocaml

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tsocaml "github.com/tree-sitter/tree-sitter-ocaml/bindings/go"
)

// Inspect parses the given OCaml source code using tree-sitter and returns
// a Program describing its syntax tree. By default positional information is
// omitted; pass an Options value with IncludePositions set to true to retain it.
func Inspect(src string, opts ...Options) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tsocaml.LanguageOCaml()))
	data := []byte(src)
	tree := p.Parse(data, nil)
	var includePos bool
	if len(opts) > 0 {
		includePos = opts[0].IncludePositions
	}
	root := convert(tree.RootNode(), data, includePos)
	if root == nil {
		return &Program{}, nil
	}
	return &Program{File: CompilationUnit{Node: *root}}, nil
}
