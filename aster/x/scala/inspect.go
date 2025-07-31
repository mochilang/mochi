package scala

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tscala "github.com/tree-sitter/tree-sitter-scala/bindings/go"
)

// Inspect parses Scala source code using tree-sitter and returns its AST. By
// default positional information is omitted. Pass an Options value with
// Positions=true to include it.
func Inspect(src string, opts ...Options) (*Program, error) {
	var opt Options
	if len(opts) > 0 {
		opt = opts[0]
	}
	return InspectWithOptions(src, opt)
}

// InspectWithOptions behaves like Inspect but accepts an explicit Options value.
func InspectWithOptions(src string, opt Options) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tscala.Language()))
	data := []byte(src)
	tree := parser.Parse(data, nil)
	n := convert(tree.RootNode(), data, opt.Positions)
	if n == nil {
		return &Program{}, nil
	}
	cu := &CompilationUnit{Node: *n}
	return &Program{Root: cu}, nil
}
