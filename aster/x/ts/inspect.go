package ts

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tstypescript "github.com/tree-sitter/tree-sitter-typescript/bindings/go"
)

// Inspect parses the given TypeScript source code using tree-sitter without
// positional information.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithOption parses the given source using tree-sitter and returns a
// Program built according to opt.
func InspectWithOption(src string, opt Option) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tstypescript.LanguageTypescript()))
	tree := p.Parse([]byte(src), nil)
	root := tree.RootNode()
	var stmts []Statement
	data := []byte(src)
	for i := uint(0); i < root.NamedChildCount(); i++ {
		child := convert(root.NamedChild(i), data, opt)
		if child != nil {
			stmts = append(stmts, Statement(*child))
		}
	}
	return &Program{Statements: stmts}, nil
}
