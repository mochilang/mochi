package ts

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tstypescript "github.com/tree-sitter/tree-sitter-typescript/bindings/go"
)

// Inspect parses the given TypeScript source code using tree-sitter and
// returns its Program structure.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tstypescript.LanguageTypescript()))
	tree := p.Parse([]byte(src), nil)
	root := tree.RootNode()
	var stmts []Node
	for i := uint(0); i < root.NamedChildCount(); i++ {
		child := root.NamedChild(i)
		if n := convertNode(child, []byte(src)); n != nil {
			stmts = append(stmts, *n)
		}
	}
	return &Program{Statements: stmts}, nil
}
