package ts

import (
	sitter "github.com/smacker/go-tree-sitter"
	tstypescript "github.com/smacker/go-tree-sitter/typescript/typescript"
)

// Inspect parses the given TypeScript source code using tree-sitter and
// returns its Program structure.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(tstypescript.GetLanguage())
	tree := p.Parse(nil, []byte(src))
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
