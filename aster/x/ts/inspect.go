package ts

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tstypescript "github.com/tree-sitter/tree-sitter-typescript/bindings/go"
)

// Inspect parses the given TypeScript source code using tree-sitter and
// returns its Program structure.
func Inspect(src string) (*Program, error) {
	return inspect(src, false)
}

// InspectWithPositions parses the source and keeps position information.
func InspectWithPositions(src string) (*Program, error) {
	return inspect(src, true)
}

func inspect(src string, pos bool) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tstypescript.LanguageTypescript()))
	tree := p.Parse([]byte(src), nil)
	root := convert(tree.RootNode(), []byte(src), pos)
	return &Program{Root: (*ProgramNode)(root)}, nil
}
