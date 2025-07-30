package fs

import (
	sitter "github.com/smacker/go-tree-sitter"
	fsharp "github.com/tree-sitter/tree-sitter-fsharp/bindings/go"
)

// Program represents a parsed F# source file.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses F# code using tree-sitter and returns its Program representation.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(fsharp.LanguageFSharp()))
	tree := parser.Parse(nil, []byte(src))
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
