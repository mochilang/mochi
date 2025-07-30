package ocaml

import (
	sitter "github.com/smacker/go-tree-sitter"
	tsocaml "github.com/smacker/go-tree-sitter/ocaml"
)

// Program represents a parsed OCaml source file.
type Program struct {
	File *Node `json:"file"`
}

// Inspect parses the given OCaml source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(tsocaml.GetLanguage())
	tree := p.Parse(nil, []byte(src))
	return &Program{File: toNode(tree.RootNode())}, nil
}
