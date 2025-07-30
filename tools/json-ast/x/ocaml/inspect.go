package ocaml

import (
	sitter "github.com/smacker/go-tree-sitter"
	tsocaml "github.com/smacker/go-tree-sitter/ocaml"
)

// Inspect parses the given OCaml source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(tsocaml.GetLanguage())
	data := []byte(src)
	tree := p.Parse(nil, data)
	root := convert(tree.RootNode(), data)
	if root == nil {
		return &Program{}, nil
	}
	return &Program{File: File{Node: *root}}, nil
}
