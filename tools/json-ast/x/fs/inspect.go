package fs

import (
	sitter "github.com/smacker/go-tree-sitter"
	fsharp "github.com/tree-sitter/tree-sitter-fsharp/bindings/go"
)

// Program represents a parsed F# source file.
// Program represents a parsed F# source file. The Root field mirrors the tree
// sitter "file" node.
type Program struct {
	Root File `json:"root"`
}

// Inspect parses F# code using tree-sitter and returns its Program representation.
// When withPos is false, positional information is omitted from the resulting AST.
func Inspect(src string, withPos bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(fsharp.LanguageFSharp()))
	tree := parser.Parse(nil, []byte(src))
	root := convert(tree.RootNode(), []byte(src), withPos)
	if root == nil {
		return &Program{}, nil
	}
	return &Program{Root: File(*root)}, nil
}
