package haskell

import (
	"context"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsHaskell "github.com/tree-sitter/tree-sitter-haskell/bindings/go"
)

// Program represents a parsed Haskell module.
type Program struct {
	Root *Haskell `json:"root"`
}

// InspectWithOption parses Haskell source code using tree-sitter and returns a Program.
func InspectWithOption(src string, opt Option) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tsHaskell.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	root := convert(tree.RootNode(), []byte(src), opt)
	if root == nil {
		root = &Node{}
	}
	return &Program{Root: (*Haskell)(root)}, nil
}

// Inspect parses the provided Haskell source code using tree-sitter and
// returns its Program representation without positional information by default.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}
