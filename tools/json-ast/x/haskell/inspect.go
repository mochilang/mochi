package haskell

import (
	"context"

	sitter "github.com/smacker/go-tree-sitter"
	tsHaskell "github.com/tree-sitter/tree-sitter-haskell/bindings/go"
)

// Program represents a parsed Haskell module.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses the provided Haskell source code using tree-sitter and
// returns its Program representation.
func Inspect(src string, includePos ...bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tsHaskell.Language()))
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, err
	}
	pos := false
	if len(includePos) > 0 && includePos[0] {
		pos = true
	}
	root := convert(tree.RootNode(), []byte(src), pos)
	if root == nil {
		root = &Node{}
	}
	return &Program{Root: root}, nil
}
