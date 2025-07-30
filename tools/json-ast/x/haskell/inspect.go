package haskell

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	thaskell "github.com/tree-sitter/tree-sitter-haskell/bindings/go"
)

// Inspect parses Haskell source code using tree-sitter and returns its AST.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(thaskell.Language()))
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
