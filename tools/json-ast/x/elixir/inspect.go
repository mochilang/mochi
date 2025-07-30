package elixir

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/elixir"
)

// Inspect parses Elixir source code using tree-sitter and returns the Program.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(ts.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convert(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
