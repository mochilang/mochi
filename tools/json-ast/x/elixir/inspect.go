package elixir

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/elixir"
)

// Inspect parses Elixir source code using tree-sitter and returns the Program.
// Inspect parses Elixir source code using tree-sitter and returns the Program.
// When includePos is false the resulting JSON omits all position information.
func Inspect(src string, includePos bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(ts.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	n := convert(tree.RootNode(), []byte(src), includePos)
	if n == nil {
		return &Program{Root: nil}, nil
	}
	root := &Source{Node: *n}
	return &Program{Root: root}, nil
}
