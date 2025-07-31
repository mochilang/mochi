package ex

import (
	"context"

	sitter "github.com/tree-sitter/go-tree-sitter"
	ts "github.com/tree-sitter/tree-sitter-elixir/bindings/go"
)

// Inspect parses Elixir source code using tree-sitter and returns its Program
// representation. Position fields are only populated when IncludePos is true.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(ts.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	n := convert(tree.RootNode(), []byte(src))
	if n == nil {
		return &Program{}, nil
	}
	root := &Source{Node: *n}
	return &Program{Root: root}, nil
}
