//go:build slow

package elixir

import (
	"context"

	sitter "github.com/tree-sitter/go-tree-sitter"
	ts "github.com/tree-sitter/tree-sitter-elixir/bindings/go"
)

// Option controls how the AST is generated. When Positions is true the
// resulting nodes include positional information. The default is false.
type Option struct {
	Positions bool
}

// Inspect parses Elixir source code using tree-sitter and returns a Program
// describing its syntax tree. Position information is omitted by default; use
// InspectWithOption to enable it.
func Inspect(src string) (*Program, error) { return InspectWithOption(src, Option{}) }

// InspectWithOption behaves like Inspect but allows callers to specify whether
// positional information should be included in the resulting AST.
func InspectWithOption(src string, opt Option) (*Program, error) {
	prev := IncludePositions
	IncludePositions = opt.Positions
	defer func() { IncludePositions = prev }()

	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(ts.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	n := convert(tree.RootNode(), []byte(src))
	if n == nil {
		return &Program{Root: nil}, nil
	}
	root := &Source{Node: *n}
	return &Program{Root: root}, nil
}
