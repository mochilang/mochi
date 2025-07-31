//go:build slow

package elixir

import (
	"context"
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	ts "github.com/tree-sitter/tree-sitter-elixir/bindings/go"
)

// Option controls how the AST is generated.
type Option struct {
	Positions bool
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}

// Inspect parses Elixir source code using tree-sitter and returns the Program.
// Position information is only included when IncludePositions is set to true.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithOption behaves like Inspect but allows callers to specify whether
// position information should be included in the resulting AST.
func InspectWithOption(src string, opt Option) (*Program, error) {
	IncludePositions = opt.Positions
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
