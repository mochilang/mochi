//go:build slow

package gox

import (
	"context"
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsgolang "github.com/tree-sitter/tree-sitter-go/bindings/go"
)

// Program represents a parsed Go source file.
type Program struct {
	Root *SourceFile `json:"root"`
}

// Inspect parses Go source code using tree-sitter and returns its Program
// representation.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{Positions: IncludePos})
}

// InspectWithPositions behaves like Inspect but forces positional information
// to be included in the resulting AST.
func InspectWithPositions(src string) (*Program, error) {
	return InspectWithOption(src, Option{Positions: true})
}

// InspectWithOption parses Go source code using tree-sitter. When
// opt.Positions is true the resulting AST contains position information.
func InspectWithOption(src string, opt Option) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tsgolang.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	root := toNode(tree.RootNode(), []byte(src), opt.Positions)
	if root == nil {
		return &Program{}, nil
	}
	return &Program{Root: (*SourceFile)(root)}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
