package c

import (
	"context"
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsc "github.com/tree-sitter/tree-sitter-c/bindings/go"
)

// Program is the root of a parsed C translation unit.
// Program represents a parsed C translation unit.
type Program struct {
	Root *TranslationUnit `json:"root"`
}

// Inspect parses the given C source code using tree-sitter and returns
// its Program structure.
// Inspect parses the given C source code using tree-sitter and returns
// its Program structure without position information.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{Positions: IncludePos})
}

// InspectWithPositions parses the C source and keeps position fields in the AST.
func InspectWithPositions(src string) (*Program, error) {
	return InspectWithOption(src, Option{Positions: true})
}

// InspectWithOption behaves like Inspect but allows callers to specify whether
// position information should be included in the resulting AST.
func InspectWithOption(src string, opt Option) (*Program, error) {
	return inspect(src, opt.Positions)
}

func inspect(src string, pos bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tsc.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	root := convert(tree.RootNode(), []byte(src), pos)
	return &Program{Root: (*TranslationUnit)(root)}, nil
}

// MarshalJSON ensures stable output ordering.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
