package haskell

import (
	"context"
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsHaskell "github.com/tree-sitter/tree-sitter-haskell/bindings/go"
)

// Program represents a parsed Haskell module.
type Program struct {
	Root   *Haskell `json:"root"`
	Source string   `json:"-"`
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
	return &Program{Root: (*Haskell)(root), Source: src}, nil
}

// Inspect parses the provided Haskell source code using tree-sitter and
// returns its Program representation without positional information by default.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output
// when encoding the AST. This mirrors the behaviour of other language
// implementations in the aster/x package.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
