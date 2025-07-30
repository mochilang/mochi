package erl

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/tree-sitter/tree-sitter-erlang/bindings/go"
)

// Program represents a parsed Erlang source file.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses Erlang source code using tree-sitter and returns its AST.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	lang := sitter.NewLanguage(ts.Language())
	p.SetLanguage(lang)
	tree, err := p.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convert(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
