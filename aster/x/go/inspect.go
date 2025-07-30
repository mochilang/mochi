//go:build slow

package gox

import (
	"context"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsgolang "github.com/tree-sitter/tree-sitter-go/bindings/go"
)

// Program represents a parsed Go source file.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses Go source code using tree-sitter and returns its Program
// representation.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tsgolang.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
