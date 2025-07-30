//go:build slow

package gox

import (
	"context"

	sitter "github.com/smacker/go-tree-sitter"
	tsgolang "github.com/smacker/go-tree-sitter/golang"
)

// Program represents a parsed Go source file.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses Go source code using tree-sitter and returns its Program
// representation.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tsgolang.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, err
	}
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
