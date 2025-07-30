//go:build slow

package java

import (
	"context"
	"fmt"

	sitter "github.com/tree-sitter/go-tree-sitter"
	javats "github.com/tree-sitter/tree-sitter-java/bindings/go"
)

// Program represents a parsed Java source file.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses the given Java source code using tree-sitter and returns
// its Program structure.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(javats.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	if tree == nil {
		return nil, fmt.Errorf("parse: failed")
	}
	root := convert(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
