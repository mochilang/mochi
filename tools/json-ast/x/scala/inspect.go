package scala

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	tscala "github.com/smacker/go-tree-sitter/scala"
)

// Program represents a parsed Scala source file.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses Scala source code using tree-sitter and returns its AST.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tscala.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convert(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
