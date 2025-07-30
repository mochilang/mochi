//go:build slow

package java

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	javats "github.com/smacker/go-tree-sitter/java"
)

// Program represents a parsed Java source file.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses the given Java source code using tree-sitter and returns
// its Program structure.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(javats.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convert(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
