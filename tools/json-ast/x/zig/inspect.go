//go:build slow

package zig

import (
	"context"
	"fmt"

	tszig "github.com/slimsag/tree-sitter-zig/bindings/go"
	sitter "github.com/smacker/go-tree-sitter"
)

// Program describes a parsed Zig source file.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses Zig source code using tree-sitter.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tszig.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
