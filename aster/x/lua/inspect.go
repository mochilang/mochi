package lua

import (
	"context"
	"fmt"

    sitter "github.com/tree-sitter/go-tree-sitter"
    tslua "github.com/tree-sitter/tree-sitter-lua/bindings/go"
)

// Program describes a parsed Lua source file.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses Lua source code using tree-sitter and returns its Program.
func Inspect(src string) (*Program, error) {
	return InspectWithPositions(src, false)
}

// InspectWithPositions parses Lua source and optionally includes position information.
func InspectWithPositions(src string, withPos bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tslua.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root, _ := convertNode(tree.RootNode(), []byte(src), withPos)
	return &Program{Root: &root}, nil
}
