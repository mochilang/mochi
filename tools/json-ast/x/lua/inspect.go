package lua

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	tslua "github.com/smacker/go-tree-sitter/lua"
)

// Program describes a parsed Lua source file.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses Lua source code using tree-sitter and returns its Program.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tslua.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root, _ := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
