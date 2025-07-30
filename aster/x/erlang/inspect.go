package erlang

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	tserlang "github.com/tree-sitter/tree-sitter-erlang/bindings/go"
)

// Program represents a parsed Erlang file composed of Node structs.
// Program represents a parsed Erlang source file.
type Program struct {
	Root *SourceFile `json:"root"`
}

// InspectWithOption parses Erlang source code using tree-sitter and returns a Program.
func InspectWithOption(src string, opt Option) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tserlang.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convert(tree.RootNode(), []byte(src), opt)
	if root == nil {
		return &Program{}, nil
	}
	return &Program{Root: (*SourceFile)(root)}, nil
}

// Inspect parses Erlang source code using tree-sitter and returns a Program.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}
