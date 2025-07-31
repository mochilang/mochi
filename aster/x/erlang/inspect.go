//go:build slow

package erlang

import (
	"context"
	"fmt"

	sitter "github.com/tree-sitter/go-tree-sitter"
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
	if err := parser.SetLanguage(sitter.NewLanguage(tserlang.Language())); err != nil {
		return nil, fmt.Errorf("language: %w", err)
	}
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	root := convert(tree.RootNode(), []byte(src), opt)
	if root == nil {
		return &Program{}, nil
	}
	return &Program{Root: (*SourceFile)(root)}, nil
}

// Inspect parses Erlang source code using tree-sitter and returns a Program.
// Inspect parses Erlang source code using tree-sitter and returns a Program.
// The resulting AST includes all syntactic nodes so that it can be converted
// back to source code using Print.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{AllNodes: true, Positions: true})
}
