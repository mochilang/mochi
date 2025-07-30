package erlang

import (
	"context"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tserlang "mochi/third_party/tree-sitter-erlang/bindings/go"
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
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
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
