//go:build slow

package zig

import (
	"context"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsz "github.com/tree-sitter/tree-sitter-zig/bindings/go"
)

// Program describes a parsed Zig source file.
type Program struct {
	Root *SourceFile `json:"root"`
}

// Inspect parses Zig source code using tree-sitter.
// By default the resulting AST omits position information. Set opts.Positions
// to true to include it.
func Inspect(src string, opts ...Options) (*Program, error) {
	var opt Options
	if len(opts) > 0 {
		opt = opts[0]
	}
	parser := sitter.NewParser()
	parser.SetLanguage(tsz.GetLanguage())
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	node, ok := convertNode(tree.RootNode(), []byte(src), opt.Positions)
	if !ok {
		return &Program{Root: nil}, nil
	}
	root := SourceFile{Node: node}
	return &Program{Root: &root}, nil
}
