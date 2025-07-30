//go:build slow

package zig

import (
	"context"

	sitter "github.com/tree-sitter/go-tree-sitter"
	"mochi/aster/x/zig/tree_sitter_zig"
)

// Program describes a parsed Zig source file.
type Program struct {
	Root SourceFile `json:"root"`
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
	parser.SetLanguage(tree_sitter_zig.GetLanguage())
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	node, ok := convertNode(tree.RootNode(), []byte(src), opt.Positions)
	if !ok {
		return &Program{Root: SourceFile{}}, nil
	}
	return &Program{Root: SourceFile{Node: node}}, nil
}
