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
	parser.SetLanguage(tszig.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	node, ok := convertNode(tree.RootNode(), []byte(src), opt.Positions)
	if !ok {
		return &Program{Root: SourceFile{}}, nil
	}
	return &Program{Root: SourceFile{Node: node}}, nil
}
