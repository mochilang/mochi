package cpp

import (
	"context"
	sitter "github.com/tree-sitter/go-tree-sitter"
	tscpp "github.com/tree-sitter/tree-sitter-cpp/bindings/go"
)

// Program is the root of a parsed C++ translation unit.
// Program represents the root of a parsed translation unit. It mirrors the
// tree-sitter structure using the minimal Node defined in ast.go.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses the given C++ source code using tree-sitter and
// returns its Program structure.
// Inspect parses the given C++ source code using tree-sitter and
// returns its Program structure. Position information is omitted unless
// opts.WithPositions is set to true.
func Inspect(src string, opts ...Options) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tscpp.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	var o Options
	if len(opts) > 0 {
		o = opts[0]
	}
	root := convert(tree.RootNode(), []byte(src), o)
	return &Program{Root: root}, nil
}
