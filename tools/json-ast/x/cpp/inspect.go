package cpp

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/cpp"
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
	parser.SetLanguage(ts.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	var o Options
	if len(opts) > 0 {
		o = opts[0]
	}
	root := convert(tree.RootNode(), []byte(src), o)
	return &Program{Root: root}, nil
}
