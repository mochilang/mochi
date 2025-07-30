package cpp

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/cpp"
)

// Program is the root of a parsed C++ translation unit.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses the given C++ source code using tree-sitter and
// returns its Program structure.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(ts.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convert(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}
