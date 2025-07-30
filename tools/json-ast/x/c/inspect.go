package c

import (
	"context"
	"encoding/json"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/c"
)

// Program is the root of a parsed C translation unit.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses the given C source code using tree-sitter and returns
// its Program structure.
// Inspect parses the given C source code using tree-sitter and returns
// its Program structure without position information.
func Inspect(src string) (*Program, error) {
	return inspect(src, false)
}

// InspectWithPositions parses the C source and keeps position fields in the AST.
func InspectWithPositions(src string) (*Program, error) {
	return inspect(src, true)
}

func inspect(src string, pos bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(ts.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := toNode(tree.RootNode(), []byte(src), pos)
	return &Program{Root: root}, nil
}

// MarshalJSON ensures stable output ordering.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
