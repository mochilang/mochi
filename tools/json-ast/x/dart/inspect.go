package dart

import (
	"encoding/json"

	ts "github.com/UserNobody14/tree-sitter-dart/bindings/go"
	sitter "github.com/smacker/go-tree-sitter"
)

// Program represents a parsed Dart source file.
type Program struct {
	File *Node `json:"file"`
}

// Inspect parses Dart source code using tree-sitter and returns
// its Program structure.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(ts.Language()))
	tree := parser.Parse(nil, []byte(src))
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{File: &root}, nil
}

// MarshalJSON ensures stable output ordering.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
