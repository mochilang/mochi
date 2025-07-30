package py

import (
	"encoding/json"

	sitter "github.com/smacker/go-tree-sitter"
	python "github.com/smacker/go-tree-sitter/python"
)

// Program represents a parsed Python source file.
// It exposes a strongly typed root node while internally reusing the generic
// Node structure defined in ast.go.
type Program struct {
	File *Module `json:"file"`
}

// Inspect parses the given Python source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(python.GetLanguage())
	tree := p.Parse(nil, []byte(src))
	return &Program{File: (*Module)(convertNode(tree.RootNode(), []byte(src)))}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
