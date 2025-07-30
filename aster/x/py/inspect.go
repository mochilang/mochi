//go:build slow

package py

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tspython "github.com/tree-sitter/tree-sitter-python/bindings/go"
)

// Program represents a parsed Python source file.
type Program struct {
	File *Module `json:"file"`
}

// Inspect parses the given Python source code using tree-sitter and returns
// a Program describing its syntax tree. When withPos is true the resulting AST
// includes positional information.
func Inspect(src string, withPos bool) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tspython.Language()))
	data := []byte(src)
	tree := p.Parse(data, nil)
	return &Program{File: (*Module)(toNode(tree.RootNode(), data, withPos))}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
