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
// Inspect parses the given Python source code using tree-sitter. Position
// information is omitted by default; use InspectWithOption to enable it.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithOption behaves like Inspect but allows callers to specify whether
// position information should be included in the resulting AST.
func InspectWithOption(src string, opt Option) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tspython.Language()))
	data := []byte(src)
	tree := p.Parse(data, nil)
	root := convert(tree.RootNode(), data, opt)
	if root == nil {
		root = &Node{}
	}
	return &Program{File: (*Module)(root)}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
