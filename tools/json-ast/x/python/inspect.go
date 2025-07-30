package python

import (
	"encoding/json"

	sitter "github.com/smacker/go-tree-sitter"
	tspython "github.com/smacker/go-tree-sitter/python"
)

// Program represents a parsed Python source file.
type Program struct {
	File *Node `json:"file"`
}

// Inspect parses the given Python source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(tspython.GetLanguage())
	tree := p.Parse(nil, []byte(src))
	return &Program{File: toNode(tree.RootNode())}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
