package kotlin

import (
	"encoding/json"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/kotlin"
)

// Program represents a parsed Kotlin source file.
type Program struct {
	File *Node `json:"file"`
}

// Inspect parses Kotlin source code using tree-sitter and returns its AST.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(ts.GetLanguage())
	tree := p.Parse(nil, []byte(src))
	return &Program{File: toNode(tree.RootNode())}, nil
}

// MarshalJSON ensures stable output for Program.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
