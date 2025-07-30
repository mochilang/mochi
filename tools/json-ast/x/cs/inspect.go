package cs

import (
	"encoding/json"

	sitter "github.com/smacker/go-tree-sitter"
	csharp "github.com/smacker/go-tree-sitter/csharp"
)

// Program is defined in ast.go and composes Node values.

// Inspect parses the given C# source code using tree-sitter and returns
// a Program describing its syntax tree.
// Inspect parses the given C# source code using tree-sitter. When withPos is
// true the returned AST includes position information.
func Inspect(src string, withPos bool) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(csharp.GetLanguage())
	data := []byte(src)
	tree := p.Parse(nil, data)
	return &Program{File: (*CompilationUnit)(toNode(tree.RootNode(), data, withPos))}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
