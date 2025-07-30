package cs

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	csharp "github.com/tree-sitter/tree-sitter-c-sharp/bindings/go"
)

// Program is defined in ast.go and composes Node values.

// Inspect parses the given C# source code using tree-sitter and returns
// a Program describing its syntax tree.
// Inspect parses the given C# source code using tree-sitter. When withPos is
// true the returned AST includes position information.
func Inspect(src string, withPos bool) (*Program, error) {
	p := sitter.NewParser()
	lang := sitter.NewLanguage(csharp.Language())
	p.SetLanguage(lang)
	data := []byte(src)
	tree := p.Parse(data, nil)
	return &Program{File: (*CompilationUnit)(toNode(tree.RootNode(), data, withPos))}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
