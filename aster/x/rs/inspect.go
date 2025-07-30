package rs

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	rust "github.com/tree-sitter/tree-sitter-rust/bindings/go"
)

// Inspect parses the given Rust source code using tree-sitter and returns
// a Program describing its syntax tree.
// Inspect parses the given Rust source code using tree-sitter and returns a
// Program describing its syntax tree. The opt argument controls optional
// features such as inclusion of positional information.
func Inspect(src string, opt Option) (*Program, error) {
	p := sitter.NewParser()
	lang := sitter.NewLanguage(rust.Language())
	p.SetLanguage(lang)
	data := []byte(src)
	tree := p.Parse(data, nil)
	return convertProgram(tree.RootNode(), data, opt), nil
}

// MarshalJSON implements json.Marshaler for Program, to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
