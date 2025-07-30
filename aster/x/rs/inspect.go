package rs

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	rust "github.com/tree-sitter/tree-sitter-rust/bindings/go"
)

// Inspect parses the given Rust source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string, includePos bool) (*Program, error) {
	p := sitter.NewParser()
	lang := sitter.NewLanguage(rust.Language())
	p.SetLanguage(lang)
	data := []byte(src)
	tree := p.Parse(data, nil)
	root := convert(tree.RootNode(), data, includePos)
	return &Program{Root: (*SourceFile)(root)}, nil
}

// MarshalJSON implements json.Marshaler for Program, to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
