package rs

import (
	"encoding/json"

	sitter "github.com/smacker/go-tree-sitter"
	rust "github.com/smacker/go-tree-sitter/rust"
)

// Inspect parses the given Rust source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string, includePos bool) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(rust.GetLanguage())
	data := []byte(src)
	tree := p.Parse(nil, data)
	root := convert(tree.RootNode(), data, includePos)
	return &Program{Root: (*SourceFile)(root)}, nil
}

// MarshalJSON implements json.Marshaler for Program, to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
