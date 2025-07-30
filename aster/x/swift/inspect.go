//go:build slow

package swift

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsswift "github.com/tree-sitter/tree-sitter-swift/bindings/go"
)

// Program represents a parsed Swift source file.
type Program struct {
	File *SourceFile `json:"file"`
}

// Option controls how Inspect behaves.
type Option struct {
	WithPositions bool
}

// Inspect parses the given Swift source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string, opts ...Option) (*Program, error) {
	var withPos bool
	if len(opts) > 0 {
		withPos = opts[0].WithPositions
	}
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tsswift.Language()))
	b := []byte(src)
	tree := p.Parse(b, nil)
	return &Program{File: ConvertFile(tree.RootNode(), b, withPos)}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
