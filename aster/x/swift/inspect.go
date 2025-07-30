//go:build slow

package swift

import (
    "encoding/json"

    sitter "github.com/tree-sitter/go-tree-sitter"
    tsswift "github.com/smacker/go-tree-sitter/swift"
)

// Program represents a parsed Swift source file.
type Program struct {
	File *SourceFile `json:"file"`
}

// Inspect parses the given Swift source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tsswift.Language()))
	b := []byte(src)
	tree := p.Parse(b, nil)
	return &Program{File: ConvertFile(tree.RootNode(), b)}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
