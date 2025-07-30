package swift

import (
	"encoding/json"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/swift"
)

// Program represents a parsed Swift source file.
type Program struct {
	File *SourceFile `json:"file"`
}

// Inspect parses the given Swift source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(ts.GetLanguage())
	tree := p.Parse(nil, []byte(src))
	return &Program{File: ConvertFile(tree.RootNode())}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
