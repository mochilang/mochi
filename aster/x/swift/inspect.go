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
	// Src holds the original Swift source code used to generate the AST.
	// It is omitted from the marshalled JSON output.
	Src string `json:"-"`
}

// Option controls how Inspect behaves. When Positions is true the
// returned AST includes positional information.
type Option struct {
	Positions bool
	Comments  bool
}

// Inspect parses the given Swift source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string, opts ...Option) (*Program, error) {
	var withPos bool
	var withComments bool
	if len(opts) > 0 {
		withPos = opts[0].Positions
		withComments = opts[0].Comments
	}
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tsswift.Language()))
	b := []byte(src)
	tree := p.Parse(b, nil)
	return &Program{
		File: ConvertFile(tree.RootNode(), b, withPos, withComments),
		Src:  src,
	}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
