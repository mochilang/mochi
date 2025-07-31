//go:build slow

package scheme

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsscheme "github.com/tree-sitter/tree-sitter-scheme/bindings/go"
)

// Option controls how the AST is generated. When Positions is true the
// positional fields of the produced nodes are populated, otherwise they remain
// zero and are omitted from the JSON output.
type Option struct {
	Positions bool
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output
// ordering when serialising test data.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}

// Inspect parses Scheme source code using tree-sitter and returns a Program
// describing its syntax tree. Position information is omitted by default; use
// InspectWithOption to enable it.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithOption behaves like Inspect but allows callers to control whether
// positional information is included in the resulting AST.
func InspectWithOption(src string, opt Option) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tsscheme.Language()))
	data := []byte(src)
	prev := IncludePos
	IncludePos = opt.Positions
	defer func() { IncludePos = prev }()
	tree := p.Parse(data, nil)
	return convertProgram(tree.RootNode(), data), nil
}
