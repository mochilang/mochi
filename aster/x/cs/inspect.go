package cs

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	csharp "github.com/tree-sitter/tree-sitter-c-sharp/bindings/go"
)

// Program is defined in ast.go and composes Node values.

// Inspect parses the given C# source code using tree-sitter and returns a
// Program describing its syntax tree. Position information is included only when
// IncludePositions is set to true.
type Option struct {
	Positions bool
}

func Inspect(src string) (*Program, error) { return InspectWithOption(src, Option{}) }

func InspectWithOption(src string, opt Option) (*Program, error) {
	IncludePositions = opt.Positions
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(csharp.Language()))
	data := []byte(src)
	tree := p.Parse(data, nil)
	return &Program{
		File: (*CompilationUnit)(convert(tree.RootNode(), data)),
	}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
