//go:build slow

package fs

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	fsharp "github.com/tree-sitter/tree-sitter-fsharp/bindings/go"
)

// Program represents a parsed F# source file.
// Program represents a parsed F# source file. The Root field mirrors the tree
// sitter "file" node.
type Program struct {
	Root *File `json:"root"`
}

// Inspect parses the given F# source code using tree-sitter. Positional fields
// are omitted unless IncludePositions is set to true.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(fsharp.LanguageFSharp()))
	tree := parser.Parse([]byte(src), nil)
	root := convert(tree.RootNode(), []byte(src))
	if root == nil {
		return &Program{}, nil
	}
	return &Program{Root: (*File)(root)}, nil
}

// InspectWithPositions parses the source and populates position fields in the
// resulting AST.
func InspectWithPositions(src string) (*Program, error) {
	IncludePositions = true
	defer func() { IncludePositions = false }()
	return Inspect(src)
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
