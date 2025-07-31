package rb

import "encoding/json"

// Program represents a parsed Ruby source file. The AST is rooted at a
// ProgramNode which embeds Node. This mirrors the structure returned by the
// parser while providing a slightly richer type hierarchy for use by the
// printer and tests.
type Program struct {
	Root *ProgramNode `json:"root"`
}

// Inspect parses the given Ruby source code and returns a Program describing
// its AST. Position information is omitted by default; use InspectWithOption to
// enable it.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithOption behaves like Inspect but allows callers to specify whether
// position information should be included in the resulting AST.
func InspectWithOption(src string, opt Option) (*Program, error) {
	prev := IncludePositions
	IncludePositions = opt.Positions
	defer func() { IncludePositions = prev }()

	node, err := Parse(src)
	if err != nil {
		return nil, err
	}
	if node == nil {
		return &Program{Root: nil}, nil
	}
	root := &ProgramNode{Node: *node}
	return &Program{Root: root}, nil
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
