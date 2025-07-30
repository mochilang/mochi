package rs

import (
	"encoding/json"

	sitter "github.com/smacker/go-tree-sitter"
	rust "github.com/smacker/go-tree-sitter/rust"
)

// Node represents a node in the Rust syntax tree.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Rust source file.
type Program struct {
	File *Node `json:"file"`
}

// Inspect parses the given Rust source code using tree-sitter and returns
// a Program describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(rust.GetLanguage())
	tree := p.Parse(nil, []byte(src))
	return &Program{File: toNode(tree.RootNode())}, nil
}

func toNode(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	out := &Node{Kind: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		out.Children = append(out.Children, toNode(child))
	}
	return out
}

// MarshalJSON implements json.Marshaler for Program, to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
