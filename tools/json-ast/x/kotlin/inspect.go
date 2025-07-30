package kotlin

import (
	"encoding/json"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/kotlin"
)

// Node represents a tree-sitter node.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Kotlin source file.
type Program struct {
	File *Node `json:"file"`
}

// Inspect parses Kotlin source code using tree-sitter and returns its AST.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(ts.GetLanguage())
	tree := p.Parse(nil, []byte(src))
	return &Program{File: toNode(tree.RootNode())}, nil
}

func toNode(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	node := &Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, toNode(child))
	}
	return node
}

// MarshalJSON ensures stable output for Program.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
