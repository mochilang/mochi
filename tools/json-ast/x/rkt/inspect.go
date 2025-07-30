package rkt

import (
	sitter "github.com/smacker/go-tree-sitter"
	racket "github.com/tree-sitter/tree-sitter-racket/bindings/go"
)

// Node mirrors a tree-sitter node in a simplified form.
type Node struct {
	Type     string  `json:"type"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Racket source file.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses Racket source code using tree-sitter and returns a Program
// describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(racket.Language()))
	tree := p.Parse(nil, []byte(src))
	return &Program{Root: convert(tree.RootNode())}, nil
}

func convert(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Type: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, convert(child))
	}
	return node
}
