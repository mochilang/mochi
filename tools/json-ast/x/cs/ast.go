package cs

import sitter "github.com/smacker/go-tree-sitter"

// Node models a portion of the C# syntax tree as returned by tree-sitter.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed C# file.
type Program struct {
	File *Node `json:"file"`
}

// convert builds a Node tree starting from the given tree-sitter node.
func convert(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	node := &Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		c := n.NamedChild(i)
		node.Children = append(node.Children, convert(c))
	}
	return node
}
