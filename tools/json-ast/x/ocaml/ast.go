package ocaml

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node mirrors a tree-sitter node in a JSON-friendly form.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// toNode recursively converts a tree-sitter node into a Node.
func toNode(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	out := &Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		out.Children = append(out.Children, toNode(child))
	}
	return out
}
