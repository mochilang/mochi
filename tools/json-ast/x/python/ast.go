package python

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a tree-sitter node in a generic form.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Value    string  `json:"value,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// toNode converts a tree-sitter node to a Node.
func toNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	out := &Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
		Value: n.Content(src),
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		out.Children = append(out.Children, toNode(child, src))
	}
	return out
}
