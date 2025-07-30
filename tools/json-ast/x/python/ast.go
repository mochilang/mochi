package python

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a tree-sitter node in a generic form.
// Node represents a simplified Python AST node converted from tree-sitter.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start"`
	StartCol int     `json:"startCol"`
	End      int     `json:"end"`
	EndCol   int     `json:"endCol"`
	Children []*Node `json:"children,omitempty"`
}

// toNode converts a tree-sitter node to a Node.
// toNode converts a tree-sitter node into our Node structure. It filters out
// pure syntax leaves to keep the JSON representation small.
func toNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{
		Kind:     n.Type(),
		Start:    int(n.StartByte()),
		StartCol: int(start.Column),
		End:      int(n.EndByte()),
		EndCol:   int(end.Column),
	}

	if isValueNode(n.Type()) {
		node.Text = n.Content(src)
		return node
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c := toNode(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether a node kind should appear as a leaf with text in
// the final JSON representation.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "integer", "string", "comment":
		return true
	default:
		return false
	}
}
