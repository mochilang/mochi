package fs

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a F# AST node converted from tree-sitter.
// Node mirrors a tree-sitter node in a JSON-friendly form.
// Node represents a F# AST node converted from tree-sitter.
// Position fields are omitted when zero so they can be removed from the
// resulting JSON when not needed.
type Node struct {
	Kind     string `json:"kind"`
	Name     string `json:"name,omitempty"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start,omitempty"`
	StartCol int    `json:"startCol,omitempty"`
	End      int    `json:"end,omitempty"`
	EndCol   int    `json:"endCol,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// isValueNode reports whether the given tree-sitter node kind represents a
// value leaf node. Only these kinds will retain their textual content.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "int", "string", "line_comment":
		return true
	default:
		return false
	}
}

// convertNode recursively converts a tree-sitter Node into our Node structure.
func convertNode(n *sitter.Node, src []byte, withPos bool) Node {
	start := n.StartPoint()
	end := n.EndPoint()

	node := Node{Kind: n.Type()}
	if withPos {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			text := n.Content(src)
			if n.Type() == "identifier" {
				node.Name = text
			} else {
				node.Text = text
			}
			return node
		}
		// Drop non-value leaf nodes
		return Node{}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		c := convertNode(child, src, withPos)
		if c.Name == "" && c.Text == "" && len(c.Children) == 0 {
			continue
		}
		node.Children = append(node.Children, c)
	}
	return node
}
