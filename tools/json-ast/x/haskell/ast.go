package haskell

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a tree-sitter node.
// Leaf nodes record their source text in the Text field while
// inner nodes only keep their kind and byte offsets.
// Node represents a simplified Haskell AST node derived from tree-sitter.
// Position information is stored as 1-indexed line numbers and 0-indexed
// columns to match other language implementations in this package.
// Only leaf nodes that carry a value store their source text in the Text field
// to keep the resulting JSON minimal.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start"`
	StartCol int    `json:"startCol"`
	End      int    `json:"end"`
	EndCol   int    `json:"endCol"`
	Children []Node `json:"children,omitempty"`
}

// convert transforms a tree-sitter node into the Node structure defined above.
// Only named children are traversed to keep the result compact.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{
		Kind:     n.Type(),
		Start:    int(start.Row) + 1,
		StartCol: int(start.Column),
		End:      int(end.Row) + 1,
		EndCol:   int(end.Column),
	}

	if n.NamedChildCount() == 0 {
		if !isValueNode(n.Type()) {
			// skip syntax-only leaves
			return nil
		}
		node.Text = n.Content(src)
		return node
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := convert(n.NamedChild(i), src)
		if child != nil {
			node.Children = append(node.Children, *child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether a leaf node of the given kind carries user
// meaningful source text. Only these kinds are kept in the resulting AST.
func isValueNode(kind string) bool {
	switch kind {
	case "comment", "constructor", "integer", "module_id", "name",
		"operator", "pragma", "string", "unit", "variable":
		return true
	default:
		return false
	}
}
