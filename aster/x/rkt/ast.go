//go:build slow

package rkt

import sitter "github.com/tree-sitter/go-tree-sitter"

// Node represents a simplified tree-sitter node.
// Only leaves that carry textual content populate the Text field. Position
// fields are optional and omitted from the JSON when zero so callers can choose
// whether to include them.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	End      int     `json:"end,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed aliases provide a slightly more structured view over Node while reusing
// the generic Node representation internally.  Only node kinds that appear in
// the golden tests are declared here.
type (
	ProgramNode Node
	Comment     Node
	Extension   Node
	LangName    Node
	List        Node
	Number      Node
	Keyword     Node
	Boolean     Node
	Quote       Node
	String      Node
	Symbol      Node
)

// convertProgram converts the tree-sitter root node into our Program
// representation. When withPos is false location fields remain zero so they are
// omitted from the marshalled JSON thanks to the `omitempty` tags.
func convertProgram(root *sitter.Node, src []byte, withPos bool) *Program {
	if root == nil {
		return &Program{}
	}
	return &Program{Root: (*ProgramNode)(convertNode(root, src, withPos))}
}

// convertNode converts a tree-sitter node into our Node representation.  Pure
// syntax leaves that do not carry text are skipped entirely to keep the
// resulting tree small.
func convertNode(n *sitter.Node, src []byte, withPos bool) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if withPos {
		sp := n.StartPosition()
		ep := n.EndPosition()
		node.Start = int(sp.Row) + 1
		node.End = int(ep.Row) + 1
		node.StartCol = int(sp.Column)
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := n.NamedChild(i)
		if c := convertNode(child, src, withPos); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if node.Text == "" && len(node.Children) == 0 {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "comment", "lang_name", "number", "string", "symbol", "list", "quote", "extension", "boolean", "keyword":
		return true
	default:
		return false
	}
}
