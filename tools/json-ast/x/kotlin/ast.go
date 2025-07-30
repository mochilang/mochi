package kotlin

import (
	sitter "github.com/smacker/go-tree-sitter"
	"strings"
)

// Node represents a simplified Kotlin AST node containing only semantic
// information. Leaf nodes are kept only when they carry a textual value.
// Node represents a simplified Kotlin AST node. Only leaves that carry a
// textual value populate the Text field. Position information can be omitted to
// keep the output compact.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start,omitempty"`
	StartCol int    `json:"startCol,omitempty"`
	End      int    `json:"end,omitempty"`
	EndCol   int    `json:"endCol,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// SourceFile is the root of a Kotlin program.
type SourceFile struct{ Node }

// isValueLeaf reports whether the given tree-sitter node represents a leaf that
// carries user visible text we want to keep in the AST.
func isValueLeaf(n *sitter.Node) bool {
	if n.NamedChildCount() != 0 {
		return false
	}
	switch n.Type() {
	case "simple_identifier", "type_identifier", "integer_literal",
		"string_literal", "string_content":
		return true
	}
	if strings.HasSuffix(n.Type(), "_identifier") {
		return true
	}
	if strings.HasSuffix(n.Type(), "_literal") {
		return true
	}
	return false
}

// convert recursively converts a tree-sitter node into our Node representation.
// Nodes that do not carry values and have no meaningful children are omitted to
// keep the JSON output compact.
func convert(n *sitter.Node, src []byte, withPos bool) Node {
	node := Node{Kind: n.Type()}
	if withPos {
		start := n.StartPoint()
		end := n.EndPoint()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if isValueLeaf(n) {
		node.Text = n.Content(src)
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		c := convert(child, src, withPos)
		// Skip nodes without text and children to remove non-value leaves.
		if c.Text == "" && len(c.Children) == 0 {
			continue
		}
		node.Children = append(node.Children, c)
	}
	return node
}
