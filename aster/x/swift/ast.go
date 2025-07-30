//go:build slow

package swift

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// IncludePositions controls whether converted AST nodes include position
// information. When false the positional fields are left zero and omitted from
// the resulting JSON thanks to `omitempty`.
var IncludePositions bool

// Node represents a minimal Swift AST node. Only nodes carrying a textual value
// keep their Text field. Source positions are optional and controlled by the
// IncludePositions flag.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Comment represents a Swift comment node.
type Comment struct{ Node }

// PropertyDeclaration models a Swift property declaration.
type PropertyDeclaration struct{ Node }

// Pattern represents a binding pattern.
type Pattern struct{ Node }

// SimpleIdentifier represents an identifier leaf node.
type SimpleIdentifier struct{ Node }

// ArrayLiteral represents an array literal expression.
type ArrayLiteral struct{ Node }

// DictionaryLiteral represents a dictionary literal expression.
type DictionaryLiteral struct{ Node }

// LineStringLiteral represents a string literal.
type LineStringLiteral struct{ Node }

// LineStrText represents the textual part of a string literal.
type LineStrText struct{ Node }

// IntegerLiteral represents an integer literal.
type IntegerLiteral struct{ Node }

// SourceFile is the root of a Swift AST.
type SourceFile struct{ Node }

// convertNode transforms a tree-sitter node into the Go AST representation.
// The conversion is recursive and ignores anonymous children.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	out := &Node{Kind: n.Kind()}
	if IncludePositions {
		start := n.StartPosition()
		end := n.EndPosition()
		out.Start = int(start.Row) + 1
		out.StartCol = int(start.Column)
		out.End = int(end.Row) + 1
		out.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(out.Kind) {
			out.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(uint(i))
		if c := convertNode(child, src); c != nil {
			out.Children = append(out.Children, c)
		}
	}

	if len(out.Children) == 0 && out.Text == "" {
		return nil
	}
	return out
}

// isValueNode reports whether the given kind represents a leaf node that holds
// semantic text which should be preserved in the JSON output.
func isValueNode(kind string) bool {
	switch kind {
	case "simple_identifier", "integer_literal", "line_str_text",
		"type_identifier", "comment", "bang":
		return true
	default:
		return false
	}
}

// ConvertFile converts the tree-sitter root node of a Swift file into a
// SourceFile AST value.
func ConvertFile(n *sitter.Node, src []byte) *SourceFile {
	if n == nil {
		return nil
	}
	root := convertNode(n, src)
	if root == nil {
		return nil
	}
	return &SourceFile{Node: *root}
}
