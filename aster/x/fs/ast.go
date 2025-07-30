package fs

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node represents a F# AST node converted from tree-sitter.
// Node mirrors a tree-sitter node in a JSON-friendly form.
// Node represents a F# AST node converted from tree-sitter.
// Position fields are omitted when zero so they can be removed from the
// resulting JSON when not needed.
// Node mirrors a tree-sitter node in a JSON friendly form.  The positional
// fields are omitted from the output when zero so callers can decide whether to
// include them.
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

// File represents the root of an F# source file.
// The other types are simple aliases around Node so the resulting JSON mirrors
// the tree-sitter node kinds that actually appear in the golden tests.
type (
	File                  Node
	LineComment           Node
	TypeDefinition        Node
	RecordTypeDefn        Node
	TypeName              Node
	RecordFields          Node
	RecordField           Node
	Identifier            Node
	Int                   Node
	String                Node
	ValueDeclaration      Node
	FunctionOrValueDefn   Node
	ValueDeclarationLeft  Node
	IdentifierPattern     Node
	LongIdentifierOrOp    Node
	PostfixType           Node
	SimpleType            Node
	LongIdentifier        Node
	ApplicationExpression Node
	BraceExpression       Node
	Const                 Node
	FieldInitializers     Node
	FieldInitializer      Node
	ForExpression         Node
	ListExpression        Node
	ParenExpression       Node
	PrefixedExpression    Node
)

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
func convert(n *sitter.Node, src []byte, withPos bool) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPosition()
	end := n.EndPosition()

	node := &Node{Kind: n.Kind()}
	if withPos {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			text := n.Utf8Text(src)
			if n.Kind() == "identifier" {
				node.Name = text
			} else {
				node.Text = text
			}
			return node
		}
		// Drop non-value leaf nodes
		return nil
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := convert(n.NamedChild(i), src, withPos)
		if child != nil {
			node.Children = append(node.Children, *child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" && node.Name == "" {
		return nil
	}
	return node
}
