package rs

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a simplified Rust AST node converted from tree-sitter.
// Leaves that carry semantic value keep their text while punctuation-only nodes
// are omitted. Positional information is optional and encoded only when
// explicitly requested.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	End      int     `json:"end,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed aliases for the node kinds that appear in the golden tests. These
// mirror the language structures while reusing Node internally.
type (
	SourceFile           Node
	LineComment          Node
	FunctionItem         Node
	Block                Node
	LetDeclaration       Node
	Identifier           Node
	FieldIdentifier      Node
	TypeIdentifier       Node
	PrimitiveType        Node
	IntegerLiteral       Node
	StringContent        Node
	StringLiteral        Node
	EscapeSequence       Node
	MutableSpecifier     Node
	Arguments            Node
	Parameters           Node
	Parameter            Node
	FieldDeclaration     Node
	FieldDeclarationList Node
	FieldExpression      Node
	FieldInitializer     Node
	FieldInitializerList Node
	MacroInvocation      Node
	TokenTree            Node
	GenericType          Node
	TypeArguments        Node
	ScopedIdentifier     Node
	ScopedTypeIdentifier Node
	ReferenceExpression  Node
	ReferenceType        Node
	Self                 Node
	SelfParameter        Node
)

// Program represents a parsed Rust source file composed of AST nodes.
type Program struct {
	Root *SourceFile `json:"root"`
}

// convert turns a tree-sitter node into our Node representation. The includePos
// flag controls whether positional information is recorded. Non semantic leaves
// (punctuation) are omitted to keep the tree compact.
func convert(n *sitter.Node, src []byte, includePos bool) *Node {
	if n == nil {
		return nil
	}

	node := &Node{Kind: n.Type()}
	if includePos {
		sp := n.StartPoint()
		ep := n.EndPoint()
		node.Start = int(sp.Row) + 1
		node.End = int(ep.Row) + 1
		node.StartCol = int(sp.Column)
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			node.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := convert(n.NamedChild(i), src, includePos)
		if child != nil {
			node.Children = append(node.Children, child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}

	return node
}

// isValueNode reports whether the given kind represents a leaf node that
// carries meaningful text and should therefore be kept in the JSON output.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "field_identifier", "type_identifier", "primitive_type",
		"integer_literal", "string_content", "escape_sequence", "mutable_specifier",
		"arguments", "parameters", "self", "token_tree", "line_comment":
		return true
	default:
		return false
	}
}
