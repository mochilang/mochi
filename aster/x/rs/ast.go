package rs

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node represents a simplified Rust AST node converted from tree-sitter.
// Leaves that carry semantic value keep their text while punctuation-only nodes
// are omitted. Positional information is optional and encoded only when
// explicitly requested.
// Option controls how the AST is produced.
// When Positions is false (the default) all position fields remain zero and are
// omitted from the marshalled JSON thanks to the `omitempty` struct tags.
type Option struct {
	Positions bool
}

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
	AttributeItem        Node
	Attribute            Node
	CallExpression       Node
	DeclarationList      Node
	ExpressionStatement  Node
	ForExpression        Node
	ImplItem             Node
	Lifetime             Node
	StructExpression     Node
	StructItem           Node
	TryExpression        Node
)

// Program represents a parsed Rust source file composed of AST nodes.
type Program struct {
	Root *SourceFile `json:"root"`
}

// convertProgram converts the root tree-sitter node into a Program. When
// opt.Positions is false the position fields remain zero and are omitted from
// the JSON output.
func convertProgram(root *sitter.Node, src []byte, opt Option) *Program {
	n := convert(root, src, opt)
	if n == nil {
		return &Program{}
	}
	return &Program{Root: (*SourceFile)(n)}
}

// convert transforms a tree-sitter node into our Node representation. Non
// semantic leaves (keywords, punctuation) are dropped. Position information is
// recorded when opt.Positions is true.
func convert(n *sitter.Node, src []byte, opt Option) *Node {
	if n == nil {
		return nil
	}

	node := &Node{Kind: n.Kind()}
	if opt.Positions {
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

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := convert(n.NamedChild(uint(i)), src, opt)
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
