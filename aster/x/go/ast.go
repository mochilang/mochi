package gox

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// IncludePos controls whether positional information should be populated when
// converting tree-sitter nodes. When false (the default) the position fields
// remain zero and are omitted from the marshalled JSON due to the omitempty
// struct tags.
var IncludePos bool

// Option controls how the AST is generated. When Positions is true the
// Start/End fields of nodes are populated, otherwise they remain zero and are
// omitted from the marshalled JSON due to the `omitempty` tags.
type Option struct {
	Positions bool
}

// Node represents a simplified Go AST node converted from tree-sitter.
// Position fields are optional and omitted from the JSON when zero.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Enumerate the node kinds that appear in the tests so Program can expose a
// more structured API while still using Node internally.
type (
	ArgumentList             Node
	AssignmentStatement      Node
	BinaryExpression         Node
	Block                    Node
	CallExpression           Node
	Comment                  Node
	CompositeLiteral         Node
	DefaultCase              Node
	ExpressionList           Node
	ExpressionStatement      Node
	False                    Node
	FieldDeclaration         Node
	FieldDeclarationList     Node
	FieldIdentifier          Node
	FloatLiteral             Node
	ForClause                Node
	ForStatement             Node
	FuncLiteral              Node
	FunctionDeclaration      Node
	FunctionType             Node
	Identifier               Node
	IfStatement              Node
	ImportDeclaration        Node
	ImportSpec               Node
	ImportSpecList           Node
	IncStatement             Node
	IndexExpression          Node
	IntLiteral               Node
	InterfaceType            Node
	InterpretedStringLiteral Node
	KeyedElement             Node
	LiteralElement           Node
	LiteralValue             Node
	MapType                  Node
	MethodDeclaration        Node
	MethodElem               Node
	Nil                      Node
	PackageClause            Node
	PackageIdentifier        Node
	ParameterDeclaration     Node
	ParameterList            Node
	ParenthesizedExpression  Node
	PointerType              Node
	QualifiedType            Node
	RangeClause              Node
	ReturnStatement          Node
	SelectorExpression       Node
	ShortVarDeclaration      Node
	SliceExpression          Node
	SliceType                Node
	SourceFile               Node
	StructType               Node
	True                     Node
	TypeAssertionExpression  Node
	TypeCase                 Node
	TypeConversionExpression Node
	TypeDeclaration          Node
	TypeIdentifier           Node
	TypeSpec                 Node
	TypeSwitchStatement      Node
	UnaryExpression          Node
	VarDeclaration           Node
	VarSpec                  Node
	VariadicArgument         Node
)

func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "field_identifier", "package_identifier",
		"type_identifier", "int_literal", "interpreted_string_literal",
		"raw_string_literal", "float_literal", "rune_literal",
		"imaginary_literal", "nil", "true", "false", "comment":
		return true
	default:
		return false
	}
}

// toNode converts a tree-sitter node into our Node representation.
// toNode converts a tree-sitter node into our Node representation. When pos is
// true the positional fields are populated, otherwise they remain zero and are
// omitted from the resulting JSON output. Non-value leaf nodes are removed to
// keep the tree small.
func toNode(n *sitter.Node, src []byte, pos bool) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPosition()
	end := n.EndPosition()
	node := &Node{Kind: n.Kind()}
	if pos {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			node.Text = string(src[n.StartByte():n.EndByte()])
		} else {
			return nil
		}
	}

	if n.ChildCount() > n.NamedChildCount() {
		switch node.Kind {
		case "binary_expression":
			if n.ChildCount() >= 3 {
				op := n.Child(1)
				node.Text = string(src[op.StartByte():op.EndByte()])
			}
		case "unary_expression":
			if n.ChildCount() >= 1 {
				op := n.Child(0)
				node.Text = string(src[op.StartByte():op.EndByte()])
			}
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c := toNode(child, src, pos); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}
