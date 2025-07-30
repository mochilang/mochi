package c

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node represents a tree-sitter node with byte offsets and optional text.
// Node represents a simplified syntax tree node. Only nodes that carry useful
// textual values are kept in the tree. Position information is stored using
// line/column pairs for easier cross language comparison.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed aliases give a clearer structure when composing a Program. Only node
// kinds that hold values or appear in the golden tests are enumerated here.
// Additional kinds will still be represented by Node at runtime.
type (
	TranslationUnit      Node
	PreprocInclude       Node
	Declaration          Node
	FunctionDefinition   Node
	ParameterDeclaration Node
	ParameterList        Node
	CompoundStatement    Node
	ExpressionStatement  Node
	ReturnStatement      Node
	CallExpression       Node
	AssignmentExpression Node
	BinaryExpression     Node
	IfStatement          Node
	ForStatement         Node
	WhileStatement       Node
	StructSpecifier      Node
	Identifier           Node
	NumberLiteral        Node
	CharLiteral          Node
	StringLiteral        Node
	SystemLibString      Node
	PrimitiveType        Node
	TypeIdentifier       Node
	StringContent        Node
	EscapeSequence       Node
	Comment              Node
)

// convert transforms a tree-sitter node into a *Node. When pos is true the
// resulting node includes positional information.
func convert(n *sitter.Node, src []byte, pos bool) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if pos {
		start := n.StartPosition()
		end := n.EndPosition()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			node.Text = n.Utf8Text(src)
		} else if node.Kind == "comment" {
			node.Text = n.Utf8Text(src)
		} else {
			// skip pure syntax leaves
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(uint(i))
		if child == nil {
			continue
		}
		if c := convert(child, src, pos); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "number_literal", "char_literal", "string_literal",
		"system_lib_string", "primitive_type", "type_identifier",
		"string_content", "escape_sequence", "comment":
		return true
	default:
		return false
	}
}
