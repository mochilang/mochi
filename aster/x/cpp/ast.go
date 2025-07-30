package cpp

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node represents a tree-sitter node in a generic form with position
// information. Leaf nodes also carry their text content.
// Node is a minimal representation of a C++ AST node. It records only
// the node kind and textual value for leaves, keeping the structure
// lightweight.
// Node represents a simplified C++ AST node. Only nodes that carry a value
// are kept as leaves. Positional information follows the common Start/End
// scheme using both byte offsets and columns.
// Node represents a simplified C++ AST node. Only leaves that carry user
// facing values store their source text in Text. Position fields are
// optional and omitted from the JSON output when zero.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	End      int     `json:"end,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed aliases expose a slightly richer structure when composing a Program.
// Only node kinds that either hold useful values or appear in the golden tests
// are enumerated here. Additional kinds are represented by Node at runtime.
type (
	TranslationUnit        Node
	PreprocInclude         Node
	FunctionDefinition     Node
	FunctionDeclarator     Node
	CompoundStatement      Node
	ExpressionStatement    Node
	ReturnStatement        Node
	CallExpression         Node
	ArgumentList           Node
	BinaryExpression       Node
	QualifiedIdentifier    Node
	NamespaceIdentifier    Node
	FieldIdentifier        Node
	PrimitiveType          Node
	TypeIdentifier         Node
	Identifier             Node
	NumberLiteral          Node
	CharLiteral            Node
	StringLiteral          Node
	StringContent          Node
	EscapeSequence         Node
	Auto                   Node
	Character              Node
	LambdaCaptureSpecifier Node
	Comment                Node
)

// Options controls how the AST is produced.
type Options struct {
	// WithPositions toggles whether position fields are populated.
	// When false (the default) position fields are left zero and
	// therefore omitted from the JSON output via omitempty tags.
	WithPositions bool
}

// convert turns a tree-sitter node into our Node representation.
// convert turns a tree-sitter node into our Node representation.
// Position information is added only when opts.WithPositions is true.
func convert(n *sitter.Node, src []byte, opts Options) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if opts.WithPositions {
		start := n.StartPosition()
		end := n.EndPosition()
		node.Start = int(start.Row) + 1
		node.End = int(end.Row) + 1
		node.StartCol = int(start.Column)
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) || n.Kind() == "comment" {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c := convert(child, src, opts); c != nil {
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
		"string_content", "escape_sequence", "field_identifier",
		"namespace_identifier", "auto", "character", "lambda_capture_specifier":
		return true
	default:
		return false
	}
}
