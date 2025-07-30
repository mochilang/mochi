package scala

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node represents a simplified Scala AST node converted from tree-sitter.  Only
// leaf nodes that carry a textual value populate the Text field. Position
// fields are omitted from the marshalled JSON when zero.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed node aliases mirroring a subset of the Scala grammar.  Only node kinds
// that appear in the golden tests are declared here so the resulting Program
// exposes a more structured API without embedding the full grammar.
type (
	CompilationUnit    struct{ Node }
	Comment            struct{ Node }
	ImportDeclaration  struct{ Node }
	NamespaceSelectors struct{ Node }
	ObjectDefinition   struct{ Node }
	TemplateBody       struct{ Node }
	FunctionDefinition struct{ Node }
	Parameters         struct{ Node }
	Parameter          struct{ Node }
	GenericType        struct{ Node }
	TypeArguments      struct{ Node }
	TypeIdentifier     struct{ Node }
	Identifier         struct{ Node }
	CallExpression     struct{ Node }
	Arguments          struct{ Node }
	Block              struct{ Node }
	String             struct{ Node }
	IntegerLiteral     struct{ Node }
)

// Option controls how the AST is generated.
type Option struct {
	// WithPositions requests inclusion of positional information when true.
	WithPositions bool
}

// Program wraps a parsed Scala file for JSON serialization.
type Program struct {
	Root CompilationUnit `json:"root"`
}

// convert transforms a tree-sitter node into our Node representation.  Non-value
// leaf nodes are skipped entirely so that the resulting JSON focuses on
// meaningful tokens.
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
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := convert(n.NamedChild(uint(i)), src, withPos)
		if child != nil {
			node.Children = append(node.Children, child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether the given kind represents a leaf node carrying a
// meaningful value that should be preserved in the JSON output.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "type_identifier", "integer_literal", "string", "comment":
		return true
	default:
		return false
	}
}
