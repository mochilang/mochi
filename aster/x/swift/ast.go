//go:build slow

package swift

import (
	"strings"

	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node represents a minimal Swift AST node. Only nodes carrying a textual value
// keep their Text field. Position fields are optional and omitted from the
// resulting JSON when zero.
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

// Additional node types mirroring tree-sitter's Swift grammar. Only the kinds
// that appear in the tests are declared here so Program can expose a structured
// API without requiring the full grammar.
type (
	AdditiveExpression           Node
	ArrayType                    Node
	AsExpression                 Node
	Assignment                   Node
	Bang                         Node
	CallExpression               Node
	CallSuffix                   Node
	ClassBody                    Node
	ClassDeclaration             Node
	ComparisonExpression         Node
	ConjunctionExpression        Node
	ControlTransferStatement     Node
	DictionaryType               Node
	DirectlyAssignableExpression Node
	DisjunctionExpression        Node
	DoStatement                  Node
	EqualityExpression           Node
	ForStatement                 Node
	FunctionBody                 Node
	FunctionDeclaration          Node
	Identifier                   Node
	IfStatement                  Node
	ImportDeclaration            Node
	InfixExpression              Node
	InterpolatedExpression       Node
	LambdaFunctionType           Node
	LambdaFunctionTypeParameters Node
	LambdaLiteral                Node
	LambdaParameter              Node
	MultiplicativeExpression     Node
	NavigationExpression         Node
	NavigationSuffix             Node
	NilCoalescingExpression      Node
	OptionalType                 Node
	Parameter                    Node
	PostfixExpression            Node
	PrefixExpression             Node
	RangeExpression              Node
	Statements                   Node
	TernaryExpression            Node
	TupleExpression              Node
	TypeAnnotation               Node
	UserType                     Node
	ValueArgument                Node
	ValueArgumentLabel           Node
	ValueArguments               Node
	WhileStatement               Node
)

// convert transforms a tree-sitter node into the Go AST representation.
// The conversion is recursive and ignores anonymous children.
func convert(n *sitter.Node, src []byte, withPos bool, keepComments bool) *Node {
	if n == nil {
		return nil
	}
	out := &Node{Kind: n.Kind()}
	if withPos {
		start := n.StartPosition()
		end := n.EndPosition()
		out.Start = int(start.Row) + 1
		out.StartCol = int(start.Column)
		out.End = int(end.Row) + 1
		out.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(out.Kind) && (keepComments || out.Kind != "comment") {
			out.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	if n.ChildCount() > n.NamedChildCount() {
		switch out.Kind {
		case "additive_expression", "multiplicative_expression", "range_expression",
			"equality_expression", "comparison_expression", "conjunction_expression",
			"disjunction_expression", "nil_coalescing_expression", "as_expression",
			"infix_expression":
			if n.ChildCount() >= 3 {
				op := n.Child(1)
				out.Text = strings.TrimSpace(op.Utf8Text(src))
			}
		case "prefix_expression":
			if n.ChildCount() >= 2 && !n.Child(0).IsNamed() {
				op := n.Child(0)
				out.Text = strings.TrimSpace(op.Utf8Text(src))
			}
               case "control_transfer_statement", "class_declaration", "property_declaration", "value_argument":
                       if n.ChildCount() >= 1 && !n.Child(0).IsNamed() {
                               kw := n.Child(0)
                               out.Text = strings.TrimSpace(kw.Utf8Text(src))
                       } else if out.Kind == "property_declaration" && n.NamedChildCount() > 0 {
                               first := n.NamedChild(0)
                               kw := strings.TrimSpace(string(src[n.StartByte():first.StartByte()]))
                               if kw == "" {
                                       txt := strings.TrimSpace(first.Utf8Text(src))
                                       if txt == "var" || txt == "let" {
                                               out.Text = txt
                                       }
                               } else if kw == "var" || kw == "let" {
                                       out.Text = kw
                               }
                       }
               }
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(uint(i))
		if c := convert(child, src, withPos, keepComments); c != nil {
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
		"type_identifier", "comment", "bang", "boolean_literal",
		"value_argument":
		return true
	default:
		return false
	}
}

// ConvertFile converts the tree-sitter root node of a Swift file into a
// SourceFile AST value.
func ConvertFile(n *sitter.Node, src []byte, withPos bool, keepComments bool) *SourceFile {
	if n == nil {
		return nil
	}
	root := convert(n, src, withPos, keepComments)
	if root == nil {
		return nil
	}
	return &SourceFile{Node: *root}
}
