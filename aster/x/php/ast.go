package php

import (
	"strings"

	sitter "github.com/tree-sitter/go-tree-sitter"
	phpts "github.com/tree-sitter/tree-sitter-php/bindings/go"
)

// Node represents a tree-sitter node in PHP's syntax tree.
// Node is a lightweight representation of a PHP AST node. Only
// semantically relevant nodes are kept. Leaf nodes contain their
// source text in the Text field.
// Node represents a compact AST node derived from tree-sitter.
//
// Start and End are 1-indexed line numbers. StartCol and EndCol are
// 0-indexed column offsets. Leaf nodes that carry semantic value store
// their source text in the Text field. Nodes without semantic value and
// no children are omitted during conversion to keep the JSON minimal.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed aliases mirror the tree-sitter named nodes that appear in the
// converted AST.  They allow Program to expose a slightly more structured API
// without duplicating the Node fields for each kind.
type (
	// ProgramNode represents the root of a PHP source file.
	ProgramNode                         Node
	AnonymousFunctionCreationExpression Node
	AnonymousFunctionUseClause          Node
	Argument                            Node
	Arguments                           Node
	ArrayCreationExpression             Node
	ArrayElementInitializer             Node
	ArrowFunction                       Node
	AssignmentExpression                Node
	AugmentedAssignmentExpression       Node
	BinaryExpression                    Node
	CastExpression                      Node
	CompoundStatement                   Node
	ConditionalExpression               Node
	EchoStatement                       Node
	ElseClause                          Node
	EncapsedString                      Node
	ExpressionStatement                 Node
	Float                               Node
	ForStatement                        Node
	ForeachStatement                    Node
	FormalParameters                    Node
	FunctionCallExpression              Node
	FunctionDefinition                  Node
	GlobalDeclaration                   Node
	IfStatement                         Node
	Integer                             Node
	ListLiteral                         Node
	MatchBlock                          Node
	MatchConditionList                  Node
	MatchConditionalExpression          Node
	MatchDefaultExpression              Node
	MatchExpression                     Node
	Name                                Node
	Pair                                Node
	ParenthesizedExpression             Node
	ReturnStatement                     Node
	SequenceExpression                  Node
	SimpleParameter                     Node
	String                              Node
	StringContent                       Node
	SubscriptExpression                 Node
	UnaryOpExpression                   Node
	UpdateExpression                    Node
	VariableName                        Node
	WhileStatement                      Node
)

// Options configures how the AST is generated.
type Options struct {
	// Positions controls whether location information is populated.
	Positions bool
}

// convert converts a tree-sitter node to our Node structure.
// convert transforms a tree-sitter node into our Node representation.
// Only named (semantic) children are included to keep the AST compact.
func convert(n *sitter.Node, src []byte, opts Options) *Node {
	if n == nil {
		return nil
	}

	node := &Node{Kind: n.Kind()}
	if opts.Positions {
		start := n.StartPosition()
		end := n.EndPosition()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) || keepEmptyNode(n.Kind()) {
			text := n.Utf8Text(src)
			if strings.TrimSpace(text) == "" {
				return nil
			}
			node.Text = text
		} else {
			switch n.Kind() {
			case "break_statement", "continue_statement":
				// keep empty node to round-trip control flow
			default:
				return nil
			}
		}
	}

	// capture operator tokens for certain node kinds
	switch n.Kind() {
	case "binary_expression", "assignment_expression", "augmented_assignment_expression",
		"unary_op_expression", "update_expression", "conditional_expression", "array_element_initializer":
		for i := uint(0); i < n.ChildCount(); i++ {
			c := n.Child(i)
			if c != nil && !c.IsNamed() {
				tok := strings.TrimSpace(c.Utf8Text(src))
				if tok != "" {
					if node.Text == "" {
						node.Text = tok
					} else {
						node.Text += " " + tok
					}
				}
			}
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(uint(i))
		if child == nil {
			continue
		}
		if c := convert(child, src, opts); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" && node.Kind != "break_statement" && node.Kind != "continue_statement" && !keepEmptyNode(node.Kind) {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "name", "variable_name", "integer", "float", "string", "string_content", "comment", "encapsed_string":
		return true
	default:
		return false
	}
}

func keepEmptyNode(kind string) bool {
	switch kind {
	case "arguments", "formal_parameters":
		return true
	default:
		return false
	}
}

// newParser returns a tree-sitter parser for PHP.
func newParser() *sitter.Parser {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(phpts.LanguagePHP()))
	return p
}
