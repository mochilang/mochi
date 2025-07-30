//go:build slow

package kotlin

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	"strings"
)

// Node represents a simplified Kotlin AST node containing only semantic
// information. Leaf nodes are kept only when they carry a textual value.
// Node represents a simplified Kotlin AST node. Only leaves that carry a
// textual value populate the Text field. Position information can be omitted to
// keep the output compact.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start,omitempty"`
	StartCol int    `json:"startCol,omitempty"`
	End      int    `json:"end,omitempty"`
	EndCol   int    `json:"endCol,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Typed node aliases mirroring the Kotlin grammar. Only the node kinds that
// appear in the golden test are defined here so Program can expose a more
// structured API without embedding the full grammar.
type (
	AnnotatedLambda      Node
	CallExpression       Node
	CallSuffix           Node
	ControlStructureBody Node
	ForStatement         Node
	FunctionBody         Node
	FunctionDeclaration  Node
	IndexingExpression   Node
	IndexingSuffix       Node
	InfixExpression      Node
	IntegerLiteral       Node
	LambdaLiteral        Node
	NavigationExpression Node
	NavigationSuffix     Node
	PropertyDeclaration  Node
	SimpleIdentifier     Node
	Statements           Node
	StringContent        Node
	StringLiteral        Node
	TypeArguments        Node
	TypeIdentifier       Node
	TypeProjection       Node
	UserType             Node
	ValueArgument        Node
	ValueArguments       Node
	VariableDeclaration  Node
)

// SourceFile is the root of a Kotlin program.
type SourceFile struct{ Node }

// isValueLeaf reports whether the given tree-sitter node represents a leaf that
// carries user visible text we want to keep in the AST.
func isValueLeaf(n *sitter.Node) bool {
	if n.NamedChildCount() != 0 {
		return false
	}
	switch n.Kind() {
	case "simple_identifier", "type_identifier", "integer_literal",
		"string_literal", "string_content":
		return true
	}
	if strings.HasSuffix(n.Kind(), "_identifier") {
		return true
	}
	if strings.HasSuffix(n.Kind(), "_literal") {
		return true
	}
	return false
}

// convert recursively converts a tree-sitter node into our Node representation.
// Nodes that do not carry values and have no meaningful children are omitted to
// keep the JSON output compact.
func convert(n *sitter.Node, src []byte, withPos bool) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if withPos {
		start := n.StartPosition()
		end := n.EndPosition()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueLeaf(n) {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := convert(n.NamedChild(uint(i)), src, withPos)
		if child != nil {
			node.Children = append(node.Children, *child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}
