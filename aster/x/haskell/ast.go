package haskell

import sitter "github.com/tree-sitter/go-tree-sitter"

// Node represents a tree-sitter node.
// Leaf nodes record their source text in the Text field while
// inner nodes only keep their kind and byte offsets.
// Node represents a simplified Haskell AST node derived from tree-sitter.
// Position information is stored as 1-indexed line numbers and 0-indexed
// columns to match other language implementations in this package.
// Only leaf nodes that carry a value store their source text in the Text field
// to keep the resulting JSON minimal.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start,omitempty"`
	StartCol int    `json:"startCol,omitempty"`
	End      int    `json:"end,omitempty"`
	EndCol   int    `json:"endCol,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Option controls how the AST is generated.
type Option struct {
	// Positions requests inclusion of positional information when true.
	Positions bool
}

// Typed aliases for the node kinds that appear in the generated JSON.  These
// give a slightly more structured view of the AST while still using Node under
// the hood.
type (
	Haskell           Node
	Pragma            Node
	Imports           Node
	Import            Node
	Module            Node
	ModuleID          Node
	ImportList        Node
	ImportName        Node
	Comment           Node
	Declarations      Node
	DataType          Node
	DataConstructors  Node
	DataConstructor   Node
	Record            Node
	Constructor       Node
	Fields            Node
	Field             Node
	FieldName         Node
	FieldUpdate       Node
	Name              Node
	Variable          Node
	Integer           Node
	String            Node
	Operator          Node
	Unit              Node
	Literal           Node
	List              Node
	Tuple             Node
	ListComprehension Node
	Match             Node
	Guards            Node
	Bind              Node
	Let               Node
	LocalBinds        Node
	Generator         Node
	Patterns          Node
	Apply             Node
	Projection        Node
	Exp               Node
	Infix             Node
	InfixID           Node
	Lambda            Node
	Do                Node
	Signature         Node
	Parens            Node
	Qualifiers        Node
	Qualified         Node
	As                Node
)

// convert transforms a tree-sitter node into the Node structure defined above.
// Only named children are traversed to keep the result compact. Position
// information is recorded when opt.Positions is true.
func convert(n *sitter.Node, src []byte, opt Option) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if opt.Positions {
		start := n.StartPosition()
		end := n.EndPosition()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if !isValueNode(n.Kind()) {
			// skip syntax-only leaves
			return nil
		}
		node.Text = n.Utf8Text(src)
		return node
	}

	for i := uint(0); i < n.ChildCount(); i++ {
		c := n.Child(i)
		if !c.IsNamed() {
			kind := c.Kind()
			switch kind {
			case "hiding", "qualified", "as":
				node.Children = append(node.Children, Node{Kind: kind, Text: c.Utf8Text(src)})
				continue
			default:
				continue
			}
		}
		child := convert(c, src, opt)
		if child != nil {
			node.Children = append(node.Children, *child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether a leaf node of the given kind carries user
// meaningful source text. Only these kinds are kept in the resulting AST.
func isValueNode(kind string) bool {
	switch kind {
	case "comment", "constructor", "integer", "module_id", "name",
		"operator", "pragma", "string", "unit", "variable":
		return true
	default:
		return false
	}
}
