package lua

import (
	"strings"

	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node is a minimal representation of a tree-sitter node. Only leaf nodes store
// their source text in the Text field. Internal nodes simply record their kind
// and children. This keeps the generated JSON small while still containing all
// information required to fully reconstruct the syntax tree.
// Node mirrors a tree-sitter node in a compact form suitable for JSON
// serialisation. Only leaves that carry a value keep their text to keep the
// output small. Position fields are omitted when zero so callers can opt out of
// them.
// Node mirrors a tree-sitter node in a compact form suitable for JSON
// serialisation. Position fields are omitted when zero so callers can choose
// whether to include them via the Option argument in Inspect.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Option controls how the AST is generated.
// When Positions is true the Start/End fields of nodes are populated,
// otherwise they remain zero and are omitted from the JSON output.
type Option struct {
	Positions bool
}

// The following aliases give a slightly more structured view of the Lua syntax
// tree while still using Node under the hood. Only node kinds that appear in
// the golden tests are defined here.
type (
	ProgramNode         Node
	Comment             Node
	Field               Node
	FieldList           Node
	ForGeneric          Node
	ForNumeric          Node
	ForStatement        Node
	Function            Node
	FunctionArguments   Node
	FunctionBody        Node
	FunctionCall        Node
	FunctionName        Node
	FunctionStatement   Node
	Identifier          Node
	IdentifierList      Node
	IfStatement         Node
	Number              Node
	ParameterList       Node
	ReturnStatement     Node
	String              Node
	TableConstructor    Node
	UnaryOperation      Node
	VariableDeclaration Node
	VariableDeclarator  Node
	BinaryOperation     Node
)

// convert recursively converts a tree-sitter node into our Node representation.
// Pure syntax leaves (keywords, punctuation) are dropped so the resulting JSON
// stays compact.
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
			return nil
		}
		text := n.Utf8Text(src)
		if strings.TrimSpace(text) == "" {
			return nil
		}
		node.Text = text
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if cn := convert(child, src, opt); cn != nil {
			node.Children = append(node.Children, cn)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// convertProgram converts the tree-sitter root node into a Program structure.
func convertProgram(root *sitter.Node, src []byte, opt Option) *Program {
	n := convert(root, src, opt)
	if n == nil {
		return &Program{}
	}
	pn := ProgramNode(*n)
	return &Program{Root: &pn}
}

// isValueNode reports whether the given node kind should be kept when it is a
// leaf node. Keywords and punctuation are discarded to keep the JSON minimal.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "number", "string", "comment":
		return true
	default:
		return false
	}
}
