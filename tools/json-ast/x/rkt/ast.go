package rkt

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a tree-sitter node in a minimal form. Only nodes that
// contain textual data carry a Text value while the structural information is
// expressed through the Kind and the list of Children.  This keeps the JSON
// representation compact while still exposing all information from the parse
// tree.
// Node represents a node in the simplified AST.  Only leaves that carry
// meaningful text populate the Text field.  Position fields are optional and
// omitted when zero so the JSON output can be compact when locations are not
// required.
// Node represents a node in the simplified AST.  Only leaves that carry
// textual information populate the Text field.  Position fields are optional
// and omitted from the JSON when zero so callers can choose whether to include
// them.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	End      int     `json:"end,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed aliases provide a slightly more structured view over Node while reusing
// the generic Node representation internally.  Only node kinds that appear in
// the golden tests are declared here.
type (
	ProgramNode Node
	Comment     Node
	Extension   Node
	LangName    Node
	List        Node
	Number      Node
	Quote       Node
	String      Node
	Symbol      Node
)

// convertProgram converts the root tree-sitter node into a Program.
// convertProgram converts the tree-sitter root node into our Program
// representation. When withPos is false location fields will be left zero so
// that they disappear from the marshalled JSON thanks to the omitempty tags.
func convertProgram(root *sitter.Node, src []byte, withPos bool) *Program {
	if root == nil {
		return &Program{}
	}
	return &Program{Root: (*ProgramNode)(convertNode(root, src, withPos))}
}

// convertNode converts a tree-sitter node into our Node representation.  Pure
// syntax leaves that do not carry text are skipped entirely to keep the
// resulting tree small.
func convertNode(n *sitter.Node, src []byte, withPos bool) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Type()}
	if withPos {
		sp := n.StartPoint()
		ep := n.EndPoint()
		node.Start = int(sp.Row) + 1
		node.End = int(ep.Row) + 1
		node.StartCol = int(sp.Column)
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			node.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if c := convertNode(child, src, withPos); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if node.Text == "" && len(node.Children) == 0 {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "comment", "lang_name", "number", "string", "symbol", "list", "quote", "extension":
		return true
	default:
		return false
	}
}
