package py

import sitter "github.com/smacker/go-tree-sitter"

// IncludePositions controls whether position information is recorded when
// converting tree-sitter nodes into our AST. When false (the default) the
// position fields remain zero and will be omitted from the marshalled JSON due
// to the omitempty struct tags.
var IncludePositions bool

// Node represents a simplified Python AST node. Only leaves that carry useful
// textual content populate the Text field. Position fields are optional and
// omitted from the JSON output when zero valued.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Some concrete node types used in the golden tests. They simply embed Node so
// the JSON output keeps the same structure while providing a richer Go type
// hierarchy.
type (
	Module              Node
	Comment             Node
	ExpressionStatement Node
	Assignment          Node
	Identifier          Node
	Integer             Node
	String              Node
	Call                Node
	Attribute           Node
	List                Node
	ListComprehension   Node
	ForStatement        Node
	ForInClause         Node
	ArgumentList        Node
	Type                Node
)

// convertNode recursively converts a tree-sitter node into our Node
// representation. Pure syntax leaves that do not carry textual content are
// skipped entirely so the resulting JSON remains compact.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{Kind: n.Type()}
	if IncludePositions {
		node.Start = int(n.StartByte())
		node.StartCol = int(start.Column)
		node.End = int(n.EndByte())
		node.EndCol = int(end.Column)
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
		if child == nil {
			continue
		}
		if c := convertNode(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether the given tree-sitter kind represents a leaf node
// whose textual content should be preserved in the JSON output.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "integer", "float", "string", "true", "false", "none", "comment", "string_content", "string_start", "string_end":
		return true
	default:
		return false
	}
}
