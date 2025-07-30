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
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start,omitempty"`
	End      int    `json:"end,omitempty"`
	StartCol int    `json:"startCol,omitempty"`
	EndCol   int    `json:"endCol,omitempty"`
	Children []Node `json:"children,omitempty"`
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
	ForStatement        Node
	FunctionArguments   Node
	FunctionCall        Node
	Identifier          Node
	IdentifierList      Node
	Null                Node
	Number              Node
	String              Node
	TableConstructor    Node
	VariableDeclaration Node
	VariableDeclarator  Node
)

// convertNode converts a tree-sitter node into our Node representation. Pure
// syntax leaves (keywords, punctuation) are dropped to keep the resulting JSON
// compact.
func convertNode(n *sitter.Node, src []byte, withPos bool) (Node, bool) {
    start := n.StartPosition()
    end := n.EndPosition()
    node := Node{Kind: n.Kind()}
	if withPos {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

        if n.NamedChildCount() == 0 {
                if !isValueNode(n.Kind()) {
			return Node{}, false
		}
                text := n.Utf8Text(src)
		if strings.TrimSpace(text) == "" {
			return Node{}, false
		}
		node.Text = text
		return node, true
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if cn, ok := convertNode(child, src, withPos); ok {
			node.Children = append(node.Children, cn)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return Node{}, false
	}
	return node, true
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
