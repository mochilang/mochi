package c

import (
        sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node with byte offsets and optional text.
type Node struct {
        Kind     string  `json:"kind"`
        Start    int     `json:"start"`
        End      int     `json:"end"`
        Text     string  `json:"text,omitempty"`
        Children []*Node `json:"children,omitempty"`
}

// toNode converts a tree-sitter node into our Node type.
func toNode(n *sitter.Node, src []byte) *Node {
        if n == nil {
                return nil
        }
        node := &Node{
                Kind:  n.Type(),
                Start: int(n.StartByte()),
                End:   int(n.EndByte()),
        }

        if n.NamedChildCount() == 0 {
                if isValueNode(n.Type()) {
                        node.Text = n.Content(src)
                } else if node.Kind == "comment" {
                        node.Text = n.Content(src)
                } else {
                        // skip pure syntax leaves
                        return nil
                }
        }

        for i := 0; i < int(n.NamedChildCount()); i++ {
                child := n.NamedChild(i)
                if child == nil {
                        continue
                }
                if c := toNode(child, src); c != nil {
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
                "string_content", "escape_sequence", "comment":
                return true
        default:
                return false
        }
}
