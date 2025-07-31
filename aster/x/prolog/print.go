package prolog

import (
	"fmt"
	"strings"
)

// Print returns Prolog source code reconstructed from the AST.
func Print(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b strings.Builder
	for i, c := range p.Children {
		if c.Kind != "clause" {
			return "", fmt.Errorf("unexpected node kind: %s", c.Kind)
		}
		if i > 0 {
			b.WriteByte('\n')
		}
		b.WriteString(printClause(c))
		if !strings.HasSuffix(b.String(), ".\n") {
			b.WriteString(".\n")
		}
	}
	out := b.String()
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return out, nil
}

func printClause(n *Node) string {
	if len(n.Children) == 0 {
		return ""
	}
	head := n.Children[0]
	var body *Node
	if len(n.Children) > 1 {
		body = n.Children[1]
	}
	if head.Kind == ":-" && len(head.Children) == 1 && (body == nil || (body.Kind == "bool" && body.Text == "true")) {
		return ":- " + nodeString(head.Children[0])
	}
	headStr := nodeString(head)
	if body == nil || (body.Kind == "bool" && body.Text == "true") {
		return headStr
	}
	return headStr + " :- " + nodeString(body)
}

func nodeString(n *Node) string {
	switch n.Kind {
	case "var", "atom", "number", "bool", "string":
		return n.Text
	case "list":
		var parts []string
		for _, c := range n.Children {
			parts = append(parts, nodeString(c))
		}
		return "[" + strings.Join(parts, ", ") + "]"
	case "{}":
		var parts []string
		for _, c := range n.Children {
			parts = append(parts, nodeString(c))
		}
		return "{" + strings.Join(parts, ", ") + "}"
	case ",":
		var parts []string
		for _, c := range n.Children {
			parts = append(parts, nodeString(c))
		}
		return strings.Join(parts, ", ")
	case ":":
		if len(n.Children) == 2 {
			return nodeString(n.Children[0]) + ": " + nodeString(n.Children[1])
		}
	case "=":
		if len(n.Children) == 2 {
			return nodeString(n.Children[0]) + " = " + nodeString(n.Children[1])
		}
	case ";":
		var parts []string
		for i, c := range n.Children {
			if i > 0 {
				parts = append(parts, "; "+nodeString(c))
			} else {
				parts = append(parts, nodeString(c))
			}
		}
		return strings.Join(parts, " ")
	case "->":
		if len(n.Children) == 2 {
			return nodeString(n.Children[0]) + " -> " + nodeString(n.Children[1])
		}
	}
	var parts []string
	for _, c := range n.Children {
		parts = append(parts, nodeString(c))
	}
	if len(parts) == 0 {
		return n.Kind
	}
	return fmt.Sprintf("%s(%s)", n.Kind, strings.Join(parts, ", "))
}
