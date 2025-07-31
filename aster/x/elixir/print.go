//go:build slow

package elixir

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Elixir source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	for _, c := range p.Root.Children {
		writeStmt(&b, c, 0)
	}
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("  ", indent)
	switch n.Kind {
	case "comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "binary_operator":
		b.WriteString(ind)
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		} else {
			writeExpr(b, n, indent)
		}
		b.WriteByte('\n')
	case "call":
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "alias", "atom", "integer":
		b.WriteString(n.Text)
	case "binary_operator":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := n.Text
			if op == "" {
				op = "="
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "string":
		b.WriteByte('"')
		for _, c := range n.Children {
			writeExpr(b, c, indent)
		}
		if n.Text != "" && len(n.Children) == 0 {
			b.WriteString(n.Text)
		}
		b.WriteByte('"')
	case "interpolation":
		b.WriteString("#{")
		for _, c := range n.Children {
			writeExpr(b, c, indent)
		}
		b.WriteByte('}')
	case "quoted_content", "string_content":
		b.WriteString(n.Text)
	case "call":
		if len(n.Children) == 0 {
			return
		}
		f := n.Children[0]
		if f.Kind == "identifier" && f.Text == "for" {
			b.WriteString("for ")
			if len(n.Children) > 1 {
				writeForArgs(b, n.Children[1], indent)
			}
			if len(n.Children) > 2 && n.Children[2].Kind == "do_block" {
				b.WriteString(" do\n")
				writeBlock(b, n.Children[2], indent+1)
				b.WriteString(strings.Repeat("  ", indent))
				b.WriteString("end")
			}
			return
		}
		writeExpr(b, f, indent)
		for _, c := range n.Children[1:] {
			if c.Kind == "arguments" {
				writeArguments(b, c, indent)
			} else if c.Kind == "do_block" {
				b.WriteString(" do\n")
				writeBlock(b, c, indent+1)
				b.WriteString(strings.Repeat("  ", indent))
				b.WriteString("end")
			} else {
				b.WriteByte(' ')
				writeExpr(b, c, indent)
			}
		}
	case "dot":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, indent)
		}
		if len(n.Children) == 1 {
			b.WriteByte('.')
		}
	case "arguments":
		writeArguments(b, n, indent)
	case "list":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(']')
	case "map":
		b.WriteString("%{")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteString("}")
	case "map_content", "keywords":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
	case "pair":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			writeExpr(b, n.Children[1], indent)
		}
	case "keyword":
		b.WriteString(n.Text)
	case "anonymous_function":
		b.WriteString("fn ")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("; ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteString(" end")
	case "stab_clause":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" -> ")
			if n.Children[1].Kind == "body" {
				writeExpr(b, n.Children[1], indent)
			} else {
				writeExpr(b, n.Children[1], indent)
			}
		}
	case "body":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
				b.WriteString(strings.Repeat("  ", indent))
			}
			writeExpr(b, c, indent)
		}
	default:
		if n.Text != "" {
			b.WriteString(n.Text)
		} else {
			for i, c := range n.Children {
				if i > 0 {
					b.WriteByte(' ')
				}
				writeExpr(b, c, indent)
			}
		}
	}
}

func writeArguments(b *bytes.Buffer, n *Node, indent int) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, indent)
	}
	b.WriteByte(')')
}

func writeForArgs(b *bytes.Buffer, n *Node, indent int) {
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		if c.Kind == "binary_operator" && len(c.Children) == 2 {
			writeExpr(b, c.Children[0], indent)
			b.WriteString(" <- ")
			writeExpr(b, c.Children[1], indent)
		} else {
			writeExpr(b, c, indent)
		}
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	for _, c := range n.Children {
		writeStmt(b, c, indent)
	}
}
