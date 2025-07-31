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
	case "identifier", "atom", "integer":
		b.WriteString(n.Text)
	case "string":
		b.WriteByte('"')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		} else {
			b.WriteString(n.Text)
		}
		b.WriteByte('"')
	case "quoted_content", "string_content":
		b.WriteString(n.Text)
	case "call":
		if len(n.Children) > 0 {
			f := n.Children[0]
			if f.Kind == "dot" && len(f.Children) == 1 && f.Children[0].Kind == "identifier" && f.Children[0].Text == "puts" {
				b.WriteString("IO.puts")
			} else {
				writeExpr(b, f, indent)
			}
			if len(n.Children) > 1 {
				writeExpr(b, n.Children[1], indent)
			} else {
				b.WriteString("()")
			}
		}
	case "dot":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, indent)
		}
	case "arguments":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
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
