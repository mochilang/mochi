//go:build slow

package scheme

import (
	"bytes"
	"fmt"
)

// Print returns Scheme source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	for i, f := range p.Forms {
		if i > 0 {
			if b.Len() == 0 || b.Bytes()[b.Len()-1] != '\n' {
				b.WriteByte('\n')
			}
		}
		writeNode(&b, (*Node)(f), 0)
		if b.Len() == 0 || b.Bytes()[b.Len()-1] != '\n' {
			b.WriteByte('\n')
		}
	}
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeNode(b *bytes.Buffer, n *Node, indent int) {
	if n == nil {
		return
	}
	switch n.Kind {
	case "comment":
		indentWrite(b, indent)
		b.WriteString(n.Text)
	default:
		indentWrite(b, indent)
		writeExpr(b, n, indent)
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "list":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				if c.Kind == "list" {
					b.WriteByte('\n')
					indentWrite(b, indent+1)
				} else {
					b.WriteByte(' ')
				}
			}
			writeExpr(b, c, indent+1)
		}
		b.WriteByte(')')
	case "quote":
		b.WriteByte('\'')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "symbol", "string", "number":
		b.WriteString(n.Text)
	default:
		if n.Text != "" {
			b.WriteString(n.Text)
		} else {
			b.WriteString(n.Kind)
		}
	}
}

func indentWrite(b *bytes.Buffer, indent int) {
	for i := 0; i < indent; i++ {
		b.WriteString("  ")
	}
}
