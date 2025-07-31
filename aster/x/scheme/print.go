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
		writeNode(&b, (*Node)(f))
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

func writeNode(b *bytes.Buffer, n *Node) {
	if n == nil {
		return
	}
	switch n.Kind {
	case "comment":
		b.WriteString(n.Text)
	default:
		writeExpr(b, n)
	}
}

func writeExpr(b *bytes.Buffer, n *Node) {
	switch n.Kind {
	case "list":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c)
		}
		b.WriteByte(')')
	case "quote":
		b.WriteByte('\'')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0])
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
