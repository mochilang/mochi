package clj

import (
	"bytes"
	"fmt"
)

// Print returns Clojure source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	if len(p.Forms) == 0 {
		return "", fmt.Errorf("empty program")
	}
	var b bytes.Buffer
	for i, f := range p.Forms {
		if f == nil {
			continue
		}
		writeNode(&b, (*Node)(f))
		if i < len(p.Forms)-1 {
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
	case "list":
		// Special-case defn without parameter vector. The parser omits
		// empty vectors, which leads to invalid code when reconstructed.
		if len(n.Children) >= 2 && n.Children[0].Kind == "symbol" && n.Children[0].Text == "defn" {
			b.WriteString("(defn ")
			writeNode(b, n.Children[1])
			b.WriteByte(' ')
			idx := 2
			if len(n.Children) > 2 && n.Children[2].Kind == "vector" {
				writeNode(b, n.Children[2])
				idx = 3
			} else {
				b.WriteString("[]")
			}
			for ; idx < len(n.Children); idx++ {
				b.WriteByte(' ')
				writeNode(b, n.Children[idx])
			}
			b.WriteByte(')')
			return
		}
		// Print common reader macros when their canonical form is present
		if len(n.Children) == 2 && n.Children[0].Kind == "symbol" {
			switch n.Children[0].Text {
			case "quote":
				b.WriteByte('\'')
				writeNode(b, n.Children[1])
				return
			case "quasiquote":
				b.WriteByte('`')
				writeNode(b, n.Children[1])
				return
			case "unquote":
				b.WriteByte('~')
				writeNode(b, n.Children[1])
				return
			case "unquote-splicing":
				b.WriteString("~@")
				writeNode(b, n.Children[1])
				return
			}
		}
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, c)
		}
		b.WriteByte(')')
	case "vector":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, c)
		}
		b.WriteByte(']')
	case "map":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			if c.Kind == "entry" && len(c.Children) == 2 {
				writeNode(b, c.Children[0])
				b.WriteByte(' ')
				writeNode(b, c.Children[1])
			} else {
				writeNode(b, c)
			}
		}
		b.WriteByte('}')
	case "set":
		b.WriteString("#{")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, c)
		}
		b.WriteByte('}')
	case "entry":
		if len(n.Children) == 2 {
			writeNode(b, n.Children[0])
			b.WriteByte(' ')
			writeNode(b, n.Children[1])
		}
	case "comment":
		b.WriteString(n.Text)
	case "string":
		fmt.Fprintf(b, "%q", n.Text)
	default:
		if n.Text != "" {
			b.WriteString(n.Text)
		} else {
			b.WriteString(n.Kind)
		}
	}
}
