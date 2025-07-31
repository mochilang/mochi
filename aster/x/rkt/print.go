//go:build slow

package rkt

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Racket source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeProgram(&b, p.Root, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeProgram(b *bytes.Buffer, n *ProgramNode, indent int) {
	for i, c := range n.Children {
		if i > 0 {
			b.WriteByte('\n')
		}
		writeTop(b, c, indent)
	}
}

func writeTop(b *bytes.Buffer, n *Node, indent int) {
	writeNode(b, n, indent)
	if b.Len() == 0 || b.Bytes()[b.Len()-1] != '\n' {
		b.WriteByte('\n')
	}
}

func writeNode(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "comment", "number", "string", "symbol", "boolean", "keyword":
		b.WriteString(n.Text)
	case "extension":
		b.WriteString("#lang ")
		if len(n.Children) > 0 {
			b.WriteString(n.Children[0].Text)
		}
	case "lang_name":
		b.WriteString(n.Text)
	case "list":
		if len(n.Children) == 0 {
			if n.Text != "" {
				b.WriteString(n.Text)
			} else {
				b.WriteString("()")
			}
			return
		}
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				if c.Kind == "list" {
					b.WriteByte('\n')
					b.WriteString(strings.Repeat("  ", indent+1))
				} else {
					b.WriteByte(' ')
				}
			}
			writeNode(b, c, indent+1)
		}
		b.WriteByte(')')
	case "quote":
		b.WriteByte('\'')
		if len(n.Children) > 0 {
			writeNode(b, n.Children[0], indent)
		} else if n.Text != "" {
			b.WriteString(n.Text)
		}
	default:
		// unknown; write kind or text
		if n.Text != "" {
			b.WriteString(n.Text)
		}
	}
}
