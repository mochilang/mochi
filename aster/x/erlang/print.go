//go:build slow

package erlang

import (
	"bytes"
	"fmt"
)

// Print reconstructs Erlang source code from the Program AST.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	if p.Root.Text != "" {
		out := p.Root.Text
		if len(out) > 0 && out[len(out)-1] != '\n' {
			out += "\n"
		}
		return out, nil
	}
	var b bytes.Buffer
	writeNode(&b, (*Node)(p.Root), 0)
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
	if n.Text != "" && len(n.Children) == 0 {
		b.WriteString(n.Text)
		return
	}
	switch n.Kind {
	case "source_file":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
			}
			writeNode(b, c, indent)
		}
	case "module_attribute":
		b.WriteString("-module(")
		if len(n.Children) > 0 {
			writeNode(b, n.Children[0], indent)
		}
		b.WriteString(").")
		b.WriteByte('\n')
	case "export_attribute":
		b.WriteString("-export([")
		if len(n.Children) > 0 {
			fa := n.Children[0]
			if len(fa.Children) >= 2 {
				writeNode(b, fa.Children[0], indent)
				b.WriteByte('/')
				writeNode(b, fa.Children[1].Children[0], indent)
			}
		}
		b.WriteString("]).")
		b.WriteByte('\n')
	case "fun_decl":
		for _, c := range n.Children {
			writeNode(b, c, indent)
		}
	case "function_clause":
		if len(n.Children) < 3 {
			return
		}
		writeNode(b, n.Children[0], indent)
		writeNode(b, n.Children[1], indent)
		b.WriteString(" ->\n")
		writeNode(b, n.Children[2], indent+1)
		b.WriteByte('.')
		b.WriteByte('\n')
	case "expr_args":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, c, indent)
		}
		b.WriteByte(')')
	case "clause_body":
		for i, c := range n.Children {
			writeIndent(b, indent)
			writeNode(b, c, indent)
			if i < len(n.Children)-1 {
				b.WriteString(",\n")
			} else {
				b.WriteByte('\n')
			}
		}
	case "call":
		if len(n.Children) >= 2 {
			writeNode(b, n.Children[0], indent)
			writeNode(b, n.Children[1], indent)
		}
	case "remote":
		if len(n.Children) == 2 {
			writeNode(b, n.Children[0], indent)
			b.WriteByte(':')
			writeNode(b, n.Children[1], indent)
		}
	case "remote_module":
		if len(n.Children) > 0 {
			writeNode(b, n.Children[0], indent)
		}
	case "list":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, c, indent)
		}
		b.WriteByte(']')
	case "comment":
		writeIndent(b, indent)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "atom", "integer", "string", "var":
		b.WriteString(n.Text)
	default:
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, c, indent)
		}
	}
}

func writeIndent(b *bytes.Buffer, indent int) {
	for i := 0; i < indent; i++ {
		b.WriteString("    ")
	}
}
