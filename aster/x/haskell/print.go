package haskell

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Haskell source code reconstructed from the AST.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
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
	switch n.Kind {
	case "haskell":
		for i := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "pragma", "comment", "variable", "module_id", "name", "operator", "integer", "string", "unit":
		b.WriteString(n.Text)
	case "imports", "declarations":
		for i := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "import":
		b.WriteString("import ")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &c, indent)
		}
	case "module":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &c, indent)
		}
	case "import_list":
		b.WriteByte('(')
		for i := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, &n.Children[i], indent)
		}
		b.WriteByte(')')
	case "import_name":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &c, indent)
		}
	case "function":
		if len(n.Children) >= 3 {
			writeNode(b, &n.Children[0], indent)
			b.WriteByte(' ')
			writeNode(b, &n.Children[1], indent)
			b.WriteString(" = ")
			writeNode(b, &n.Children[2], indent)
		}
	case "bind":
		if len(n.Children) == 2 {
			writeNode(b, &n.Children[0], indent)
			b.WriteString(" = ")
			writeNode(b, &n.Children[1], indent)
		}
	case "signature":
		if len(n.Children) == 2 {
			writeNode(b, &n.Children[0], indent)
			b.WriteString(" :: ")
			writeNode(b, &n.Children[1], indent)
		}
	case "patterns":
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "match":
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "do":
		b.WriteString("do")
		for _, c := range n.Children {
			b.WriteByte('\n')
			writeIndent(b, indent+1)
			writeNode(b, &c, indent+1)
		}
	case "exp":
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "apply":
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "lambda":
		if len(n.Children) >= 2 {
			b.WriteByte('\\')
			writeNode(b, &n.Children[0], indent)
			b.WriteString(" -> ")
			writeNode(b, &n.Children[1], indent)
		}
	case "infix":
		if len(n.Children) == 3 {
			writeNode(b, &n.Children[0], indent)
			b.WriteByte(' ')
			writeNode(b, &n.Children[1], indent)
			b.WriteByte(' ')
			writeNode(b, &n.Children[2], indent)
		}
	case "parens":
		b.WriteByte('(')
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
		}
		b.WriteByte(')')
	case "list":
		b.WriteByte('[')
		for i := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, &n.Children[i], indent)
		}
		b.WriteByte(']')
	case "arithmetic_sequence":
		if len(n.Children) == 2 {
			writeNode(b, &n.Children[0], indent)
			b.WriteString(" .. ")
			writeNode(b, &n.Children[1], indent)
		}
	case "negation":
		b.WriteByte('-')
		if len(n.Children) > 0 {
			writeNode(b, &n.Children[0], indent)
		}
	default:
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
		}
	}
}

func writeIndent(b *bytes.Buffer, n int) {
	b.WriteString(strings.Repeat("    ", n))
}
