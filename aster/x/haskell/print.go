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
	case "pragma", "comment", "variable", "module_id", "name", "operator", "integer", "string", "unit", "constructor":
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
		idx := 0
		if idx < len(n.Children) && n.Children[idx].Kind == "qualified" {
			b.WriteString("qualified ")
			idx++
		}
		if idx < len(n.Children) {
			writeNode(b, &n.Children[idx], indent)
			idx++
		}
		if idx < len(n.Children) && n.Children[idx].Kind == "as" {
			idx++
			if idx < len(n.Children) {
				b.WriteString(" as ")
				writeNode(b, &n.Children[idx], indent)
				idx++
			}
		}
		for ; idx < len(n.Children); idx++ {
			b.WriteByte(' ')
			c := n.Children[idx]
			if c.Kind == "hiding" {
				b.WriteString("hiding ")
				continue
			}
			writeNode(b, &c, indent)
		}
	case "module":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
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
	case "prefix_id":
		b.WriteByte('(')
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
		}
		b.WriteByte(')')
	case "function":
		if len(n.Children) >= 2 {
			writeNode(b, &n.Children[0], indent)
			if n.Children[1].Kind == "patterns" {
				b.WriteByte(' ')
				writeNode(b, &n.Children[1], indent)
			}
			idx := 2
			if n.Children[1].Kind != "patterns" {
				idx = 1
			}
			for i := idx; i < len(n.Children); i++ {
				c := &n.Children[i]
				if i == idx {
					b.WriteString(" = ")
				} else {
					b.WriteByte('\n')
					writeIndent(b, indent+1)
					b.WriteString("| ")
				}
				writeNode(b, c, indent+1)
			}
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
		if len(n.Children) == 1 {
			writeNode(b, &n.Children[0], indent)
		} else if len(n.Children) == 2 {
			if n.Children[0].Kind == "guards" {
				writeNode(b, &n.Children[0], indent)
				b.WriteString(" = ")
				writeNode(b, &n.Children[1], indent)
			} else {
				writeNode(b, &n.Children[0], indent)
				b.WriteString(" = ")
				writeNode(b, &n.Children[1], indent)
			}
		} else {
			for i := range n.Children {
				if i > 0 {
					b.WriteByte(' ')
				}
				writeNode(b, &n.Children[i], indent)
			}
		}
	case "do":
		b.WriteString("do")
		for _, c := range n.Children {
			b.WriteByte('\n')
			writeIndent(b, indent+1)
			writeNode(b, &c, indent+1)
		}
	case "let":
		b.WriteString("let")
		for _, c := range n.Children {
			b.WriteByte('\n')
			writeIndent(b, indent+1)
			writeNode(b, &c, indent+1)
		}
	case "local_binds":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
				writeIndent(b, indent)
			}
			writeNode(b, &c, indent)
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
	case "infix_id":
		b.WriteByte('`')
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
		}
		b.WriteByte('`')
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
	case "tuple":
		b.WriteByte('(')
		for i := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, &n.Children[i], indent)
		}
		b.WriteByte(')')
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
	case "data_type":
		if len(n.Children) >= 2 {
			b.WriteString("data ")
			writeNode(b, &n.Children[0], indent)
			b.WriteString(" = ")
			writeNode(b, &n.Children[1], indent)
		}
		if len(n.Children) >= 3 {
			b.WriteByte(' ')
			writeNode(b, &n.Children[2], indent)
		}
	case "data_constructors":
		for i := range n.Children {
			if i > 0 {
				b.WriteString(" | ")
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "data_constructor":
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "record":
		if len(n.Children) >= 1 {
			writeNode(b, &n.Children[0], indent)
		}
		if len(n.Children) >= 2 {
			b.WriteString(" {")
			for i, c := range n.Children[1:] {
				if i > 0 {
					b.WriteString(", ")
				}
				writeNode(b, &c, indent)
			}
			b.WriteString("}")
		}
	case "fields":
		for i := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "field":
		if len(n.Children) == 2 {
			writeNode(b, &n.Children[0], indent)
			b.WriteString(" :: ")
			writeNode(b, &n.Children[1], indent)
		}
	case "field_name":
		for i := range n.Children {
			writeNode(b, &n.Children[i], indent)
		}
	case "field_update":
		if len(n.Children) == 2 {
			writeNode(b, &n.Children[0], indent)
			b.WriteString(" = ")
			writeNode(b, &n.Children[1], indent)
		}
	case "list_comprehension":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i == 0 {
				writeNode(b, &c, indent)
				if len(n.Children) > 1 {
					b.WriteString(" | ")
				}
			} else {
				writeNode(b, &c, indent)
			}
		}
		b.WriteByte(']')
	case "qualifiers":
		for i := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, &n.Children[i], indent)
		}
	case "generator":
		if len(n.Children) == 2 {
			writeNode(b, &n.Children[0], indent)
			b.WriteString(" <- ")
			writeNode(b, &n.Children[1], indent)
		}
	case "guards":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, &c, indent)
		}
	case "projection":
		if len(n.Children) == 2 {
			writeNode(b, &n.Children[0], indent)
			b.WriteByte('.')
			writeNode(b, &n.Children[1], indent)
		}
	case "qualified":
		if len(n.Children) == 2 {
			writeNode(b, &n.Children[0], indent)
			b.WriteByte('.')
			writeNode(b, &n.Children[1], indent)
		}
	case "literal":
		for i := range n.Children {
			writeNode(b, &n.Children[i], indent)
		}
	case "deriving":
		b.WriteString("deriving ")
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, &n.Children[i], indent)
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
