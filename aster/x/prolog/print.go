package prolog

import (
	"fmt"
	"strings"
	"unicode"
)

// Print returns Prolog source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b strings.Builder
	for _, c := range p.Children {
		if c.Kind != "clause" {
			continue
		}
		writeClause(&b, c)
	}
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeClause(b *strings.Builder, c *Node) {
	if len(c.Children) == 0 {
		return
	}
	head := c.Children[0]
	var body *Node
	if len(c.Children) > 1 {
		body = c.Children[1]
	}
	// directive
	if head.Kind == ":-" && len(head.Children) == 1 {
		b.WriteString(":- ")
		writeTerm(b, head.Children[0])
		b.WriteString(".\n")
		return
	}
	writeHead(b, head)
	if body != nil && !(body.Kind == "bool" && body.Text == "true") {
		b.WriteString(" :- ")
		writeTerm(b, body)
	}
	b.WriteString(".\n")
}

func writeHead(b *strings.Builder, n *Node) {
	if n == nil {
		return
	}
	b.WriteString(n.Kind)
	if len(n.Children) > 0 {
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			if c.Kind == "atom" && len(c.Text) > 0 && unicode.IsUpper(rune(c.Text[0])) {
				b.WriteString(c.Text)
			} else {
				writeTerm(b, c)
			}
		}
		b.WriteByte(')')
	}
}

func writeTerm(b *strings.Builder, n *Node) {
	switch n.Kind {
	case "var", "number", "bool":
		b.WriteString(n.Text)
	case "atom":
		writeAtom(b, n.Text)
	case "list":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeTerm(b, c)
		}
		b.WriteByte(']')
	case "{}":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeTerm(b, c)
		}
		b.WriteByte('}')
	case ":":
		if len(n.Children) == 2 {
			writeTerm(b, n.Children[0])
			b.WriteString(": ")
			writeTerm(b, n.Children[1])
		} else {
			writeFunctor(b, n)
		}
	case ",":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeTerm(b, c)
		}
	case ";":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("; ")
			}
			writeTerm(b, c)
		}
	case "-\u003e":
		if len(n.Children) == 2 {
			writeTerm(b, n.Children[0])
			b.WriteString(" -> ")
			writeTerm(b, n.Children[1])
		} else {
			writeFunctor(b, n)
		}
	default:
		writeFunctor(b, n)
	}
}

func writeFunctor(b *strings.Builder, n *Node) {
	b.WriteString(n.Kind)
	if len(n.Children) > 0 {
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeTerm(b, c)
		}
		b.WriteByte(')')
	}
}

func writeAtom(b *strings.Builder, s string) {
	if strings.ContainsRune(s, '(') && len(s) > 0 && unicode.IsLower(rune(s[0])) {
		b.WriteString(s)
		return
	}
	if strings.Count(s, " ") == 1 {
		parts := strings.SplitN(s, " ", 2)
		if isBareAtom(parts[0]) && isBareAtom(parts[1]) {
			b.WriteString(parts[0])
			b.WriteByte('(')
			b.WriteString(parts[1])
			b.WriteByte(')')
			return
		}
	}
	if isBareAtom(s) {
		b.WriteString(s)
		return
	}
	b.WriteByte('\'')
	for _, r := range s {
		if r == '\'' {
			b.WriteString("''")
		} else {
			b.WriteRune(r)
		}
	}
	b.WriteByte('\'')
}

func isBareAtom(s string) bool {
	if s == "" {
		return false
	}
	r := rune(s[0])
	if unicode.IsLower(r) {
		for _, r := range s[1:] {
			if !(unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_') {
				return false
			}
		}
		return true
	}
	for _, r := range s {
		if !strings.ContainsRune("!#$&*+-./:<=>?@^~\\", r) {
			return false
		}
	}
	return true
}
