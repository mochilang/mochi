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
			op := "="
			if strings.TrimSpace(n.Text) != "" {
				op = strings.TrimSpace(n.Text)
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
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
	case "string":
		if len(n.Children) == 0 {
			if strings.HasPrefix(n.Text, "\"") && strings.HasSuffix(n.Text, "\"") {
				b.WriteString(n.Text)
			} else {
				b.WriteByte('"')
				b.WriteString(n.Text)
				b.WriteByte('"')
			}
			return
		}
		b.WriteByte('"')
		for _, c := range n.Children {
			writeExpr(b, c, indent)
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
		if f.Kind == "identifier" && f.Text == "defmodule" {
			b.WriteString("defmodule ")
			if len(n.Children) > 1 && n.Children[1].Kind == "arguments" && len(n.Children[1].Children) > 0 {
				writeExpr(b, n.Children[1].Children[0], indent)
			}
			b.WriteString(" do\n")
			if len(n.Children) > 2 && n.Children[2].Kind == "do_block" {
				writeBlock(b, n.Children[2], indent+1)
			}
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("end")
			return
		}
		if f.Kind == "identifier" && f.Text == "def" {
			b.WriteString("def ")
			if len(n.Children) > 1 && n.Children[1].Kind == "arguments" && len(n.Children[1].Children) > 0 {
				fc := n.Children[1].Children[0]
				if fc.Kind == "call" && len(fc.Children) > 0 {
					writeExpr(b, fc.Children[0], indent)
					if len(fc.Children) > 1 {
						writeArguments(b, fc.Children[1], indent)
					} else {
						b.WriteString("()")
					}
				} else {
					writeExpr(b, fc, indent)
				}
			}
			b.WriteString(" do\n")
			if len(n.Children) > 2 && n.Children[2].Kind == "do_block" {
				writeBlock(b, n.Children[2], indent+1)
			}
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("end")
			return
		}
		writeExpr(b, f, indent)
		if len(n.Children) == 1 {
			if f.Kind != "dot" {
				b.WriteString("()")
			}
			return
		}
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
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
		} else {
			for i, c := range n.Children {
				if i > 0 {
					b.WriteByte('.')
				}
				writeExpr(b, c, indent)
			}
		}
	case "arguments":
		writeArguments(b, n, indent)
	case "list":
		if len(n.Children) == 0 && n.Text != "" {
			b.WriteString(n.Text)
		} else {
			b.WriteByte('[')
			for i, c := range n.Children {
				if i > 0 {
					b.WriteString(", ")
				}
				writeExpr(b, c, indent)
			}
			b.WriteByte(']')
		}
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
	case "binary_operator":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := "="
			if strings.TrimSpace(n.Text) != "" {
				op = strings.TrimSpace(n.Text)
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "unary_operator":
		if len(n.Children) == 1 {
			op := "-"
			if strings.TrimSpace(n.Text) != "" {
				op = strings.TrimSpace(n.Text)
			}
			b.WriteString(op)
			writeExpr(b, n.Children[0], indent)
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
	case "block":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(" ")
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
			op := "<-"
			if strings.TrimSpace(c.Text) != "" {
				op = strings.TrimSpace(c.Text)
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
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
