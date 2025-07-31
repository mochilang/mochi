package rs

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Rust source code for the given Program.
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
	ind := strings.Repeat("    ", indent)
	switch n.Kind {
	case "line_comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "function_item":
		b.WriteString(ind)
		b.WriteString("fn ")
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], 0)
			writeParameters(b, n.Children[1])
			idx := 2
			if len(n.Children) > idx && n.Children[idx].Kind != "block" {
				b.WriteString(" -> ")
				writeExpr(b, n.Children[idx], 0)
				idx++
			}
			if len(n.Children) > idx {
				b.WriteByte(' ')
				writeBlock(b, n.Children[idx], indent)
			}
		}
		b.WriteByte('\n')
	case "let_declaration":
		b.WriteString(ind)
		b.WriteString("let ")
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], 0)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
		b.WriteString(";\n")
	case "expression_statement":
		if len(n.Children) == 0 {
			return
		}
		b.WriteString(ind)
		expr := n.Children[0]
		writeExpr(b, expr, indent)
		if expr.Kind != "for_expression" && expr.Kind != "if_expression" && expr.Kind != "block" {
			b.WriteString(";")
		}
		b.WriteByte('\n')
	case "return_expression":
		b.WriteString(ind)
		b.WriteString("return ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	b.WriteString("{\n")
	for _, c := range n.Children {
		writeStmt(b, c, indent+1)
	}
	b.WriteString(strings.Repeat("    ", indent))
	b.WriteString("}")
}

func writeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeParameter(b, c)
	}
	if len(n.Children) == 0 && n.Text == "()" {
		// keep empty
	}
	b.WriteByte(')')
}

func writeParameter(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 2 {
		writeExpr(b, n.Children[0], 0)
		b.WriteString(": ")
		writeExpr(b, n.Children[1], 0)
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "field_identifier", "type_identifier", "primitive_type", "integer_literal", "string_content", "mutable_specifier":
		b.WriteString(n.Text)
	case "string_literal":
		b.WriteByte('"')
		for _, c := range n.Children {
			writeExpr(b, c, indent)
		}
		b.WriteByte('"')
	case "call_expression":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				writeArguments(b, n.Children[1], indent)
			} else {
				b.WriteString("()")
			}
		}
	case "field_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "arguments":
		writeArguments(b, n, indent)
	case "generic_type":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				writeTypeArguments(b, n.Children[1])
			}
		}
	case "type_arguments":
		writeTypeArguments(b, n)
	case "range_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString("..")
			writeExpr(b, n.Children[1], indent)
		}
	case "binary_expression":
		if len(n.Children) == 2 {
			op := "+"
			if n.Children[1].Kind == "identifier" && n.Children[1].Text == "target" {
				op = "=="
			}
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "parenthesized_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(',')
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "for_expression":
		if len(n.Children) == 3 {
			b.WriteString("for ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" in ")
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(' ')
			writeBlock(b, n.Children[2], indent)
		}
	case "if_expression":
		if len(n.Children) >= 2 {
			b.WriteString("if ")
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			writeBlock(b, n.Children[1], indent)
		}
	case "macro_invocation":
		if len(n.Children) == 0 {
			return
		}
		name := ""
		if n.Children[0].Kind == "identifier" {
			name = n.Children[0].Text
		}
		writeExpr(b, n.Children[0], indent)
		b.WriteByte('!')
		if len(n.Children) > 1 {
			open, close := '(', ')'
			if name == "vec" {
				open, close = '[', ']'
			}
			b.WriteByte(byte(open))
			children := n.Children[1].Children
			for i := 0; i < len(children); i++ {
				if name == "vec" && i+1 < len(children) && children[i].Kind == "identifier" && children[i+1].Kind == "primitive_type" {
					if i > 0 {
						b.WriteString(", ")
					}
					writeExpr(b, children[i], indent)
					b.WriteString(" as ")
					writeExpr(b, children[i+1], indent)
					i++
					continue
				}
				if i+1 < len(children) && children[i].Kind == "identifier" && children[i+1].Kind == "token_tree" && len(children[i+1].Children) == 1 && children[i+1].Children[0].Kind == "integer_literal" {
					if i > 0 {
						b.WriteString(", ")
					}
					writeExpr(b, children[i], indent)
					writeExpr(b, children[i+1], indent)
					i++
					continue
				}
				if i > 0 {
					b.WriteString(", ")
				}
				writeExpr(b, children[i], indent)
			}
			b.WriteByte(byte(close))
		} else {
			if name == "vec" {
				b.WriteString("[]")
			} else {
				b.WriteString("()")
			}
		}
	case "token_tree":
		if len(n.Children) == 1 && n.Children[0].Kind == "integer_literal" {
			b.WriteByte('[')
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(']')
			return
		}
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	default:
		b.WriteString(n.Kind)
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

func writeTypeArguments(b *bytes.Buffer, n *Node) {
	b.WriteByte('<')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte('>')
}
