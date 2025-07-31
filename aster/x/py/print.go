//go:build slow

package py

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Python source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.File == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeModule(&b, p.File, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeModule(b *bytes.Buffer, m *Module, indent int) {
	for _, c := range m.Children {
		writeStmt(b, c, indent)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	switch n.Kind {
	case "comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "future_import_statement":
		b.WriteString(ind)
		b.WriteString("from __future__ import ")
		writeDottedNames(b, n.Children)
		b.WriteByte('\n')
	case "import_statement":
		if len(n.Children) > 0 {
			b.WriteString(ind)
			b.WriteString("import ")
			writeDottedName(b, n.Children[0])
			b.WriteByte('\n')
		}
	case "import_from_statement":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("from ")
			writeDottedName(b, n.Children[0])
			b.WriteString(" import ")
			writeDottedNames(b, n.Children[1:])
			b.WriteByte('\n')
		}
	case "decorated_definition":
		if len(n.Children) == 0 {
			return
		}
		for _, d := range n.Children[:len(n.Children)-1] {
			b.WriteString(ind)
			writeDecorator(b, d, indent)
		}
		writeStmt(b, n.Children[len(n.Children)-1], indent)
	case "class_definition":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("class ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(":\n")
			writeBlock(b, n.Children[1], indent+1)
		}
	case "expression_statement":
		if len(n.Children) > 0 {
			b.WriteString(ind)
			c := n.Children[0]
			if c.Kind == "assignment" {
				writeAssignment(b, c, indent)
			} else {
				writeExpr(b, c, indent)
			}
			b.WriteByte('\n')
		}
	case "assignment":
		b.WriteString(ind)
		writeAssignment(b, n, indent)
		b.WriteByte('\n')
	case "return_statement":
		b.WriteString(ind)
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "break_statement":
		b.WriteString(ind)
		b.WriteString("break\n")
	case "continue_statement":
		b.WriteString(ind)
		b.WriteString("continue\n")
	case "function_definition":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("def ")
			writeExpr(b, n.Children[0], indent)
			idx := 1
			if idx < len(n.Children) && n.Children[idx].Kind == "parameters" {
				writeParameters(b, n.Children[idx])
				idx++
			} else {
				b.WriteString("()")
			}
			b.WriteString(":\n")
			if idx < len(n.Children) {
				writeBlock(b, n.Children[idx], indent+1)
			}
		}
	case "for_statement":
		if len(n.Children) >= 3 {
			b.WriteString(ind)
			b.WriteString("for ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" in ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(":\n")
			writeBlock(b, n.Children[2], indent+1)
		}
	case "if_statement":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("if ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(":\n")
			writeBlock(b, n.Children[1], indent+1)
			for _, c := range n.Children[2:] {
				if c.Kind == "else_clause" && len(c.Children) > 0 {
					b.WriteString(ind)
					b.WriteString("else:\n")
					writeBlock(b, c.Children[0], indent+1)
				}
			}
		}
	case "global_statement":
		if len(n.Children) > 0 {
			b.WriteString(ind)
			b.WriteString("global ")
			for i, c := range n.Children {
				if i > 0 {
					b.WriteString(", ")
				}
				writeExpr(b, c, 0)
			}
			b.WriteByte('\n')
		}
	case "try_statement":
		if len(n.Children) > 0 {
			b.WriteString(ind)
			b.WriteString("try:\n")
			writeBlock(b, n.Children[0], indent+1)
			for _, ex := range n.Children[1:] {
				if ex.Kind != "except_clause" {
					continue
				}
				b.WriteString(ind)
				b.WriteString("except")
				if len(ex.Children) > 0 {
					b.WriteByte(' ')
					writeExpr(b, ex.Children[0], indent)
				}
				b.WriteString(":\n")
				if len(ex.Children) > 1 {
					writeBlock(b, ex.Children[1], indent+1)
				} else {
					b.WriteString(strings.Repeat("    ", indent+1))
					b.WriteString("pass\n")
				}
			}
		}
	case "block":
		writeBlock(b, n, indent)
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		b.WriteString(strings.Repeat("    ", indent))
		b.WriteString("pass\n")
		return
	}
	for _, c := range n.Children {
		writeStmt(b, c, indent)
	}
}

func writeAssignment(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) != 2 {
		b.WriteString("assignment")
		return
	}
	writeExpr(b, n.Children[0], indent)
	if n.Children[1].Kind == "type" {
		b.WriteString(": ")
	} else {
		b.WriteString(" = ")
	}
	writeExpr(b, n.Children[1], indent)
}

func writeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte(')')
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "integer", "float", "true", "false", "none":
		b.WriteString(n.Text)
	case "string", "string_content":
		if n.Text != "" {
			fmt.Fprintf(b, "%q", n.Text)
		} else if len(n.Children) > 0 {
			fmt.Fprintf(b, "%q", n.Children[0].Text)
		} else {
			b.WriteString("\"\"")
		}
	case "call":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('(')
			if len(n.Children) > 1 {
				for i, arg := range n.Children[1].Children {
					if i > 0 {
						b.WriteString(", ")
					}
					writeExpr(b, arg, indent)
				}
			}
			b.WriteByte(')')
		}
	case "argument_list":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "attribute":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "list":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(']')
	case "list_comprehension":
		b.WriteByte('[')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
			for _, r := range n.Children[1:] {
				b.WriteByte(' ')
				switch r.Kind {
				case "for_in_clause":
					b.WriteString("for ")
					if len(r.Children) >= 2 {
						writeExpr(b, r.Children[0], indent)
						b.WriteString(" in ")
						writeExpr(b, r.Children[1], indent)
					}
				case "if_clause":
					b.WriteString("if ")
					if len(r.Children) > 0 {
						writeExpr(b, r.Children[0], indent)
					}
				default:
					writeExpr(b, r, indent)
				}
			}
		}
		b.WriteByte(']')
	case "for_in_clause":
		// handled by list comprehension
	case "type":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "dotted_name":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, indent)
		}
	case "dictionary":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte('}')
	case "pair":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(": ")
			writeExpr(b, n.Children[1], indent)
		}
	case "binary_operator":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := "+"
			if n.Text != "" {
				op = strings.TrimSpace(n.Text)
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "unary_operator":
		if len(n.Children) == 1 {
			b.WriteByte('-')
			writeExpr(b, n.Children[0], indent)
		}
	case "not_operator":
		if len(n.Children) == 1 {
			b.WriteString("not ")
			writeExpr(b, n.Children[0], indent)
		}
	case "comparison_operator":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := "=="
			if n.Text != "" {
				op = strings.TrimSpace(n.Text)
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "boolean_operator":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := "and"
			if n.Text != "" {
				op = strings.TrimSpace(n.Text)
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "parenthesized_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "keyword_argument":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('=')
			writeExpr(b, n.Children[1], indent)
		}
	case "lambda":
		b.WriteString("lambda ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString(": ")
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], indent)
		}
	case "lambda_parameters":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
	case "conditional_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" if ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" else ")
			writeExpr(b, n.Children[2], indent)
		}
	case "subscript":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "slice":
		switch len(n.Children) {
		case 0:
			b.WriteString(":")
		case 1:
			b.WriteByte(':')
			writeExpr(b, n.Children[0], indent)
		default:
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(':')
			writeExpr(b, n.Children[1], indent)
		}
	default:
		// unsupported; write kind
		b.WriteString(n.Kind)
	}
}

func writeDecorator(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 1 {
		b.WriteString("@")
		writeExpr(b, n.Children[0], indent)
		b.WriteByte('\n')
	}
}

func writeDottedNames(b *bytes.Buffer, list []*Node) {
	for i, n := range list {
		if i > 0 {
			b.WriteString(", ")
		}
		writeDottedName(b, n)
	}
}

func writeDottedName(b *bytes.Buffer, n *Node) {
	if n == nil {
		return
	}
	if n.Kind == "dotted_name" {
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, 0)
		}
	} else {
		writeExpr(b, n, 0)
	}
}
