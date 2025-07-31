//go:build slow

package ocaml

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns OCaml source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeCompilationUnit(&b, &p.File.Node, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeCompilationUnit(b *bytes.Buffer, n *Node, indent int) {
	for i := range n.Children {
		writeStmt(b, n.Children[i], indent)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("  ", indent)
	switch n.Kind {
	case "comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "value_definition":
		b.WriteString(ind)
		writeValueDefinition(b, n.Children[0], indent)
		b.WriteByte('\n')
	case "exception_definition":
		b.WriteString(ind)
		b.WriteString("exception ")
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

func writeValueDefinition(b *bytes.Buffer, n *Node, indent int) {
	b.WriteString("let ")
	writeLetBinding(b, n, indent)
}

func writeLetBinding(b *bytes.Buffer, n *Node, indent int) {
	idx := 0
	if idx < len(n.Children) && n.Children[idx].Kind == "value_name" {
		b.WriteString(n.Children[idx].Text)
		idx++
	} else if idx < len(n.Children) && n.Children[idx].Kind == "unit" {
		b.WriteString(n.Children[idx].Text)
		idx++
	}
	for idx < len(n.Children) && n.Children[idx].Kind == "parameter" {
		b.WriteByte(' ')
		writeParameter(b, n.Children[idx])
		idx++
	}
	if idx < len(n.Children) {
		expr := n.Children[idx]
		switch expr.Kind {
		case "let_expression", "sequence_expression", "for_expression", "if_expression":
			b.WriteString(" =\n")
			writeExpr(b, expr, indent+1)
		default:
			b.WriteString(" = ")
			writeExpr(b, expr, indent)
		}
	}
}

func writeParameter(b *bytes.Buffer, n *Node) {
	if len(n.Children) > 0 {
		writeExpr(b, n.Children[0], 0)
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "value_name", "value_pattern", "number", "rel_operator", "add_operator", "sign_operator", "prefix_operator", "boolean", "assign_operator":
		b.WriteString(n.Text)
	case "value_path":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, indent)
		}
	case "module_path":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, indent)
		}
	case "module_name":
		b.WriteString(n.Text)
	case "string":
		if len(n.Children) > 0 {
			fmt.Fprintf(b, "%q", n.Children[0].Text)
		} else {
			b.WriteString("\"\"")
		}
	case "string_content":
		b.WriteString(n.Text)
	case "unit":
		b.WriteString(n.Text)
	case "parameter":
		writeParameter(b, n)
	case "application_expression":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
	case "let_expression":
		if len(n.Children) == 2 {
			b.WriteString(strings.Repeat("  ", indent))
			if len(n.Children[0].Children) > 0 {
				writeValueDefinition(b, n.Children[0].Children[0], indent)
			}
			b.WriteString(" in\n")
			writeExpr(b, n.Children[1], indent)
		}
	case "list_expression":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("; ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(']')
	case "tuple_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "parenthesized_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("; ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "sign_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			writeExpr(b, n.Children[1], indent)
		}
	case "prefix_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			writeExpr(b, n.Children[1], indent)
		}
	case "fun_expression":
		if len(n.Children) >= 2 {
			b.WriteString("fun ")
			for i, c := range n.Children[:len(n.Children)-1] {
				if i > 0 {
					b.WriteByte(' ')
				}
				writeExpr(b, c, indent)
			}
			b.WriteString(" -> ")
			writeExpr(b, n.Children[len(n.Children)-1], indent)
		}
	case "infix_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[2], indent)
		}
	case "for_expression":
		if len(n.Children) >= 4 {
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("for ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" to ")
			writeExpr(b, n.Children[2], indent)
			b.WriteString(" do\n")
			if len(n.Children) > 3 {
				writeExpr(b, n.Children[3], indent+1)
				b.WriteByte('\n')
			}
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("done")
		}
	case "do_clause":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
				b.WriteString(strings.Repeat("  ", indent))
			}
			writeExpr(b, c, indent)
		}
	case "sequence_expression":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(";\n")
				b.WriteString(strings.Repeat("  ", indent))
			}
			writeExpr(b, c, indent)
		}
	case "try_expression":
		if len(n.Children) >= 2 {
			b.WriteString("try ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" with ")
			for i, c := range n.Children[1:] {
				if i > 0 {
					b.WriteString(" | ")
				}
				writeMatchCase(b, c, indent)
			}
		}
	case "if_expression":
		if len(n.Children) >= 2 {
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("if ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" then ")
			writeExpr(b, n.Children[1], indent)
		}
	case "then_clause":
		for _, c := range n.Children {
			if c.Kind == "sequence_expression" {
				b.WriteByte('(')
				writeExpr(b, c, indent)
				b.WriteByte(')')
			} else {
				writeExpr(b, c, indent)
			}
		}
	case "constructor_path":
		for _, c := range n.Children {
			writeExpr(b, c, indent)
		}
	case "constructor_name":
		b.WriteString(n.Text)
	case "constructor_declaration":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
	case "open_module":
		b.WriteString("open ")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, indent)
		}
	case "let_open_expression":
		if len(n.Children) >= 2 {
			b.WriteString("let open ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" in ")
			writeExpr(b, n.Children[1], indent)
		}
	case "typed_expression":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	default:
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
	}
}

func writeMatchCase(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) >= 2 {
		writeExpr(b, n.Children[0], indent)
		b.WriteString(" -> ")
		writeExpr(b, n.Children[1], indent)
	}
}
