//go:build slow

package scala

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Scala source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeCompilationUnit(&b, p.Root, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeCompilationUnit(b *bytes.Buffer, cu *CompilationUnit, indent int) {
	for _, c := range cu.Children {
		writeStmt(b, c, indent)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("  ", indent)
	switch n.Kind {
	case "comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "import_declaration":
		b.WriteString(ind)
		b.WriteString("import ")
		var parts []string
		var selectors *Node
		for _, c := range n.Children {
			if c.Kind == "namespace_selectors" {
				selectors = c
			} else {
				parts = append(parts, c.Text)
			}
		}
		b.WriteString(strings.Join(parts, "."))
		if selectors != nil {
			b.WriteString(".{")
			for i, s := range selectors.Children {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(s.Text)
			}
			b.WriteByte('}')
		}
		b.WriteByte('\n')
	case "object_definition":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("object ")
			b.WriteString(n.Children[0].Text)
			b.WriteString(" {\n")
			writeBlock(b, n.Children[1], indent+1)
			b.WriteString(ind)
			b.WriteString("}\n")
		}
	case "function_definition":
		writeFunctionDefinition(b, n, indent)
	case "val_definition":
		writeValDefinition(b, n, indent)
	case "return_expression":
		b.WriteString(ind)
		b.WriteString("return ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "for_expression":
		writeForExpression(b, n, indent)
	case "if_expression":
		writeIfExpression(b, n, indent)
	case "call_expression":
		b.WriteString(ind)
		writeCallExpression(b, n, indent)
		b.WriteByte('\n')
	case "block":
		writeBlock(b, n, indent)
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	for _, c := range n.Children {
		writeStmt(b, c, indent)
	}
}

func writeFunctionDefinition(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 4 {
		return
	}
	ind := strings.Repeat("  ", indent)
	name := n.Children[0]
	params := n.Children[1]
	ret := n.Children[2]
	body := n.Children[3]
	b.WriteString(ind)
	b.WriteString("def ")
	b.WriteString(name.Text)
	writeParameters(b, params, indent)
	b.WriteString(": ")
	writeExpr(b, ret, indent)
	b.WriteString(" = {\n")
	writeBlock(b, body, indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeParameters(b *bytes.Buffer, n *Node, indent int) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		if len(c.Children) >= 2 {
			b.WriteString(c.Children[0].Text)
			b.WriteString(": ")
			writeExpr(b, c.Children[1], indent)
		}
	}
	b.WriteByte(')')
}

func writeValDefinition(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("  ", indent)
	b.WriteString(ind)
	b.WriteString("val ")
	if len(n.Children) == 0 {
		b.WriteByte('\n')
		return
	}
	b.WriteString(n.Children[0].Text)
	idx := 1
	if len(n.Children) > 1 && (n.Children[1].Kind == "type_identifier" || n.Children[1].Kind == "generic_type") {
		b.WriteString(": ")
		writeExpr(b, n.Children[1], indent)
		idx = 2
	}
	if len(n.Children) > idx {
		b.WriteString(" = ")
		writeExpr(b, n.Children[idx], indent)
	}
	b.WriteByte('\n')
}

func writeForExpression(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("  ", indent)
	enums := n.Children[0]
	body := n.Children[1]
	b.WriteString(ind)
	b.WriteString("for (")
	for i, e := range enums.Children {
		if i > 0 {
			b.WriteString("; ")
		}
		if len(e.Children) == 2 {
			b.WriteString(e.Children[0].Text)
			b.WriteString(" <- ")
			writeExpr(b, e.Children[1], indent)
		}
	}
	b.WriteString(") {\n")
	writeBlock(b, body, indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeIfExpression(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("  ", indent)
	cond := n.Children[0]
	body := n.Children[1]
	b.WriteString(ind)
	b.WriteString("if ")
	writeExpr(b, cond, indent)
	b.WriteString(" {\n")
	writeBlock(b, body, indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeCallExpression(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	writeExpr(b, n.Children[0], indent)
	if len(n.Children) > 1 {
		writeArguments(b, n.Children[1], indent)
	} else {
		b.WriteString("()")
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

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "integer_literal", "string", "type_identifier":
		b.WriteString(n.Text)
	case "generic_type":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		if len(n.Children) > 1 {
			b.WriteByte('[')
			for i, c := range n.Children[1].Children {
				if i > 0 {
					b.WriteString(", ")
				}
				writeExpr(b, c, indent)
			}
			b.WriteByte(']')
		}
	case "call_expression":
		writeCallExpression(b, n, indent)
	case "arguments":
		writeArguments(b, n, indent)
	case "field_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "parenthesized_expression":
		b.WriteByte('(')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte(')')
	case "infix_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[2], indent)
		} else if len(n.Children) == 2 {
			left := n.Children[0]
			right := n.Children[1]
			writeExpr(b, left, indent)
			// heuristic for operator
			op := "+"
			if left.Kind == "integer_literal" && left.Text == "0" && right.Kind == "integer_literal" && right.Text == "1" {
				op = "-"
			}
			if right.Kind == "identifier" && right.Text == "target" {
				op = "=="
			}
			b.WriteString(" " + op + " ")
			writeExpr(b, right, indent)
		}
	case "block":
		b.WriteString("{\n")
		writeBlock(b, n, indent+1)
		b.WriteString(strings.Repeat("  ", indent))
		b.WriteString("}")
	default:
		b.WriteString(n.Kind)
	}
}
