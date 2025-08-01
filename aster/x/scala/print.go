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
		writeObjectDefinition(b, n, indent)
	case "trait_definition":
		writeTraitDefinition(b, n, indent)
	case "class_definition":
		writeClassDefinition(b, n, indent)
	case "function_definition":
		writeFunctionDefinition(b, n, indent)
	case "val_definition":
		writeValDefinition(b, n, indent)
	case "var_definition":
		writeVarDefinition(b, n, indent)
	case "return_expression":
		b.WriteString(ind)
		b.WriteString("return ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "for_expression":
		writeForExpression(b, n, indent)
	case "while_expression":
		writeWhileExpression(b, n, indent)
	case "if_expression":
		writeIfExpression(b, n, indent)
	case "call_expression":
		b.WriteString(ind)
		writeCallExpression(b, n, indent)
		b.WriteByte('\n')
	case "assignment_expression":
		b.WriteString(ind)
		writeAssignmentExpression(b, n, indent)
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
	if len(n.Children) < 3 {
		return
	}
	ind := strings.Repeat("  ", indent)
	name := n.Children[0]
	var params *Node
	idx := 1
	if len(n.Children) > 3 {
		params = n.Children[1]
		idx = 2
	}
	ret := n.Children[idx]
	body := n.Children[idx+1]
	b.WriteString(ind)
	b.WriteString("def ")
	b.WriteString(name.Text)
	if params != nil {
		writeParameters(b, params, indent)
	} else {
		b.WriteString("()")
	}
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

func writeVarDefinition(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("  ", indent)
	b.WriteString(ind)
	b.WriteString("var ")
	if len(n.Children) == 0 {
		b.WriteByte('\n')
		return
	}
	b.WriteString(n.Children[0].Text)
	if len(n.Children) > 1 {
		b.WriteString(" = ")
		writeExpr(b, n.Children[1], indent)
	}
	b.WriteByte('\n')
}

func writeClassDefinition(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	ind := strings.Repeat("  ", indent)
	b.WriteString(ind)
	b.WriteString("case class ")
	b.WriteString(n.Children[0].Text)
	if len(n.Children) > 1 {
		writeClassParameters(b, n.Children[1], indent)
	}
	b.WriteByte('\n')
}

func writeClassParameters(b *bytes.Buffer, n *Node, indent int) {
	b.WriteByte('(')
	for i, p := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		if len(p.Children) >= 2 {
			b.WriteString(p.Children[0].Text)
			b.WriteString(": ")
			writeExpr(b, p.Children[1], indent)
		}
	}
	b.WriteByte(')')
}

func writeObjectDefinition(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	ind := strings.Repeat("  ", indent)
	b.WriteString(ind)
	b.WriteString("object ")
	b.WriteString(n.Children[0].Text)
	idx := 1
	if idx < len(n.Children) && n.Children[idx].Kind == "extends_clause" {
		b.WriteString(" extends ")
		for i, ex := range n.Children[idx].Children {
			if i > 0 {
				b.WriteString(" with ")
			}
			writeExpr(b, ex, indent)
		}
		idx++
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "template_body" {
		b.WriteString(" {\n")
		writeBlock(b, n.Children[idx], indent+1)
		b.WriteString(ind)
		b.WriteString("}\n")
	} else {
		b.WriteByte('\n')
	}
}

func writeTraitDefinition(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	ind := strings.Repeat("  ", indent)
	b.WriteString(ind)
	b.WriteString("trait ")
	b.WriteString(n.Children[0].Text)
	idx := 1
	if idx < len(n.Children) && n.Children[idx].Kind == "extends_clause" {
		b.WriteString(" extends ")
		for i, ex := range n.Children[idx].Children {
			if i > 0 {
				b.WriteString(" with ")
			}
			writeExpr(b, ex, indent)
		}
		idx++
	}
	b.WriteByte('\n')
}

func writeWhileExpression(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("  ", indent)
	cond := n.Children[0]
	body := n.Children[1]
	b.WriteString(ind)
	b.WriteString("while ")
	writeExpr(b, cond, indent)
	b.WriteString(" {\n")
	writeBlock(b, body, indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeAssignmentExpression(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) != 2 {
		return
	}
	writeExpr(b, n.Children[0], indent)
	b.WriteString(" = ")
	writeExpr(b, n.Children[1], indent)
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
	if body.Kind == "block" {
		b.WriteString(") {\n")
		writeBlock(b, body, indent+1)
		b.WriteString(ind)
		b.WriteString("}\n")
	} else {
		b.WriteString(") yield ")
		writeExpr(b, body, indent)
	}
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
	case "identifier", "integer_literal", "string", "type_identifier", "boolean_literal", "operator_identifier":
		b.WriteString(n.Text)
	case "generic_type", "generic_function":
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
	case "match_expression":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" match ")
			for i, c := range n.Children[1:] {
				if i > 0 {
					b.WriteByte(' ')
				}
				writeExpr(b, c, indent)
			}
		}
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
	case "tuple_expression", "tuple_type", "tuple_pattern":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "infix_expression":
		if len(n.Children) >= 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[2], indent)
			for i := 3; i < len(n.Children); i++ {
				b.WriteByte(' ')
				writeExpr(b, n.Children[i], indent)
			}
		} else if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "ascription_expression":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(": _*")
		}
	case "case_block":
		b.WriteString("{ ")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
		b.WriteString(" }")
	case "case_clause":
		b.WriteString("case ")
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
		}
		if len(n.Children) >= 2 {
			b.WriteString(" => ")
			writeExpr(b, n.Children[1], indent)
		}
	case "case_class_pattern":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('(')
			for i, c := range n.Children[1:] {
				if i > 0 {
					b.WriteString(", ")
				}
				writeExpr(b, c, indent)
			}
			b.WriteByte(')')
		}
	case "lambda_expression":
		if len(n.Children) >= 2 {
			b.WriteByte('(')
			if len(n.Children[0].Children) > 0 {
				for i, bind := range n.Children[0].Children {
					if i > 0 {
						b.WriteString(", ")
					}
					if len(bind.Children) >= 1 {
						b.WriteString(bind.Children[0].Text)
					}
					if len(bind.Children) >= 2 {
						b.WriteString(": ")
						writeExpr(b, bind.Children[1], indent)
					}
				}
			}
			b.WriteString(") => ")
			writeExpr(b, n.Children[1], indent)
		}
	case "block":
		b.WriteString("{\n")
		writeBlock(b, n, indent+1)
		b.WriteString(strings.Repeat("  ", indent))
		b.WriteString("}")
	case "indented_block":
		b.WriteString("{\n")
		writeBlock(b, n, indent+1)
		b.WriteString(strings.Repeat("  ", indent))
		b.WriteString("}")
	default:
		b.WriteString(n.Kind)
	}
}
