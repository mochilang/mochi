//go:build slow

package dart

import (
	"bytes"
	"fmt"
)

// Print returns Dart source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeProgram(&b, p.Root, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func indent(b *bytes.Buffer, n int) {
	for i := 0; i < n; i++ {
		b.WriteString("  ")
	}
}

func writeProgram(b *bytes.Buffer, n *ProgramNode, indentLevel int) {
	for i := 0; i < len(n.Children); i++ {
		c := n.Children[i]
		switch c.Kind {
		case "function_signature":
			if i+1 < len(n.Children) && n.Children[i+1].Kind == "function_body" {
				writeFunction(b, c, n.Children[i+1], indentLevel)
				i++
				continue
			}
		case "class_definition":
			writeClass(b, c, indentLevel)
			continue
		}
		writeStmt(b, c, indentLevel)
	}
}

func writeFunction(b *bytes.Buffer, sig *Node, body *Node, indentLevel int) {
	indent(b, indentLevel)
	writeFuncSig(b, sig)
	b.WriteString(" {")
	b.WriteByte('\n')
	if len(body.Children) > 0 {
		writeBlock(b, body.Children[0], indentLevel+1)
	}
	indent(b, indentLevel)
	b.WriteString("}")
	b.WriteByte('\n')
}

func writeFuncSig(b *bytes.Buffer, n *Node) {
	idx := 0
	if idx < len(n.Children) && (n.Children[idx].Kind == "type_identifier" || n.Children[idx].Kind == "void_type") {
		writeExpr(b, n.Children[idx])
		idx++
		if idx < len(n.Children) && n.Children[idx].Kind == "type_arguments" {
			writeExpr(b, n.Children[idx])
			idx++
		}
		b.WriteByte(' ')
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "identifier" {
		writeExpr(b, n.Children[idx])
		idx++
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "formal_parameter_list" {
		writeParameters(b, n.Children[idx])
	} else {
		b.WriteString("()")
	}
}

func writeClass(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 2 {
		return
	}
	indent(b, indentLevel)
	b.WriteString("class ")
	writeExpr(b, n.Children[0])
	b.WriteString(" {")
	b.WriteByte('\n')
	if body := n.Children[1]; body != nil {
		for _, m := range body.Children {
			writeClassMember(b, m, indentLevel+1)
		}
	}
	indent(b, indentLevel)
	b.WriteString("}")
	b.WriteByte('\n')
}

func writeClassMember(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) == 0 {
		return
	}
	switch n.Children[0].Kind {
	case "constant_constructor_signature":
		indent(b, indentLevel)
		writeConstructor(b, n.Children[0])
		b.WriteString(";")
		b.WriteByte('\n')
	default:
		indent(b, indentLevel)
		writeFieldDecl(b, n)
		b.WriteString(";")
		b.WriteByte('\n')
	}
}

func writeConstructor(b *bytes.Buffer, n *Node) {
	b.WriteString("const ")
	idx := 0
	if idx < len(n.Children) && n.Children[idx].Kind == "const_builtin" {
		idx++
	}
	if idx < len(n.Children) {
		writeExpr(b, n.Children[idx])
		idx++
	}
	if idx < len(n.Children) {
		writeParameters(b, n.Children[idx])
	} else {
		b.WriteString("()")
	}
}

func writeFieldDecl(b *bytes.Buffer, n *Node) {
	idx := 0
	if idx < len(n.Children) && (n.Children[idx].Kind == "final_builtin" || n.Children[idx].Kind == "const_builtin") {
		b.WriteString(n.Children[idx].Text)
		b.WriteByte(' ')
		idx++
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "type_identifier" {
		writeExpr(b, n.Children[idx])
		idx++
		b.WriteByte(' ')
	}
	if idx < len(n.Children) {
		list := n.Children[idx]
		for i, id := range list.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			if len(id.Children) > 0 {
				writeExpr(b, id.Children[0])
			}
		}
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indentLevel int) {
	for _, c := range n.Children {
		writeStmt(b, c, indentLevel)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indentLevel int) {
	switch n.Kind {
	case "comment":
		indent(b, indentLevel)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "expression_statement":
		if len(n.Children) > 0 {
			indent(b, indentLevel)
			writeExpr(b, n.Children[0])
			for i := 1; i < len(n.Children); i++ {
				writeExpr(b, n.Children[i])
			}
			b.WriteString(";")
			b.WriteByte('\n')
		}
	case "return_statement":
		indent(b, indentLevel)
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0])
		}
		b.WriteString(";")
		b.WriteByte('\n')
	case "local_variable_declaration":
		indent(b, indentLevel)
		if len(n.Children) > 0 {
			writeVarDef(b, n.Children[0])
		}
		b.WriteString(";")
		b.WriteByte('\n')
	case "for_statement":
		if len(n.Children) >= 2 {
			indent(b, indentLevel)
			b.WriteString("for (")
			writeForParts(b, n.Children[0])
			b.WriteString(") {")
			b.WriteByte('\n')
			writeBlock(b, n.Children[1], indentLevel+1)
			indent(b, indentLevel)
			b.WriteString("}")
			b.WriteByte('\n')
		}
	case "if_statement":
		if len(n.Children) >= 2 {
			indent(b, indentLevel)
			b.WriteString("if (")
			writeExpr(b, n.Children[0])
			b.WriteString(") {")
			b.WriteByte('\n')
			writeBlock(b, n.Children[1], indentLevel+1)
			indent(b, indentLevel)
			b.WriteString("}")
			b.WriteByte('\n')
		}
	default:
		// fall back to expression statement
		indent(b, indentLevel)
		writeExpr(b, n)
		b.WriteString(";")
		b.WriteByte('\n')
	}
}

func writeVarDef(b *bytes.Buffer, n *Node) {
	idx := 0
	// Optional qualifiers like 'final', 'const', or inferred 'var'.
	if idx < len(n.Children) {
		switch n.Children[idx].Kind {
		case "final_builtin", "const_builtin", "inferred_type":
			b.WriteString(n.Children[idx].Text)
			b.WriteByte(' ')
			idx++
		}
	}
	// Type information if present.
	if idx < len(n.Children) && n.Children[idx].Kind == "type_identifier" {
		writeExpr(b, n.Children[idx])
		idx++
		if idx < len(n.Children) && n.Children[idx].Kind == "type_arguments" {
			writeExpr(b, n.Children[idx])
			idx++
		}
		b.WriteByte(' ')
	} else if idx < len(n.Children) && n.Children[idx].Kind == "inferred_type" {
		// handle 'var' when preceding type was not printed above
		b.WriteString(n.Children[idx].Text)
		b.WriteByte(' ')
		idx++
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "identifier" {
		writeExpr(b, n.Children[idx])
		idx++
	}
	if idx < len(n.Children) {
		b.WriteString(" = ")
		writeExpr(b, n.Children[idx])
		for i := idx + 1; i < len(n.Children); i++ {
			writeExpr(b, n.Children[i])
		}
	}
}

func writeForParts(b *bytes.Buffer, n *Node) {
	// for-in loop: var x in xs
	if len(n.Children) >= 3 && n.Children[0].Kind == "inferred_type" {
		b.WriteString(n.Children[0].Text)
		b.WriteByte(' ')
		writeExpr(b, n.Children[1])
		b.WriteString(" in ")
		writeExpr(b, n.Children[2])
		return
	}
	// classic for(init; cond; update)
	if len(n.Children) >= 1 {
		switch n.Children[0].Kind {
		case "local_variable_declaration":
			writeVarDef(b, n.Children[0].Children[0])
		default:
			writeExpr(b, n.Children[0])
		}
	}
	b.WriteString("; ")
	if len(n.Children) >= 2 {
		writeExpr(b, n.Children[1])
	}
	b.WriteString("; ")
	if len(n.Children) >= 3 {
		writeExpr(b, n.Children[2])
	}
}

func writeParameters(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 1 && n.Children[0].Kind == "optional_formal_parameters" {
		b.WriteString("({")
		for i, p := range n.Children[0].Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeParam(b, p)
		}
		b.WriteString("})")
		return
	}
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeParam(b, c)
	}
	b.WriteByte(')')
}

func writeParam(b *bytes.Buffer, n *Node) {
	if n.Kind == "formal_parameter" && len(n.Children) > 0 {
		n = n.Children[0]
	}
	if n.Kind == "constructor_param" {
		if len(n.Children) == 2 && n.Children[0].Kind == "this" && n.Children[1].Kind == "identifier" {
			b.WriteString("this.")
			b.WriteString(n.Children[1].Text)
			return
		}
	}
	writeExpr(b, n)
}

func writeExpr(b *bytes.Buffer, n *Node) {
	switch n.Kind {
	case "identifier", "decimal_integer_literal", "comment", "type_identifier", "void_type", "true", "false", "null", "const_builtin", "final_builtin", "inferred_type", "this":
		b.WriteString(n.Text)
	case "string_literal":
		if n.Text != "" {
			b.WriteString(n.Text)
		} else {
			b.WriteByte('"')
			for _, c := range n.Children {
				writeExpr(b, c)
			}
			b.WriteByte('"')
		}
	case "escape_sequence":
		b.WriteString(n.Text)
	case "type_arguments":
		b.WriteByte('<')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c)
		}
		b.WriteByte('>')
	case "list_literal":
		b.WriteByte('[')
		for i := 0; i < len(n.Children); i++ {
			c := n.Children[i]
			if i > 0 {
				prev := n.Children[i-1]
				if c.Kind == "for_element" || prev.Kind == "for_element" {
					b.WriteByte(' ')
				} else {
					b.WriteString(", ")
				}
			}
			if c.Kind == "identifier" && i+1 < len(n.Children) && n.Children[i+1].Kind == "selector" {
				writeExpr(b, c)
				writeExpr(b, n.Children[i+1])
				i++
				continue
			}
			writeExpr(b, c)
		}
		b.WriteByte(']')
	case "for_element":
		if len(n.Children) >= 2 {
			b.WriteString("for (")
			writeForParts(b, n.Children[0])
			b.WriteString(") ")
			for i := 1; i < len(n.Children); i++ {
				if i > 1 && !(n.Children[i-1].Kind == "identifier" && n.Children[i].Kind == "selector") {
					b.WriteString(" ")
				}
				writeExpr(b, n.Children[i])
			}
		}
	case "additive_expression":
		if len(n.Children) >= 2 {
			for i := 0; i < len(n.Children)-1; i++ {
				writeExpr(b, n.Children[i])
			}
			b.WriteString(" + ")
			writeExpr(b, n.Children[len(n.Children)-1])
		}
	case "unary_expression":
		if len(n.Children) == 1 {
			b.WriteByte('-')
			writeExpr(b, n.Children[0])
		}
	case "relational_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0])
			b.WriteString(" < ")
			writeExpr(b, n.Children[1])
		}
	case "equality_expression":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0])
			for i := 1; i < len(n.Children)-1; i++ {
				writeExpr(b, n.Children[i])
			}
			b.WriteString(" == ")
			writeExpr(b, n.Children[len(n.Children)-1])
		}
	case "postfix_expression":
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0])
			b.WriteString("++")
		}
	case "assignable_expression":
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0])
		}
	case "selector":
		if len(n.Children) == 1 {
			child := n.Children[0]
			switch child.Kind {
			case "argument_part":
				writeExpr(b, child)
			case "unconditional_assignable_selector":
				if len(child.Children) > 0 && child.Children[0].Kind == "index_selector" {
					writeExpr(b, child.Children[0])
				} else {
					b.WriteByte('.')
					writeExpr(b, child)
				}
			}
		}
	case "argument_part":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0])
		}
	case "arguments":
		b.WriteByte('(')
		for i, arg := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, arg)
		}
		b.WriteByte(')')
	case "argument":
		for _, c := range n.Children {
			writeExpr(b, c)
		}
	case "named_argument":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0])
			b.WriteString(": ")
			for i := 1; i < len(n.Children); i++ {
				writeExpr(b, n.Children[i])
			}
		}
	case "label":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0])
		}
	case "unconditional_assignable_selector":
		if len(n.Children) > 0 {
			switch n.Children[0].Kind {
			case "identifier":
				b.WriteString(n.Children[0].Text)
			case "index_selector":
				writeExpr(b, n.Children[0])
			}
		}
	case "index_selector":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c)
		}
		b.WriteByte(']')
	default:
		// fallback: print kind
		b.WriteString(n.Kind)
	}
}
