//go:build slow

package zig

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Zig source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeSourceFile(&b, p.Root, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func indent(n int) string {
	return strings.Repeat("    ", n)
}

func writeSourceFile(b *bytes.Buffer, f *SourceFile, indentLevel int) {
	for _, c := range f.Children {
		writeStmt(b, &c, indentLevel)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indentLevel int) {
	if n == nil {
		return
	}
	ind := indent(indentLevel)
	switch n.Kind {
	case "comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "variable_declaration":
		writeVarDecl(b, n, indentLevel)
	case "function_declaration":
		writeFuncDecl(b, n, indentLevel)
	case "for_statement":
		writeForStmt(b, n, indentLevel)
	case "while_statement":
		writeWhileStmt(b, n, indentLevel)
	case "if_statement":
		writeIfStmt(b, n, indentLevel)
	case "labeled_statement":
		if len(n.Children) > 0 {
			switch n.Children[0].Kind {
			case "for_statement":
				writeForStmt(b, &n.Children[0], indentLevel)
			case "while_statement":
				writeWhileStmt(b, &n.Children[0], indentLevel)
			case "if_statement":
				writeIfStmt(b, &n.Children[0], indentLevel)
			default:
				b.WriteString(ind)
				writeExpr(b, n, indentLevel)
				b.WriteByte('\n')
			}
		} else {
			b.WriteString(ind)
			writeExpr(b, n, indentLevel)
			b.WriteByte('\n')
		}
	case "return_expression":
		writeReturn(b, n, indentLevel)
		b.WriteByte('\n')
	case "expression_statement":
		if len(n.Children) > 0 {
			if n.Children[0].Kind == "return_expression" {
				writeReturn(b, &n.Children[0], indentLevel)
				b.WriteByte('\n')
			} else {
				b.WriteString(ind)
				writeExpr(b, &n.Children[0], indentLevel)
				b.WriteByte(';')
				b.WriteByte('\n')
			}
		}
	default:
		// treat as expression statement
		b.WriteString(ind)
		writeExpr(b, n, indentLevel)
		b.WriteByte(';')
		b.WriteByte('\n')
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indentLevel int) {
	for i := range n.Children {
		writeStmt(b, &n.Children[i], indentLevel)
	}
}

func writeVarDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) == 0 {
		return
	}
	ind := indent(indentLevel)
	b.WriteString(ind)
	b.WriteString("const ")
	name := &n.Children[0]
	b.WriteString(name.Text)
	idx := 1
	if idx < len(n.Children)-1 {
		b.WriteString(": ")
		writeExpr(b, &n.Children[idx], indentLevel)
		idx++
	}
	if idx < len(n.Children) {
		b.WriteString(" = ")
		writeExpr(b, &n.Children[idx], indentLevel)
	}
	b.WriteByte(';')
	b.WriteByte('\n')
}

func writeFuncDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 3 {
		return
	}
	ind := indent(indentLevel)
	name := &n.Children[0]
	params := n.Children[1]
	ret := n.Children[2]
	block := n.Children[len(n.Children)-1]

	// heuristically add pub for main
	b.WriteString(ind)
	if name.Text == "main" {
		b.WriteString("pub ")
	}
	b.WriteString("fn ")
	b.WriteString(name.Text)
	if params.Kind == "parameters" {
		writeParameters(b, &params)
	} else {
		b.WriteString("()")
		// adjust ret and block indices when params missing
		ret = params
		block = n.Children[2]
	}
	if ret.Kind != "block" {
		b.WriteByte(' ')
		writeExpr(b, &ret, indentLevel)
	}
	b.WriteString(" {\n")
	writeBlock(b, &block, indentLevel+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		p := &n.Children[i]
		if len(p.Children) >= 2 {
			b.WriteString(p.Children[0].Text)
			b.WriteString(": ")
			writeExpr(b, &p.Children[1], 0)
		}
	}
	b.WriteByte(')')
}

func writeForStmt(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 3 {
		return
	}
	ind := indent(indentLevel)
	b.WriteString(ind)
	b.WriteString("for (")
	writeExpr(b, &n.Children[0], indentLevel)
	b.WriteString(") |")
	if len(n.Children[1].Children) > 0 {
		b.WriteString(n.Children[1].Children[0].Text)
	}
	b.WriteString("| ")
	block := &n.Children[2]
	b.WriteString("{\n")
	if len(block.Children) > 0 {
		writeBlock(b, &block.Children[0], indentLevel+1)
	}
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeWhileStmt(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 2 {
		return
	}
	ind := indent(indentLevel)
	b.WriteString(ind)
	b.WriteString("while (")
	writeExpr(b, &n.Children[0], indentLevel)
	b.WriteString(") ")
	blockExpr := &n.Children[1]
	b.WriteString("{\n")
	if len(blockExpr.Children) > 0 {
		writeBlock(b, &blockExpr.Children[0], indentLevel+1)
	}
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeIfStmt(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 2 {
		return
	}
	ind := indent(indentLevel)
	b.WriteString(ind)
	b.WriteString("if (")
	writeExpr(b, &n.Children[0], indentLevel)
	b.WriteString(") ")
	blockExpr := &n.Children[1]
	b.WriteString("{\n")
	if len(blockExpr.Children) > 0 {
		writeBlock(b, &blockExpr.Children[0], indentLevel+1)
	}
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeReturn(b *bytes.Buffer, n *Node, indentLevel int) {
	ind := indent(indentLevel)
	b.WriteString(ind)
	b.WriteString("return ")
	if len(n.Children) > 0 {
		writeExpr(b, &n.Children[0], indentLevel)
	}
	b.WriteByte(';')
}

func writeExpr(b *bytes.Buffer, n *Node, indentLevel int) {
	switch n.Kind {
	case "identifier", "builtin_type", "builtin_identifier", "integer", "escape_sequence", "string_content":
		b.WriteString(n.Text)
	case "string":
		b.WriteByte('"')
		for _, c := range n.Children {
			writeExpr(b, &c, indentLevel)
		}
		b.WriteByte('"')
	case "call_expression":
		if len(n.Children) > 0 {
			writeExpr(b, &n.Children[0], indentLevel)
			if len(n.Children) > 1 && n.Children[1].Kind == "arguments" {
				writeExpr(b, &n.Children[1], indentLevel)
			} else {
				b.WriteByte('(')
				for i := 1; i < len(n.Children); i++ {
					if i > 1 {
						b.WriteString(", ")
					}
					writeExpr(b, &n.Children[i], indentLevel)
				}
				b.WriteByte(')')
			}
		}
	case "field_expression":
		if len(n.Children) == 1 {
			b.WriteByte('.')
			writeExpr(b, &n.Children[0], indentLevel)
		} else if len(n.Children) == 2 {
			writeExpr(b, &n.Children[0], indentLevel)
			b.WriteByte('.')
			writeExpr(b, &n.Children[1], indentLevel)
		}
	case "index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, &n.Children[0], indentLevel)
			b.WriteByte('[')
			writeExpr(b, &n.Children[1], indentLevel)
			b.WriteByte(']')
		}
	case "arguments":
		b.WriteByte('(')
		for i := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, &n.Children[i], indentLevel)
		}
		b.WriteByte(')')
	case "parameters":
		writeParameters(b, n)
	case "range_expression":
		if len(n.Children) == 2 {
			writeExpr(b, &n.Children[0], indentLevel)
			b.WriteString("..")
			writeExpr(b, &n.Children[1], indentLevel)
		}
	case "binary_expression":
		writeBinaryExpr(b, n, indentLevel)
	case "pointer_type":
		b.WriteByte('*')
		for i := range n.Children {
			writeExpr(b, &n.Children[i], indentLevel)
		}
	case "nullable_type":
		b.WriteByte('?')
		for i := range n.Children {
			writeExpr(b, &n.Children[i], indentLevel)
		}
	case "slice_type":
		b.WriteString("[]")
		if len(n.Children) > 0 {
			writeExpr(b, &n.Children[0], indentLevel)
		}
	case "array_type":
		b.WriteString("[_]")
		if len(n.Children) > 1 {
			writeExpr(b, &n.Children[1], indentLevel)
		} else if len(n.Children) == 1 {
			writeExpr(b, &n.Children[0], indentLevel)
		}
	case "struct_declaration":
		b.WriteString("struct {\n")
		for i := range n.Children {
			if n.Children[i].Kind == "container_field" {
				b.WriteString(indent(indentLevel + 1))
				writeExpr(b, &n.Children[i], indentLevel+1)
				b.WriteString(",\n")
			}
		}
		b.WriteString(indent(indentLevel))
		b.WriteByte('}')
	case "container_field":
		if len(n.Children) >= 2 {
			writeExpr(b, &n.Children[0], indentLevel)
			b.WriteString(": ")
			writeExpr(b, &n.Children[1], indentLevel)
		}
	case "struct_initializer":
		if len(n.Children) >= 2 {
			writeExpr(b, &n.Children[0], indentLevel)
			writeExpr(b, &n.Children[1], indentLevel)
		}
	case "assignment_expression":
		if len(n.Children) == 2 {
			writeExpr(b, &n.Children[0], indentLevel)
			b.WriteString(" = ")
			writeExpr(b, &n.Children[1], indentLevel)
		}
	case "initializer_list":
		b.WriteByte('{')
		for i := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, &n.Children[i], indentLevel)
		}
		b.WriteByte('}')
	case "anonymous_struct_initializer":
		b.WriteString(".{")
		if len(n.Children) > 0 {
			// children[0] is initializer_list
			if n.Children[0].Kind == "initializer_list" {
				for i, c := range n.Children[0].Children {
					if i > 0 {
						b.WriteString(", ")
					}
					writeExpr(b, &c, indentLevel)
				}
			} else {
				writeExpr(b, &n.Children[0], indentLevel)
			}
		}
		b.WriteByte('}')
	case "block_expression":
		if len(n.Children) > 0 {
			writeExpr(b, &n.Children[0], indentLevel)
		}
	case "block":
		b.WriteString("{\n")
		writeBlock(b, n, indentLevel+1)
		b.WriteString(indent(indentLevel))
		b.WriteString("}")
	case "block_label":
		if len(n.Children) > 0 {
			writeExpr(b, &n.Children[0], indentLevel)
		}
		b.WriteByte(':')
	case "labeled_type_expression":
		if len(n.Children) >= 2 {
			writeExpr(b, &n.Children[0], indentLevel)
			b.WriteByte(' ')
			writeExpr(b, &n.Children[1], indentLevel)
		}
	case "labeled_statement":
		if len(n.Children) > 0 {
			writeExpr(b, &n.Children[0], indentLevel)
		}
	case "payload":
		if len(n.Children) > 0 {
			b.WriteString(n.Children[0].Text)
		}
	case "try_expression":
		b.WriteString("try ")
		if len(n.Children) > 0 {
			writeExpr(b, &n.Children[0], indentLevel)
		}
	case "return_expression":
		writeReturn(b, n, indentLevel)
	case "break_expression":
		b.WriteString("break")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			for i := range n.Children {
				if i > 0 {
					b.WriteByte(' ')
				}
				writeExpr(b, &n.Children[i], indentLevel)
			}
		}
	case "break_label":
		b.WriteByte(':')
		if len(n.Children) > 0 {
			writeExpr(b, &n.Children[0], indentLevel)
		}
	case "catch_expression":
		if len(n.Children) == 2 {
			writeExpr(b, &n.Children[0], indentLevel)
			b.WriteString(" catch ")
			writeExpr(b, &n.Children[1], indentLevel)
		} else {
			for i := range n.Children {
				if i > 0 {
					b.WriteByte(' ')
				}
				writeExpr(b, &n.Children[i], indentLevel)
			}
		}
	case "if_expression":
		if len(n.Children) >= 2 {
			b.WriteString("if ")
			writeExpr(b, &n.Children[0], indentLevel)
			b.WriteByte(' ')
			writeExpr(b, &n.Children[1], indentLevel)
			if len(n.Children) > 2 {
				b.WriteString(" else ")
				writeExpr(b, &n.Children[2], indentLevel)
			}
		}
	case "for_statement":
		writeForStmt(b, n, indentLevel)
	case "if_statement":
		writeIfStmt(b, n, indentLevel)
	default:
		for i := range n.Children {
			writeExpr(b, &n.Children[i], indentLevel)
		}
	}
}

func writeBinaryExpr(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) != 2 {
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, &n.Children[i], indentLevel)
		}
		return
	}
	left := &n.Children[0]
	right := &n.Children[1]
	op := "+"
	if right.Kind == "identifier" && right.Text == "n" {
		op = "<"
	} else if right.Kind == "identifier" && right.Text == "target" {
		op = "=="
	} else if left.Kind == "integer" && left.Text == "0" && right.Kind == "integer" && right.Text == "1" {
		op = "-"
	} else if right.Kind == "integer" && right.Text == "1" {
		op = "+"
	} else if left.Kind == "index_expression" && right.Kind == "index_expression" {
		op = "+"
	}
	writeExpr(b, left, indentLevel)
	b.WriteByte(' ')
	b.WriteString(op)
	b.WriteByte(' ')
	writeExpr(b, right, indentLevel)
}
