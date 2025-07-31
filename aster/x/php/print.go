//go:build slow

package php

import (
	"bytes"
	"fmt"
)

// Print reconstructs PHP source code from the AST contained in Program.
// Only a subset of PHP constructs used in tests are supported.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	b.WriteString("<?php\n")
	writeBlock(&b, (*Node)(p.Root), 0, false)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeBlock(b *bytes.Buffer, n *Node, indent int, braces bool) {
	if braces {
		b.WriteString("{\n")
	}
	for _, c := range n.Children {
		writeStmt(b, c, indent)
	}
	if braces {
		writeIndent(b, indent-1)
		b.WriteString("}\n")
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "expression_statement":
		if len(n.Children) > 0 {
			writeIndent(b, indent)
			writeExpr(b, n.Children[0], indent)
			b.WriteString(";\n")
		}
	case "return_statement":
		writeIndent(b, indent)
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString(";\n")
	case "echo_statement":
		writeIndent(b, indent)
		b.WriteString("echo ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString(";\n")
	case "function_definition":
		if len(n.Children) >= 3 {
			writeIndent(b, indent)
			b.WriteString("function ")
			writeExpr(b, n.Children[0], indent)
			writeParameters(b, n.Children[1])
			b.WriteString(" ")
			writeBlock(b, n.Children[2], indent+1, true)
		}
	case "anonymous_function":
		if len(n.Children) >= 2 {
			b.WriteString("function")
			idx := 0
			if n.Children[0].Kind == "formal_parameters" {
				writeParameters(b, n.Children[0])
				idx = 1
			} else {
				b.WriteString("()")
			}
			if idx < len(n.Children) && n.Children[idx].Kind == "anonymous_function_use_clause" {
				b.WriteString(" use ")
				writeUseClause(b, n.Children[idx])
				idx++
			}
			b.WriteByte(' ')
			if idx < len(n.Children) {
				writeBlock(b, n.Children[idx], indent+1, true)
			} else {
				writeBlock(b, &Node{}, indent+1, true)
			}
		}
	case "for_statement":
		if len(n.Children) == 4 {
			writeIndent(b, indent)
			b.WriteString("for (")
			writeExpr(b, n.Children[0], indent)
			b.WriteString("; ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString("; ")
			writeExpr(b, n.Children[2], indent)
			b.WriteString(") ")
			writeBlock(b, n.Children[3], indent+1, true)
		}
	case "if_statement":
		if len(n.Children) >= 2 {
			writeIndent(b, indent)
			b.WriteString("if (")
			// first child is parenthesized_expression
			cond := n.Children[0]
			if cond.Kind == "parenthesized_expression" && len(cond.Children) > 0 {
				writeExpr(b, cond.Children[0], indent)
			} else {
				writeExpr(b, cond, indent)
			}
			b.WriteString(") ")
			writeBlock(b, n.Children[1], indent+1, true)
			if len(n.Children) == 3 {
				writeIndent(b, indent)
				b.WriteString("else ")
				writeBlock(b, n.Children[2], indent+1, true)
			}
		}
	case "foreach_statement":
		if len(n.Children) >= 3 {
			writeIndent(b, indent)
			b.WriteString("foreach (")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" as ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(") ")
			writeBlock(b, n.Children[2], indent+1, true)
		}
	case "break_statement":
		writeIndent(b, indent)
		b.WriteString("break;\n")
	case "continue_statement":
		writeIndent(b, indent)
		b.WriteString("continue;\n")
	case "global_declaration":
		writeIndent(b, indent)
		b.WriteString("global ")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteString(";\n")
	case "compound_statement":
		writeIndent(b, indent)
		writeBlock(b, n, indent+1, true)
	default:
		writeIndent(b, indent)
		writeExpr(b, n, indent)
		b.WriteString(";\n")
	}
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
	case "name", "integer", "float":
		b.WriteString(n.Text)
	case "variable_name":
		b.WriteByte('$')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "encapsed_string", "string":
		if len(n.Children) > 0 {
			b.WriteByte('"')
			b.WriteString(n.Children[0].Text)
			b.WriteByte('"')
		} else {
			b.WriteByte('"')
			b.WriteString(n.Text)
			b.WriteByte('"')
		}
	case "string_content":
		b.WriteByte('"')
		b.WriteString(n.Text)
		b.WriteByte('"')
	case "simple_parameter":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "anonymous_function":
		b.WriteString("function")
		idx := 0
		if len(n.Children) > 0 && n.Children[0].Kind == "formal_parameters" {
			writeParameters(b, n.Children[0])
			idx = 1
		} else {
			b.WriteString("()")
		}
		if idx < len(n.Children) && n.Children[idx].Kind == "anonymous_function_use_clause" {
			b.WriteString(" use ")
			writeUseClause(b, n.Children[idx])
			idx++
		}
		b.WriteByte(' ')
		if idx < len(n.Children) {
			writeBlock(b, n.Children[idx], indent+1, true)
		} else {
			writeBlock(b, &Node{}, indent+1, true)
		}
	case "arrow_function":
		b.WriteString("fn")
		idx := 0
		if len(n.Children) > 0 && n.Children[0].Kind == "formal_parameters" {
			writeParameters(b, n.Children[0])
			idx = 1
		} else {
			b.WriteString("()")
		}
		b.WriteString(" => ")
		if idx < len(n.Children) {
			writeExpr(b, n.Children[idx], indent)
		}
	case "function_call_expression":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) >= 2 {
				writeArguments(b, n.Children[1])
			} else {
				b.WriteString("()")
			}
		}
	case "arguments":
		writeArguments(b, n)
	case "argument":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "binary_expression":
		if len(n.Children) >= 2 {
			op := n.Text
			if op == "" {
				op = "+"
			}
			for i, c := range n.Children {
				if i > 0 {
					b.WriteByte(' ')
					b.WriteString(op)
					b.WriteByte(' ')
				}
				writeExpr(b, c, indent)
			}
		} else if len(n.Children) == 1 {
			writeExpr(b, n.Children[0], indent)
		}
	case "assignment_expression", "augmented_assignment_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			op := n.Text
			if op == "" {
				op = "="
			}
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "update_expression":
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(n.Text)
		}
	case "unary_op_expression":
		if len(n.Children) == 1 {
			b.WriteString(n.Text)
			writeExpr(b, n.Children[0], indent)
		}
	case "parenthesized_expression":
		b.WriteByte('(')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte(')')
	case "subscript_expression":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			if len(n.Children) == 2 {
				writeExpr(b, n.Children[1], indent)
			}
			b.WriteByte(']')
		}
	case "sequence_expression":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
	case "array_creation_expression":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(']')
	case "array_element_initializer":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" => ")
			writeExpr(b, n.Children[1], indent)
		} else if len(n.Children) == 1 {
			writeExpr(b, n.Children[0], indent)
		}
	case "match_expression":
		if len(n.Children) >= 2 {
			b.WriteString("match (")
			cond := n.Children[0]
			if cond.Kind == "parenthesized_expression" && len(cond.Children) > 0 {
				writeExpr(b, cond.Children[0], indent)
			} else {
				writeExpr(b, cond, indent)
			}
			b.WriteString(") {\n")
			if blk := n.Children[1]; blk.Kind == "match_block" {
				for _, c := range blk.Children {
					writeIndent(b, indent+1)
					writeMatchArm(b, c, indent+1)
					b.WriteString(",\n")
				}
			}
			writeIndent(b, indent)
			b.WriteByte('}')
		}
	case "conditional_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" ? ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" : ")
			writeExpr(b, n.Children[2], indent)
		}
	default:
		if n.Text != "" {
			b.WriteString(n.Text)
		} else {
			b.WriteString(n.Kind)
		}
	}
}

func writeArguments(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte(')')
}

func writeUseClause(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte(')')
}

func writeIndent(b *bytes.Buffer, indent int) {
	for i := 0; i < indent; i++ {
		b.WriteString("    ")
	}
}

func writeMatchArm(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "match_conditional_expression":
		if len(n.Children) >= 2 {
			condList := n.Children[0]
			for i, c := range condList.Children {
				if i > 0 {
					b.WriteString(", ")
				}
				writeExpr(b, c, indent)
			}
			b.WriteString(" => ")
			writeExpr(b, n.Children[1], indent)
		}
	case "match_default_expression":
		b.WriteString("default => ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	default:
		writeExpr(b, n, indent)
	}
}
