//go:build slow

package ruby

import (
	"fmt"
	"os"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// convertNode traverses the Ruby AST and emits Mochi source lines.
func convertNode(n Node, level int, out *[]string) {
	idt := strings.Repeat("  ", level)
	switch n.Type {
	case "program":
		for _, c := range n.Children {
			convertNode(c, level, out)
		}
	case "command", "method_add_arg":
		if len(n.Children) > 0 {
			f := n.Children[0]
			if (f.Type == "@ident" && f.Value == "puts") ||
				(f.Type == "fcall" && len(f.Children) > 0 && f.Children[0].Type == "@ident" && f.Children[0].Value == "puts") {
				argNode := n.Children[len(n.Children)-1]
				base := firstArg(argNode)
				if val, ok := digitsStringValue(base); ok {
					*out = append(*out, idt+"print(int(\""+val+"\"))")
					return
				}
				if base.Type == "call" && len(base.Children) == 3 && base.Children[2].Type == "@ident" && base.Children[2].Value == "length" {
					expr := exprString(base.Children[0])
					*out = append(*out, idt+"print(len("+expr+"))")
					return
				}
				if base.Type == "ifop" {
					cond := exprString(base.Children[0])
					t := exprString(base.Children[1])
					e := exprString(base.Children[2])
					if t == "1" && e == "0" {
						*out = append(*out, idt+"print("+cond+")")
					} else {
						*out = append(*out, idt+"print(if "+cond+" then "+t+" else "+e+")")
					}
					return
				}
				arg := exprString(argNode)
				if idx := strings.Index(arg, ".join"); idx != -1 {
					pre := strings.TrimSpace(arg[:idx])
					if strings.HasPrefix(pre, "(") && strings.HasSuffix(pre, ")") {
						pre = strings.TrimSuffix(strings.TrimPrefix(pre, "("), ")")
					}
					var parts []string
					var b strings.Builder
					inQuote := false
					for _, r := range pre {
						if r == '"' {
							inQuote = !inQuote
						}
						if r == ' ' && !inQuote {
							if b.Len() > 0 {
								parts = append(parts, b.String())
								b.Reset()
							}
							continue
						}
						b.WriteRune(r)
					}
					if b.Len() > 0 {
						parts = append(parts, b.String())
					}
					if len(parts) > 0 {
						*out = append(*out, idt+"print("+strings.Join(parts, ", ")+")")
						return
					}
				}
				*out = append(*out, idt+"print("+arg+")")
				return
			}
		}
		*out = append(*out, idt+exprString(n))
	case "assign":
		if len(n.Children) < 2 {
			return
		}
		lhs := n.Children[0]
		name := exprString(lhs)
		if fields, ok := structFields(n.Children[1]); ok {
			*out = append(*out, idt+"type "+name+" {")
			for _, f := range fields {
				*out = append(*out, idt+"  "+f+": any")
			}
			*out = append(*out, idt+"}")
			return
		}
		expr := exprString(n.Children[1])
		kw := ""
		if lhs.Type == "var_field" {
			kw = "let "
			if level == 0 {
				kw = "var "
			}
		}
		if kw != "" {
			*out = append(*out, idt+kw+name+" = "+expr)
		} else {
			*out = append(*out, idt+name+" = "+expr)
		}
	case "while":
		if len(n.Children) < 2 {
			return
		}
		cond := exprString(n.Children[0])
		*out = append(*out, idt+"while "+cond+" {")
		convertNode(n.Children[1], level+1, out)
		*out = append(*out, idt+"}")
	case "return", "break", "next":
		expr := exprString(n)
		if expr == "" {
			if n.Type == "next" {
				expr = "continue"
			} else {
				expr = n.Type
			}
		} else if n.Type == "next" {
			expr = strings.Replace(expr, "next", "continue", 1)
		}
		*out = append(*out, idt+expr)
	case "for":
		if len(n.Children) < 3 {
			return
		}
		varName := exprString(n.Children[0])
		iter := exprString(n.Children[1])
		*out = append(*out, idt+"for "+varName+" in "+iter+" {")
		convertNode(n.Children[2], level+1, out)
		*out = append(*out, idt+"}")
	case "method_add_block":
		if len(n.Children) == 2 {
			call := n.Children[0]
			block := n.Children[1]
			if call.Type == "call" && len(call.Children) == 3 {
				meth := call.Children[2]
				if meth.Type == "@ident" && meth.Value == "times" {
					count := exprString(call.Children[0])
					varName := "i"
					body := block
					if block.Type == "do_block" && len(block.Children) >= 2 {
						if len(block.Children) > 0 && block.Children[0].Type == "block_var" {
							bv := block.Children[0]
							if len(bv.Children) > 0 {
								params := bv.Children[0]
								if params.Type == "params" && len(params.Children) > 0 && len(params.Children[0].Children) > 0 {
									ident := params.Children[0].Children[0]
									if ident.Type == "@ident" {
										varName = ident.Value
									}
								}
							}
						}
						body = block.Children[len(block.Children)-1]
					}
					*out = append(*out, idt+"for "+varName+" in range("+count+") {")
					convertNode(body, level+1, out)
					*out = append(*out, idt+"}")
					return
				}
				if meth.Type == "@ident" && meth.Value == "each" {
					recvNode := call.Children[0]
					if recvNode.Type == "method_add_arg" && len(recvNode.Children) >= 1 {
						inner := recvNode.Children[0]
						if inner.Type == "call" && len(inner.Children) >= 3 {
							m := inner.Children[2]
							if m.Type == "@ident" && m.Value == "keys" {
								recvNode = inner.Children[0]
							}
						}
					}
					recvExpr := exprString(recvNode)
					varName := "v"
					body := block
					if block.Type == "do_block" && len(block.Children) >= 2 {
						if len(block.Children) > 0 && block.Children[0].Type == "block_var" {
							if len(block.Children[0].Children) > 0 {
								params := block.Children[0].Children[0]
								if params.Type == "params" && len(params.Children) > 0 && len(params.Children[0].Children) > 0 {
									ident := params.Children[0].Children[0]
									if ident.Type == "@ident" {
										varName = ident.Value
									}
								}
							}
						}
						body = block.Children[len(block.Children)-1]
					}
					*out = append(*out, idt+"for "+varName+" in "+recvExpr+" {")
					convertNode(body, level+1, out)
					*out = append(*out, idt+"}")
					return
				}
			}
		}
		for _, c := range n.Children {
			convertNode(c, level, out)
		}
		return
	case "if":
		if len(n.Children) < 2 {
			return
		}
		cond := exprString(n.Children[0])
		*out = append(*out, idt+"if "+cond+" {")
		convertNode(n.Children[1], level+1, out)
		if len(n.Children) > 2 {
			var handleElse func(Node)
			handleElse = func(e Node) {
				switch e.Type {
				case "else":
					*out = append(*out, idt+"} else {")
					convertNode(e, level+1, out)
				case "elsif":
					cond := exprString(e.Children[0])
					*out = append(*out, idt+"} else if "+cond+" {")
					convertNode(e.Children[1], level+1, out)
					if len(e.Children) > 2 {
						handleElse(e.Children[2])
					}
				}
			}
			handleElse(n.Children[2])
		}
		*out = append(*out, idt+"}")
	case "case":
		if len(n.Children) < 2 {
			return
		}
		cond := exprString(n.Children[0])
		var handleCase func(Node, bool)
		handleCase = func(w Node, first bool) {
			switch w.Type {
			case "when":
				if len(w.Children) < 2 {
					return
				}
				var condParts []string
				for _, c := range w.Children[0].Children {
					condParts = append(condParts, cond+" == "+exprString(c))
				}
				if len(condParts) == 0 {
					condParts = append(condParts, cond+" == "+exprString(w.Children[0]))
				}
				cStr := strings.Join(condParts, " || ")
				if first {
					*out = append(*out, idt+"if "+cStr+" {")
				} else {
					*out = append(*out, idt+"} else if "+cStr+" {")
				}
				convertNode(w.Children[1], level+1, out)
				if len(w.Children) > 2 {
					handleCase(w.Children[2], false)
				} else {
					*out = append(*out, idt+"}")
				}
			case "else":
				*out = append(*out, idt+"} else {")
				if len(w.Children) > 0 {
					convertNode(w.Children[0], level+1, out)
				}
				*out = append(*out, idt+"}")
			}
		}
		handleCase(n.Children[1], true)
	case "else", "bodystmt":
		for _, c := range n.Children {
			convertNode(c, level, out)
		}
	case "def":
		if len(n.Children) < 3 {
			return
		}
		name := exprString(n.Children[0])
		params := params(n.Children[1])
		*out = append(*out, idt+"fun "+name+"("+params+") {")
		convertNode(n.Children[2], level+1, out)
		*out = append(*out, idt+"}")
	case "class":
		if len(n.Children) < 3 {
			return
		}
		name := exprString(n.Children[0])
		*out = append(*out, idt+"type "+name+" {")
		convertNode(n.Children[2], level+1, out)
		*out = append(*out, idt+"}")
	default:
		for _, c := range n.Children {
			convertNode(c, level, out)
		}
	}
}

func Transform(n *Node) (*ast.Node, error) {
	if n == nil {
		return nil, fmt.Errorf("nil node")
	}
	var lines []string
	convertNode(*n, 0, &lines)
	if len(lines) == 0 {
		return nil, fmt.Errorf("no convertible statements")
	}
	src := strings.Join(lines, "\n")
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}

	root := &ast.Node{Kind: "program"}
	if prog.Package != "" {
		root.Value = prog.Package
	}
	for _, st := range prog.Statements {
		root.Children = append(root.Children, ast.FromStatement(st))
	}
	return root, nil
}

// TransformFile reads a Ruby file and converts it.
func TransformFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	node, err := Parse(string(data))
	if err != nil {
		return nil, err
	}
	return Transform(node)
}
