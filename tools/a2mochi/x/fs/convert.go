package fs

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// ParseFile reads the F# source at path and parses it into a Program.
func ParseFile(path string) (*Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}

// ConvertSource converts a parsed F# Program into Mochi source code.
func ConvertSource(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b strings.Builder
	if len(p.Stmts) > 0 {
		writeStmts(&b, p.Stmts, 0)
		return b.String(), nil
	}
	if len(p.Vars) > 0 || len(p.Prints) > 0 {
		for _, v := range p.Vars {
			kw := "let "
			if v.Mutable {
				kw = "var "
			}
			b.WriteString(kw)
			b.WriteString(v.Name)
			if v.Type != "" {
				if t := mapType(v.Type); t != "" {
					b.WriteString(": ")
					b.WriteString(t)
				}
			}
			if v.Expr != "" {
				b.WriteString(" = ")
				b.WriteString(fixIndex(v.Expr))
			}
			b.WriteByte('\n')
		}
		for _, p := range p.Prints {
			b.WriteString("print(")
			b.WriteString(p)
			b.WriteString(")\n")
		}
		return b.String(), nil
	}
	return "", fmt.Errorf("unsupported")
}

// Convert converts a parsed Program into a Mochi AST node.
func Convert(p *Program) (*ast.Node, error) {
	src, err := ConvertSource(p)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

var indexRe = regexp.MustCompile(`(\w+)\.\[(.+)\]`)

func fixIndex(expr string) string {
	expr = indexRe.ReplaceAllString(expr, `$1[$2]`)
	expr = strings.ReplaceAll(expr, "[|", "[")
	expr = strings.ReplaceAll(expr, "|]", "]")
	expr = strings.ReplaceAll(expr, ";", ",")
	return expr
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "unit":
		return ""
	case "int", "int32", "int64", "uint32", "uint64":
		return "int"
	case "float", "double", "decimal":
		return "float"
	case "bool":
		return "bool"
	case "string":
		return "string"
	}
	if strings.HasSuffix(t, " list") {
		inner := mapType(strings.TrimSuffix(t, " list"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := mapType(strings.TrimSuffix(t, " array"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return ""
}

func writeStmts(out *strings.Builder, stmts []Stmt, indent int) {
	idt := strings.Repeat("  ", indent)
	for _, s := range stmts {
		switch v := s.(type) {
		case Var:
			kw := "let "
			if v.Mutable {
				kw = "var "
			}
			out.WriteString(idt + kw + v.Name)
			if v.Type != "" {
				if t := mapType(v.Type); t != "" {
					out.WriteString(": " + t)
				}
			}
			if v.Expr != "" {
				out.WriteString(" = " + fixIndex(v.Expr))
			}
			out.WriteByte('\n')
		case Assign:
			lhs := v.Name
			if v.Index != "" {
				lhs += "[" + fixIndex(v.Index) + "]"
			}
			out.WriteString(idt + lhs + " = " + fixIndex(v.Expr) + "\n")
		case Print:
			out.WriteString(idt + "print(" + fixIndex(v.Expr) + ")\n")
		case Expect:
			out.WriteString(idt + "expect(" + v.Cond + ")\n")
		case ForRange:
			out.WriteString(idt + "for " + v.Var + " in " + v.Start + ".." + v.End + " {\n")
			writeStmts(out, v.Body, indent+1)
			out.WriteString(idt + "}\n")
		case ForIn:
			out.WriteString(idt + "for " + v.Var + " in " + v.Expr + " {\n")
			writeStmts(out, v.Body, indent+1)
			out.WriteString(idt + "}\n")
		case While:
			out.WriteString(idt + "while " + v.Cond + " {\n")
			writeStmts(out, v.Body, indent+1)
			out.WriteString(idt + "}\n")
		case If:
			out.WriteString(idt + "if " + fixIndex(v.Cond) + " {\n")
			writeStmts(out, v.Then, indent+1)
			if len(v.Else) > 0 {
				out.WriteString(idt + "} else {\n")
				writeStmts(out, v.Else, indent+1)
			}
			out.WriteString(idt + "}\n")
		case Return:
			out.WriteString(idt + "return " + fixIndex(v.Expr) + "\n")
		case Fun:
			out.WriteString(idt + "fun " + v.Name + "(")
			for i, p := range v.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.Name)
			}
			out.WriteByte(')')
			if t := mapType(v.Ret); t != "" {
				out.WriteString(": " + t)
			}
			out.WriteString(" {\n")
			writeStmts(out, v.Body, indent+1)
			out.WriteString(idt + "}\n")
		case TypeDecl:
			if len(v.Fields) > 0 {
				out.WriteString(idt + "type " + v.Name + " {\n")
				for _, f := range v.Fields {
					typ := mapType(f.Type)
					if typ == "" {
						typ = "any"
					}
					out.WriteString(idt + "  " + f.Name + ": " + typ + "\n")
				}
				out.WriteString(idt + "}\n")
			} else if len(v.Variants) > 0 {
				out.WriteString(idt + "type " + v.Name + " =\n")
				for i, vr := range v.Variants {
					sep := "  "
					if i > 0 {
						sep = "  | "
					}
					out.WriteString(idt + sep + vr.Name)
					if len(vr.Fields) > 0 {
						out.WriteByte('(')
						for j, f := range vr.Fields {
							if j > 0 {
								out.WriteString(", ")
							}
							typ := mapType(f.Type)
							if typ == "" {
								typ = "any"
							}
							out.WriteString(f.Name + ": " + typ)
						}
						out.WriteByte(')')
					}
					out.WriteByte('\n')
				}
			} else {
				out.WriteString(idt + "type " + v.Name + " {}\n")
			}
		}
	}
}
