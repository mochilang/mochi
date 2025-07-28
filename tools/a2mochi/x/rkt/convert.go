//go:build slow

package rkt

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
)

// Convert parses Racket source code and converts it to a Mochi AST node.
func Convert(src string) (*ast.Node, error) {
	forms, err := ParseForms(src)
	if err != nil {
		return nil, err
	}
	mochiSrc := ConvertSource(forms, src)
	prog, err := parser.ParseString(mochiSrc)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// ConvertSource converts parsed forms into Mochi source code string.
func ConvertSource(forms []Form, original string) string {
	var b strings.Builder
	b.Write(meta.Header("//"))
	if strings.TrimSpace(original) != "" {
		b.WriteString("/*\n")
		b.WriteString(original)
		if !strings.HasSuffix(original, "\n") {
			b.WriteByte('\n')
		}
		b.WriteString("*/\n")
	}
	for _, f := range forms {
		emitForm(&b, f.Datum)
	}
	return b.String()
}

func emitForm(b *strings.Builder, d any) {
	list, ok := d.([]any)
	if !ok || len(list) == 0 {
		return
	}
	head, _ := list[0].(string)
	switch head {
	case "define":
		if fn, ok := list[1].([]any); ok {
			name, _ := fn[0].(string)
			var params []string
			for _, p := range fn[1:] {
				if s, ok := p.(string); ok {
					params = append(params, s)
				}
			}
			b.WriteString("fun ")
			b.WriteString(name)
			b.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(p)
				b.WriteString(": int")
			}
			b.WriteString(") int {\n")
			for _, body := range list[2:] {
				b.WriteString("  return ")
				emitExpr(b, body)
				b.WriteByte('\n')
			}
			b.WriteString("}\n")
		} else if name, ok := list[1].(string); ok && len(list) >= 3 {
			b.WriteString("let ")
			b.WriteString(name)
			b.WriteString(" = ")
			emitExpr(b, list[2])
			b.WriteByte('\n')
		}
	case "displayln":
		b.WriteString("print(")
		if len(list) > 1 {
			emitExpr(b, list[1])
		}
		b.WriteString(")\n")
	case "for":
		if len(list) >= 3 {
			if bindings, ok := list[1].([]any); ok && len(bindings) > 0 {
				if bind, ok := bindings[0].([]any); ok && len(bind) == 2 {
					varName, _ := bind[0].(string)
					b.WriteString("for ")
					b.WriteString(varName)
					b.WriteString(" in ")
					emitExpr(b, bind[1])
					b.WriteString(" {\n")
					for _, st := range list[2:] {
						b.WriteString("  ")
						emitForm(b, st)
					}
					b.WriteString("}\n")
				}
			}
		}
	case "set!":
		if len(list) == 3 {
			if name, ok := list[1].(string); ok {
				if call, ok := list[2].([]any); ok && len(call) == 4 && call[0] == "list-set" {
					if target, ok := call[1].(string); ok && target == name {
						b.WriteString(name)
						b.WriteByte('[')
						emitExpr(b, call[2])
						b.WriteString("] = ")
						emitExpr(b, call[3])
						b.WriteByte('\n')
						return
					}
				}
				b.WriteString(name)
				b.WriteString(" = ")
				emitExpr(b, list[2])
				b.WriteByte('\n')
			}
		}
	default:
		// treat as expression
		emitExpr(b, d)
		b.WriteByte('\n')
	}
}

func emitExpr(b *strings.Builder, d any) {
	switch v := d.(type) {
	case string:
		if v == "#t" {
			b.WriteString("true")
		} else if v == "#f" {
			b.WriteString("false")
		} else if strings.HasPrefix(v, "\"") && strings.HasSuffix(v, "\"") {
			b.WriteString(v)
		} else {
			b.WriteString(v)
		}
	case float64:
		if v == float64(int(v)) {
			fmt.Fprintf(b, "%d", int(v))
		} else {
			fmt.Fprintf(b, "%g", v)
		}
	case []any:
		if len(v) == 0 {
			b.WriteString("[]")
			return
		}
		head, _ := v[0].(string)
		switch head {
		case "list":
			b.WriteByte('[')
			for i, e := range v[1:] {
				if i > 0 {
					b.WriteString(", ")
				}
				emitExpr(b, e)
			}
			b.WriteByte(']')
		case "hash":
			b.WriteByte('{')
			for i := 1; i+1 < len(v); i += 2 {
				if i > 1 {
					b.WriteString(", ")
				}
				k, _ := v[i].(string)
				fmt.Fprintf(b, "%s: ", strings.Trim(k, "\""))
				emitExpr(b, v[i+1])
			}
			b.WriteByte('}')
		case "string-ref":
			emitExpr(b, v[1])
			b.WriteByte('[')
			emitExpr(b, v[2])
			b.WriteByte(']')
		case "list-ref":
			emitExpr(b, v[1])
			b.WriteByte('[')
			emitExpr(b, v[2])
			b.WriteByte(']')
		case "substring":
			b.WriteString("substr(")
			emitExpr(b, v[1])
			b.WriteString(", ")
			emitExpr(b, v[2])
			if len(v) > 3 {
				b.WriteString(", ")
				emitExpr(b, v[3])
			}
			b.WriteByte(')')
		default:
			b.WriteString(head)
			b.WriteByte('(')
			for i, e := range v[1:] {
				if i > 0 {
					b.WriteString(", ")
				}
				emitExpr(b, e)
			}
			b.WriteByte(')')
		}
	}
}
