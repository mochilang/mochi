//go:build slow

package rkt

import (
	"fmt"
	"os"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
)

type Item struct {
	Kind   string
	Name   string
	Params []string
	Expr   string
}

// Parse parses Racket source into high level Items using the official parser.
func Parse(src string) ([]Item, error) {
	raw, err := ParseAST(src)
	if err != nil {
		return nil, err
	}
	forms, ok := raw.([]interface{})
	if !ok {
		return nil, fmt.Errorf("unexpected AST root")
	}
	var items []Item
	for _, f := range forms {
		list, ok := f.([]interface{})
		if !ok || len(list) == 0 {
			continue
		}
		head, ok := list[0].(string)
		if !ok {
			continue
		}
		switch head {
		case "define":
			if len(list) >= 2 {
				if fn, ok := list[1].([]interface{}); ok && len(fn) > 0 {
					name, _ := fn[0].(string)
					var params []string
					for _, p := range fn[1:] {
						if s, ok := p.(string); ok {
							params = append(params, s)
						}
					}
					items = append(items, Item{Kind: "func", Name: name, Params: params})
				} else if name, ok := list[1].(string); ok {
					items = append(items, Item{Kind: "var", Name: name})
				}
			}
		case "require":
			for _, m := range list[1:] {
				if s, ok := m.(string); ok {
					items = append(items, Item{Kind: "import", Name: s})
				}
			}
		case "displayln":
			if len(list) == 2 {
				items = append(items, Item{Kind: "print", Expr: fmt.Sprintf("%v", list[1])})
			}
		}
	}
	return items, nil
}

// ConvertSource converts Items into Mochi source code.
func ConvertSource(items []Item, src string) string {
	var b strings.Builder
	b.Write(meta.Header("//"))
	if strings.TrimSpace(src) != "" {
		b.WriteString("/*\n")
		b.WriteString(src)
		if !strings.HasSuffix(src, "\n") {
			b.WriteByte('\n')
		}
		b.WriteString("*/\n")
	}
	for _, it := range items {
		switch it.Kind {
		case "func":
			b.WriteString("fun ")
			b.WriteString(sanitizeName(it.Name))
			b.WriteByte('(')
			for i, p := range it.Params {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(sanitizeName(p))
				b.WriteString(": any")
			}
			b.WriteString(") {}\n")
		case "var":
			b.WriteString("let ")
			b.WriteString(sanitizeName(it.Name))
			b.WriteByte('\n')
		case "import":
			b.WriteString("use ")
			b.WriteString(sanitizeName(it.Name))
			b.WriteByte('\n')
		case "print":
			b.WriteString("print(")
			b.WriteString(it.Expr)
			b.WriteString(")\n")
		}
	}
	return b.String()
}

// Convert parses the Racket source and returns a Mochi AST node.
func Convert(src string) (*ast.Node, error) {
	items, err := Parse(src)
	if err != nil {
		return nil, err
	}
	code := ConvertSource(items, src)
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// ConvertFile reads the file and converts it.
func ConvertFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// sanitizeName converts identifiers into Mochi compatible form.
func sanitizeName(s string) string {
	s = strings.ReplaceAll(s, "->", "_to_")
	s = strings.ReplaceAll(s, "-", "_")
	s = strings.ReplaceAll(s, "?", "_p")
	s = strings.ReplaceAll(s, "!", "_bang")
	return s
}
