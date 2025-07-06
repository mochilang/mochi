package cs

import (
	"fmt"
	"regexp"
	"strings"
)

// AST represents a very small subset of a C# source file.
type AST struct {
	Types []Type
}

type Type struct {
	Name    string
	Kind    string // class, struct, interface, enum
	Fields  []Field
	Methods []Func
}

type Field struct {
	Name string
	Type string
	Line int
}

type Func struct {
	Name   string
	Params []Param
	Ret    string
	Body   []string
}

type Param struct {
	Name string
	Type string
}

var (
	typeRE  = regexp.MustCompile(`(?i)^\s*(?:public\s+)?(class|struct|interface|enum)\s+([A-Za-z_][A-Za-z0-9_]*)`)
	funcRE  = regexp.MustCompile(`(?i)^\s*(?:public\s+|private\s+|protected\s+)?(?:static\s+)?([A-Za-z0-9_<>\[\]]+)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*{`)
	fieldRE = regexp.MustCompile(`(?i)^\s*(?:public\s+|private\s+|protected\s+)?([A-Za-z0-9_<>\[\]]+)\s+([A-Za-z_][A-Za-z0-9_]*)`)
)

// parseSimple parses a restricted subset of C# source code.
func parseSimple(src string) (*AST, error) {
	lines := strings.Split(src, "\n")
	var ast AST
	var cur *Type
	depth := 0
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		if strings.HasPrefix(l, "//") || l == "" {
			continue
		}
		if m := typeRE.FindStringSubmatch(l); m != nil && strings.HasSuffix(l, "{") {
			t := &Type{Name: m[2], Kind: strings.ToLower(m[1])}
			ast.Types = append(ast.Types, *t)
			cur = &ast.Types[len(ast.Types)-1]
			depth++
			continue
		}
		if strings.Contains(l, "{") {
			depth++
		}
		if strings.Contains(l, "}") {
			depth--
			if depth == 0 {
				cur = nil
			}
			continue
		}
		if cur == nil {
			continue
		}
		if m := funcRE.FindStringSubmatch(l); m != nil {
			fn := Func{Name: m[2], Ret: m[1]}
			params := strings.Split(strings.TrimSpace(m[3]), ",")
			for _, p := range params {
				p = strings.TrimSpace(p)
				if p == "" {
					continue
				}
				parts := strings.Fields(p)
				if len(parts) == 1 {
					fn.Params = append(fn.Params, Param{Name: parts[0]})
				} else {
					fn.Params = append(fn.Params, Param{Type: parts[0], Name: parts[1]})
				}
			}
			// capture body
			var body []string
			braces := 1
			for j := i + 1; j < len(lines); j++ {
				if strings.Contains(lines[j], "{") {
					braces++
				}
				if strings.Contains(lines[j], "}") {
					braces--
					if braces == 0 {
						i = j
						break
					}
				}
				body = append(body, lines[j])
			}
			fn.Body = body
			cur.Methods = append(cur.Methods, fn)
			continue
		}
		if m := fieldRE.FindStringSubmatch(l); m != nil && strings.HasSuffix(l, ";") {
			cur.Fields = append(cur.Fields, Field{Name: m[2], Type: m[1], Line: i})
			continue
		}
	}
	if len(ast.Types) == 0 {
		return nil, fmt.Errorf("no types found")
	}
	return &ast, nil
}
