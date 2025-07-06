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
	Name      string
	Kind      string // class, struct, interface, enum
	Access    string
	StartLine int
	EndLine   int
	Fields    []Field
	Methods   []Func
	Doc       string
}

type Field struct {
	Name   string
	Type   string
	Access string
	Line   int
	Doc    string
}

type Func struct {
	Name      string
	Params    []Param
	Ret       string
	Access    string
	Static    bool
	Body      []string
	StartLine int
	EndLine   int
	Doc       string
}

type Param struct {
	Name string
	Type string
}

var (
	typeRE      = regexp.MustCompile(`(?i)^\s*(?:public\s+|private\s+|protected\s+)?(class|struct|interface|enum)\s+([A-Za-z_][A-Za-z0-9_]*)`)
	funcRE      = regexp.MustCompile(`(?i)^\s*(?:public\s+|private\s+|protected\s+)?(?:static\s+)?([A-Za-z0-9_<>\[\],\s]+)\s+([A-Za-z_][A-Za-z0-9_]*)(?:<[^>]+>)?\s*\(([^)]*)\)\s*\{?`)
	fieldRE     = regexp.MustCompile(`(?i)^\s*(?:public\s+|private\s+|protected\s+)?([A-Za-z0-9_<>\[\],\s]+)\s+([A-Za-z_][A-Za-z0-9_]*)`)
	usingRE     = regexp.MustCompile(`^\s*using\s+`)
	namespaceRE = regexp.MustCompile(`^\s*namespace\s+`)
)

// parseSimple parses a restricted subset of C# source code.
func parseSimple(src string) (*AST, error) {
	lines := strings.Split(src, "\n")
	var ast AST
	var cur *Type
	depth := 0
	var unknownLine int = -1
	var unknownCol int
	var unknownMsg string
	var pendingDoc []string
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		if strings.HasPrefix(l, "//") || l == "" {
			if strings.HasPrefix(l, "//") {
				pendingDoc = append(pendingDoc, strings.TrimSpace(strings.TrimPrefix(l, "//")))
			}
			continue
		}
		if m := typeRE.FindStringSubmatch(l); m != nil {
			t := &Type{Name: m[2], Kind: strings.ToLower(m[1]), StartLine: i + 1, Doc: strings.Join(pendingDoc, "\n")}
			if strings.Contains(l, "public") {
				t.Access = "public"
			} else if strings.Contains(l, "protected") {
				t.Access = "protected"
			} else if strings.Contains(l, "private") {
				t.Access = "private"
			}
			pendingDoc = nil
			ast.Types = append(ast.Types, *t)
			cur = &ast.Types[len(ast.Types)-1]
			if strings.Contains(l, "{") {
				depth++
			} else if i+1 < len(lines) && strings.TrimSpace(lines[i+1]) == "{" {
				depth++
				i++
			}
			continue
		}
		if strings.Contains(l, "{") {
			depth++
		}
		if strings.Contains(l, "}") {
			depth--
			if depth == 0 {
				if cur != nil {
					cur.EndLine = i + 1
				}
				cur = nil
			}
			continue
		}
		if cur == nil {
			continue
		}
		if m := funcRE.FindStringSubmatch(l); m != nil {
			fn := Func{Name: m[2], Ret: strings.TrimSpace(m[1]), StartLine: i + 1, Doc: strings.Join(pendingDoc, "\n")}
			if strings.Contains(l, "static") {
				fn.Static = true
			}
			if strings.Contains(l, "public") {
				fn.Access = "public"
			} else if strings.Contains(l, "protected") {
				fn.Access = "protected"
			} else if strings.Contains(l, "private") {
				fn.Access = "private"
			}
			pendingDoc = nil
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
			if !strings.Contains(l, "{") && i+1 < len(lines) && strings.TrimSpace(lines[i+1]) == "{" {
				i++
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
						fn.EndLine = j + 1
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
			f := Field{Name: m[2], Type: strings.TrimSpace(m[1]), Line: i + 1, Doc: strings.Join(pendingDoc, "\n")}
			if strings.Contains(l, "public") {
				f.Access = "public"
			} else if strings.Contains(l, "protected") {
				f.Access = "protected"
			} else if strings.Contains(l, "private") {
				f.Access = "private"
			}
			cur.Fields = append(cur.Fields, f)
			pendingDoc = nil
			continue
		}

		if usingRE.MatchString(l) || namespaceRE.MatchString(l) {
			continue
		}

		if unknownLine == -1 {
			unknownLine = i
			unknownCol = len(lines[i]) - len(strings.TrimLeft(lines[i], " \t"))
			unknownMsg = "unsupported line"
		}
	}
	if len(ast.Types) == 0 {
		return nil, fmt.Errorf("no types found")
	}
	if unknownLine != -1 {
		start := unknownLine - 2
		if start < 0 {
			start = 0
		}
		end := unknownLine + 2
		if end >= len(lines) {
			end = len(lines) - 1
		}
		var ctx strings.Builder
		for i := start; i <= end; i++ {
			ctx.WriteString(fmt.Sprintf("%3d| %s\n", i+1, lines[i]))
			if i == unknownLine {
				ctx.WriteString("   | " + strings.Repeat(" ", unknownCol) + "^\n")
			}
		}
		return &ast, fmt.Errorf("line %d:%d: %s\n%s", unknownLine+1, unknownCol+1, unknownMsg, strings.TrimSuffix(ctx.String(), "\n"))
	}
	return &ast, nil
}
