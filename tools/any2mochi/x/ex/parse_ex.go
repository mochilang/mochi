package ex

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

// Func represents a parsed Elixir function.
type Func struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   []string `json:"body"`
}

// AST is a collection of functions in a source file.
type AST struct {
	Funcs []Func `json:"funcs"`
}

var fnHeader = regexp.MustCompile(`^def\s+([a-zA-Z0-9_]+)(?:\(([^)]*)\))?\s*do\s*$`)

func parseParams(paramStr string) []string {
	if strings.TrimSpace(paramStr) == "" {
		return nil
	}
	parts := strings.Split(paramStr, ",")
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p != "" {
			out = append(out, p)
		}
	}
	return out
}

// Parse parses a subset of Elixir into an AST structure.
func Parse(src string) (*AST, error) {
	lines := strings.Split(src, "\n")
	ast := &AST{}
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		m := fnHeader.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		name := m[1]
		params := parseParams(m[2])
		var body []string
		for j := i + 1; j < len(lines); j++ {
			l := strings.TrimSpace(lines[j])
			if l == "end" {
				i = j
				break
			}
			body = append(body, l)
		}
		ast.Funcs = append(ast.Funcs, Func{Name: name, Params: params, Body: body})
	}
	if len(ast.Funcs) == 0 {
		return nil, fmt.Errorf("no functions found")
	}
	return ast, nil
}

func translateLine(l string) string {
	l = strings.TrimSpace(l)
	if strings.HasPrefix(l, "IO.puts(") && strings.HasSuffix(l, ")") {
		expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts("), ")")
		return "print(" + strings.TrimSpace(expr) + ")"
	}
	if idx := strings.Index(l, "="); idx > 0 {
		left := strings.TrimSpace(l[:idx])
		right := strings.TrimSpace(l[idx+1:])
		return "let " + left + " = " + right
	}
	return l
}

// ConvertAST converts a parsed AST to Mochi source code.
func ConvertAST(ast *AST) ([]byte, error) {
	var out strings.Builder
	for _, fn := range ast.Funcs {
		out.WriteString("fun ")
		out.WriteString(fn.Name)
		out.WriteByte('(')
		out.WriteString(strings.Join(fn.Params, ", "))
		out.WriteString(") {\n")
		for _, l := range fn.Body {
			if l == "" {
				continue
			}
			out.WriteString("  ")
			out.WriteString(translateLine(l))
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
		if fn.Name == "main" {
			out.WriteString("main()\n")
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("empty ast")
	}
	return []byte(out.String()), nil
}

// Convert parses Elixir source using Parse and converts it to Mochi.
func Convert(src string) ([]byte, error) {
	ast, err := Parse(src)
	if err != nil {
		return nil, err
	}
	return ConvertAST(ast)
}

// ConvertFile reads an Elixir file and converts it using Convert.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}
