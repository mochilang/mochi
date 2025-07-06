package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

// ExFunc represents a parsed Elixir function.
type ExFunc struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   []string `json:"body"`
}

// ExAST is a collection of functions in a source file.
type ExAST struct {
	Funcs []ExFunc `json:"funcs"`
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

// ParseExAST parses a subset of Elixir into an AST structure.
func ParseExAST(src string) (*ExAST, error) {
	lines := strings.Split(src, "\n")
	ast := &ExAST{}
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
		ast.Funcs = append(ast.Funcs, ExFunc{Name: name, Params: params, Body: body})
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

// ConvertExAST converts a parsed ExAST to Mochi source code.
func ConvertExAST(ast *ExAST) ([]byte, error) {
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

// ConvertEx2 parses Elixir source using ParseExAST and converts it to Mochi.
func ConvertEx2(src string) ([]byte, error) {
	ast, err := ParseExAST(src)
	if err != nil {
		return nil, err
	}
	return ConvertExAST(ast)
}

// ConvertExFile2 reads an Elixir file and converts it using ConvertEx2.
func ConvertExFile2(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertEx2(string(data))
}
