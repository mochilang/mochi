package ex

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
)

// Func represents a parsed Elixir function.
type Func struct {
	Name      string   `json:"name"`
	Params    []string `json:"params"`
	Body      []string `json:"body"`
	StartLine int      `json:"start"`
	EndLine   int      `json:"end"`
	Doc       string   `json:"doc,omitempty"`
	Raw       []string `json:"raw,omitempty"`
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

// ConvertError represents a detailed parsing or conversion error.
type ConvertError struct {
	Line int
	Msg  string
	Snip string
}

func (e *ConvertError) Error() string {
	if e.Line > 0 {
		return fmt.Sprintf("line %d: %s\n%s", e.Line, e.Msg, e.Snip)
	}
	return fmt.Sprintf("%s\n%s", e.Msg, e.Snip)
}

func newConvertError(line int, lines []string, msg string) error {
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line
	if end >= len(lines) {
		end = len(lines) - 1
	}
	var b strings.Builder
	for i := start; i <= end; i++ {
		prefix := "   "
		if i+1 == line {
			prefix = ">>>"
		}
		fmt.Fprintf(&b, "%s %d: %s\n", prefix, i+1, strings.TrimRight(lines[i], "\n"))
	}
	return &ConvertError{Line: line, Msg: msg, Snip: strings.TrimRight(b.String(), "\n")}
}

func validateWithElixir(src string) error {
	if _, err := exec.LookPath("elixir"); err != nil {
		return nil
	}
	cmd := exec.Command("elixir", "-e", "Code.string_to_quoted!(IO.read(:stdio, :eof))")
	cmd.Stdin = strings.NewReader(src)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		out := strings.TrimSpace(stderr.String())
		if out == "" {
			return err
		}
		line := 0
		if m := regexp.MustCompile(`nofile:(\d+)`).FindStringSubmatch(out); m != nil {
			line, _ = strconv.Atoi(m[1])
		}
		if line > 0 {
			return newConvertError(line, strings.Split(src, "\n"), out)
		}
		return fmt.Errorf("%s", out)
	}
	return nil
}

// Parse parses a subset of Elixir into an AST structure.
func Parse(src string) (*AST, error) {
	if err := validateWithElixir(src); err != nil {
		return nil, err
	}
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
		var docLines []string
		for j := i - 1; j >= 0; j-- {
			l := strings.TrimSpace(lines[j])
			if strings.HasPrefix(l, "#") {
				docLines = append([]string{strings.TrimSpace(strings.TrimPrefix(l, "#"))}, docLines...)
				continue
			}
			if l == "" {
				continue
			}
			break
		}
		var body []string
		startLine := i + 1
		endLine := startLine
		for j := i + 1; j < len(lines); j++ {
			l := strings.TrimSpace(lines[j])
			if l == "end" {
				endLine = j + 1
				i = j
				break
			}
			body = append(body, l)
		}
		fn := Func{Name: name, Params: params, Body: body, StartLine: startLine, EndLine: endLine}
		if len(docLines) > 0 {
			fn.Doc = strings.Join(docLines, "\n")
		}
		fn.Raw = lines[startLine-1 : endLine]
		ast.Funcs = append(ast.Funcs, fn)
	}
	if len(ast.Funcs) == 0 {
		return nil, newConvertError(1, lines, "no functions found")
	}
	return ast, nil
}

func translateLine(l string) string {
	l = strings.TrimSpace(l)
	if strings.HasPrefix(l, "IO.puts(") && strings.HasSuffix(l, ")") {
		expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts("), ")")
		return "print(" + translateExpr(strings.TrimSpace(expr)) + ")"
	}
	if idx := strings.Index(l, "="); idx > 0 {
		left := strings.TrimSpace(l[:idx])
		right := translateExpr(strings.TrimSpace(l[idx+1:]))
		return "let " + left + " = " + right
	}
	return translateExpr(l)
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

// ConvertParsed parses Elixir source using Parse and converts it to Mochi.
func ConvertParsed(src string) ([]byte, error) {
	ast, err := Parse(src)
	if err != nil {
		return nil, err
	}
	return ConvertAST(ast)
}

// ConvertFileParsed reads an Elixir file and converts it using ConvertParsed.
func ConvertFileParsed(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertParsed(string(data))
}
