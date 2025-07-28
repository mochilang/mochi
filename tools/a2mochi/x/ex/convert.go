//go:build slow

package ex

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
)

var skipFuncs = map[string]bool{
	"_index_string": true,
	"_slice_string": true,
	"_count":        true,
	"_sum":          true,
	"_avg":          true,
	"_union":        true,
	"_except":       true,
	"_intersect":    true,
	"_group_by":     true,
	"_query":        true,
}

type Func struct {
	Name      string
	Params    []string
	Body      []string
	StartLine int
	EndLine   int
	Header    string
	Doc       string
	Comments  []string
	Raw       []string
}

type AST struct {
	Funcs  []Func
	Module string
}

var fnHeader = regexp.MustCompile(`^defp?\s+([a-zA-Z0-9_]+)(?:\(([^)]*)\))?\s*do\s*$`)

func parseParams(paramStr string) []string {
	if strings.TrimSpace(paramStr) == "" {
		return nil
	}
	parts := strings.Split(paramStr, ",")
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p != "" {
			if idx := strings.Index(p, "\\"); idx >= 0 {
				p = strings.TrimSpace(p[:idx])
			}
			if p != "" {
				out = append(out, p)
			}
		}
	}
	return out
}

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
		return nil
	}
	return nil
}

func Parse(src string) (*AST, error) {
	if err := validateWithElixir(src); err != nil {
		return nil, err
	}
	lines := strings.Split(src, "\n")
	ast := &AST{}
	for _, l := range lines {
		l = strings.TrimSpace(l)
		if strings.HasPrefix(l, "defmodule") {
			parts := strings.Fields(l)
			if len(parts) >= 2 {
				ast.Module = strings.TrimSpace(parts[1])
			}
			break
		}
	}
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		m := fnHeader.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		header := strings.TrimSpace(lines[i])
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
		fn := Func{Name: name, Params: params, Body: body, StartLine: startLine, EndLine: endLine, Header: header}
		if len(docLines) > 0 {
			fn.Doc = strings.Join(docLines, "\n")
			fn.Comments = docLines
		}
		fn.Raw = lines[startLine-1 : endLine]
		if !skipFuncs[fn.Name] && !strings.HasPrefix(fn.Name, "_") {
			ast.Funcs = append(ast.Funcs, fn)
		}
	}
	if len(ast.Funcs) == 0 {
		return nil, newConvertError(1, lines, "no functions found")
	}
	return ast, nil
}

func splitArgs(s string) []string {
	var args []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '(', '[', '{':
			depth++
		case ')', ']', '}':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				args = append(args, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		args = append(args, strings.TrimSpace(s[start:]))
	}
	return args
}

func hasOuterParens(s string) bool {
	if len(s) < 2 || s[0] != '(' || s[len(s)-1] != ')' {
		return false
	}
	depth := 0
	for i, r := range s {
		switch r {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 && i < len(s)-1 {
				return false
			}
		}
	}
	return depth == 0
}

func translateExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	for hasOuterParens(expr) {
		expr = strings.TrimSpace(expr[1 : len(expr)-1])
	}
	switch {
	case strings.HasPrefix(expr, "_union(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_union(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + " union " + parts[1]
		}
	case strings.HasPrefix(expr, "_except(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_except(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + " except " + parts[1]
		}
	case strings.HasPrefix(expr, "_intersect(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_intersect(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + " intersect " + parts[1]
		}
	case strings.HasPrefix(expr, "_count(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_count(") : len(expr)-1])
		if len(parts) == 1 {
			return "count(" + parts[0] + ")"
		}
	case strings.HasPrefix(expr, "_sum(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_sum(") : len(expr)-1])
		if len(parts) == 1 {
			return "sum(" + parts[0] + ")"
		}
	case strings.HasPrefix(expr, "_avg(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_avg(") : len(expr)-1])
		if len(parts) == 1 {
			return "avg(" + parts[0] + ")"
		}
	case strings.HasPrefix(expr, "_index_string(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_index_string(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + "[" + parts[1] + "]"
		}
	case strings.HasPrefix(expr, "_slice_string(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("_slice_string(") : len(expr)-1])
		if len(parts) == 3 {
			return parts[0] + "[" + parts[1] + ":" + parts[2] + "]"
		}
	case strings.HasPrefix(expr, "String.length(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("String.length(") : len(expr)-1]
		return "len(" + inner + ")"
	case strings.HasPrefix(expr, "Enum.count(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Enum.count(") : len(expr)-1]
		return "len(" + inner + ")"
	case strings.HasPrefix(expr, "Enum.sum(") && strings.HasSuffix(expr, ")"):
		inner := expr[len("Enum.sum(") : len(expr)-1]
		return "sum(" + inner + ")"
	case strings.Contains(expr, "++"):
		parts := strings.Split(expr, "++")
		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			if strings.HasPrefix(right, "[") && strings.HasSuffix(right, "]") {
				val := strings.TrimSuffix(strings.TrimPrefix(right, "["), "]")
				return "append(" + left + ", " + strings.TrimSpace(val) + ")"
			}
		}
	case strings.HasPrefix(expr, "rem(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("rem(") : len(expr)-1])
		if len(parts) == 2 {
			return parts[0] + " % " + parts[1]
		}
	case strings.Contains(expr, "<>"):
		parts := strings.Split(expr, "<>")
		if len(parts) == 2 {
			left := translateExpr(strings.TrimSpace(parts[0]))
			right := translateExpr(strings.TrimSpace(parts[1]))
			return left + " + " + right
		}
	case strings.HasPrefix(expr, "length(String.graphemes(") && strings.HasSuffix(expr, "))"):
		inner := expr[len("length(String.graphemes(") : len(expr)-2]
		return "len(" + inner + ")"
	case strings.HasPrefix(expr, "length(") && strings.HasSuffix(expr, ")"):
		parts := splitArgs(expr[len("length(") : len(expr)-1])
		if len(parts) == 1 {
			return "len(" + parts[0] + ")"
		}
	}
	return expr
}

func translateLine(l string) string {
	l = strings.TrimSpace(l)
	if strings.HasPrefix(l, "IO.puts(") && strings.HasSuffix(l, ")") {
		expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts("), ")")
		return "print(" + translateExpr(strings.TrimSpace(expr)) + ")"
	}
	if strings.HasPrefix(l, "IO.inspect(") && strings.HasSuffix(l, ")") {
		expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.inspect("), ")")
		return "print(" + translateExpr(strings.TrimSpace(expr)) + ")"
	}
	if idx := strings.Index(l, "="); idx > 0 {
		left := strings.TrimSpace(l[:idx])
		right := translateExpr(strings.TrimSpace(l[idx+1:]))
		return "let " + left + " = " + right
	}
	return translateExpr(l)
}

func convertSimpleScript(src string) string {
	lines := strings.Split(src, "\n")
	var b strings.Builder
	b.WriteString("fun main() {\n")
	for _, l := range lines {
		l = strings.TrimSpace(l)
		if l == "" || strings.HasPrefix(l, "#") {
			continue
		}
		b.WriteString("  " + translateLine(l) + "\n")
	}
	b.WriteString("}\nmain()\n")
	return b.String()
}

func convertFunc(fn Func) string {
	seg := fn.Raw
	if len(seg) == 0 {
		return ""
	}
	name := fn.Name
	paramStr := strings.Join(fn.Params, ", ")
	var b strings.Builder
	retType := ""
	b.WriteString("fun " + name + "(" + paramStr + ")")
	// return type will be set later if needed
	b.WriteString(" {")
	b.WriteByte('\n')

	retPrefix := "throw {:return,"
	level := 1
	inTry := false
	for i := 1; i < len(seg); i++ {
		l := strings.TrimSpace(seg[i])
		if l == "catch {:return, v} -> v end" {
			break
		}
		switch {
		case l == "" || strings.HasPrefix(l, "try do"):
			inTry = true
			continue
		case i+4 < len(seg) && strings.Contains(l, "= (fn ->") && strings.TrimSpace(seg[i+2]) == "cond do":
			assign := strings.TrimSpace(seg[i+1])
			parts := strings.SplitN(assign, "=", 2)
			if len(parts) == 2 {
				tmp := strings.TrimSpace(parts[0])
				expr := translateExpr(strings.TrimSpace(parts[1]))
				var cases []string
				j := i + 3
				for ; j < len(seg); j++ {
					line := strings.TrimSpace(seg[j])
					if line == "end" {
						break
					}
					if strings.HasPrefix(line, tmp+" ==") {
						idx := strings.Index(line, "->")
						if idx > 0 {
							pat := strings.TrimSpace(line[len(tmp)+3 : idx])
							res := translateExpr(strings.TrimSpace(line[idx+2:]))
							cases = append(cases, pat+" => "+res)
						}
					} else if strings.HasPrefix(line, "true ->") {
						res := translateExpr(strings.TrimSpace(line[len("true ->"):]))
						cases = append(cases, "_ => "+res)
					}
				}
				if j+1 < len(seg) && strings.TrimSpace(seg[j+1]) == "end)()" {
					b.WriteString(strings.Repeat("  ", level) + strings.TrimSpace(strings.Split(l, "=")[0]) + " = match " + expr + " {\n")
					for _, ccase := range cases {
						b.WriteString(strings.Repeat("  ", level+1) + ccase + "\n")
					}
					b.WriteString(strings.Repeat("  ", level) + "}\n")
					i = j + 1
					continue
				}
			}
		case l == "end":
			if level > 1 {
				level--
				b.WriteString(strings.Repeat("  ", level) + "}\n")
			}
			if !inTry && level == 1 {
				continue
			}
		case l == "else":
			if level > 1 {
				level--
				b.WriteString(strings.Repeat("  ", level) + "} else {\n")
				level++
			}
		case strings.HasPrefix(l, "if ") && strings.HasSuffix(l, " do"):
			cond := strings.TrimSuffix(strings.TrimPrefix(l, "if "), " do")
			b.WriteString(strings.Repeat("  ", level) + "if " + cond + " {\n")
			level++
		case strings.HasPrefix(l, "for ") && strings.Contains(l, "<-") && strings.HasSuffix(l, " do"):
			rest := strings.TrimSuffix(strings.TrimPrefix(l, "for "), " do")
			parts := strings.SplitN(rest, "<-", 2)
			if len(parts) == 2 {
				v := strings.TrimSpace(parts[0])
				coll := strings.TrimSpace(parts[1])
				b.WriteString(strings.Repeat("  ", level) + "for " + v + " in " + coll + " {\n")
				level++
			}
		case strings.HasPrefix(l, "while ") && strings.HasSuffix(l, " do"):
			cond := strings.TrimSuffix(strings.TrimPrefix(l, "while"), " do")
			b.WriteString(strings.Repeat("  ", level) + "while " + cond + " {\n")
			level++
		case strings.HasPrefix(l, retPrefix):
			val := strings.TrimSuffix(strings.TrimSpace(l[len(retPrefix):]), "}")
			b.WriteString(strings.Repeat("  ", level) + "return " + strings.TrimSpace(val) + "\n")
		case l == "throw :break":
			b.WriteString(strings.Repeat("  ", level) + "break\n")
		case l == "throw :continue":
			b.WriteString(strings.Repeat("  ", level) + "continue\n")
		case strings.HasPrefix(l, "IO.puts(Enum.join(Enum.map(") && strings.HasSuffix(l, "], &to_string(&1)), \" \"))"):
			inner := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts(Enum.join(Enum.map("), "], &to_string(&1)), \" \"))")
			args := splitArgs(inner)
			for i, a := range args {
				args[i] = translateExpr(a)
			}
			b.WriteString(strings.Repeat("  ", level) + "print(" + strings.Join(args, ", ") + ")\n")
		case strings.HasPrefix(l, "IO.puts(") && strings.HasSuffix(l, ")"):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts("), ")")
			b.WriteString(strings.Repeat("  ", level) + "print(" + translateExpr(strings.TrimSpace(expr)) + ")\n")
		case strings.HasPrefix(l, "IO.inspect(") && strings.HasSuffix(l, ")"):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.inspect("), ")")
			b.WriteString(strings.Repeat("  ", level) + "print(" + translateExpr(strings.TrimSpace(expr)) + ")\n")
		case strings.Contains(l, "="):
			parts := strings.SplitN(l, "=", 2)
			left := strings.TrimSpace(parts[0])
			right := translateExpr(strings.TrimSpace(parts[1]))
			b.WriteString(strings.Repeat("  ", level) + "let " + left + " = " + right + "\n")
		default:
			line := translateExpr(l)
			if i == len(seg)-2 {
				if line == "true" || line == "false" {
					retType = ": bool"
				} else if strings.HasPrefix(line, "\"") && strings.HasSuffix(line, "\"") {
					retType = ": string"
				} else if strings.HasPrefix(line, "[") || strings.HasPrefix(line, "{") {
					retType = ": any"
				} else if strings.HasPrefix(line, "len(") || strings.HasPrefix(line, "sum(") {
					retType = ": int"
				} else {
					retType = ": any"
				}
				b.WriteString(strings.Repeat("  ", level) + "return " + line + "\n")
			} else {
				b.WriteString(strings.Repeat("  ", level) + line + "\n")
			}
		}
	}
	b.WriteString("}")
	if retType != "" {
		// insert return type after function signature
		code := b.String()
		idx := strings.Index(code, "{")
		if idx > 0 {
			code = code[:idx] + retType + " " + code[idx:]
		}
		return code
	}
	return b.String()
}

func ConvertAST(astObj *AST) ([]byte, error) {
	var out strings.Builder
	hasMain := false
	for _, fn := range astObj.Funcs {
		code := convertFunc(fn)
		if code == "" {
			continue
		}
		out.WriteString(code)
		out.WriteByte('\n')
		if fn.Name == "main" {
			hasMain = true
		}
	}
	if hasMain {
		out.WriteString("main()\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("empty ast")
	}
	return []byte(out.String()), nil
}

func ConvertParsed(src string) ([]byte, error) {
	astObj, err := Parse(src)
	var code []byte
	if err != nil {
		if ce, ok := err.(*ConvertError); ok && strings.Contains(ce.Msg, "no functions found") {
			code = []byte(convertSimpleScript(src))
		} else {
			return nil, err
		}
	} else {
		code, err = ConvertAST(astObj)
		if err != nil {
			return nil, err
		}
	}
	var out strings.Builder
	out.Write(meta.Header("//"))
	out.WriteByte('\n')
	out.WriteString("/*\n")
	out.WriteString(src)
	if !strings.HasSuffix(src, "\n") {
		out.WriteByte('\n')
	}
	out.WriteString("*/\n")
	out.Write(code)
	return []byte(out.String()), nil
}

func ConvertFileParsed(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertParsed(string(data))
}

func Convert(src string) (*ast.Node, error) {
	code, err := ConvertParsed(src)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(string(code))
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func ConvertFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}
