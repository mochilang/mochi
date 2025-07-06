package py

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"

	pycode "mochi/compile/py"
	parent "mochi/tools/any2mochi"
)

// Convert converts Python source code to a minimal Mochi representation.
// It uses the configured language server when available, otherwise falls back to
// a small parser that invokes Python to produce an AST as JSON.
func Convert(src string) ([]byte, error) {
	ls := parent.Servers["python"]
	if ls.Command != "" {
		_ = pycode.EnsurePyright()
		syms, diags, err := parent.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
		if err == nil && len(diags) == 0 {
			var out strings.Builder
			writeSymbols(&out, nil, syms, src, ls)
			if out.Len() > 0 {
				return []byte(out.String()), nil
			}
		}
	}
	if out, err := ConvertAST(src); err == nil {
		return out, nil
	}
	return convertFallback(src)
}

// ConvertFile reads the Python file at path and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func writeSymbols(out *strings.Builder, prefix []string, syms []parent.DocumentSymbol, src string, ls parent.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case parent.SymbolKindFunction, parent.SymbolKindMethod, parent.SymbolKindConstructor:
			writeFunc(out, strings.Join(nameParts, "."), s, src, ls)
		case parent.SymbolKindClass:
			writeClass(out, nameParts, s, src, ls)
		case parent.SymbolKindVariable, parent.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ := getVarType(src, s.SelectionRange.Start, ls); typ != "" && typ != "Unknown" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				if val := getVarValue(src, s); val != "" {
					out.WriteString(" = ")
					out.WriteString(val)
				}
				out.WriteString("\n")
			}
		}
	}
}

type param struct {
	name string
	typ  string
}

func writeFunc(out *strings.Builder, name string, sym parent.DocumentSymbol, src string, ls parent.LanguageServer) {
	params, ret := getSignature(src, sym.SelectionRange.Start, ls)
	if len(params) == 0 {
		for _, p := range extractParams(sym) {
			params = append(params, param{name: p})
		}
	}
	out.WriteString("fun ")
	out.WriteString(name)
	out.WriteByte('(')
	for i, p := range params {
		if i > 0 {
			out.WriteString(", ")
		}
		out.WriteString(p.name)
		if p.typ != "" {
			out.WriteString(": ")
			out.WriteString(p.typ)
		}
	}
	out.WriteByte(')')
	if ret != "" && ret != "None" {
		out.WriteString(": ")
		out.WriteString(ret)
	}
	body := parseFunctionBody(src, sym)
	if len(body) == 0 {
		out.WriteString(" {}\n")
		return
	}
	out.WriteString(" {\n")
	for _, line := range body {
		out.WriteString("  ")
		out.WriteString(line)
		out.WriteByte('\n')
	}
	out.WriteString("}\n")
}

func writeClass(out *strings.Builder, prefix []string, sym parent.DocumentSymbol, src string, ls parent.LanguageServer) {
	name := strings.Join(prefix, ".")
	fields := extractFields(sym)
	methods := make([]parent.DocumentSymbol, 0)
	for _, c := range sym.Children {
		switch c.Kind {
		case parent.SymbolKindFunction, parent.SymbolKindMethod, parent.SymbolKindConstructor:
			methods = append(methods, c)
		}
	}
	out.WriteString("type ")
	out.WriteString(name)
	if len(fields) == 0 && len(methods) == 0 {
		out.WriteString(" {}\n")
		return
	}
	out.WriteString(" {\n")
	for _, f := range fields {
		out.WriteString("  ")
		out.WriteString(f.name)
		if typ := getVarType(src, f.sym.SelectionRange.Start, ls); typ != "" && typ != "Unknown" {
			out.WriteString(": ")
			out.WriteString(typ)
		}
		out.WriteByte('\n')
	}
	for _, m := range methods {
		var b strings.Builder
		writeFunc(&b, m.Name, m, src, ls)
		for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
			out.WriteString("  ")
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	out.WriteString("}\n")
}

type field struct {
	name string
	sym  parent.DocumentSymbol
}

func extractFields(sym parent.DocumentSymbol) []field {
	var fields []field
	for _, c := range sym.Children {
		if c.Kind == parent.SymbolKindVariable || c.Kind == parent.SymbolKindConstant {
			fields = append(fields, field{name: c.Name, sym: c})
		}
	}
	return fields
}

func extractParams(sym parent.DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == parent.SymbolKindVariable && c.Range.Start.Line == start {
			params = append(params, c.Name)
		}
	}
	return params
}

func getSignature(src string, pos parent.Position, ls parent.LanguageServer) ([]param, string) {
	hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(parent.MarkupContent)
	if !ok {
		return nil, ""
	}
	return parseSignature(mc.Value)
}

func parseSignature(sig string) ([]param, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if i := strings.Index(sig, "def "); i != -1 {
		sig = sig[i+4:]
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := strings.TrimSpace(sig[open+1 : close])
	rest := strings.TrimSpace(sig[close+1:])
	ret := ""
	if strings.HasPrefix(rest, "->") {
		ret = mapType(strings.TrimSpace(rest[2:]))
	}
	var params []param
	if paramsPart != "" {
		parts := strings.Split(paramsPart, ",")
		for i, p := range parts {
			p = strings.TrimSpace(p)
			if p == "" || strings.HasPrefix(p, "self") && i == 0 {
				continue
			}
			name := p
			typ := ""
			if colon := strings.Index(p, ":"); colon != -1 {
				name = strings.TrimSpace(p[:colon])
				typ = strings.TrimSpace(p[colon+1:])
				if eq := strings.Index(typ, "="); eq != -1 {
					typ = strings.TrimSpace(typ[:eq])
				}
				typ = mapType(typ)
			}
			params = append(params, param{name: name, typ: typ})
		}
	}
	return params, ret
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	if t == "" || t == "None" || t == "Any" || t == "Unknown" {
		return ""
	}
	if strings.Contains(t, "|") {
		var parts []string
		for _, p := range splitArgs(strings.ReplaceAll(t, "|", ",")) {
			p = strings.TrimSpace(p)
			if p == "None" || p == "" {
				continue
			}
			if mp := mapType(p); mp != "" {
				parts = append(parts, mp)
			}
		}
		if len(parts) == 1 {
			return parts[0]
		}
		if len(parts) > 1 {
			return strings.Join(parts, " | ")
		}
		return ""
	}
	if idx := strings.LastIndex(t, "."); idx != -1 {
		t = t[idx+1:]
	}
	switch t {
	case "int":
		return "int"
	case "float":
		return "float"
	case "str":
		return "string"
	case "bool":
		return "bool"
	}
	if strings.HasSuffix(t, "]") {
		open := strings.Index(t, "[")
		if open != -1 {
			outer := strings.TrimSpace(t[:open])
			inner := t[open+1 : len(t)-1]
			args := splitArgs(inner)
			switch outer {
			case "list", "List", "Sequence", "Iterable":
				innerType := "any"
				if len(args) > 0 {
					if a := mapType(args[0]); a != "" {
						innerType = a
					}
				}
				return "list<" + innerType + ">"
			case "dict", "Dict", "Mapping":
				key := "any"
				val := "any"
				if len(args) > 0 {
					if k := mapType(args[0]); k != "" {
						key = k
					}
				}
				if len(args) > 1 {
					if v := mapType(args[1]); v != "" {
						val = v
					}
				}
				return "map<" + key + ", " + val + ">"
			case "set", "Set":
				innerType := "any"
				if len(args) > 0 {
					if a := mapType(args[0]); a != "" {
						innerType = a
					}
				}
				return "set<" + innerType + ">"
			case "tuple", "Tuple":
				var mapped []string
				for _, a := range args {
					mt := mapType(a)
					if mt == "" {
						mt = "any"
					}
					mapped = append(mapped, mt)
				}
				if len(mapped) > 0 {
					return "tuple<" + strings.Join(mapped, ", ") + ">"
				}
			case "Union":
				var mapped []string
				for _, a := range args {
					mt := mapType(a)
					if mt != "" {
						mapped = append(mapped, mt)
					}
				}
				if len(mapped) == 1 {
					return mapped[0]
				}
				if len(mapped) > 1 {
					return strings.Join(mapped, " | ")
				}
				return ""
			case "Optional":
				if len(args) == 1 {
					mt := mapType(args[0])
					if mt == "" {
						mt = "any"
					}
					return mt + "?"
				}
			case "Callable":
				if len(args) == 2 {
					argList := strings.Trim(args[0], "[]")
					ret := mapType(args[1])
					var argTypes []string
					if strings.TrimSpace(argList) != "" && argList != "..." {
						for _, a := range splitArgs(argList) {
							at := mapType(a)
							if at == "" {
								at = "any"
							}
							argTypes = append(argTypes, at)
						}
					}
					if ret == "" {
						ret = "any"
					}
					return "fun(" + strings.Join(argTypes, ", ") + "): " + ret
				}
			}
		}
	}
	return t
}

func splitArgs(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '[', '(', '<':
			depth++
		case ']', ')', '>':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, strings.TrimSpace(s[start:]))
	}
	return parts
}

func getVarType(src string, pos parent.Position, ls parent.LanguageServer) string {
	hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return ""
	}
	mc, ok := hov.Contents.(parent.MarkupContent)
	if !ok {
		return ""
	}
	return parseVarType(mc.Value)
}

func parseVarType(hov string) string {
	if i := strings.Index(hov, "\n"); i != -1 {
		hov = hov[:i]
	}
	if colon := strings.Index(hov, ":"); colon != -1 {
		typ := strings.TrimSpace(hov[colon+1:])
		return mapType(typ)
	}
	return ""
}

func getVarValue(src string, sym parent.DocumentSymbol) string {
	code := extractRange(src, sym.Range)
	if idx := strings.Index(code, "="); idx != -1 {
		return strings.TrimSpace(code[idx+1:])
	}
	return ""
}

func parseFunctionBody(src string, sym parent.DocumentSymbol) []string {
	code := extractRange(src, sym.Range)
	lines := strings.Split(code, "\n")
	if len(lines) < 2 {
		return nil
	}
	indent := ""
	for _, l := range lines[1:] {
		t := strings.TrimSpace(l)
		if t != "" {
			indent = l[:len(l)-len(strings.TrimLeft(l, " \t"))]
			break
		}
	}
	if indent == "" {
		return nil
	}
	stmts, _ := parseLines(lines[1:], indent)
	return stmts
}

func parseLines(lines []string, indent string) ([]string, int) {
	var stmts []string
	i := 0
	step := indent + "    "
	for i < len(lines) {
		l := lines[i]
		if !strings.HasPrefix(l, indent) {
			if strings.TrimSpace(l) == "" {
				i++
				continue
			}
			break
		}
		s := strings.TrimSpace(l[len(indent):])
		if s == "" || strings.HasPrefix(s, "global ") || s == "pass" {
			i++
			continue
		}
		switch {
		case s == "return":
			stmts = append(stmts, "return")
			i++
		case strings.HasPrefix(s, "return "):
			stmts = append(stmts, "return "+strings.TrimSpace(s[len("return "):]))
			i++
		case strings.HasPrefix(s, "print("):
			stmts = append(stmts, s)
			i++
		case strings.HasPrefix(s, "if ") && strings.HasSuffix(s, ":"):
			cond := strings.TrimSpace(strings.TrimSuffix(s[3:], ":"))
			body, n := parseLines(lines[i+1:], step)
			i = i + 1 + n
			var elseBody []string
			if i < len(lines) {
				nextLine := lines[i]
				if strings.HasPrefix(nextLine, indent) {
					nextLine = nextLine[len(indent):]
				}
				next := strings.TrimSpace(nextLine)
				if strings.HasPrefix(next, "else:") {
					var m int
					elseBody, m = parseLines(lines[i+1:], step)
					i = i + 1 + m
				} else if strings.HasPrefix(next, "elif ") {
					var m int
					elifCond := strings.TrimSpace(strings.TrimSuffix(next[5:], ":"))
					elifBody, m := parseLines(lines[i+1:], step)
					i = i + 1 + m
					bodyBlock := formatBlock(body)
					elifBlock := formatBlock(elifBody)
					stmts = append(stmts, "if "+cond+bodyBlock+" else if "+elifCond+elifBlock)
					continue
				}
			}
			var b strings.Builder
			b.WriteString("if ")
			b.WriteString(cond)
			if len(body) == 0 {
				b.WriteString(" {}")
			} else {
				b.WriteString(" {")
				for _, st := range body {
					b.WriteString("\n  ")
					b.WriteString(st)
				}
				b.WriteString("\n}")
			}
			if len(elseBody) > 0 {
				b.WriteString(" else {")
				for _, st := range elseBody {
					b.WriteString("\n  ")
					b.WriteString(st)
				}
				b.WriteString("\n}")
			}
			stmts = append(stmts, b.String())
		case strings.HasPrefix(s, "for ") && strings.HasSuffix(s, ":"):
			rest := strings.TrimSpace(strings.TrimSuffix(s[4:], ":"))
			if idx := strings.Index(rest, " in "); idx != -1 {
				varName := strings.TrimSpace(rest[:idx])
				iterable := strings.TrimSpace(rest[idx+4:])
				body, n := parseLines(lines[i+1:], step)
				i = i + 1 + n
				var b strings.Builder
				b.WriteString("for ")
				b.WriteString(varName)
				b.WriteString(" in ")
				b.WriteString(iterable)
				if len(body) == 0 {
					b.WriteString(" {}")
				} else {
					b.WriteString(" {")
					for _, st := range body {
						b.WriteString("\n  ")
						b.WriteString(st)
					}
					b.WriteString("\n}")
				}
				stmts = append(stmts, b.String())
			} else {
				stmts = append(stmts, s)
				i++
			}
		case strings.HasPrefix(s, "while ") && strings.HasSuffix(s, ":"):
			cond := strings.TrimSpace(strings.TrimSuffix(s[6:], ":"))
			body, n := parseLines(lines[i+1:], step)
			i = i + 1 + n
			var b strings.Builder
			b.WriteString("while ")
			b.WriteString(cond)
			if len(body) == 0 {
				b.WriteString(" {}")
			} else {
				b.WriteString(" {")
				for _, st := range body {
					b.WriteString("\n  ")
					b.WriteString(st)
				}
				b.WriteString("\n}")
			}
			stmts = append(stmts, b.String())
		case strings.HasPrefix(s, "with ") && strings.HasSuffix(s, ":"):
			ctx := strings.TrimSpace(strings.TrimSuffix(s[5:], ":"))
			body, n := parseLines(lines[i+1:], step)
			i = i + 1 + n
			var b strings.Builder
			b.WriteString("with ")
			b.WriteString(ctx)
			if len(body) == 0 {
				b.WriteString(" {}")
			} else {
				b.WriteString(" {")
				for _, st := range body {
					b.WriteString("\n  ")
					b.WriteString(st)
				}
				b.WriteString("\n}")
			}
			stmts = append(stmts, b.String())
		case s == "continue":
			stmts = append(stmts, "continue")
			i++
		case s == "break":
			stmts = append(stmts, "break")
			i++
		default:
			if op := strings.Index(s, "+="); op != -1 {
				name := strings.TrimSpace(s[:op])
				expr := strings.TrimSpace(s[op+2:])
				stmts = append(stmts, "let "+name+" = "+name+" + "+expr)
				i++
			} else if op := strings.Index(s, "-="); op != -1 {
				name := strings.TrimSpace(s[:op])
				expr := strings.TrimSpace(s[op+2:])
				stmts = append(stmts, "let "+name+" = "+name+" - "+expr)
				i++
			} else if op := strings.Index(s, "*="); op != -1 {
				name := strings.TrimSpace(s[:op])
				expr := strings.TrimSpace(s[op+2:])
				stmts = append(stmts, "let "+name+" = "+name+" * "+expr)
				i++
			} else if op := strings.Index(s, "/="); op != -1 {
				name := strings.TrimSpace(s[:op])
				expr := strings.TrimSpace(s[op+2:])
				stmts = append(stmts, "let "+name+" = "+name+" / "+expr)
				i++
			} else if idx := strings.Index(s, "="); idx != -1 && !strings.Contains(s[:idx], "==") {
				name := strings.TrimSpace(s[:idx])
				expr := strings.TrimSpace(s[idx+1:])
				if !balanced(expr) {
					j := i + 1
					parts := []string{expr}
					depth := bracketDelta(expr)
					for j < len(lines) && depth > 0 {
						ln := strings.TrimSpace(lines[j])
						parts = append(parts, ln)
						depth += bracketDelta(ln)
						j++
					}
					expr = strings.Join(parts, " ")
					i = j
				} else {
					i++
				}
				stmts = append(stmts, "let "+name+" = "+expr)
			} else {
				stmts = append(stmts, s)
				i++
			}
		}
	}
	return stmts, i
}

func formatBlock(lines []string) string {
	if len(lines) == 0 {
		return " {}"
	}
	var b strings.Builder
	b.WriteString(" {")
	for _, st := range lines {
		b.WriteString("\n  ")
		b.WriteString(st)
	}
	b.WriteString("\n}")
	return b.String()
}

func bracketDelta(s string) int {
	delta := 0
	for _, r := range s {
		switch r {
		case '[', '(', '{':
			delta++
		case ']', ')', '}':
			delta--
		}
	}
	return delta
}

func balanced(s string) bool { return bracketDelta(s) == 0 }

func extractRange(src string, r parent.Range) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for i := int(r.Start.Line); i <= int(r.End.Line) && i < len(lines); i++ {
		line := lines[i]
		if i == int(r.Start.Line) && int(r.Start.Character) < len(line) {
			line = line[int(r.Start.Character):]
		}
		if i == int(r.End.Line) && int(r.End.Character) <= len(line) {
			line = line[:int(r.End.Character)]
		}
		out.WriteString(line)
		if i != int(r.End.Line) {
			out.WriteByte('\n')
		}
	}
	return out.String()
}

var pythonCmd = "python3"

// convertFallback performs a very small subset conversion of Python
// source without relying on a language server. Only top level function
// definitions and simple variable assignments are handled.
func convertFallback(src string) ([]byte, error) {
	items, err := runParse(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, it := range items {
		switch it.Kind {
		case "func":
			out.WriteString("fun ")
			out.WriteString(it.Name)
			out.WriteByte('(')
			for i, p := range it.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.Name)
				if p.Type != "" {
					out.WriteString(": ")
					out.WriteString(mapType(p.Type))
				}
			}
			out.WriteByte(')')
			if it.Ret != "" && it.Ret != "None" {
				out.WriteString(": ")
				out.WriteString(mapType(it.Ret))
			}
			body := extractBody(src, it.Start, it.End)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, st := range body {
					out.WriteString("  ")
					out.WriteString(st)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case "assign":
			out.WriteString("let ")
			out.WriteString(it.Name)
			if it.Type != "" {
				out.WriteString(": ")
				out.WriteString(mapType(it.Type))
			}
			if it.Value != "" {
				out.WriteString(" = ")
				out.WriteString(strings.ReplaceAll(it.Value, "\n", " "))
			}
			out.WriteByte('\n')
		case "class":
			out.WriteString("type ")
			out.WriteString(it.Name)
			if len(it.Fields) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range it.Fields {
					out.WriteString("  ")
					out.WriteString(f.Name)
					if f.Type != "" {
						out.WriteString(": ")
						out.WriteString(mapType(f.Type))
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func numberedSnippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func snippetAround(src string, line int) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	for i := start; i < end; i++ {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, lines[i])
	}
	return strings.Join(lines[start:end], "\n")
}

func snippetDetailed(src string, line int) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	var out strings.Builder
	for i := start; i < end; i++ {
		prefix := "   "
		if i+1 == line {
			prefix = ">>>"
		}
		fmt.Fprintf(&out, "%s %d: %s\n", prefix, i+1, lines[i])
	}
	return strings.TrimSuffix(out.String(), "\n")
}

func parseErrorLine(msg string) int {
	re := regexp.MustCompile(`line (\d+)`)
	matches := re.FindAllStringSubmatch(msg, -1)
	if len(matches) == 0 {
		return 0
	}
	m := matches[len(matches)-1]
	if len(m) == 2 {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	return 0
}

func indentOf(s string) string {
	i := 0
	for i < len(s) && (s[i] == ' ' || s[i] == '\t') {
		i++
	}
	return s[:i]
}

type fieldItem struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

type paramItem struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

type item struct {
	Kind   string      `json:"kind"`
	Name   string      `json:"name"`
	Fields []fieldItem `json:"fields,omitempty"`
	Params []paramItem `json:"params,omitempty"`
	Ret    string      `json:"ret,omitempty"`
	Start  int         `json:"start,omitempty"`
	End    int         `json:"end,omitempty"`
	Value  string      `json:"value,omitempty"`
	Type   string      `json:"type,omitempty"`
}

const parseScript = `import ast, json, sys

def ann(node):
    if node is None:
        return ""
    try:
        return ast.unparse(node)
    except Exception:
        return ""

tree = ast.parse(sys.stdin.read())
items = []
for n in tree.body:
    if isinstance(n, ast.FunctionDef):
        params = []
        for a in n.args.args:
            if a.arg != "self":
                params.append({"name": a.arg, "type": ann(a.annotation)})
        items.append({"kind": "func", "name": n.name,
                       "params": params, "ret": ann(n.returns),
                       "start": n.lineno, "end": n.end_lineno})
    elif isinstance(n, ast.Assign):
        if len(n.targets) == 1 and isinstance(n.targets[0], ast.Name):
            val = ""
            try:
                val = ast.unparse(n.value)
            except Exception:
                pass
            items.append({"kind": "assign", "name": n.targets[0].id, "value": val})
    elif isinstance(n, ast.AnnAssign) and isinstance(n.target, ast.Name):
        val = ""
        if n.value is not None:
            try:
                val = ast.unparse(n.value)
            except Exception:
                pass
        items.append({"kind": "assign", "name": n.target.id, "type": ann(n.annotation), "value": val})
    elif isinstance(n, ast.ClassDef):
        fields = []
        for b in n.body:
            if isinstance(b, ast.AnnAssign) and isinstance(b.target, ast.Name):
                typ = ""
                if b.annotation is not None:
                    try:
                        typ = ast.unparse(b.annotation)
                    except Exception:
                        pass
                fields.append({"name": b.target.id, "type": typ})
        items.append({"kind": "class", "name": n.name, "fields": fields})
json.dump(items, sys.stdout)`

func runParse(src string) ([]item, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, pythonCmd, "-c", parseScript)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf.String())
		if msg != "" {
			if line := parseErrorLine(msg); line > 0 {
				msg += "\n" + snippetDetailed(src, line)
			}
			return nil, fmt.Errorf("%v: %s", err, msg)
		}
		return nil, err
	}
	var items []item
	if err := json.Unmarshal(out.Bytes(), &items); err != nil {
		return nil, err
	}
	return items, nil
}

func extractBody(src string, startLine, endLine int) []string {
	lines := strings.Split(src, "\n")
	if startLine >= len(lines) {
		return nil
	}
	if endLine > len(lines) {
		endLine = len(lines)
	}
	if startLine < 0 {
		startLine = 0
	}
	bodyLines := lines[startLine:endLine]
	indent := ""
	for _, l := range bodyLines {
		t := strings.TrimSpace(l)
		if t != "" {
			indent = l[:len(l)-len(strings.TrimLeft(l, " \t"))]
			break
		}
	}
	if indent == "" {
		return nil
	}
	stmts, _ := parseLines(bodyLines, indent)
	return stmts
}
