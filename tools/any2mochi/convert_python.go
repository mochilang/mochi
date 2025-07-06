package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertPython converts Python source code to a minimal Mochi representation.
// The conversion is performed by invoking Python's AST parser via a small
// helper script and translating the resulting JSON to Mochi.
func ConvertPython(src string) ([]byte, error) {
	return convertPythonFallback(src)
}

// ConvertPythonFile reads the Python file at path and converts it to Mochi.
func ConvertPythonFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPython(string(data))
}

func writePySymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			writePyFunc(out, strings.Join(nameParts, "."), s, src, ls)
		case protocol.SymbolKindClass:
			writePyClass(out, nameParts, s, src, ls)
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ := getPyVarType(src, s.SelectionRange.Start, ls); typ != "" && typ != "Unknown" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				if val := getPyVarValue(src, s); val != "" {
					out.WriteString(" = ")
					out.WriteString(val)
				}
				out.WriteString("\n")
			}
		}
	}
}

type pyParam struct {
	name string
	typ  string
}

func writePyFunc(out *strings.Builder, name string, sym protocol.DocumentSymbol, src string, ls LanguageServer) {
	params, ret := getPySignature(src, sym.SelectionRange.Start, ls)
	if len(params) == 0 {
		for _, p := range extractPyParams(sym) {
			params = append(params, pyParam{name: p})
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
	body := parsePyFunctionBody(src, sym)
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

func writePyClass(out *strings.Builder, prefix []string, sym protocol.DocumentSymbol, src string, ls LanguageServer) {
	name := strings.Join(prefix, ".")
	fields := extractPyFields(sym)
	methods := make([]protocol.DocumentSymbol, 0)
	for _, c := range sym.Children {
		switch c.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
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
		if typ := getPyVarType(src, f.sym.SelectionRange.Start, ls); typ != "" && typ != "Unknown" {
			out.WriteString(": ")
			out.WriteString(typ)
		}
		out.WriteByte('\n')
	}
	for _, m := range methods {
		var b strings.Builder
		writePyFunc(&b, m.Name, m, src, ls)
		for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
			out.WriteString("  ")
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	out.WriteString("}\n")
}

type pyField struct {
	name string
	sym  protocol.DocumentSymbol
}

func extractPyFields(sym protocol.DocumentSymbol) []pyField {
	var fields []pyField
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable || c.Kind == protocol.SymbolKindConstant {
			fields = append(fields, pyField{name: c.Name, sym: c})
		}
	}
	return fields
}

func extractPyParams(sym protocol.DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable && c.Range.Start.Line == start {
			params = append(params, c.Name)
		}
	}
	return params
}

func getPySignature(src string, pos protocol.Position, ls LanguageServer) ([]pyParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return nil, ""
	}
	return parsePySignature(mc.Value)
}

func parsePySignature(sig string) ([]pyParam, string) {
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
		ret = mapPyType(strings.TrimSpace(rest[2:]))
	}
	var params []pyParam
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
				typ = mapPyType(typ)
			}
			params = append(params, pyParam{name: name, typ: typ})
		}
	}
	return params, ret
}

func mapPyType(t string) string {
	t = strings.TrimSpace(t)
	if t == "" || t == "None" || t == "Any" || t == "Unknown" {
		return ""
	}
	if strings.Contains(t, "|") {
		var parts []string
		for _, p := range splitPyArgs(strings.ReplaceAll(t, "|", ",")) {
			p = strings.TrimSpace(p)
			if p == "None" || p == "" {
				continue
			}
			if mp := mapPyType(p); mp != "" {
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
			args := splitPyArgs(inner)
			switch outer {
			case "list", "List", "Sequence", "Iterable":
				innerType := "any"
				if len(args) > 0 {
					if a := mapPyType(args[0]); a != "" {
						innerType = a
					}
				}
				return "list<" + innerType + ">"
			case "dict", "Dict", "Mapping":
				key := "any"
				val := "any"
				if len(args) > 0 {
					if k := mapPyType(args[0]); k != "" {
						key = k
					}
				}
				if len(args) > 1 {
					if v := mapPyType(args[1]); v != "" {
						val = v
					}
				}
				return "map<" + key + ", " + val + ">"
			case "set", "Set":
				innerType := "any"
				if len(args) > 0 {
					if a := mapPyType(args[0]); a != "" {
						innerType = a
					}
				}
				return "set<" + innerType + ">"
			case "tuple", "Tuple":
				var mapped []string
				for _, a := range args {
					mt := mapPyType(a)
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
					mt := mapPyType(a)
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
					mt := mapPyType(args[0])
					if mt == "" {
						mt = "any"
					}
					return mt + "?"
				}
			case "Callable":
				if len(args) == 2 {
					argList := strings.Trim(args[0], "[]")
					ret := mapPyType(args[1])
					var argTypes []string
					if strings.TrimSpace(argList) != "" && argList != "..." {
						for _, a := range splitPyArgs(argList) {
							at := mapPyType(a)
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

func splitPyArgs(s string) []string {
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

func getPyVarType(src string, pos protocol.Position, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return ""
	}
	return parsePyVarType(mc.Value)
}

func parsePyVarType(hov string) string {
	if i := strings.Index(hov, "\n"); i != -1 {
		hov = hov[:i]
	}
	if colon := strings.Index(hov, ":"); colon != -1 {
		typ := strings.TrimSpace(hov[colon+1:])
		return mapPyType(typ)
	}
	return ""
}

func getPyVarValue(src string, sym protocol.DocumentSymbol) string {
	code := pyExtractRange(src, sym.Range)
	if idx := strings.Index(code, "="); idx != -1 {
		return strings.TrimSpace(code[idx+1:])
	}
	return ""
}

func parsePyFunctionBody(src string, sym protocol.DocumentSymbol) []string {
	code := pyExtractRange(src, sym.Range)
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
	stmts, _ := parsePyLines(lines[1:], indent)
	return stmts
}

func parsePyLines(lines []string, indent string) ([]string, int) {
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
			body, n := parsePyLines(lines[i+1:], step)
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
					elseBody, m = parsePyLines(lines[i+1:], step)
					i = i + 1 + m
				} else if strings.HasPrefix(next, "elif ") {
					var m int
					elifCond := strings.TrimSpace(strings.TrimSuffix(next[5:], ":"))
					elifBody, m := parsePyLines(lines[i+1:], step)
					i = i + 1 + m
					bodyBlock := formatPyBlock(body)
					elifBlock := formatPyBlock(elifBody)
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
				body, n := parsePyLines(lines[i+1:], step)
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
			body, n := parsePyLines(lines[i+1:], step)
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
		default:
			if idx := strings.Index(s, "="); idx != -1 && !strings.Contains(s[:idx], "==") {
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

func formatPyBlock(lines []string) string {
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

func pyExtractRange(src string, r protocol.Range) string {
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

var pyCmd = "python3"

// convertPythonFallback performs a very small subset conversion of Python
// source without relying on a language server. Only top level function
// definitions and simple variable assignments are handled.
func convertPythonFallback(src string) ([]byte, error) {
	items, err := runPyParse(src)
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
			out.WriteString(strings.Join(it.Params, ", "))
			out.WriteByte(')')
			body := extractPyBody(src, it.Start, it.End)
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
			if it.Value != "" {
				out.WriteString(" = ")
				out.WriteString(strings.ReplaceAll(it.Value, "\n", " "))
			}
			out.WriteByte('\n')
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func indentOf(s string) string {
	i := 0
	for i < len(s) && (s[i] == ' ' || s[i] == '\t') {
		i++
	}
	return s[:i]
}

type pyItem struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	Params []string `json:"params,omitempty"`
	Start  int      `json:"start,omitempty"`
	End    int      `json:"end,omitempty"`
	Value  string   `json:"value,omitempty"`
}

const pyParseScript = `import ast, json, sys
tree = ast.parse(sys.stdin.read())
items = []
for n in tree.body:
    if isinstance(n, ast.FunctionDef):
        items.append({"kind": "func", "name": n.name,
                       "params": [a.arg for a in n.args.args if a.arg != "self"],
                       "start": n.lineno, "end": n.end_lineno})
    elif isinstance(n, ast.Assign):
        if len(n.targets) == 1 and isinstance(n.targets[0], ast.Name):
            val = ""
            try:
                val = ast.unparse(n.value)
            except Exception:
                pass
            items.append({"kind": "assign", "name": n.targets[0].id, "value": val})
json.dump(items, sys.stdout)`

func runPyParse(src string) ([]pyItem, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, pyCmd, "-c", pyParseScript)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var items []pyItem
	if err := json.Unmarshal(out.Bytes(), &items); err != nil {
		return nil, err
	}
	return items, nil
}

func extractPyBody(src string, startLine, endLine int) []string {
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
	stmts, _ := parsePyLines(bodyLines, indent)
	return stmts
}
