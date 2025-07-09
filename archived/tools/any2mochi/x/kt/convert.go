//go:build slow

package kt

import (
	"bytes"
	"encoding/json"
	"fmt"
	any2mochi "mochi/archived/tools/any2mochi"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"unicode"
)

// Convert converts Kotlin source code to Mochi. It first tries to parse the
// source using the ktast CLI which outputs a JSON AST. If that fails, it
// falls back to a regex based parser.
func Convert(src string) ([]byte, error) {
	ast, err := parseKotlinc(src)
	if err != nil {
		ast, err = parseCLI(src)
	}
	if err != nil {
		return nil, fmt.Errorf("%s", formatParseError(err, src))
	}
	out, err := convertAST(ast)
	if err != nil {
		return nil, fmt.Errorf("convert error: %w\n\n%s", err, snippet(src))
	}
	return out, nil
}

// ConvertFile reads the kt file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}
func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func formatParseError(err error, src string) string {
	msg := err.Error()
	ln := -1
	if m := regexp.MustCompile(`(?i)line\s+(\d+)`).FindStringSubmatch(msg); m != nil {
		if n, e := strconv.Atoi(m[1]); e == nil {
			ln = n
		}
	}
	if ln <= 0 {
		return fmt.Sprintf("parse error: %s\n\n%s", msg, snippet(src))
	}
	lines := strings.Split(src, "\n")
	start := ln - 2
	if start < 0 {
		start = 0
	}
	end := ln
	if end >= len(lines) {
		end = len(lines) - 1
	}
	var out strings.Builder
	out.WriteString(fmt.Sprintf("parse error: %s\n", msg))
	for i := start; i <= end; i++ {
		out.WriteString(fmt.Sprintf("%3d | %s\n", i+1, lines[i]))
		if i+1 == ln {
			out.WriteString("    | " + strings.Repeat(" ", len(lines[i])) + "^\n")
		}
	}
	return strings.TrimSpace(out.String())
}

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindClass, any2mochi.SymbolKindStruct, any2mochi.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []any2mochi.DocumentSymbol{}
			methods := []any2mochi.DocumentSymbol{}
			for _, c := range s.Children {
				switch c.Kind {
				case any2mochi.SymbolKindField, any2mochi.SymbolKindProperty:
					fields = append(fields, c)
				case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod, any2mochi.SymbolKindConstructor:
					methods = append(methods, c)
				}
			}
			if len(fields) == 0 && len(methods) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					out.WriteString("  ")
					out.WriteString(f.Name)
					if typ := fieldType(src, f, ls); typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				for _, m := range methods {
					var b strings.Builder
					writeFunc(&b, m, src, ls, strings.Join(nameParts, ".")+"."+m.Name)
					for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
						out.WriteString("  ")
						out.WriteString(line)
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
			}
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod, any2mochi.SymbolKindConstructor:
			writeFunc(out, s, src, ls, strings.Join(nameParts, "."))
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant, any2mochi.SymbolKindField, any2mochi.SymbolKindProperty:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ := fieldType(src, s, ls); typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			if len(s.Children) > 0 {
				writeSymbols(out, nameParts, s.Children, src, ls)
			}
		default:
			if len(s.Children) > 0 {
				writeSymbols(out, nameParts, s.Children, src, ls)
			}
		}
	}
}

func writeFunc(out *strings.Builder, sym any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer, name string) {
	params, ret := parseSignature(sym.Detail)
	if len(params) == 0 && ret == "" {
		p, r := hoverSignature(src, sym, ls)
		params, ret = p, r
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
			out.WriteString(mapType(p.typ))
		}
	}
	out.WriteByte(')')
	if ret != "" && ret != "Unit" {
		out.WriteString(": ")
		out.WriteString(mapType(ret))
	}
	out.WriteString(" {}\n")
}

func hoverSignature(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) ([]param, string) {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			l := strings.TrimSpace(strings.Trim(line, "`"))
			if strings.HasPrefix(l, "fun ") || strings.HasPrefix(l, "suspend fun ") {
				return parseSignature(&l)
			}
		}
	}
	return nil, ""
}

func parseSignature(detail *string) ([]param, string) {
	if detail == nil {
		return nil, ""
	}
	sig := strings.TrimSpace(*detail)
	if sig == "" {
		return nil, ""
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := sig[open+1 : close]
	params := parseParams(paramsPart)
	rest := strings.TrimSpace(sig[close+1:])
	ret := ""
	if strings.HasPrefix(rest, ":") {
		ret = strings.TrimSpace(rest[1:])
	}
	return params, ret
}

type param struct{ name, typ string }

func parseParams(s string) []param {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<', '(', '[':
			depth++
		case '>', ')', ']':
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
	params := make([]param, 0, len(parts))
	for _, p := range parts {
		if eq := strings.Index(p, "="); eq != -1 {
			p = strings.TrimSpace(p[:eq])
		}
		name := p
		typ := ""
		if colon := strings.Index(p, ":"); colon != -1 {
			name = strings.TrimSpace(p[:colon])
			typ = strings.TrimSpace(p[colon+1:])
		} else if fields := strings.Fields(p); len(fields) > 0 {
			name = fields[len(fields)-1]
		}
		if name != "" {
			params = append(params, param{name: name, typ: typ})
		}
	}
	return params
}

func fieldType(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) string {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			l := strings.TrimSpace(line)
			if idx := strings.Index(l, ":"); idx != -1 {
				typ := strings.TrimSpace(l[idx+1:])
				if typ != "" {
					return mapType(typ)
				}
			}
		}
	}
	return ""
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	switch t {
	case "", "Unit", "Nothing":
		return ""
	case "Int", "Long", "Short", "Byte":
		return "int"
	case "Float", "Double":
		return "float"
	case "Boolean":
		return "bool"
	case "String", "Char":
		return "string"
	}
	if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[5 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Array<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[6 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Map<") && strings.HasSuffix(t, ">") {
		inner := t[4 : len(t)-1]
		parts := splitGeneric(inner)
		key := "any"
		val := "any"
		if len(parts) == 2 {
			if k := mapType(parts[0]); k != "" {
				key = k
			}
			if v := mapType(parts[1]); v != "" {
				val = v
			}
		}
		return "map<" + key + ", " + val + ">"
	}
	return t
}

func splitGeneric(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<':
			depth++
		case '>':
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

type ast struct {
	Classes   []cls   `json:"classes"`
	Functions []fn    `json:"functions"`
	Vars      []vdecl `json:"vars"`
}

type cls struct {
	Name      string  `json:"name"`
	Kind      string  `json:"kind,omitempty"`
	Data      bool    `json:"data,omitempty"`
	Sealed    bool    `json:"sealed,omitempty"`
	Extends   string  `json:"extends,omitempty"`
	Fields    []field `json:"fields"`
	Methods   []fn    `json:"methods"`
	StartLine int     `json:"start,omitempty"`
	StartCol  int     `json:"startCol,omitempty"`
	EndLine   int     `json:"end,omitempty"`
	EndCol    int     `json:"endCol,omitempty"`
	Snippet   string  `json:"snippet,omitempty"`
}

type field struct {
	Name      string `json:"name"`
	Type      string `json:"type"`
	StartLine int    `json:"start,omitempty"`
	StartCol  int    `json:"startCol,omitempty"`
	EndLine   int    `json:"end,omitempty"`
	EndCol    int    `json:"endCol,omitempty"`
	Snippet   string `json:"snippet,omitempty"`
}

type vdecl struct {
	Name      string `json:"name"`
	Type      string `json:"type,omitempty"`
	Value     string `json:"value"`
	StartLine int    `json:"start,omitempty"`
	StartCol  int    `json:"startCol,omitempty"`
	EndLine   int    `json:"end,omitempty"`
	EndCol    int    `json:"endCol,omitempty"`
	Snippet   string `json:"snippet,omitempty"`
}

type fn struct {
	Name      string      `json:"name"`
	Generics  []string    `json:"generics,omitempty"`
	Params    []paramDecl `json:"params"`
	Ret       string      `json:"ret,omitempty"`
	Lines     []string    `json:"lines"`
	StartLine int         `json:"start,omitempty"`
	StartCol  int         `json:"startCol,omitempty"`
	EndLine   int         `json:"end,omitempty"`
	EndCol    int         `json:"endCol,omitempty"`
	Snippet   string      `json:"snippet,omitempty"`
}

type paramDecl struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}

// parseKotlinc tries to parse the source using kotlinc's -Xdump-declarations-to
// flag which emits a JSON description of the program. If kotlinc is not
// available or parsing fails, an error is returned.
func parseKotlinc(src string) (*ast, error) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "ktsrc_*.kt")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())
	outFile := tmp.Name() + ".json"
	cmd := exec.Command("kotlinc", tmp.Name(), "-Xdump-declarations-to="+outFile)
	if err := cmd.Run(); err != nil {
		os.Remove(outFile)
		return nil, err
	}
	defer os.Remove(outFile)
	data, err := os.ReadFile(outFile)
	if err != nil {
		return nil, err
	}
	var ast ast
	if err := json.Unmarshal(data, &ast); err != nil {
		return nil, err
	}
	if len(ast.Classes) == 0 && len(ast.Functions) == 0 && len(ast.Vars) == 0 {
		return nil, fmt.Errorf("no symbols found")
	}
	return &ast, nil
}

func parseCLI(src string) (*ast, error) {
	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "ktsrc_*.kt")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())
	cmd := exec.Command("go", "run", filepath.Join(root, "tools", "ktast"), tmp.Name())
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("ktast: %s", msg)
	}
	var ast ast
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	if len(ast.Classes) == 0 && len(ast.Functions) == 0 && len(ast.Vars) == 0 {
		return nil, fmt.Errorf("no symbols found")
	}
	return &ast, nil
}

func convertAST(ast *ast) ([]byte, error) {
	var out strings.Builder
	indent := 0
	ind := func() string { return strings.Repeat("  ", indent) }
	hasMain := false
	skip := map[string]bool{}

	for _, c := range ast.Classes {
		if skip[c.Name] {
			continue
		}
		if c.Sealed && c.Kind == "interface" {
			var members []cls
			for _, sub := range ast.Classes {
				if sub.Extends == c.Name {
					members = append(members, sub)
					skip[sub.Name] = true
				}
			}
			if len(members) > 0 {
				out.WriteString(ind())
				out.WriteString("type ")
				out.WriteString(c.Name)
				out.WriteString(" =\n")
				indent++
				for i, m := range members {
					out.WriteString(ind())
					if i > 0 {
						out.WriteString("| ")
					}
					out.WriteString(m.Name)
					if len(m.Fields) > 0 {
						out.WriteByte('(')
						for j, f := range m.Fields {
							if j > 0 {
								out.WriteString(", ")
							}
							out.WriteString(f.Name)
							if f.Type != "" {
								out.WriteString(": ")
								out.WriteString(mapType(f.Type))
							}
						}
						out.WriteByte(')')
					}
					out.WriteByte('\n')
				}
				indent--
				continue
			}
		}
		out.WriteString(ind())
		out.WriteString("type ")
		out.WriteString(c.Name)
		if len(c.Fields) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			indent++
			for _, f := range c.Fields {
				out.WriteString(ind())
				out.WriteString(f.Name)
				if f.Type != "" {
					out.WriteString(": ")
					out.WriteString(mapType(f.Type))
				}
				out.WriteByte('\n')
			}
			indent--
			out.WriteString(ind())
			out.WriteString("}\n")
		}
		for _, m := range c.Methods {
			out.WriteString(ind())
			out.WriteString("fun ")
			out.WriteString(c.Name + "." + m.Name)
			out.WriteByte('(')
			for i, p := range m.Params {
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
			if m.Ret != "" {
				out.WriteString(": ")
				out.WriteString(mapType(m.Ret))
			}
			out.WriteString(" {\n")
			indent++
			for _, line := range m.Lines {
				writeBodyLine(&out, strings.TrimSpace(line), &indent)
			}
			if indent > 0 {
				indent--
			}
			out.WriteString(ind())
			out.WriteString("}\n")
		}
	}

	for _, v := range ast.Vars {
		out.WriteString(ind())
		if v.Type == "" {
			out.WriteString("let ")
		} else {
			out.WriteString("let ")
		}
		out.WriteString(v.Name)
		if v.Type != "" {
			out.WriteString(": ")
			out.WriteString(mapType(v.Type))
		}
		if v.Value != "" {
			out.WriteString(" = ")
			out.WriteString(strings.TrimSpace(v.Value))
		}
		out.WriteByte('\n')
	}

	for _, fn := range ast.Functions {
		if fn.Name == "main" {
			hasMain = true
		}
		out.WriteString(ind())
		out.WriteString("fun ")
		out.WriteString(fn.Name)
		out.WriteByte('(')
		for i, p := range fn.Params {
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
		if fn.Ret != "" {
			out.WriteString(": ")
			out.WriteString(mapType(fn.Ret))
		}
		out.WriteString(" {\n")
		indent++
		for _, line := range fn.Lines {
			writeBodyLine(&out, strings.TrimSpace(line), &indent)
		}
		if indent > 0 {
			indent--
		}
		out.WriteString(ind())
		out.WriteString("}\n")
	}
	if hasMain {
		out.WriteString("main()\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(""))
	}
	return []byte(out.String()), nil
}

func writeBodyLine(out *strings.Builder, line string, indent *int) {
	ind := func() string { return strings.Repeat("  ", *indent) }
	switch {
	case line == "}":
		if *indent > 0 {
			(*indent)--
		}
		out.WriteString(ind())
		out.WriteString("}\n")
	case strings.HasPrefix(line, "for ") && strings.Contains(line, " in "):
		if v, it, ok := parseForLine(line); ok {
			out.WriteString(ind())
			it = strings.ReplaceAll(it, " until ", " .. ")
			fmt.Fprintf(out, "for %s in %s {\n", v, strings.TrimSpace(it))
			(*indent)++
			return
		}
		out.WriteString(ind())
		out.WriteString(strings.TrimSuffix(line, ";"))
		out.WriteByte('\n')
	case strings.HasPrefix(line, "while "):
		cond := strings.TrimPrefix(line, "while ")
		cond = strings.Trim(cond, "(){} ")
		out.WriteString(ind())
		fmt.Fprintf(out, "while %s {\n", cond)
		(*indent)++
	case strings.HasPrefix(line, "if "):
		if cond, stmt, ok := parseSingleIf(line); ok {
			out.WriteString(ind())
			fmt.Fprintf(out, "if %s {\n", cond)
			(*indent)++
			out.WriteString(ind())
			out.WriteString(strings.TrimSuffix(stmt, ";"))
			out.WriteByte('\n')
			if *indent > 0 {
				(*indent)--
			}
			out.WriteString(ind())
			out.WriteString("}\n")
		} else {
			cond := strings.TrimPrefix(line, "if ")
			cond = strings.Trim(cond, "(){} ")
			out.WriteString(ind())
			fmt.Fprintf(out, "if %s {\n", cond)
			(*indent)++
		}
	case line == "else {":
		if *indent > 0 {
			(*indent)--
		}
		out.WriteString(ind())
		out.WriteString("} else {\n")
		(*indent)++
	case strings.HasPrefix(line, "val "):
		out.WriteString(ind())
		out.WriteString("let ")
		out.WriteString(strings.TrimSuffix(strings.TrimPrefix(line, "val "), ";"))
		out.WriteByte('\n')
	case strings.HasPrefix(line, "var "):
		out.WriteString(ind())
		out.WriteString("var ")
		out.WriteString(strings.TrimSuffix(strings.TrimPrefix(line, "var "), ";"))
		out.WriteByte('\n')
	case strings.HasPrefix(line, "return "):
		out.WriteString(ind())
		out.WriteString("return ")
		out.WriteString(strings.TrimSuffix(strings.TrimPrefix(line, "return "), ";"))
		out.WriteByte('\n')
	case strings.HasPrefix(line, "println("):
		out.WriteString(ind())
		out.WriteString("print(")
		out.WriteString(strings.TrimSuffix(strings.TrimPrefix(line, "println("), ")"))
		out.WriteString(")\n")
	default:
		out.WriteString(ind())
		converted := convertListOf(strings.TrimSuffix(line, ";"))
		out.WriteString(converted)
		out.WriteByte('\n')
	}
}

// convertListOf replaces Kotlin listOf(...) expressions with Mochi list
// literals to aid round-trip conversion.
func convertListOf(line string) string {
	for {
		idx := strings.Index(line, "listOf")
		if idx == -1 {
			break
		}
		// locate opening parenthesis after optional generics
		start := idx + len("listOf")
		for start < len(line) && unicode.IsSpace(rune(line[start])) {
			start++
		}
		if start < len(line) && line[start] == '<' {
			depth := 1
			start++
			for ; start < len(line) && depth > 0; start++ {
				switch line[start] {
				case '<':
					depth++
				case '>':
					depth--
				}
			}
		}
		for start < len(line) && unicode.IsSpace(rune(line[start])) {
			start++
		}
		if start >= len(line) || line[start] != '(' {
			// not a call expression
			idx = strings.Index(line[idx+len("listOf"):], "listOf")
			if idx == -1 {
				break
			}
			idx += idx + len("listOf")
			continue
		}
		open := start
		depth := 1
		i := open + 1
		for ; i < len(line); i++ {
			switch line[i] {
			case '(':
				depth++
			case ')':
				depth--
				if depth == 0 {
					inner := line[open+1 : i]
					line = line[:idx] + "[" + inner + "]" + line[i+1:]
					i = idx + len("[") + len(inner) + 1
					break
				}
			}
		}
		if depth != 0 {
			break
		}
	}
	return line
}

func parseForLine(line string) (string, string, bool) {
	line = strings.TrimSpace(line)
	if !strings.HasPrefix(line, "for ") {
		return "", "", false
	}
	rest := strings.TrimSpace(strings.TrimPrefix(line, "for "))
	if !strings.HasPrefix(rest, "(") {
		return "", "", false
	}
	rest = rest[1:]
	depth := 1
	i := 0
	for ; i < len(rest); i++ {
		switch rest[i] {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				inside := rest[:i]
				parts := strings.SplitN(inside, " in ", 2)
				if len(parts) != 2 {
					return "", "", false
				}
				return strings.TrimSpace(parts[0]), strings.TrimSpace(parts[1]), true
			}
		}
	}
	return "", "", false
}

func parseSingleIf(line string) (string, string, bool) {
	l := strings.TrimSpace(strings.TrimSuffix(line, ";"))
	if !strings.HasPrefix(l, "if ") || strings.Contains(l, "{") {
		return "", "", false
	}
	open := strings.Index(l, "(")
	close := strings.LastIndex(l, ")")
	if open == -1 || close == -1 || close <= open {
		return "", "", false
	}
	cond := strings.TrimSpace(l[open+1 : close])
	stmt := strings.TrimSpace(l[close+1:])
	if stmt == "" {
		return "", "", false
	}
	return cond, stmt, true
}
