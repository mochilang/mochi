//go:build archive

package zig

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	any2mochi "mochi/archived/tools/any2mochi"
)

// ConvertError provides detailed information suitable for golden tests.
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

type param struct {
	name string
	typ  string
}

type ast struct {
	Vars      []variable  `json:"vars"`
	Structs   []structDef `json:"structs"`
	Functions []function  `json:"functions"`
}

type variable struct {
	Name  string `json:"name"`
	Type  string `json:"type"`
	Value string `json:"value,omitempty"`
	Const bool   `json:"const,omitempty"`
	Pub   bool   `json:"pub,omitempty"`
	Line  int    `json:"line"`
}

type function struct {
	Name    string   `json:"name"`
	Params  string   `json:"params"`
	Ret     string   `json:"ret"`
	Pub     bool     `json:"pub,omitempty"`
	Line    int      `json:"line"`
	EndLine int      `json:"endLine"`
	Lines   []string `json:"lines"`
}

type structDef struct {
	Name    string  `json:"name"`
	Pub     bool    `json:"pub,omitempty"`
	Line    int     `json:"line"`
	EndLine int     `json:"endLine"`
	Fields  []field `json:"fields"`
}

type field struct {
	Name string `json:"name"`
	Type string `json:"type"`
	Line int    `json:"line"`
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

func arrowSnippet(src string, line int) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var b strings.Builder
	for i := start; i < end; i++ {
		mark := "   "
		if i+1 == line {
			mark = ">>>"
		}
		fmt.Fprintf(&b, "%4d:%s %s\n", i+1, mark, lines[i])
		if i+1 == line {
			b.WriteString("     ^\n")
		}
	}
	return b.String()
}

func formatError(path, src string, err error) error {
	if ce, ok := err.(*ConvertError); ok {
		if ce.Line > 0 {
			return fmt.Errorf("%s:%d: %s\n%s", path, ce.Line, ce.Msg, ce.Snip)
		}
		return fmt.Errorf("%s: %s\n%s", path, ce.Msg, ce.Snip)
	}
	re := regexp.MustCompile(`:(\d+):(\d+)?:`)
	if m := re.FindStringSubmatch(err.Error()); m != nil {
		line, _ := strconv.Atoi(m[1])
		return fmt.Errorf("%s:%d: %v\n%s", path, line, err, arrowSnippet(src, line))
	}
	return fmt.Errorf("%s: %v", path, err)
}

func diagnostics(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		start := int(d.Range.Start.Line)
		end := int(d.Range.End.Line)
		msg := d.Message
		out.WriteString(fmt.Sprintf("line %d-%d: %s\n", start+1, end+1, msg))
		from := start - 1
		if from < 0 {
			from = 0
		}
		to := end + 1
		if to >= len(lines) {
			to = len(lines) - 1
		}
		for i := from; i <= to; i++ {
			out.WriteString(fmt.Sprintf(" %4d| %s\n", i+1, lines[i]))
		}
	}
	return strings.TrimSpace(out.String())
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

func parseCLI(src string) (*ast, error) {
	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "zigsrc_*.zig")
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

	if _, err := exec.LookPath("zig"); err == nil {
		cmd := exec.Command("zig", "ast-check", "--format", "json", tmp.Name())
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			var a ast
			if json.Unmarshal(out.Bytes(), &a) == nil && (len(a.Functions) > 0 || len(a.Structs) > 0) {
				return &a, nil
			}
		}
	}

	cmd := exec.Command("go", "run", filepath.Join(root, "tools", "zigast"), tmp.Name())
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("zigast: %s", msg)
	}
	var ast ast
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return &ast, nil
}

func convertAST(a *ast) ([]byte, error) {
	var out strings.Builder
	for _, v := range a.Vars {
		if strings.Contains(v.Value, "@import(") {
			continue
		}
		if v.Line > 0 {
			out.WriteString("// line ")
			out.WriteString(fmt.Sprint(v.Line))
			out.WriteByte('\n')
		}
		if v.Const {
			out.WriteString("let ")
		} else {
			out.WriteString("var ")
		}
		out.WriteString(v.Name)
		if v.Type != "" {
			out.WriteString(": ")
			out.WriteString(mapType(v.Type))
		}
		if v.Value != "" {
			out.WriteString(" = ")
			out.WriteString(convertExpr(v.Value))
		}
		out.WriteByte('\n')
	}

	for _, st := range a.Structs {
		if st.Line > 0 {
			out.WriteString("// line ")
			out.WriteString(fmt.Sprint(st.Line))
			out.WriteByte('\n')
		}
		out.WriteString("type ")
		out.WriteString(st.Name)
		if len(st.Fields) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, f := range st.Fields {
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
	// ignore top-level variables since imports are not needed
	for _, fn := range a.Functions {
		if fn.Line > 0 {
			out.WriteString("// line ")
			out.WriteString(fmt.Sprint(fn.Line))
			out.WriteByte('\n')
		}
		out.WriteString("fun ")
		out.WriteString(fn.Name)
		out.WriteByte('(')
		params := parseParams(fn.Params)
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
		if fn.Ret != "" && fn.Ret != "void" {
			out.WriteString(": ")
			out.WriteString(mapType(fn.Ret))
		}
		body := parseFunctionBodyLines(fn.Lines)
		if len(body) == 0 {
			out.WriteString(" {}\n")
			continue
		}
		out.WriteString(" {\n")
		for _, l := range body {
			out.WriteString("  ")
			out.WriteString(l)
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
	}
	for _, fn := range a.Functions {
		if fn.Name == "main" {
			out.WriteString("main()\n")
			break
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found")
	}
	return []byte(out.String()), nil
}

func parseFunctionBodyLines(lines []string) []string {
	var out []string
	indent := 0
	var post []string
	for _, line := range lines {
		l := strings.TrimSpace(line)
		if l == "" {
			continue
		}
		for strings.HasPrefix(l, "}") {
			indent--
			if len(post) > 0 {
				p := post[len(post)-1]
				if p != "" {
					out = append(out, strings.Repeat("  ", indent)+p)
				}
				post = post[:len(post)-1]
			}
			out = append(out, strings.Repeat("  ", indent)+"}")
			l = strings.TrimSpace(strings.TrimPrefix(l, "}"))
			if l == "" {
				break
			}
		}
		if l == "" {
			continue
		}
		if strings.HasSuffix(l, "{") {
			hdr := strings.TrimSpace(strings.TrimSuffix(l, "{"))
			head, p := parseBlockHeader(hdr)
			out = append(out, strings.Repeat("  ", indent)+head+" {")
			indent++
			post = append(post, p)
			continue
		}
		if l == "}" {
			indent--
			if len(post) > 0 {
				p := post[len(post)-1]
				if p != "" {
					out = append(out, strings.Repeat("  ", indent)+p)
				}
				post = post[:len(post)-1]
			}
			out = append(out, strings.Repeat("  ", indent)+"}")
			continue
		}
		stmt := parseStmt(l)
		if stmt != "" {
			out = append(out, strings.Repeat("  ", indent)+stmt)
		}
	}
	return out
}

// Convert converts Zig source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	if ast, err := parseCLI(src); err == nil {
		return convertAST(ast)
	}
	ls := any2mochi.Servers["zig"]
	syms, diags, err := any2mochi.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, &ConvertError{Msg: "diagnostics", Snip: diagnostics(src, diags)}
	}
	var out strings.Builder
	writeSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, &ConvertError{Msg: "no convertible symbols found", Snip: snippet(src)}
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the Zig file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	out, err := Convert(string(data))
	if err != nil {
		return nil, formatError(path, string(data), err)
	}
	return out, nil
}

func parseParams(list string) []param {
	parts := strings.Split(list, ",")
	params := make([]param, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		name := p
		typ := ""
		if colon := strings.Index(p, ":"); colon != -1 {
			name = strings.TrimSpace(p[:colon])
			typ = strings.TrimSpace(p[colon+1:])
			typ = mapType(typ)
		}
		if strings.HasSuffix(name, ":") {
			name = strings.TrimSuffix(name, ":")
		}
		params = append(params, param{name: name, typ: typ})
	}
	return params
}

func parseDetail(detail string) ([]param, string) {
	start := strings.Index(detail, "(")
	end := strings.LastIndex(detail, ")")
	if start == -1 || end == -1 || end < start {
		return nil, ""
	}
	paramsPart := detail[start+1 : end]
	retPart := strings.TrimSpace(detail[end+1:])
	params := parseParams(paramsPart)
	return params, mapType(retPart)
}

func hoverSignature(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) ([]param, string) {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
		lines := strings.Split(mc.Value, "\n")
		for i := len(lines) - 1; i >= 0; i-- {
			l := strings.TrimSpace(lines[i])
			if strings.HasPrefix(l, "fn") && strings.Contains(l, "(") && strings.Contains(l, ")") {
				return parseDetail(l)
			}
		}
	}
	return nil, ""
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	if t == "" || t == "void" {
		return ""
	}
	switch t {
	case "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "usize", "isize":
		return "int"
	case "f16", "f32", "f64":
		return "float"
	case "bool":
		return "bool"
	}
	if strings.HasPrefix(t, "[]const u8") || strings.HasPrefix(t, "[]u8") {
		return "string"
	}
	if strings.HasPrefix(t, "[]") {
		inner := mapType(strings.TrimPrefix(t, "[]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

func extractRange(src string, r any2mochi.Range) string {
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

func trimOuterParens(s string) string {
	s = strings.TrimSpace(s)
	if len(s) >= 2 && s[0] == '(' && s[len(s)-1] == ')' {
		return strings.TrimSpace(s[1 : len(s)-1])
	}
	return s
}

func convertExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	expr = strings.ReplaceAll(expr, " catch unreachable", "")
	for strings.Contains(expr, "@as(") {
		start := strings.Index(expr, "@as(")
		rest := expr[start+4:]
		depth := 1
		end := start + 4
		for i := 0; i < len(rest); i++ {
			switch rest[i] {
			case '(':
				depth++
			case ')':
				depth--
				if depth == 0 {
					end = start + 4 + i
					i = len(rest)
				}
			}
		}
		inner := expr[start+4 : end]
		comma := strings.Index(inner, ",")
		if comma == -1 {
			break
		}
		innerExpr := strings.TrimSpace(inner[comma+1:])
		expr = expr[:start] + innerExpr + expr[end+1:]
	}
	for strings.Contains(expr, "@intCast(") {
		start := strings.Index(expr, "@intCast(")
		rest := expr[start+9:]
		depth := 1
		end := start + 9
		for i := 0; i < len(rest); i++ {
			switch rest[i] {
			case '(':
				depth++
			case ')':
				depth--
				if depth == 0 {
					end = start + 9 + i
					i = len(rest)
				}
			}
		}
		inner := strings.TrimSpace(expr[start+9 : end])
		expr = expr[:start] + inner + expr[end+1:]
	}
	expr = strings.ReplaceAll(expr, "std.debug.print", "print")
	if strings.HasPrefix(expr, ".{") && strings.HasSuffix(expr, "}") {
		expr = "[" + strings.TrimSpace(expr[2:len(expr)-1]) + "]"
	} else {
		r := regexp.MustCompile(`\.{([^}]*)}`)
		expr = r.ReplaceAllString(expr, "[$1]")
	}
	if strings.HasPrefix(expr, "print(\"{any}\\n\", [") && strings.HasSuffix(expr, "])") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "print(\"{any}\\n\", ["), "])")
		expr = "print(" + inner + ")"
	}
	return expr
}

func parseBlockHeader(h string) (string, string) {
	switch {
	case strings.HasPrefix(h, "if "):
		cond := strings.TrimSpace(strings.TrimPrefix(h, "if "))
		cond = trimOuterParens(cond)
		return "if " + convertExpr(cond), ""
	case strings.HasPrefix(h, "else if "):
		cond := strings.TrimSpace(strings.TrimPrefix(h, "else if "))
		cond = trimOuterParens(cond)
		return "else if " + convertExpr(cond), ""
	case h == "else":
		return "else", ""
	case strings.HasPrefix(h, "while "):
		inner := strings.TrimSpace(strings.TrimPrefix(h, "while "))
		parts := strings.SplitN(inner, ":", 2)
		cond := trimOuterParens(strings.TrimSpace(parts[0]))
		post := ""
		if len(parts) == 2 {
			post = trimOuterParens(strings.TrimSpace(parts[1]))
			if post != "" {
				post = parseStmt(post)
			}
		}
		return "while " + convertExpr(cond), post
	case strings.HasPrefix(h, "for "):
		inner := strings.TrimSpace(strings.TrimPrefix(h, "for"))
		if strings.HasPrefix(inner, "(") {
			close := strings.Index(inner, ")")
			if close != -1 {
				coll := strings.TrimSpace(inner[1:close])
				after := strings.TrimSpace(inner[close+1:])
				name := "it"
				if strings.HasPrefix(after, "|") {
					if end := strings.Index(after[1:], "|"); end != -1 {
						name = strings.TrimSpace(after[1 : 1+end])
					}
				}
				return "for " + name + " in " + convertExpr(coll), ""
			}
		}
		return convertExpr(h), ""
	}
	return convertExpr(h), ""
}

func parseStmt(l string) string {
	if strings.HasSuffix(l, ";") {
		l = strings.TrimSuffix(l, ";")
	}
	switch {
	case strings.HasPrefix(l, "return "):
		expr := strings.TrimSpace(l[len("return "):])
		return "return " + convertExpr(expr)
	case strings.HasPrefix(l, "var "):
		parts := strings.SplitN(l[4:], "=", 2)
		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			name := left
			typ := ""
			if colon := strings.Index(left, ":"); colon != -1 {
				name = strings.TrimSpace(left[:colon])
				typ = mapType(strings.TrimSpace(left[colon+1:]))
			}
			expr := convertExpr(parts[1])
			if typ != "" {
				return "var " + name + ": " + typ + " = " + expr
			}
			return "var " + name + " = " + expr
		}
	case strings.HasPrefix(l, "const "):
		parts := strings.SplitN(l[6:], "=", 2)
		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			name := left
			typ := ""
			if colon := strings.Index(left, ":"); colon != -1 {
				name = strings.TrimSpace(left[:colon])
				typ = mapType(strings.TrimSpace(left[colon+1:]))
			}
			expr := convertExpr(parts[1])
			if typ != "" {
				return "let " + name + ": " + typ + " = " + expr
			}
			return "let " + name + " = " + expr
		}
	case strings.HasPrefix(l, "break "):
		return "break"
	case strings.HasPrefix(l, "continue "):
		return "continue"
	case strings.HasPrefix(l, "defer "):
		return ""
	case l == "break":
		return "break"
	case l == "continue":
		return "continue"
	}
	return convertExpr(l)
}

func parseFunctionBody(src string, sym any2mochi.DocumentSymbol) []string {
	code := extractRange(src, sym.Range)
	start := strings.Index(code, "{")
	end := strings.LastIndex(code, "}")
	if start == -1 || end == -1 || end <= start {
		return nil
	}
	body := code[start+1 : end]
	lines := strings.Split(body, "\n")
	var out []string
	indent := 0
	var post []string
	for _, line := range lines {
		l := strings.TrimSpace(line)
		if l == "" {
			continue
		}
		for strings.HasPrefix(l, "}") {
			indent--
			if len(post) > 0 {
				p := post[len(post)-1]
				if p != "" {
					out = append(out, strings.Repeat("  ", indent)+p)
				}
				post = post[:len(post)-1]
			}
			out = append(out, strings.Repeat("  ", indent)+"}")
			l = strings.TrimSpace(strings.TrimPrefix(l, "}"))
			if l == "" {
				break
			}
		}
		if l == "" {
			continue
		}
		if strings.HasSuffix(l, "{") {
			hdr := strings.TrimSpace(strings.TrimSuffix(l, "{"))
			head, p := parseBlockHeader(hdr)
			out = append(out, strings.Repeat("  ", indent)+head+" {")
			indent++
			post = append(post, p)
			continue
		}
		if l == "}" {
			indent--
			if len(post) > 0 {
				p := post[len(post)-1]
				if p != "" {
					out = append(out, strings.Repeat("  ", indent)+p)
				}
				post = post[:len(post)-1]
			}
			out = append(out, strings.Repeat("  ", indent)+"}")
			continue
		}
		stmt := parseStmt(l)
		if stmt != "" {
			out = append(out, strings.Repeat("  ", indent)+stmt)
		}
	}
	return out
}

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindFunction:
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			params, ret := parseDetail(detail)
			if len(params) == 0 {
				p, r := hoverSignature(src, s, ls)
				if len(p) > 0 {
					params = p
				}
				if r != "" {
					ret = r
				}
			}
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
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
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := parseFunctionBody(src, s)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, stmt := range body {
					out.WriteString("  ")
					out.WriteString(stmt)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case any2mochi.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			if len(s.Children) == 0 {
				out.WriteString(" {}\n")
				break
			}
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind != any2mochi.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				if c.Detail != nil {
					t := mapType(strings.TrimSpace(*c.Detail))
					if t != "" {
						out.WriteString(": ")
						out.WriteString(t)
					}
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			if s.Kind == any2mochi.SymbolKindVariable {
				out.WriteString("var ")
			} else {
				out.WriteString("let ")
			}
			out.WriteString(strings.Join(nameParts, "."))
			if s.Detail != nil {
				t := mapType(strings.TrimSpace(*s.Detail))
				if t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
			}
			out.WriteByte('\n')
		case any2mochi.SymbolKindNamespace, any2mochi.SymbolKindPackage, any2mochi.SymbolKindModule:
			writeSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}
