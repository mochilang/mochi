package any2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
)

type zigParam struct {
	name string
	typ  string
}

type zigAST struct {
	Vars      []zigVar  `json:"vars"`
	Functions []zigFunc `json:"functions"`
}

type zigVar struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type zigFunc struct {
	Name   string   `json:"name"`
	Params string   `json:"params"`
	Ret    string   `json:"ret"`
	Lines  []string `json:"lines"`
}

func parseZigCLI(src string) (*zigAST, error) {
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
	var ast zigAST
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return &ast, nil
}

func convertZigAST(ast *zigAST) ([]byte, error) {
	var out strings.Builder
	// ignore top-level variables since imports are not needed
	for _, fn := range ast.Functions {
		out.WriteString("fun ")
		out.WriteString(fn.Name)
		out.WriteByte('(')
		params := parseZigParams(fn.Params)
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
			out.WriteString(mapZigType(fn.Ret))
		}
		body := parseZigFunctionBodyLines(fn.Lines)
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
	for _, fn := range ast.Functions {
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

func parseZigFunctionBodyLines(lines []string) []string {
	var out []string
	indent := 0
	for _, line := range lines {
		l := strings.TrimSpace(line)
		if l == "" {
			continue
		}
		for strings.HasPrefix(l, "}") {
			indent--
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
			out = append(out, strings.Repeat("  ", indent)+parseZigBlockHeader(hdr)+" {")
			indent++
			continue
		}
		if l == "}" {
			indent--
			out = append(out, strings.Repeat("  ", indent)+"}")
			continue
		}
		out = append(out, strings.Repeat("  ", indent)+parseZigStmt(l))
	}
	return out
}

// ConvertZig converts zig source code to Mochi using the language server.
func ConvertZig(src string) ([]byte, error) {
	if ast, err := parseZigCLI(src); err == nil {
		return convertZigAST(ast)
	}
	ls := Servers["zig"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeZigSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertZigFile reads the zig file and converts it to Mochi.
func ConvertZigFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertZig(string(data))
}

func parseZigParams(list string) []zigParam {
	parts := strings.Split(list, ",")
	params := make([]zigParam, 0, len(parts))
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
			typ = mapZigType(typ)
		}
		if strings.HasSuffix(name, ":") {
			name = strings.TrimSuffix(name, ":")
		}
		params = append(params, zigParam{name: name, typ: typ})
	}
	return params
}

func parseZigDetail(detail string) ([]zigParam, string) {
	start := strings.Index(detail, "(")
	end := strings.LastIndex(detail, ")")
	if start == -1 || end == -1 || end < start {
		return nil, ""
	}
	paramsPart := detail[start+1 : end]
	retPart := strings.TrimSpace(detail[end+1:])
	params := parseZigParams(paramsPart)
	return params, mapZigType(retPart)
}

func zigHoverSignature(src string, sym DocumentSymbol, ls LanguageServer) ([]zigParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(MarkupContent); ok {
		lines := strings.Split(mc.Value, "\n")
		for i := len(lines) - 1; i >= 0; i-- {
			l := strings.TrimSpace(lines[i])
			if strings.HasPrefix(l, "fn") && strings.Contains(l, "(") && strings.Contains(l, ")") {
				return parseZigDetail(l)
			}
		}
	}
	return nil, ""
}

func mapZigType(t string) string {
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
		inner := mapZigType(strings.TrimPrefix(t, "[]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

func extractRange(src string, r Range) string {
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

func convertZigExpr(expr string) string {
	expr = strings.TrimSpace(expr)
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

func parseZigBlockHeader(h string) string {
	switch {
	case strings.HasPrefix(h, "if "):
		cond := strings.TrimSpace(strings.TrimPrefix(h, "if "))
		cond = strings.Trim(cond, "()")
		return "if " + convertZigExpr(cond)
	case strings.HasPrefix(h, "else if "):
		cond := strings.TrimSpace(strings.TrimPrefix(h, "else if "))
		cond = strings.Trim(cond, "()")
		return "else if " + convertZigExpr(cond)
	case h == "else":
		return "else"
	case strings.HasPrefix(h, "while "):
		cond := strings.TrimSpace(strings.TrimPrefix(h, "while "))
		cond = strings.Trim(cond, "()")
		return "while " + convertZigExpr(cond)
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
				return "for " + name + " in " + convertZigExpr(coll)
			}
		}
		return convertZigExpr(h)
	}
	return convertZigExpr(h)
}

func parseZigStmt(l string) string {
	if strings.HasSuffix(l, ";") {
		l = strings.TrimSuffix(l, ";")
	}
	switch {
	case strings.HasPrefix(l, "return "):
		expr := strings.TrimSpace(l[len("return "):])
		return "return " + convertZigExpr(expr)
	case strings.HasPrefix(l, "var "):
		parts := strings.SplitN(l[4:], "=", 2)
		if len(parts) == 2 {
			name := strings.Fields(parts[0])[0]
			expr := convertZigExpr(parts[1])
			return "let " + name + " = " + expr
		}
	case strings.HasPrefix(l, "const "):
		parts := strings.SplitN(l[6:], "=", 2)
		if len(parts) == 2 {
			name := strings.Fields(parts[0])[0]
			expr := convertZigExpr(parts[1])
			return "let " + name + " = " + expr
		}
	case l == "break":
		return "break"
	case l == "continue":
		return "continue"
	}
	return convertZigExpr(l)
}

func parseZigFunctionBody(src string, sym DocumentSymbol) []string {
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
	for _, line := range lines {
		l := strings.TrimSpace(line)
		if l == "" {
			continue
		}
		for strings.HasPrefix(l, "}") {
			indent--
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
			out = append(out, strings.Repeat("  ", indent)+parseZigBlockHeader(hdr)+" {")
			indent++
			continue
		}
		if l == "}" {
			indent--
			out = append(out, strings.Repeat("  ", indent)+"}")
			continue
		}
		out = append(out, strings.Repeat("  ", indent)+parseZigStmt(l))
	}
	return out
}

func writeZigSymbols(out *strings.Builder, prefix []string, syms []DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case SymbolKindFunction:
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			params, ret := parseZigDetail(detail)
			if len(params) == 0 {
				p, r := zigHoverSignature(src, s, ls)
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
			body := parseZigFunctionBody(src, s)
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
		case SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			if len(s.Children) == 0 {
				out.WriteString(" {}\n")
				break
			}
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind != SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				if c.Detail != nil {
					t := mapZigType(strings.TrimSpace(*c.Detail))
					if t != "" {
						out.WriteString(": ")
						out.WriteString(t)
					}
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case SymbolKindVariable, SymbolKindConstant:
			out.WriteString("let ")
			out.WriteString(strings.Join(nameParts, "."))
			if s.Detail != nil {
				t := mapZigType(strings.TrimSpace(*s.Detail))
				if t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
			}
			out.WriteByte('\n')
		case SymbolKindNamespace, SymbolKindPackage, SymbolKindModule:
			writeZigSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}
