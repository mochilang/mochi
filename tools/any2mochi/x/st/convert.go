package st

import (
	any2mochi "mochi/tools/any2mochi"

	"encoding/json"
	"fmt"
	"os"
	"strings"
)

// Convert converts Smalltalk source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	if ast, err := parseCLI(src); err == nil {
		if out, err := convertAST(ast, src); err == nil {
			return out, nil
		}
	}
	ls := any2mochi.Servers["st"]
	syms, diags, err := any2mochi.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		// fall back to a tiny regex based converter when the language
		// server isn't available. this only handles very small
		// programs consisting of assignments, prints and simple loops.
		return convertFallback(src)
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	appendSymbols(&out, syms, src, ls)

	if out.Len() == 0 {
		// try the lightweight fallback parser if the language server
		// returned no symbols.
		return convertFallback(src)
	}
	return []byte(out.String()), nil
}

func appendSymbols(out *strings.Builder, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		appendSymbol(out, s, src, ls, parentNone)
	}
}

const (
	parentNone = iota
	parentClass
)

func appendSymbol(out *strings.Builder, s any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer, parent int) {
	switch s.Kind {
	case any2mochi.SymbolKindClass:
		fields, rest := splitFields(s.Children)
		out.WriteString("type ")
		out.WriteString(s.Name)
		if len(fields) == 0 && len(rest) == 0 {
			out.WriteString(" {}\n")
			return
		}
		out.WriteString(" {\n")
		for _, f := range fields {
			out.WriteString("  ")
			out.WriteString(f.Name)
			out.WriteByte('\n')
		}
		for _, c := range rest {
			var b strings.Builder
			appendSymbol(&b, c, src, ls, parentClass)
			for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
				out.WriteString("  ")
				out.WriteString(line)
				out.WriteByte('\n')
			}
		}
		out.WriteString("}\n")
	case any2mochi.SymbolKindMethod, any2mochi.SymbolKindConstructor, any2mochi.SymbolKindFunction:
		out.WriteString("fun ")
		out.WriteString(cleanName(s.Name))
		params := extractParams(s)
		if len(params) == 0 {
			params = hoverParams(src, s.SelectionRange.Start, ls)
		}
		out.WriteByte('(')
		for i, p := range params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteByte(')')
		body := convertBody(src, s)
		if body == "" {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, line := range strings.Split(body, "\n") {
				if strings.TrimSpace(line) == "" {
					continue
				}
				out.WriteString("  ")
				out.WriteString(line)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
		for _, c := range s.Children {
			appendSymbol(out, c, src, ls, parent)
		}
	case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
		if parent == parentClass {
			out.WriteString(s.Name)
			out.WriteByte('\n')
		} else if parent == parentNone {
			out.WriteString("let ")
			out.WriteString(s.Name)
			out.WriteByte('\n')
		}
	default:
		for _, c := range s.Children {
			appendSymbol(out, c, src, ls, parent)
		}
	}
}

func splitFields(syms []any2mochi.DocumentSymbol) (fields, rest []any2mochi.DocumentSymbol) {
	for _, s := range syms {
		switch s.Kind {
		case any2mochi.SymbolKindField, any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			fields = append(fields, s)
		default:
			rest = append(rest, s)
		}
	}
	return
}

func cleanName(name string) string {
	if i := strings.Index(name, ":"); i != -1 {
		return name[:i]
	}
	return name
}

func extractParams(sym any2mochi.DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == any2mochi.SymbolKindVariable && c.Range.Start.Line == start {
			if c.Name != "" {
				params = append(params, c.Name)
			}
		}
	}
	return params
}

func hoverParams(src string, pos any2mochi.Position, ls any2mochi.LanguageServer) []string {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil
	}
	text := hoverString(hov)
	text = strings.ReplaceAll(text, "\n", " ")
	parts := strings.Split(text, ":")
	var params []string
	for i := 1; i < len(parts); i++ {
		sec := strings.TrimSpace(parts[i])
		if sec == "" {
			continue
		}
		fields := strings.Fields(sec)
		if len(fields) > 0 {
			p := strings.Trim(fields[0], "()|")
			if p != "" {
				params = append(params, p)
			}
		}
	}
	return params
}

// ConvertFile reads the Smalltalk file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// convertBody extracts the body for the given symbol and converts a few simple
// statements. Only assignments, prints and return expressions are handled.
func convertBody(src string, sym any2mochi.DocumentSymbol) string {
	lines := strings.Split(src, "\n")
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start < 0 || end >= len(lines) || start >= end {
		return ""
	}
	seg := lines[start : end+1]
	// skip method header
	seg = seg[1:]
	// drop trailing ! if present
	if len(seg) > 0 && strings.TrimSpace(seg[len(seg)-1]) == "!" {
		seg = seg[:len(seg)-1]
	}
	var stmts []string
	for _, l := range seg {
		l = strings.TrimSpace(strings.TrimSuffix(l, "."))
		if l == "" || strings.HasPrefix(l, "|") {
			continue
		}
		if s := convertSimpleStmt(l); s != "" {
			stmts = append(stmts, s)
		} else {
			stmts = append(stmts, "// "+l)
		}
	}
	return strings.Join(stmts, "\n")
}

// convertFallback performs a very small subset conversion of Smalltalk source
// without relying on a language server. Only simple global statements are
// supported: variable assignments, prints and basic loops.
func convertFallback(src string) ([]byte, error) {
	lines := strings.Split(src, "\n")
	start := -1
	for i, l := range lines {
		if strings.HasPrefix(strings.TrimSpace(l), "!!") {
			start = i + 1
			break
		}
	}
	if start == -1 {
		start = 0
	}
	var out strings.Builder
	for i := start; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		if l == "" || l == "." {
			continue
		}
		// while loop of the form [(cond)] whileTrue: [ body ]
		if strings.Contains(l, "whileTrue:") {
			condStart := strings.Index(l, "[")
			condEnd := strings.Index(l, "]")
			if condStart != -1 && condEnd != -1 && condEnd > condStart {
				cond := strings.TrimSpace(l[condStart+1 : condEnd])
				cond = strings.Trim(cond, "()")
				body := ""
				// body may start on same line after '[' or next lines until ']'
				bodyParts := []string{}
				rest := l[condEnd+1:]
				if idx := strings.Index(rest, "["); idx != -1 {
					rest = rest[idx+1:]
					if id2 := strings.Index(rest, "]"); id2 != -1 {
						bodyParts = append(bodyParts, strings.TrimSpace(rest[:id2]))
					} else {
						bodyParts = append(bodyParts, strings.TrimSpace(rest))
						for j := i + 1; j < len(lines); j++ {
							if k := strings.Index(lines[j], "]"); k != -1 {
								bodyParts = append(bodyParts, strings.TrimSpace(lines[j][:k]))
								i = j
								break
							}
							bodyParts = append(bodyParts, strings.TrimSpace(lines[j]))
						}
					}
				}
				body = strings.Join(bodyParts, " ")
				if b := convertSimpleStmt(body); b != "" {
					out.WriteString("while " + cond + " {\n  " + b + "\n}\n")
				}
				continue
			}
		}
		if stmt := convertSimpleStmt(strings.TrimSuffix(l, ".")); stmt != "" {
			out.WriteString(stmt + "\n")
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func convertSimpleStmt(l string) string {
	if strings.HasSuffix(l, "Transcript cr") {
		l = strings.TrimSuffix(l, "Transcript cr")
		l = strings.TrimSpace(strings.TrimSuffix(l, "."))
	}
	if strings.Contains(l, "displayOn: Transcript") {
		parts := strings.Split(l, "displayOn: Transcript")
		expr := strings.TrimSpace(parts[0])
		expr = strings.Trim(expr, "()")
		expr = strings.ReplaceAll(expr, "'", "\"")
		expr = strings.ReplaceAll(expr, "\\", "%")
		return "print(" + expr + ")"
	}
	if strings.Contains(l, ":=") {
		parts := strings.SplitN(l, ":=", 2)
		left := strings.TrimSpace(parts[0])
		right := strings.TrimSpace(parts[1])
		right = strings.TrimSuffix(right, ".")
		right = strings.ReplaceAll(right, "\\", "%")
		return left + " = " + right
	}
	if strings.HasPrefix(l, "^") {
		expr := strings.TrimSpace(strings.TrimPrefix(l, "^"))
		expr = strings.ReplaceAll(expr, "\\", "%")
		return "return " + expr
	}
	return ""
}

func formatError(src string, line int, col int, msg string) string {
	return formatErrorSnippet(src, line, col, msg, "")
}

func formatErrorSnippet(src string, line int, col int, msg, snippet string) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var out strings.Builder
	out.WriteString(fmt.Sprintf("line %d:%d: %s\n", line, col, msg))
	for i := start; i < end; i++ {
		out.WriteString(fmt.Sprintf("%3d: %s\n", i+1, lines[i]))
		if i+1 == line && col > 0 {
			out.WriteString(strings.Repeat(" ", col+4))
			out.WriteString("^\n")
		}
	}
	if snippet != "" {
		out.WriteString("snippet:\n")
		out.WriteString(snippet)
		if !strings.HasSuffix(snippet, "\n") {
			out.WriteByte('\n')
		}
	}
	return strings.TrimSpace(out.String())
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

func formatDiagnostics(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		start := int(d.Range.Start.Line)
		msg := d.Message
		line := ""
		if start < len(lines) {
			line = strings.TrimSpace(lines[start])
		}
		out.WriteString(fmt.Sprintf("line %d: %s\n  %s\n", start+1, msg, line))
	}
	return strings.TrimSpace(out.String())
}

func hoverString(h any2mochi.Hover) string {
	switch v := h.Contents.(type) {
	case any2mochi.MarkupContent:
		return v.Value
	case any2mochi.MarkedString:
		b, err := json.Marshal(v)
		if err == nil {
			var ms any2mochi.MarkedStringStruct
			if err := json.Unmarshal(b, &ms); err == nil {
				if ms.Value != "" {
					return ms.Value
				}
			}
			var s string
			if err := json.Unmarshal(b, &s); err == nil {
				return s
			}
		}
		return ""
	case []any2mochi.MarkedString:
		parts := make([]string, 0, len(v))
		for _, m := range v {
			parts = append(parts, hoverString(any2mochi.Hover{Contents: m}))
		}
		return strings.Join(parts, "\n")
	case string:
		return v
	default:
		return fmt.Sprint(v)
	}
}
