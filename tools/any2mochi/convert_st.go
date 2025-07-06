package any2mochi

import (
	"fmt"
	"os"
	"strings"
)

// ConvertSt converts st source code to Mochi using the language server.
func ConvertSt(src string) ([]byte, error) {
	if ast, err := parseStCLI(src); err == nil {
		if out, err := convertStAST(ast); err == nil {
			return out, nil
		}
	}
	ls := Servers["st"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		// fall back to a tiny regex based converter when the language
		// server isn't available. this only handles very small
		// programs consisting of assignments, prints and simple loops.
		return convertStFallback(src)
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	appendStSymbols(&out, syms, src, ls)

	if out.Len() == 0 {
		// try the lightweight fallback parser if the language server
		// returned no symbols.
		return convertStFallback(src)
	}
	return []byte(out.String()), nil
}

func appendStSymbols(out *strings.Builder, syms []DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		appendStSymbol(out, s, src, ls, parentNone)
	}
}

const (
	parentNone = iota
	parentClass
)

func appendStSymbol(out *strings.Builder, s DocumentSymbol, src string, ls LanguageServer, parent int) {
	switch s.Kind {
	case SymbolKindClass:
		fields, rest := splitStFields(s.Children)
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
			appendStSymbol(&b, c, src, ls, parentClass)
			for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
				out.WriteString("  ")
				out.WriteString(line)
				out.WriteByte('\n')
			}
		}
		out.WriteString("}\n")
	case SymbolKindMethod, SymbolKindConstructor, SymbolKindFunction:
		out.WriteString("fun ")
		out.WriteString(cleanStName(s.Name))
		params := extractStParams(s)
		if len(params) == 0 {
			params = getStHoverParams(src, s.SelectionRange.Start, ls)
		}
		out.WriteByte('(')
		for i, p := range params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteByte(')')
		body := convertStBody(src, s)
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
			appendStSymbol(out, c, src, ls, parent)
		}
	case SymbolKindVariable, SymbolKindConstant:
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
			appendStSymbol(out, c, src, ls, parent)
		}
	}
}

func splitStFields(syms []DocumentSymbol) (fields, rest []DocumentSymbol) {
	for _, s := range syms {
		switch s.Kind {
		case SymbolKindField, SymbolKindVariable, SymbolKindConstant:
			fields = append(fields, s)
		default:
			rest = append(rest, s)
		}
	}
	return
}

func cleanStName(name string) string {
	if i := strings.Index(name, ":"); i != -1 {
		return name[:i]
	}
	return name
}

func extractStParams(sym DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == SymbolKindVariable && c.Range.Start.Line == start {
			if c.Name != "" {
				params = append(params, c.Name)
			}
		}
	}
	return params
}

func getStHoverParams(src string, pos Position, ls LanguageServer) []string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
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

// ConvertStFile reads the st file and converts it to Mochi.
func ConvertStFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSt(string(data))
}

// convertStBody extracts the body for the given symbol and converts a few simple
// statements. Only assignments, prints and return expressions are handled.
func convertStBody(src string, sym DocumentSymbol) string {
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
		if s := convertStSimpleStmt(l); s != "" {
			stmts = append(stmts, s)
		} else {
			stmts = append(stmts, "// "+l)
		}
	}
	return strings.Join(stmts, "\n")
}

// convertStFallback performs a very small subset conversion of Smalltalk source
// without relying on a language server. Only simple global statements are
// supported: variable assignments, prints and basic loops.
func convertStFallback(src string) ([]byte, error) {
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
				if b := convertStSimpleStmt(body); b != "" {
					out.WriteString("while " + cond + " {\n  " + b + "\n}\n")
				}
				continue
			}
		}
		if stmt := convertStSimpleStmt(strings.TrimSuffix(l, ".")); stmt != "" {
			out.WriteString(stmt + "\n")
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func convertStSimpleStmt(l string) string {
	if strings.HasSuffix(l, "Transcript cr") {
		l = strings.TrimSuffix(l, "Transcript cr")
		l = strings.TrimSpace(strings.TrimSuffix(l, "."))
	}
	if strings.Contains(l, "displayOn: Transcript") {
		parts := strings.Split(l, "displayOn: Transcript")
		expr := strings.TrimSpace(parts[0])
		expr = strings.Trim(expr, "()")
		expr = strings.ReplaceAll(expr, "'", "\"")
		return "print(" + expr + ")"
	}
	if strings.Contains(l, ":=") {
		parts := strings.SplitN(l, ":=", 2)
		left := strings.TrimSpace(parts[0])
		right := strings.TrimSpace(parts[1])
		right = strings.TrimSuffix(right, ".")
		return left + " = " + right
	}
	if strings.HasPrefix(l, "^") {
		return "return " + strings.TrimSpace(strings.TrimPrefix(l, "^"))
	}
	return ""
}
