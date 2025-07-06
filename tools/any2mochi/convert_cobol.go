package any2mochi

import (
	"fmt"
	"os"
	"strings"
)

// ConvertCobol converts COBOL source code to Mochi using the language server.
func ConvertCobol(src string) ([]byte, error) {
	ls := Servers["cobol"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	writeCobolSymbols(&out, ls, src, nil, syms)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertCobolFile reads the COBOL file and converts it to Mochi.
func ConvertCobolFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCobol(string(data))
}

func writeCobolSymbols(out *strings.Builder, ls LanguageServer, src string, prefix []string, syms []DocumentSymbol) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}

		switch s.Kind {
		case SymbolKindFunction, SymbolKindMethod, SymbolKindConstructor:
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			if detail == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					detail = hoverString(hov)
				}
			}
			params, ret := parseCobolSignature(detail)

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
			bodyLines := linesInRange(src, s.Range)
			stmts := parseCobolStatements(bodyLines)
			if len(stmts) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, st := range stmts {
					out.WriteString("  ")
					out.WriteString(st)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}

		case SymbolKindVariable, SymbolKindConstant, SymbolKindField, SymbolKindProperty:
			typ := ""
			if s.Detail != nil {
				typ = parseCobolType(*s.Detail)
			}
			if typ == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					typ = parseCobolType(hoverString(hov))
				}
			}
			out.WriteString("var ")
			out.WriteString(strings.Join(nameParts, "."))
			if typ != "" {
				out.WriteString(": ")
				out.WriteString(typ)
			}
			out.WriteString(" = nil\n")

		case SymbolKindClass, SymbolKindStruct, SymbolKindInterface, SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			var fields []DocumentSymbol
			for _, c := range s.Children {
				switch c.Kind {
				case SymbolKindField, SymbolKindProperty, SymbolKindVariable:
					fields = append(fields, c)
				}
			}
			if len(fields) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					typ := ""
					if f.Detail != nil {
						typ = parseCobolType(*f.Detail)
					}
					if typ == "" {
						if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, f.SelectionRange.Start); err == nil {
							typ = parseCobolType(hoverString(hov))
						}
					}
					out.WriteString("  ")
					out.WriteString(f.Name)
					if typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
			for _, c := range s.Children {
				switch c.Kind {
				case SymbolKindField, SymbolKindProperty, SymbolKindVariable:
					continue
				default:
					writeCobolSymbols(out, ls, src, nameParts, []DocumentSymbol{c})
				}
			}
			continue

		case SymbolKindNamespace, SymbolKindPackage, SymbolKindModule:
			if len(s.Children) > 0 {
				writeCobolSymbols(out, ls, src, nameParts, s.Children)
			}
			continue
		}

		if len(s.Children) > 0 {
			writeCobolSymbols(out, ls, src, nameParts, s.Children)
		}
	}
}

func parseCobolParams(detail string) []cobolParam {
	start := strings.Index(detail, "(")
	end := strings.Index(detail, ")")
	if start == -1 || end == -1 || end < start {
		return nil
	}
	list := detail[start+1 : end]
	parts := strings.Split(list, ",")
	params := make([]cobolParam, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.Fields(p)
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		name = strings.TrimSuffix(name, ".")
		name = strings.TrimPrefix(name, "using")
		name = strings.TrimPrefix(name, "by")
		name = strings.TrimPrefix(name, "value")
		name = strings.TrimPrefix(name, "reference")
		params = append(params, cobolParam{name: name, typ: parseCobolType(p)})
	}
	return params
}

type cobolParam struct {
	name string
	typ  string
}

func parseCobolSignature(detail string) ([]cobolParam, string) {
	params := parseCobolParams(detail)
	ret := ""
	lower := strings.ToLower(detail)
	if idx := strings.Index(lower, "returning"); idx != -1 {
		r := strings.TrimSpace(detail[idx+len("returning"):])
		r = strings.TrimSuffix(r, ".")
		if f := strings.Fields(r); len(f) > 0 {
			ret = mapCobolType(f[0])
			if ret == "" {
				ret = parseCobolType(r)
			}
		}
	}
	return params, ret
}

func mapCobolType(t string) string {
	t = strings.ToLower(t)
	switch {
	case strings.Contains(t, "9"):
		return "int"
	case strings.Contains(t, "x"), strings.Contains(t, "char"), strings.Contains(t, "string"):
		return "string"
	case strings.Contains(t, "comp-2"), strings.Contains(t, "float"), strings.Contains(t, "decimal"):
		return "float"
	case strings.Contains(t, "comp-5"), strings.Contains(t, "binary"):
		return "int"
	case strings.Contains(t, "comp-3"):
		return "int"
	case strings.Contains(t, "boolean"), strings.Contains(t, "bool"):
		return "bool"
	default:
		return ""
	}
}

func parseCobolType(detail string) string {
	lower := strings.ToLower(detail)
	if idx := strings.Index(lower, "pic"); idx != -1 {
		rest := strings.TrimSpace(detail[idx+3:])
		if f := strings.Fields(rest); len(f) > 0 {
			if t := mapCobolType(f[0]); t != "" {
				return t
			}
		}
	}
	if idx := strings.Index(lower, "usage is"); idx != -1 {
		rest := strings.TrimSpace(detail[idx+8:])
		if f := strings.Fields(rest); len(f) > 0 {
			if t := mapCobolType(f[0]); t != "" {
				return t
			}
		}
	}
	fields := strings.Fields(detail)
	if len(fields) > 0 {
		if t := mapCobolType(fields[0]); t != "" {
			return t
		}
	}
	return ""
}

// linesInRange returns the lines of src covered by r. If the range is outside
// the bounds of src, it returns an empty slice.
func linesInRange(src string, r Range) []string {
	lines := strings.Split(src, "\n")
	start := int(r.Start.Line)
	end := int(r.End.Line)
	if start >= len(lines) || start < 0 {
		return nil
	}
	if end >= len(lines) {
		end = len(lines) - 1
	}
	if end < start {
		end = start
	}
	return lines[start : end+1]
}

// parseCobolStatements converts a list of COBOL statements to Mochi equivalents.
// Only a very small subset used by the tests is recognised.
func parseCobolStatements(lines []string) []string {
	var out []string
	for i := 0; i < len(lines); i++ {
		ll := strings.TrimSpace(strings.TrimSuffix(lines[i], "."))
		switch {
		case strings.HasPrefix(ll, "DISPLAY "):
			expr := strings.TrimSpace(strings.TrimPrefix(ll, "DISPLAY "))
			out = append(out, "print("+expr+")")

		case strings.HasPrefix(ll, "MOVE "):
			rest := strings.TrimSpace(strings.TrimPrefix(ll, "MOVE "))
			parts := strings.Split(rest, " TO ")
			if len(parts) == 2 {
				expr := strings.TrimSpace(parts[0])
				dest := strings.TrimSpace(parts[1])
				out = append(out, dest+" = "+expr)
			}

		case strings.HasPrefix(ll, "COMPUTE "):
			expr := strings.TrimSpace(strings.TrimPrefix(ll, "COMPUTE "))
			out = append(out, expr)

		case strings.HasPrefix(ll, "ADD "):
			rest := strings.TrimSpace(strings.TrimPrefix(ll, "ADD "))
			parts := strings.Split(rest, " TO ")
			if len(parts) == 2 {
				val := strings.TrimSpace(parts[0])
				dest := strings.TrimSpace(parts[1])
				out = append(out, dest+" = "+dest+" + "+val)
			}

		case strings.HasPrefix(ll, "PERFORM UNTIL "):
			cond := strings.TrimSpace(strings.TrimPrefix(ll, "PERFORM UNTIL "))
			body := []string{}
			for j := i + 1; j < len(lines); j++ {
				n := strings.TrimSpace(lines[j])
				if n == "END-PERFORM" {
					i = j
					break
				}
				body = append(body, lines[j])
			}
			isNot := strings.HasPrefix(cond, "NOT ")
			if isNot {
				cond = strings.TrimSpace(strings.TrimPrefix(cond, "NOT "))
				out = append(out, "while "+cond+" {")
			} else {
				out = append(out, "while !("+cond+") {")
			}
			inner := parseCobolStatements(body)
			for _, st := range inner {
				out = append(out, "  "+st)
			}
			out = append(out, "}")

		case strings.HasPrefix(ll, "PERFORM VARYING "):
			rest := strings.TrimSpace(strings.TrimPrefix(ll, "PERFORM VARYING "))
			parts := strings.Split(rest, " FROM ")
			if len(parts) != 2 {
				break
			}
			varName := strings.TrimSpace(parts[0])
			rest = parts[1]
			parts = strings.Split(rest, " BY ")
			if len(parts) != 2 {
				break
			}
			start := strings.TrimSpace(parts[0])
			rest = parts[1]
			parts = strings.Split(rest, " UNTIL ")
			if len(parts) != 2 {
				break
			}
			step := strings.TrimSpace(parts[0])
			cond := strings.TrimSpace(parts[1])
			body := []string{}
			for j := i + 1; j < len(lines); j++ {
				n := strings.TrimSpace(lines[j])
				if n == "END-PERFORM" {
					i = j
					break
				}
				body = append(body, lines[j])
			}
			end := ""
			if strings.Contains(cond, ">=") {
				end = strings.TrimSpace(strings.Split(cond, ">=")[1])
			} else if strings.Contains(cond, "<=") {
				end = strings.TrimSpace(strings.Split(cond, "<=")[1])
			}
			loop := "for " + strings.ToLower(varName) + " in range(" + start + ", " + end
			if step != "1" && step != "-1" {
				loop += ", " + step
			}
			loop += ") {"
			out = append(out, loop)
			inner := parseCobolStatements(body)
			for _, st := range inner {
				out = append(out, "  "+st)
			}
			out = append(out, "}")

		case strings.HasPrefix(ll, "IF "):
			cond := strings.TrimSpace(strings.TrimPrefix(ll, "IF "))
			thenLines := []string{}
			elseLines := []string{}
			j := i + 1
			inElse := false
			for ; j < len(lines); j++ {
				n := strings.TrimSpace(lines[j])
				if n == "ELSE" {
					inElse = true
					continue
				}
				if n == "END-IF" {
					break
				}
				if inElse {
					elseLines = append(elseLines, lines[j])
				} else {
					thenLines = append(thenLines, lines[j])
				}
			}
			i = j
			out = append(out, "if "+cond+" {")
			inner := parseCobolStatements(thenLines)
			for _, st := range inner {
				out = append(out, "  "+st)
			}
			if len(elseLines) > 0 {
				out = append(out, "} else {")
				inner2 := parseCobolStatements(elseLines)
				for _, st := range inner2 {
					out = append(out, "  "+st)
				}
			}
			out = append(out, "}")
		}
	}
	return out
}
