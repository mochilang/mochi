package racket

import (
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"strings"

	parent "mochi/tools/any2mochi"
)

// Convert converts Racket source code to Mochi. It first tries the configured
// language server and falls back to a small parser implemented in this
// package. Only information returned by the language server is used—no regex
// parsing or other fallbacks.
func Convert(src string) ([]byte, error) {
	ls := parent.Servers["rkt"]
	var out strings.Builder
	if ls.Command != "" {
		syms, diags, err := parent.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
		if err == nil && len(diags) == 0 {
			writeSymbols(&out, nil, syms, src, ls)
		}
	}
	var items []item
	if out.Len() == 0 {
		items = parse(src)
		writeItems(&out, items)
	}
	if len(items) == 0 {
		parseToplevel(&out, src)
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the rkt file and converts it to Mochi.
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
		case parent.SymbolKindNamespace, parent.SymbolKindPackage, parent.SymbolKindModule:
			writeSymbols(out, nameParts, s.Children, src, ls)
		case parent.SymbolKindFunction:
			params, ret := parseSignature(s.Detail)
			if len(params) == 0 && ret == "" {
				if hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					params, ret = parseHoverSignature(hov)
				}
			}
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			if len(params) > 0 {
				out.WriteString(strings.Join(params, ", "))
			}
			out.WriteByte(')')
			if ret != "" && ret != "Void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := functionBody(src, s)
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
		case parent.SymbolKindStruct, parent.SymbolKindClass:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			if len(s.Children) == 0 {
				out.WriteString(" {}\n")
				continue
			}
			out.WriteString(" {\n")
			for _, f := range s.Children {
				if f.Kind != parent.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(f.Name)
				typ := parseVarType(f.Detail)
				if typ == "" {
					typ = getVarType(src, f.SelectionRange.Start, ls)
				}
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case parent.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == parent.SymbolKindEnumMember {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []parent.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != parent.SymbolKindEnumMember {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeSymbols(out, nameParts, rest, src, ls)
			}
		case parent.SymbolKindVariable, parent.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				typ := parseVarType(s.Detail)
				if typ == "" {
					typ = getVarType(src, s.SelectionRange.Start, ls)
				}
				if typ != "" {
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

func parseParams(detail *string) []string {
	if detail == nil {
		return nil
	}
	d := *detail
	open := strings.Index(d, "(")
	close := strings.Index(d, ")")
	if open == -1 || close == -1 || close <= open {
		return nil
	}
	list := strings.TrimSpace(d[open+1 : close])
	if list == "" {
		return nil
	}
	return strings.Fields(list)
}

func parseHoverParams(h parent.Hover) []string {
	var text string
	switch c := h.Contents.(type) {
	case parent.MarkupContent:
		text = c.Value
	case parent.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m parent.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []parent.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m parent.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		open := strings.Index(line, "(")
		close := strings.Index(line, ")")
		if open == -1 || close == -1 || close <= open {
			continue
		}
		list := strings.TrimSpace(line[open+1 : close])
		if list == "" {
			continue
		}
		return strings.Fields(list)
	}
	return nil
}

func parseSignature(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	open := strings.Index(d, "(")
	close := strings.Index(d, ")")
	if open == -1 || close == -1 || close <= open {
		return nil, ""
	}
	params := strings.Fields(strings.TrimSpace(d[open+1 : close]))
	rest := strings.TrimSpace(d[close+1:])
	ret := ""
	if strings.HasPrefix(rest, "->") {
		ret = strings.TrimSpace(rest[2:])
	}
	return params, ret
}

func parseHoverSignature(h parent.Hover) ([]string, string) {
	var text string
	switch c := h.Contents.(type) {
	case parent.MarkupContent:
		text = c.Value
	case parent.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m parent.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []parent.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m parent.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, "(") && strings.Contains(line, ")") {
			open := strings.Index(line, "(")
			close := strings.Index(line, ")")
			if open != -1 && close != -1 && close > open {
				params := strings.Fields(strings.TrimSpace(line[open+1 : close]))
				rest := strings.TrimSpace(line[close+1:])
				ret := ""
				if strings.HasPrefix(rest, "->") {
					ret = strings.TrimSpace(rest[2:])
				}
				if len(params) > 0 || ret != "" {
					return params, ret
				}
			}
		}
	}
	return nil, ""
}

func parseVarType(detail *string) string {
	if detail == nil {
		return ""
	}
	if idx := strings.Index(*detail, ":"); idx != -1 {
		return strings.TrimSpace((*detail)[idx+1:])
	}
	return ""
}

func getVarType(src string, pos parent.Position, ls parent.LanguageServer) string {
	hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return ""
	}
	return parseHoverVarType(hov)
}

func parseHoverVarType(h parent.Hover) string {
	var text string
	switch c := h.Contents.(type) {
	case parent.MarkupContent:
		text = c.Value
	case parent.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m parent.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []parent.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m parent.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	if i := strings.Index(text, ":"); i != -1 {
		return strings.TrimSpace(text[i+1:])
	}
	return ""
}

// parseToplevel looks for simple variable definitions and loops
// in the source that are not reported by the language server.
// This is a best-effort regex based fallback to make the generated
// Mochi code more complete and runnable.
func parseToplevel(out *strings.Builder, src string) {
	varDefine := regexp.MustCompile(`(?m)^\(define\s+([A-Za-z0-9_-]+)\s+(.+)\)$`)
	for _, m := range varDefine.FindAllStringSubmatch(src, -1) {
		name := m[1]
		expr := strings.TrimSpace(m[2])
		// Skip function definitions which start with '(' after the name
		if strings.HasPrefix(expr, "(") {
			continue
		}
		if val := parseValue(expr); val != "" {
			fmt.Fprintf(out, "let %s = %s\n", name, val)
		} else {
			fmt.Fprintf(out, "let %s\n", name)
		}
	}

	loopRe := regexp.MustCompile(`(?s)\(for\s*\(\s*\[([A-Za-z0-9_-]+)\s+([^\]]+)\]\s*\)\s*\(displayln\s+([^\)]+)\)\s*\)`)
	for _, m := range loopRe.FindAllStringSubmatch(src, -1) {
		varName := strings.TrimSpace(m[1])
		collection := strings.TrimSpace(m[2])
		bodyExpr := strings.TrimSpace(m[3])
		fmt.Fprintf(out, "for %s in %s {\n  print(%s)\n}\n", varName, convertExpr(collection), convertExpr(bodyExpr))
	}
}

// parseValue converts simple Racket expressions into Mochi expressions.
// It supports numbers, strings, lists and hashes generated by the Mochi
// Racket backend.
func parseValue(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "\"") {
		return expr
	}
	if strings.HasPrefix(expr, "'") {
		expr = strings.TrimPrefix(expr, "'")
	}
	if regexp.MustCompile(`^[0-9-]+$`).MatchString(expr) {
		return expr
	}
	if strings.HasPrefix(expr, "(list") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSpace(expr[len("(list") : len(expr)-1])
		itemRe := regexp.MustCompile(`\(hash[^\)]*\)`)
		items := itemRe.FindAllString(inner, -1)
		var out []string
		for _, it := range items {
			out = append(out, parseHash(it))
		}
		if len(out) > 0 {
			return "[" + strings.Join(out, ", ") + "]"
		}
	}
	if strings.HasPrefix(expr, "(hash") && strings.HasSuffix(expr, ")") {
		return parseHash(expr)
	}
	return ""
}

func parseHash(expr string) string {
	inner := strings.TrimSpace(expr[len("(hash") : len(expr)-1])
	fields := strings.Fields(inner)
	var pairs []string
	for i := 0; i+1 < len(fields); i += 2 {
		key := strings.Trim(fields[i], `"`)
		val := convertExpr(fields[i+1])
		pairs = append(pairs, fmt.Sprintf("%s: %s", key, val))
	}
	return "{" + strings.Join(pairs, ", ") + "}"
}

// convertExpr performs a very small set of conversions from Racket
// expressions to Mochi expressions.
func convertExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "(hash-ref") {
		parts := strings.Fields(expr)
		if len(parts) == 3 {
			return fmt.Sprintf("%s[%s]", parts[1], strings.Trim(parts[2], `"`))
		}
	} else if strings.HasPrefix(expr, "(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSpace(expr[1 : len(expr)-1])
		parts := strings.Fields(inner)
		if len(parts) == 3 && (parts[0] == "+" || parts[0] == "-" || parts[0] == "*" || parts[0] == "/") {
			return fmt.Sprintf("%s %s %s", parts[1], parts[0], parts[2])
		}
	}
	return expr
}

// functionBody extracts a very small subset of statements from the
// function body so that common examples become runnable. Unsupported
// statements are ignored.
func functionBody(src string, sym parent.DocumentSymbol) []string {
	start := posIndex(src, sym.Range.Start)
	end := posIndex(src, sym.Range.End)
	if start >= len(src) || end > len(src) || start >= end {
		return nil
	}
	snippet := src[start:end]
	open := strings.Index(snippet, "\n")
	if open == -1 {
		return nil
	}
	body := snippet[open:]
	var out []string
	lineRe := regexp.MustCompile(`\([^()]+\)`)
	for _, m := range lineRe.FindAllString(body, -1) {
		stmt := strings.TrimSpace(m)
		if strings.HasPrefix(stmt, "(displayln") {
			expr := strings.TrimSpace(stmt[len("(displayln") : len(stmt)-1])
			out = append(out, "print("+convertExpr(expr)+")")
		} else if strings.HasPrefix(stmt, "(return") {
			expr := strings.TrimSpace(stmt[len("(return") : len(stmt)-1])
			if expr != "" && expr != "(void)" {
				out = append(out, "return "+convertExpr(expr))
			} else {
				out = append(out, "return")
			}
		}
	}
	return out
}

// indexForPosition converts a protocol position to a byte offset in src.
func posIndex(src string, pos parent.Position) int {
	lines := strings.Split(src, "\n")
	if int(pos.Line) >= len(lines) {
		return len(src)
	}
	idx := 0
	for i := 0; i < int(pos.Line); i++ {
		idx += len(lines[i]) + 1
	}
	if int(pos.Character) > len(lines[int(pos.Line)]) {
		idx += len(lines[int(pos.Line)])
	} else {
		idx += int(pos.Character)
	}
	return idx
}

// ---------- Fallback parser ----------

// writeItems converts parsed items to Mochi code.
func writeItems(out *strings.Builder, items []item) {
	for _, it := range items {
		switch it.Kind {
		case "func":
			out.WriteString("fun ")
			out.WriteString(it.Name)
			out.WriteByte('(')
			if len(it.Params) > 0 {
				out.WriteString(strings.Join(it.Params, ", "))
			}
			out.WriteByte(')')
			body := bodyFromSnippet(it.Body)
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
		case "var":
			out.WriteString("let ")
			out.WriteString(it.Name)
			if val := parseValue(it.Value); val != "" {
				out.WriteString(" = ")
				out.WriteString(val)
			}
			out.WriteByte('\n')
		case "print":
			out.WriteString("print(")
			out.WriteString(convertExpr(it.Value))
			out.WriteString(")\n")
		}
	}
}

func bodyFromSnippet(snippet string) []string {
	var out []string
	lineRe := regexp.MustCompile(`\([^()]+\)`)
	for _, m := range lineRe.FindAllString(snippet, -1) {
		stmt := strings.TrimSpace(m)
		if strings.HasPrefix(stmt, "(displayln") {
			expr := strings.TrimSpace(stmt[len("(displayln") : len(stmt)-1])
			out = append(out, "print("+convertExpr(expr)+")")
		} else if strings.HasPrefix(stmt, "(return") {
			expr := strings.TrimSpace(stmt[len("(return") : len(stmt)-1])
			if expr != "" && expr != "(void)" {
				out = append(out, "return "+convertExpr(expr))
			} else {
				out = append(out, "return")
			}
		}
	}
	return out
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
