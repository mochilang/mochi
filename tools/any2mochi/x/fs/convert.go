package fs

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	any2mochi "mochi/tools/any2mochi"
)

// Convert converts F# source code to Mochi using information extracted from
// the fsautocomplete language server. The converter avoids regex parsing when
// the server provides enough detail.
func Convert(src string) ([]byte, error) {
	ast, err := parseAST(src)
	if err == nil && ast != nil && (len(ast.Vars) > 0 || len(ast.Prints) > 0) {
		var out strings.Builder
		for _, v := range ast.Vars {
			out.WriteString("let ")
			out.WriteString(v.Name)
			out.WriteString(" = ")
			out.WriteString(v.Expr)
			out.WriteByte('\n')
		}
		for _, p := range ast.Prints {
			out.WriteString("print(")
			out.WriteString(p)
			out.WriteString(")\n")
		}
		return []byte(out.String()), nil
	}

	// Fallback to simple regex based conversion if parsing via CLI failed.
	if out, ferr := convertFallback(src); ferr == nil {
		return out, nil
	}
	if err != nil {
		return nil, err
	}
	return nil, fmt.Errorf("unsupported")
}

// ConvertFile reads the F# file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

type param struct {
	name string
	typ  string
}

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod:
			params, ret := parseDetail(s.Detail)
			if len(params) == 0 && ret == "" {
				if hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					params, ret = parseHover(hov)
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
			}
			out.WriteByte(')')
			if t := mapType(ret); t != "" {
				out.WriteString(": ")
				out.WriteString(t)
			}
			body := functionBody(src, s)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, ln := range body {
					out.WriteString("  ")
					out.WriteString(ln)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant, any2mochi.SymbolKindField:
			out.WriteString("let ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('\n')
		case any2mochi.SymbolKindStruct, any2mochi.SymbolKindClass, any2mochi.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		}
		if len(s.Children) > 0 {
			writeSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func parseDetail(detail *string) ([]param, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	if d == "" {
		return nil, ""
	}
	if strings.HasPrefix(d, "val") {
		if idx := strings.Index(d, ":"); idx != -1 {
			d = strings.TrimSpace(d[idx+1:])
		}
	}
	parts := strings.Split(d, "->")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	ret := ""
	if len(parts) > 0 {
		ret = parts[len(parts)-1]
	}
	params := make([]param, 0, len(parts)-1)
	for _, p := range parts[:len(parts)-1] {
		name := ""
		if idx := strings.Index(p, ":"); idx != -1 {
			name = strings.TrimSpace(p[:idx])
		} else {
			fields := strings.Fields(p)
			if len(fields) > 0 {
				name = fields[len(fields)-1]
			}
		}
		if name == "" {
			name = fmt.Sprintf("p%d", len(params))
		}
		params = append(params, param{name: name})
	}
	return params, ret
}

func parseHover(h any2mochi.Hover) ([]param, string) {
	text := hoverString(h)
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, "->") {
			if p, r := parseDetail(&line); len(p) > 0 || r != "" {
				return p, r
			}
		}
	}
	return nil, ""
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "unit":
		return ""
	case "int", "int32", "int64", "uint32", "uint64":
		return "int"
	case "float", "double", "decimal":
		return "float"
	case "bool":
		return "bool"
	case "string":
		return "string"
	}
	if strings.HasSuffix(t, " list") {
		inner := mapType(strings.TrimSuffix(t, " list"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := mapType(strings.TrimSuffix(t, " array"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return ""
}

func functionBody(src string, sym any2mochi.DocumentSymbol) []string {
	lines := strings.Split(src, "\n")
	start := posToOffset(lines, sym.Range.Start)
	end := posToOffset(lines, sym.Range.End)
	if start >= len(src) || end > len(src) || start >= end {
		return nil
	}
	snippet := src[start:end]
	key := "raise (Return_" + sym.Name + " ("
	idx := strings.Index(snippet, key)
	if idx == -1 {
		return nil
	}
	exprStart := idx + len(key)
	depth := 1
	i := exprStart
	for i < len(snippet) && depth > 0 {
		switch snippet[i] {
		case '(':
			depth++
		case ')':
			depth--
		}
		i++
	}
	if depth != 0 {
		return nil
	}
	expr := strings.TrimSpace(snippet[exprStart : i-1])
	return []string{"return " + expr}
}

func posToOffset(lines []string, pos any2mochi.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		if i < len(lines) {
			off += len(lines[i]) + 1
		}
	}
	off += int(pos.Character)
	return off
}

func convertFallback(src string) ([]byte, error) {
	for _, line := range strings.Split(src, "\n") {
		l := strings.TrimSpace(line)
		if strings.HasPrefix(l, "ignore (printfn") {
			open := strings.Index(l, "(")
			close := strings.LastIndex(l, ")")
			if open != -1 && close != -1 && close > open {
				expr := strings.TrimSpace(l[open+1 : close])
				parts := strings.Split(expr, ",")
				if len(parts) > 1 {
					expr = strings.Join(parts[1:], ",")
				}
				return []byte("print(" + strings.TrimSpace(expr) + ")\n"), nil
			}
		}
	}
	return nil, fmt.Errorf("unsupported")
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
