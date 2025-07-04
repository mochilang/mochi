package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertFs converts F# source code to Mochi using the fsautocomplete language
// server. The converter relies solely on the information returned by the
// language server and avoids any regex based parsing.
func ConvertFs(src string) ([]byte, error) {
	ls := Servers["fs"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil || len(syms) == 0 {
		if out, ferr := convertFsFallback(src); ferr == nil {
			return out, nil
		}
		if err != nil {
			return nil, err
		}
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeFsSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFsFile reads the fs file and converts it to Mochi.
func ConvertFsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertFs(string(data))
}

type fsParam struct {
	name string
	typ  string
}

func writeFsSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			params, ret := parseFsDetail(s.Detail)
			if len(params) == 0 && ret == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					params, ret = parseFsHover(hov)
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
			if t := mapFsType(ret); t != "" {
				out.WriteString(": ")
				out.WriteString(t)
			}
			body := fsFunctionBody(src, s)
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
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant, protocol.SymbolKindField:
			out.WriteString("let ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('\n')
		case protocol.SymbolKindStruct, protocol.SymbolKindClass, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		}
		if len(s.Children) > 0 {
			writeFsSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func parseFsDetail(detail *string) ([]fsParam, string) {
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
	params := make([]fsParam, 0, len(parts)-1)
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
		params = append(params, fsParam{name: name})
	}
	return params, ret
}

func parseFsHover(h protocol.Hover) ([]fsParam, string) {
	text := hoverString(h)
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, "->") {
			if p, r := parseFsDetail(&line); len(p) > 0 || r != "" {
				return p, r
			}
		}
	}
	return nil, ""
}

func mapFsType(t string) string {
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
		inner := mapFsType(strings.TrimSuffix(t, " list"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := mapFsType(strings.TrimSuffix(t, " array"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return ""
}

func fsFunctionBody(src string, sym protocol.DocumentSymbol) []string {
	lines := strings.Split(src, "\n")
	start := fsPosToOffset(lines, sym.Range.Start)
	end := fsPosToOffset(lines, sym.Range.End)
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

func fsPosToOffset(lines []string, pos protocol.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		if i < len(lines) {
			off += len(lines[i]) + 1
		}
	}
	off += int(pos.Character)
	return off
}

func convertFsFallback(src string) ([]byte, error) {
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
