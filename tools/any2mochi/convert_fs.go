package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertFs converts F# source code to Mochi using the fsautocomplete language
// server. The converter relies solely on the information returned by the
// language server and avoids any regex based parsing.
func ConvertFs(src string) ([]byte, error) {
	ls := Servers["fs"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
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
			start := indexForPositionFs(src, s.Range.Start)
			end := indexForPositionFs(src, s.Range.End)
			body := ""
			if start < end && end <= len(src) {
				snippet := src[start:end]
				if idx := strings.Index(snippet, "try"); idx != -1 {
					snippet = snippet[idx+3:]
				}
				if close := strings.LastIndex(snippet, "with"); close != -1 {
					snippet = snippet[:close]
				}
				body = snippet
			}
			stmts := fsFunctionBody(body)
			if len(stmts) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, ln := range stmts {
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

var (
	fsForRangeRE = regexp.MustCompile(`^for\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.+)\s+to\s+(.+)\s+do$`)
	fsForInRE    = regexp.MustCompile(`^for\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+in\s+(.+)\s+do$`)
	fsWhileRE    = regexp.MustCompile(`^while\s+(.+)\s+do$`)
	fsIfRE       = regexp.MustCompile(`^if\s+(.+)\s+then$`)
	fsLetMutRE   = regexp.MustCompile(`^let\s+mutable\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.+)$`)
	fsAssignRE   = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\s*<-\s*(.+)$`)
	fsPrintRE    = regexp.MustCompile(`^ignore \(printfn \\"%A\\" \((.+)\)\)$`)
	fsReturnRE   = regexp.MustCompile(`raise \(Return_[^(]+\((.+)\)\)`)
)

func fsFunctionBody(src string) []string {
	lines := strings.Split(src, "\n")
	var out []string
	var stack []int
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}
		indent := len(line) - len(strings.TrimLeft(line, " \t"))
		trimmed := strings.TrimSpace(line)
		for len(stack) > 0 && indent < stack[len(stack)-1] {
			stack = stack[:len(stack)-1]
			out = append(out, "}")
		}
		switch {
		case fsForRangeRE.MatchString(trimmed):
			m := fsForRangeRE.FindStringSubmatch(trimmed)
			out = append(out, fmt.Sprintf("for %s in %s..%s {", m[1], strings.TrimSpace(m[2]), strings.TrimSpace(m[3])))
			stack = append(stack, indent+4)
		case fsForInRE.MatchString(trimmed):
			m := fsForInRE.FindStringSubmatch(trimmed)
			out = append(out, fmt.Sprintf("for %s in %s {", m[1], strings.TrimSpace(m[2])))
			stack = append(stack, indent+4)
		case fsWhileRE.MatchString(trimmed):
			m := fsWhileRE.FindStringSubmatch(trimmed)
			out = append(out, fmt.Sprintf("while %s {", strings.TrimSpace(m[1])))
			stack = append(stack, indent+4)
		case fsIfRE.MatchString(trimmed):
			m := fsIfRE.FindStringSubmatch(trimmed)
			out = append(out, fmt.Sprintf("if %s {", strings.TrimSpace(m[1])))
			stack = append(stack, indent+4)
		case trimmed == "else":
			if len(out) > 0 {
				out[len(out)-1] = strings.TrimSuffix(out[len(out)-1], "{") + "}"
			}
			out = append(out, "else {")
			stack = append(stack, indent+4)
		case fsPrintRE.MatchString(trimmed):
			m := fsPrintRE.FindStringSubmatch(trimmed)
			out = append(out, fmt.Sprintf("print(%s)", strings.TrimSpace(m[1])))
		case fsLetMutRE.MatchString(trimmed):
			m := fsLetMutRE.FindStringSubmatch(trimmed)
			out = append(out, fmt.Sprintf("let %s = %s", m[1], strings.TrimSpace(m[2])))
		case fsAssignRE.MatchString(trimmed):
			m := fsAssignRE.FindStringSubmatch(trimmed)
			out = append(out, fmt.Sprintf("%s = %s", m[1], strings.TrimSpace(m[2])))
		case fsReturnRE.MatchString(trimmed):
			m := fsReturnRE.FindStringSubmatch(trimmed)
			out = append(out, fmt.Sprintf("return %s", strings.TrimSpace(m[1])))
		default:
			// ignore other lines
		}
	}
	for len(stack) > 0 {
		stack = stack[:len(stack)-1]
		out = append(out, "}")
	}
	return out
}

func indexForPositionFs(src string, pos protocol.Position) int {
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
