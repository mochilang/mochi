package any2mochi

import (
	"fmt"
	"os"
	"strings"
	"unicode"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertDart converts dart source code to Mochi.
func ConvertDart(src string) ([]byte, error) {
	ls := Servers["dart"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	writeDartSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertDartFile reads the dart file and converts it to Mochi.
func ConvertDartFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertDart(string(data))
}

type dartParam struct {
	name string
	typ  string
}

func parseDartDetail(detail string) ([]dartParam, string) {
	open := strings.Index(detail, "(")
	close := strings.LastIndex(detail, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := detail[open+1 : close]
	retPart := strings.TrimSpace(detail[close+1:])
	if i := strings.Index(retPart, "→"); i != -1 {
		retPart = strings.TrimSpace(retPart[i+len("→"):])
	} else if i := strings.Index(retPart, "->"); i != -1 {
		retPart = strings.TrimSpace(retPart[i+2:])
	}
	var params []dartParam
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(strings.Trim(p, "{}[]"))
		if p == "" {
			continue
		}
		p = strings.TrimPrefix(p, "required ")
		if eq := strings.Index(p, "="); eq != -1 {
			p = p[:eq]
		}
		fields := strings.FieldsFunc(p, func(r rune) bool { return unicode.IsSpace(r) || r == ':' })
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		if strings.HasPrefix(name, "this.") {
			name = strings.TrimPrefix(name, "this.")
		}
		typ := ""
		if len(fields) > 1 {
			typ = dartToMochiType(strings.Join(fields[:len(fields)-1], " "))
		}
		params = append(params, dartParam{name: name, typ: typ})
	}
	return params, dartToMochiType(retPart)
}

func parseDartHover(h protocol.Hover) ([]dartParam, string) {
	text := hoverString(h)
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, "(") && strings.Contains(line, ")") {
			if p, r := parseDartDetail(line); len(p) > 0 || r != "" {
				return p, r
			}
		}
	}
	return nil, ""
}

func writeDartSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}

		switch s.Kind {
		case protocol.SymbolKindClass, protocol.SymbolKindInterface, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindField || c.Kind == protocol.SymbolKindProperty || c.Kind == protocol.SymbolKindVariable {
					out.WriteString("  ")
					out.WriteString(c.Name)
					if t := dartFieldType(src, c, ls); t != "" {
						out.WriteString(": ")
						out.WriteString(t)
					}
					out.WriteByte('\n')
				}
			}
			out.WriteString("}\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindMethod || c.Kind == protocol.SymbolKindConstructor || c.Kind == protocol.SymbolKindFunction {
					writeDartSymbols(out, nameParts, []protocol.DocumentSymbol{c}, src, ls)
				}
			}
			continue
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 && s.Name != "" {
				out.WriteString("let ")
				out.WriteString(s.Name)
				if t := dartFieldType(src, s, ls); t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
				out.WriteByte('\n')
			}
		case protocol.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindEnumMember {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []protocol.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindEnumMember {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeDartSymbols(out, nameParts, rest, src, ls)
			}
			continue
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			params, ret := parseDartDetail(detail)
			if len(params) == 0 && ret == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					if p, r := parseDartHover(hov); len(p) > 0 || r != "" {
						params, ret = p, r
					}
				}
			}
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
				if p.typ != "" && p.typ != "any" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "any" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		}

		if len(s.Children) > 0 {
			writeDartSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func dartTypeFromDetail(d *string) string {
	if d == nil {
		return ""
	}
	return dartToMochiType(strings.TrimSpace(*d))
}

func dartFieldType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	if t := dartTypeFromDetail(sym.Detail); t != "" && t != "any" {
		return t
	}
	return dartTypeFromHover(src, sym, ls)
}

func dartTypeFromHover(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	text := hoverString(hov)
	for _, line := range strings.Split(text, "\n") {
		l := strings.TrimSpace(line)
		if idx := strings.Index(l, ":"); idx != -1 {
			t := strings.TrimSpace(l[idx+1:])
			if t != "" {
				return dartToMochiType(t)
			}
		}
		if idx := strings.Index(l, "→"); idx != -1 {
			t := strings.TrimSpace(l[idx+len("→"):])
			if t != "" {
				return dartToMochiType(t)
			}
		} else if idx := strings.Index(l, "->"); idx != -1 {
			t := strings.TrimSpace(l[idx+2:])
			if t != "" {
				return dartToMochiType(t)
			}
		}
	}
	return ""
}

func splitDartArgs(s string) []string {
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

func dartToMochiType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	switch t {
	case "", "dynamic", "Object":
		return "any"
	case "int":
		return "int"
	case "double", "num":
		return "float"
	case "bool":
		return "bool"
	case "String":
		return "string"
	case "void":
		return ""
	}
	if strings.HasSuffix(t, ">") {
		if open := strings.Index(t, "<"); open != -1 {
			outer := strings.TrimSpace(t[:open])
			inner := strings.TrimSuffix(t[open+1:], ">")
			args := splitDartArgs(inner)
			switch outer {
			case "List", "Iterable", "Set":
				a := "any"
				if len(args) > 0 {
					if at := dartToMochiType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Map":
				if len(args) == 2 {
					k := dartToMochiType(args[0])
					if k == "" {
						k = "any"
					}
					v := dartToMochiType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Future":
				if len(args) == 1 {
					return dartToMochiType(args[0])
				}
			}
		}
	}
	return t
}
