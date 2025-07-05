package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertOcaml converts ocaml source code to Mochi using the language server.
func ConvertOcaml(src string) ([]byte, error) {
	ls := Servers["ocaml"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	lines := strings.Split(src, "\n")
	writeOcamlSymbols(&out, nil, syms, lines, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertOcamlFile reads the ocaml file and converts it to Mochi.
func ConvertOcamlFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertOcaml(string(data))
}

func writeOcamlSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, lines []string, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			names := parseOcamlParamNames(lines, s)
			types, ret := ocamlHoverSignature(src, s, ls)
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, n := range names {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(n)
				if i < len(types) && types[i] != "" {
					out.WriteString(": ")
					out.WriteString(types[i])
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "unit" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				typ := ocamlHoverType(src, s, ls)
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ != "" && typ != "unit" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		case protocol.SymbolKindStruct, protocol.SymbolKindClass, protocol.SymbolKindInterface, protocol.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []protocol.DocumentSymbol{}
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindField || c.Kind == protocol.SymbolKindProperty {
					fields = append(fields, c)
				}
			}
			if len(fields) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					out.WriteString("  ")
					out.WriteString(f.Name)
					if typ := ocamlHoverType(src, f, ls); typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case protocol.SymbolKindModule, protocol.SymbolKindNamespace:
			writeOcamlSymbols(out, nameParts, s.Children, lines, src, ls)
		}
		if len(s.Children) > 0 && s.Kind != protocol.SymbolKindStruct && s.Kind != protocol.SymbolKindClass && s.Kind != protocol.SymbolKindInterface && s.Kind != protocol.SymbolKindEnum && s.Kind != protocol.SymbolKindModule && s.Kind != protocol.SymbolKindNamespace && s.Kind != protocol.SymbolKindVariable && s.Kind != protocol.SymbolKindConstant {
			writeOcamlSymbols(out, nameParts, s.Children, lines, src, ls)
		}
	}
}

func parseOcamlParamNames(lines []string, sym protocol.DocumentSymbol) []string {
	start := posToOffset(lines, sym.SelectionRange.End)
	src := strings.Join(lines, "\n")
	rest := src[start:]
	eq := strings.Index(rest, "=")
	if eq == -1 {
		eq = len(rest)
	}
	header := strings.TrimSpace(rest[:eq])
	if idx := strings.LastIndex(header, ":"); idx != -1 {
		if idx > strings.LastIndex(header, ")") {
			header = strings.TrimSpace(header[:idx])
		}
	}
	fields := strings.Fields(header)
	var names []string
	for _, f := range fields {
		if strings.HasPrefix(f, "(") {
			f = strings.TrimPrefix(f, "(")
			if i := strings.Index(f, ")"); i != -1 {
				f = f[:i]
			}
			if i := strings.Index(f, ":"); i != -1 {
				f = f[:i]
			}
			f = strings.TrimSpace(f)
			if f != "" {
				names = append(names, f)
			}
		} else if strings.HasPrefix(f, ":") {
			break
		} else {
			if i := strings.Index(f, ":"); i != -1 {
				f = f[:i]
			}
			names = append(names, f)
		}
	}
	return names
}

func ocamlHoverSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) ([]string, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return nil, ""
	}
	sig := strings.TrimSpace(mc.Value)
	if strings.HasPrefix(sig, "val") {
		if idx := strings.Index(sig, ":"); idx != -1 {
			sig = strings.TrimSpace(sig[idx+1:])
		}
	}
	parts := strings.Split(sig, "->")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	if len(parts) == 0 {
		return nil, ""
	}
	ret := mapOcamlType(parts[len(parts)-1])
	paramTypes := make([]string, 0, len(parts)-1)
	for i := 0; i < len(parts)-1; i++ {
		paramTypes = append(paramTypes, mapOcamlType(parts[i]))
	}
	return paramTypes, ret
}

func ocamlHoverType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		return mapOcamlType(strings.TrimSpace(mc.Value))
	}
	return ""
}

func mapOcamlType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "unit":
		return ""
	case "int":
		return "int"
	case "float":
		return "float"
	case "string":
		return "string"
	case "bool":
		return "bool"
	}
	if strings.HasSuffix(t, " list") {
		inner := mapOcamlType(strings.TrimSpace(t[:len(t)-5]))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := mapOcamlType(strings.TrimSpace(t[:len(t)-6]))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " option") {
		return mapOcamlType(strings.TrimSpace(t[:len(t)-7]))
	}
	return t
}

func posToOffset(lines []string, pos protocol.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		off += len(lines[i]) + 1
	}
	off += int(pos.Character)
	return off
}
