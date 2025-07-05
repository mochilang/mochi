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
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindFunction:
			names := parseOcamlParamNames(lines, s)
			types, ret := ocamlHoverSignature(src, s, ls)
			out.WriteString("fun ")
			out.WriteString(s.Name)
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
			typ := ocamlHoverType(src, s, ls)
			out.WriteString("let ")
			out.WriteString(s.Name)
			if typ != "" && typ != "unit" {
				out.WriteString(": ")
				out.WriteString(typ)
			}
			out.WriteByte('\n')
		}
	}
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
	switch strings.TrimSpace(t) {
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
	default:
		return t
	}
}

func posToOffset(lines []string, pos protocol.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		off += len(lines[i]) + 1
	}
	off += int(pos.Character)
	return off
}
