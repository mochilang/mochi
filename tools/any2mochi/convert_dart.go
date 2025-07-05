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
	writeDartSymbols(&out, nil, syms)
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

func parseDartDetail(detail string) ([]string, string) {
	open := strings.Index(detail, "(")
	close := strings.LastIndex(detail, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := detail[open+1 : close]
	retPart := strings.TrimSpace(detail[close+1:])
	params := []string{}
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.FieldsFunc(p, func(r rune) bool { return unicode.IsSpace(r) || r == '=' })
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		params = append(params, name)
	}
	return params, retPart
}

func writeDartSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol) {
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
					if t := dartTypeFromDetail(c.Detail); t != "" && t != "dynamic" {
						out.WriteString(": ")
						out.WriteString(t)
					}
					out.WriteByte('\n')
				}
			}
			out.WriteString("}\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindMethod || c.Kind == protocol.SymbolKindConstructor || c.Kind == protocol.SymbolKindFunction {
					writeDartSymbols(out, nameParts, []protocol.DocumentSymbol{c})
				}
			}
			continue
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 && s.Name != "" {
				out.WriteString("let ")
				out.WriteString(s.Name)
				if t := dartTypeFromDetail(s.Detail); t != "" && t != "dynamic" {
					out.WriteString(": ")
					out.WriteString(t)
				}
				out.WriteByte('\n')
			}
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			params, ret := parseDartDetail(detail)
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteByte(')')
			if ret != "" && ret != "void" && ret != "dynamic" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		}

		if len(s.Children) > 0 {
			writeDartSymbols(out, nameParts, s.Children)
		}
	}
}

func dartTypeFromDetail(d *string) string {
	if d == nil {
		return ""
	}
	return strings.TrimSpace(*d)
}
