package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertHs converts hs source code to Mochi using the language server.
func ConvertHs(src string) ([]byte, error) {
	ls := Servers["hs"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeHsSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertHsFile reads the hs file and converts it to Mochi.
func ConvertHsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertHs(string(data))
}

func getHsSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) ([]string, string) {
	if sym.Detail != nil {
		if params, ret := parseHsSigTypes(*sym.Detail); len(params) > 0 || ret != "" {
			return params, ret
		}
	}
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	return parseHsSigTypes(hoverString(hov))
}

func parseHsSigTypes(sig string) ([]string, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if i := strings.Index(sig, "::"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if i := strings.Index(sig, "=>"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if strings.HasPrefix(sig, "forall") {
		if i := strings.Index(sig, "."); i != -1 {
			sig = strings.TrimSpace(sig[i+1:])
		}
	}
	parts := strings.Split(sig, "->")
	for i, p := range parts {
		parts[i] = strings.TrimSpace(p)
	}
	if len(parts) == 0 {
		return nil, ""
	}
	ret := mapHsType(parts[len(parts)-1])
	params := make([]string, 0, len(parts)-1)
	for _, p := range parts[:len(parts)-1] {
		params = append(params, mapHsType(p))
	}
	return params, ret
}

func extractHsParams(sym protocol.DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable && c.Range.Start.Line == start {
			params = append(params, c.Name)
		}
	}
	return params
}

func combineHsParams(names, types []string) []string {
	params := make([]string, 0, len(names))
	for i, n := range names {
		t := ""
		if i < len(types) {
			t = types[i]
		}
		if t != "" {
			params = append(params, fmt.Sprintf("%s: %s", n, t))
		} else {
			params = append(params, n)
		}
	}
	return params
}

func mapHsType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "()":
		return ""
	case "Int", "Integer":
		return "int"
	case "Float", "Double":
		return "float"
	case "String", "[Char]":
		return "string"
	case "Bool":
		return "bool"
	}
	if strings.HasPrefix(t, "[") && strings.HasSuffix(t, "]") {
		inner := mapHsType(strings.TrimSuffix(strings.TrimPrefix(t, "["), "]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

func extractHsBody(src string, sym protocol.DocumentSymbol) string {
	lines := strings.Split(src, "\n")
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start >= len(lines) {
		return ""
	}
	if end >= len(lines) {
		end = len(lines) - 1
	}
	snippet := strings.Join(lines[start:end+1], "\n")
	if i := strings.Index(snippet, "="); i != -1 {
		body := strings.TrimSpace(snippet[i+1:])
		if j := strings.Index(body, "\n"); j != -1 {
			body = strings.TrimSpace(body[:j])
		}
		return body
	}
	return ""
}

func writeHsSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindNamespace, protocol.SymbolKindModule, protocol.SymbolKindPackage:
			writeHsSymbols(out, nameParts, s.Children, src, ls)
			continue
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			names := extractHsParams(s)
			types, ret := getHsSignature(src, s, ls)
			params := combineHsParams(names, types)
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteByte(')')
			if ret != "" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := extractHsBody(src, s)
			if body == "" {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" { ")
				out.WriteString(body)
				out.WriteString(" }\n")
			}
		case protocol.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindEnumMember || (c.Kind == protocol.SymbolKindEnum && len(c.Children) == 0) {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []protocol.DocumentSymbol
			for _, c := range s.Children {
				if !(c.Kind == protocol.SymbolKindEnumMember || (c.Kind == protocol.SymbolKindEnum && len(c.Children) == 0)) {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeHsSymbols(out, nameParts, rest, src, ls)
			}
			continue
		case protocol.SymbolKindStruct, protocol.SymbolKindClass, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			if len(s.Children) == 0 {
				out.WriteString(" {}\n")
				break
			}
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				typ := hsFieldType(src, c, ls)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				typ := getHsVarType(src, s, ls)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
		if len(s.Children) > 0 && s.Kind != protocol.SymbolKindStruct && s.Kind != protocol.SymbolKindClass && s.Kind != protocol.SymbolKindInterface {
			writeHsSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func getHsVarType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	if sym.Detail != nil {
		if _, ret := parseHsSigTypes(*sym.Detail); ret != "" {
			return ret
		}
	}
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	typ, _ := parseHsVarSig(hoverString(hov))
	return typ
}

func hsFieldType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	if sym.Detail != nil && strings.TrimSpace(*sym.Detail) != "" {
		if t, _ := parseHsVarSig(*sym.Detail); t != "" {
			return t
		}
	}
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	t, _ := parseHsVarSig(hoverString(hov))
	return t
}

func parseHsVarSig(sig string) (string, bool) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if i := strings.Index(sig, "::"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if i := strings.Index(sig, "=>"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if strings.HasPrefix(sig, "forall") {
		if i := strings.Index(sig, "."); i != -1 {
			sig = strings.TrimSpace(sig[i+1:])
		}
	}
	sig = strings.TrimSpace(sig)
	if sig == "" {
		return "", false
	}
	return mapHsType(sig), true
}
