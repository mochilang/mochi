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
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction && s.Kind != protocol.SymbolKindMethod {
			continue
		}
		names := extractHsParams(s)
		types, ret := getHsSignature(src, s, ls)
		params := combineHsParams(names, types)
		out.WriteString("fun ")
		out.WriteString(s.Name)
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
		out.WriteString(" {}\n")
	}
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
