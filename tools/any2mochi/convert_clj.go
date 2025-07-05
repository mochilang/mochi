package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertClj converts Clojure source code to Mochi using the language server.
func ConvertClj(src string) ([]byte, error) {
	ls := Servers["clj"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeCljSymbols(&out, nil, syms)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func writeCljSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params := parseCljParams(s.Detail)
			out.WriteByte('(')
			if len(params) > 0 {
				out.WriteString(strings.Join(params, ", "))
			}
			out.WriteString(") {}\n")
		case protocol.SymbolKindClass, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable:
			out.WriteString("var ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString("\n")
		}
		if len(s.Children) > 0 {
			writeCljSymbols(out, nameParts, s.Children)
		}
	}
}

func parseCljParams(detail *string) []string {
	if detail == nil {
		return nil
	}
	d := *detail
	start := strings.Index(d, "[")
	end := strings.Index(d, "]")
	if start == -1 || end == -1 || end <= start {
		return nil
	}
	list := strings.TrimSpace(d[start+1 : end])
	if list == "" {
		return nil
	}
	fields := strings.Fields(list)
	params := make([]string, 0, len(fields))
	for _, f := range fields {
		if strings.HasPrefix(f, "&") {
			f = strings.TrimPrefix(f, "&")
		}
		params = append(params, f)
	}
	return params
}

// ConvertCljFile reads the Clojure file and converts it to Mochi.
func ConvertCljFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertClj(string(data))
}
