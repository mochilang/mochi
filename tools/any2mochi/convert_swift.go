package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertSwift converts swift source code to Mochi using the language server.
func ConvertSwift(src string) ([]byte, error) {
	ls := Servers["swift"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeSwiftSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func writeSwiftSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params := getSwiftParams(src, s.SelectionRange.Start, ls)
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteString(") {}\n")
		case protocol.SymbolKindClass, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				out.WriteByte('\n')
			}
		}
		if len(s.Children) > 0 {
			writeSwiftSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func getSwiftParams(src string, pos protocol.Position, ls LanguageServer) []string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return nil
	}
	params, _ := parseSwiftSignature(mc.Value)
	return params
}

func parseSwiftSignature(sig string) ([]string, string) {
	sig = strings.TrimSpace(sig)
	if strings.HasPrefix(sig, "```swift") {
		sig = strings.TrimPrefix(sig, "```swift")
		if i := strings.LastIndex(sig, "```"); i != -1 {
			sig = sig[:i]
		}
	}
	sig = strings.TrimSpace(sig)
	if strings.HasPrefix(sig, "func ") {
		sig = sig[5:]
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := sig[open+1 : close]
	rest := strings.TrimSpace(sig[close+1:])
	ret := ""
	if strings.HasPrefix(rest, "->") {
		ret = strings.TrimSpace(rest[2:])
	}
	var params []string
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		words := strings.Fields(p)
		if len(words) == 0 {
			continue
		}
		name := words[0]
		if strings.HasPrefix(name, "_") && len(words) > 1 {
			name = words[1]
		} else if len(words) > 1 && strings.HasSuffix(words[1], ":") {
			name = strings.TrimSuffix(words[1], ":")
		}
		if strings.HasSuffix(name, ":") {
			name = strings.TrimSuffix(name, ":")
		}
		if name != "" {
			params = append(params, name)
		}
	}
	return params, ret
}

// ConvertSwiftFile reads the swift file and converts it to Mochi.
func ConvertSwiftFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSwift(string(data))
}
