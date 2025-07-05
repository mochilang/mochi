package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertFs converts F# source code to Mochi using a simple Go translator.
// The F# code is first parsed via its language server to surface any
// diagnostics before translation.
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

type fsParam struct{ name, typ string }

func writeFsSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			params, ret := getFsSignature(src, s.SelectionRange.Start, ls)
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
				if p.typ != "" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "unit" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant, protocol.SymbolKindField:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ := getFsVarType(src, s.SelectionRange.Start, ls); typ != "" && typ != "unit" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		case protocol.SymbolKindClass, protocol.SymbolKindStruct, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
			if len(s.Children) > 0 {
				writeFsSymbols(out, nameParts, s.Children, src, ls)
			}
		default:
			if len(s.Children) > 0 {
				writeFsSymbols(out, nameParts, s.Children, src, ls)
			}
		}
	}
}

func getFsSignature(src string, pos protocol.Position, ls LanguageServer) ([]fsParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil, ""
	}
	return parseFsSignature(hoverString(hov))
}

func parseFsSignature(sig string) ([]fsParam, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if idx := strings.Index(sig, "val "); idx != -1 {
		sig = sig[idx+4:]
	}
	if idx := strings.Index(sig, ":"); idx != -1 {
		sig = strings.TrimSpace(sig[idx+1:])
	}
	parts := strings.Split(sig, "->")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	if len(parts) == 0 {
		return nil, ""
	}
	ret := mapFsType(parts[len(parts)-1])
	params := make([]fsParam, 0, len(parts)-1)
	for _, p := range parts[:len(parts)-1] {
		name := ""
		typ := p
		if colon := strings.Index(p, ":"); colon != -1 {
			name = strings.TrimSpace(p[:colon])
			typ = strings.TrimSpace(p[colon+1:])
		}
		params = append(params, fsParam{name: name, typ: mapFsType(typ)})
	}
	return params, ret
}

func getFsVarType(src string, pos protocol.Position, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return ""
	}
	t := hoverString(hov)
	if idx := strings.Index(t, ":"); idx != -1 {
		t = strings.TrimSpace(t[idx+1:])
	}
	if idx := strings.Index(t, "->"); idx != -1 {
		return ""
	}
	if i := strings.Index(t, "\n"); i != -1 {
		t = t[:i]
	}
	return mapFsType(strings.TrimSpace(t))
}

func mapFsType(t string) string {
	switch t {
	case "int":
		return "int"
	case "float", "double":
		return "float"
	case "string":
		return "string"
	case "bool":
		return "bool"
	case "unit":
		return "void"
	}
	if strings.HasSuffix(t, " list") {
		inner := strings.TrimSpace(t[:len(t)-5])
		return "list<" + mapFsType(inner) + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := strings.TrimSpace(t[:len(t)-6])
		return "list<" + mapFsType(inner) + ">"
	}
	if strings.HasPrefix(t, "'") {
		return "any"
	}
	if strings.HasPrefix(t, "(") && strings.HasSuffix(t, ")") {
		return mapFsType(strings.TrimSpace(t[1 : len(t)-1]))
	}
	return t
}
