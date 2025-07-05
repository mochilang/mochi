package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

type ftParam struct{ name, typ string }

// ConvertFortran converts Fortran source code to a minimal Mochi representation
// using the fortls language server.
func ConvertFortran(src string) ([]byte, error) {
	return convertFortran(src, "")
}

func convertFortran(src, root string) ([]byte, error) {
	ls := Servers["fortran"]
	syms, diags, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeFtSymbols(&out, nil, syms, src, ls, root)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFortranFile reads the Fortran file and converts it to Mochi.
func ConvertFortranFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return convertFortran(string(data), filepath.Dir(path))
}

func getFtSignatureWithRoot(src string, pos protocol.Position, ls LanguageServer, root string) ([]ftParam, string) {
	hov, err := EnsureAndHoverWithRoot(ls.Command, ls.Args, ls.LangID, src, pos, root)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return nil, ""
	}
	return parseFtSignature(mc.Value)
}

func parseFtSignature(sig string) ([]ftParam, string) {
	lines := strings.Split(sig, "\n")
	if len(lines) == 0 {
		return nil, ""
	}
	first := strings.TrimSpace(lines[0])
	lower := strings.ToLower(first)
	open := strings.Index(first, "(")
	close := strings.LastIndex(first, ")")
	paramsPart := ""
	if open != -1 && close != -1 && close > open {
		paramsPart = first[open+1 : close]
	}
	ret := ""
	if idx := strings.Index(lower, "function"); idx != -1 {
		typ := strings.TrimSpace(first[:idx])
		ret = mapFtType(typ)
	}
	var params []ftParam
	if paramsPart != "" {
		for _, p := range strings.Split(paramsPart, ",") {
			p = strings.TrimSpace(p)
			if p != "" {
				params = append(params, ftParam{name: p})
			}
		}
	}
	for _, line := range lines[1:] {
		line = strings.TrimSpace(line)
		idx := strings.Index(line, "::")
		if idx == -1 {
			continue
		}
		names := strings.Split(line[idx+2:], ",")
		typ := mapFtType(strings.TrimSpace(line[:idx]))
		for _, n := range names {
			n = strings.TrimSpace(n)
			for i := range params {
				if params[i].name == n {
					params[i].typ = typ
				}
			}
		}
	}
	return params, ret
}

func mapFtType(t string) string {
	t = strings.ToLower(t)
	switch {
	case strings.Contains(t, "real"):
		return "float"
	case strings.Contains(t, "integer"):
		return "int"
	case strings.Contains(t, "character"):
		return "string"
	case strings.Contains(t, "logical"):
		return "bool"
	default:
		return ""
	}
}

func writeFtSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer, root string) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindModule, protocol.SymbolKindFunction:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params, ret := getFtSignatureWithRoot(src, s.SelectionRange.Start, ls, root)
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
			if ret != "" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		}
		if len(s.Children) > 0 {
			writeFtSymbols(out, nameParts, s.Children, src, ls, root)
		}
	}
}
