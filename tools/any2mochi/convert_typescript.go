package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertTypeScript converts TypeScript source code to a minimal Mochi representation using the language server.
func ConvertTypeScript(src string) ([]byte, error) {
	ls := Servers["typescript"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeTSSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertTypeScriptFile reads the TS file and converts it to Mochi.
func ConvertTypeScriptFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertTypeScript(string(data))
}

// ConvertTypeScriptWithJSON converts the source and also returns the parsed
// symbols encoded as JSON.
func ConvertTypeScriptWithJSON(src string) ([]byte, []byte, error) {
	ls := Servers["typescript"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, nil, err
	}
	if len(diags) > 0 {
		return nil, nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeTSSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	js, _ := json.MarshalIndent(syms, "", "  ")
	return []byte(out.String()), js, nil
}

// ConvertTypeScriptFileWithJSON reads the TS file and converts it to Mochi
// while also returning the parsed symbols as JSON.
func ConvertTypeScriptFileWithJSON(path string) ([]byte, []byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, nil, err
	}
	return ConvertTypeScriptWithJSON(string(data))
}

func writeTSSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindClass, protocol.SymbolKindInterface, protocol.SymbolKindStruct:
			writeTSClass(out, nameParts, s, src, ls)
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant, protocol.SymbolKindField, protocol.SymbolKindProperty:
			if s.Name != "" && len(prefix) == 0 {
				typ := tsFieldType(src, s, ls)
				out.WriteString("let ")
				out.WriteString(s.Name)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			writeTSFunc(out, strings.Join(nameParts, "."), s, src, ls)
		}
		if len(s.Children) > 0 && s.Kind != protocol.SymbolKindClass && s.Kind != protocol.SymbolKindInterface && s.Kind != protocol.SymbolKindStruct {
			writeTSSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func writeTSClass(out *strings.Builder, nameParts []string, sym protocol.DocumentSymbol, src string, ls LanguageServer) {
	out.WriteString("type ")
	out.WriteString(strings.Join(nameParts, "."))
	fields := []protocol.DocumentSymbol{}
	methods := []protocol.DocumentSymbol{}
	for _, c := range sym.Children {
		switch c.Kind {
		case protocol.SymbolKindField, protocol.SymbolKindProperty:
			fields = append(fields, c)
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			methods = append(methods, c)
		}
	}
	if len(fields) == 0 && len(methods) == 0 {
		out.WriteString(" {}\n")
		return
	}
	out.WriteString(" {\n")
	for _, f := range fields {
		out.WriteString("  ")
		out.WriteString(f.Name)
		if typ := tsFieldType(src, f, ls); typ != "" {
			out.WriteString(": ")
			out.WriteString(typ)
		}
		out.WriteByte('\n')
	}
	for _, m := range methods {
		var b strings.Builder
		writeTSFunc(&b, m.Name, m, src, ls)
		for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
			out.WriteString("  ")
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	out.WriteString("}\n")
}

func writeTSFunc(out *strings.Builder, name string, sym protocol.DocumentSymbol, src string, ls LanguageServer) {
	params, ret := tsHoverSignature(src, sym, ls)
	out.WriteString("fun ")
	out.WriteString(name)
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
	if ret != "" && ret != "void" {
		out.WriteString(": ")
		out.WriteString(ret)
	}
	out.WriteString(" {}\n")
}

func tsHoverSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) ([]tsParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		lines := strings.Split(mc.Value, "\n")
		for i := len(lines) - 1; i >= 0; i-- {
			l := strings.TrimSpace(lines[i])
			if strings.Contains(l, "(") && strings.Contains(l, ")") {
				return parseTSSignature(l)
			}
		}
	}
	return nil, ""
}

func tsFieldType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		lines := strings.Split(mc.Value, "\n")
		for _, l := range lines {
			l = strings.TrimSpace(l)
			if idx := strings.Index(l, ":"); idx != -1 {
				typ := strings.TrimSpace(l[idx+1:])
				if typ != "" {
					return tsToMochiType(typ)
				}
			}
		}
	}
	return ""
}

type tsParam struct {
	name string
	typ  string
}

func parseTSSignature(sig string) ([]tsParam, string) {
	sig = strings.TrimSpace(sig)
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, tsToMochiType(strings.TrimSpace(sig))
	}
	paramsPart := sig[open+1 : close]
	var params []tsParam
	for _, p := range splitTSParams(paramsPart) {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		name := p
		typ := ""
		if colon := strings.Index(p, ":"); colon != -1 {
			name = strings.TrimSpace(p[:colon])
			typ = strings.TrimSpace(p[colon+1:])
		}
		params = append(params, tsParam{name: name, typ: tsToMochiType(typ)})
	}
	rest := strings.TrimSpace(sig[close+1:])
	if strings.HasPrefix(rest, ":") {
		rest = strings.TrimSpace(rest[1:])
	} else if strings.HasPrefix(rest, "=>") {
		rest = strings.TrimSpace(rest[2:])
	}
	return params, tsToMochiType(rest)
}

func splitTSParams(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<', '(', '[':
			depth++
		case '>', ')', ']':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, s[start:i])
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, s[start:])
	}
	return parts
}

func tsToMochiType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "any", "unknown", "object":
		return ""
	case "number":
		return "int"
	case "string":
		return "string"
	case "boolean":
		return "bool"
	case "void", "undefined", "null":
		return ""
	}
	if strings.HasSuffix(t, "[]") {
		inner := tsToMochiType(t[:len(t)-2])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Array<") && strings.HasSuffix(t, ">") {
		inner := tsToMochiType(t[len("Array<") : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}
