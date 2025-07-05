package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertLua converts lua source code to Mochi using the language server.
func ConvertLua(src string) ([]byte, error) {
	ls := Servers["lua"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeLuaSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func writeLuaSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			writeLuaFunc(out, strings.Join(nameParts, "."), s, src, ls)
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			out.WriteString("val ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" = nil\n")
		}
		if len(s.Children) > 0 {
			writeLuaSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

type luaParam struct {
	name string
	typ  string
}

func writeLuaFunc(out *strings.Builder, name string, sym protocol.DocumentSymbol, src string, ls LanguageServer) {
	params, ret := luaHoverSignature(src, sym, ls)
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

func luaHoverSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) ([]luaParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	sig := hoverString(hov)
	return parseLuaSignature(sig)
}

func parseLuaSignature(sig string) ([]luaParam, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if i := strings.Index(sig, "function"); i != -1 {
		sig = strings.TrimSpace(sig[i+len("function"):])
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, mapLuaType(strings.TrimSpace(sig))
	}
	paramsPart := strings.TrimSpace(sig[open+1 : close])
	rest := strings.TrimSpace(sig[close+1:])
	ret := ""
	if strings.HasPrefix(rest, ":") {
		ret = strings.TrimSpace(rest[1:])
	} else if strings.HasPrefix(rest, "->") {
		ret = strings.TrimSpace(rest[2:])
	}
	var params []luaParam
	if paramsPart != "" {
		for _, p := range strings.Split(paramsPart, ",") {
			p = strings.TrimSpace(p)
			if p == "" {
				continue
			}
			name := p
			typ := ""
			if colon := strings.Index(p, ":"); colon != -1 {
				name = strings.TrimSpace(p[:colon])
				typ = mapLuaType(strings.TrimSpace(p[colon+1:]))
			}
			params = append(params, luaParam{name: name, typ: typ})
		}
	}
	return params, mapLuaType(ret)
}

func mapLuaType(t string) string {
	switch strings.TrimSpace(t) {
	case "number":
		return "float"
	case "integer":
		return "int"
	case "string":
		return "string"
	case "boolean":
		return "bool"
	default:
		return strings.TrimSpace(t)
	}
}

// ConvertLuaFile reads the lua file and converts it to Mochi.
func ConvertLuaFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertLua(string(data))
}
