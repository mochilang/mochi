package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertPas converts pas source code to Mochi using the language server.
func ConvertPas(src string) ([]byte, error) {
	ls := Servers["pas"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writePasSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertPasFile reads the pas file and converts it to Mochi.
func ConvertPasFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPas(string(data))
}

type pasParam struct {
	name string
	typ  string
}

func writePasSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			params, ret := pasHoverSignature(src, s, ls)
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
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		case protocol.SymbolKindStruct, protocol.SymbolKindClass, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []protocol.DocumentSymbol{}
			methods := []protocol.DocumentSymbol{}
			for _, c := range s.Children {
				switch c.Kind {
				case protocol.SymbolKindField, protocol.SymbolKindProperty:
					fields = append(fields, c)
				case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
					methods = append(methods, c)
				}
			}
			if len(fields) == 0 && len(methods) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					out.WriteString("  ")
					out.WriteString(f.Name)
					if typ := pasFieldType(src, f, ls); typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				for _, m := range methods {
					var b strings.Builder
					writePasSymbols(&b, []string{m.Name}, []protocol.DocumentSymbol{m}, src, ls)
					for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
						out.WriteString("  ")
						out.WriteString(line)
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
			}
		case protocol.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindEnumMember {
					out.WriteString("  ")
					out.WriteString(c.Name)
					out.WriteByte('\n')
				}
			}
			out.WriteString("}\n")
			var rest []protocol.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindEnumMember {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writePasSymbols(out, nameParts, rest, src, ls)
			}
			continue
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(s.Name)
				if typ := pasFieldType(src, s, ls); typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
		if len(s.Children) > 0 && s.Kind != protocol.SymbolKindStruct && s.Kind != protocol.SymbolKindClass && s.Kind != protocol.SymbolKindInterface {
			writePasSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func pasHoverSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) ([]pasParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		if sym.Detail != nil {
			return parsePasSignature(*sym.Detail)
		}
		return nil, ""
	}
	lines := strings.Split(hoverString(hov), "\n")
	for _, l := range lines {
		l = strings.TrimSpace(l)
		lower := strings.ToLower(l)
		if strings.HasPrefix(lower, "function") || strings.HasPrefix(lower, "procedure") {
			return parsePasSignature(l)
		}
	}
	if sym.Detail != nil {
		return parsePasSignature(*sym.Detail)
	}
	return nil, ""
}

func pasFieldType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	lines := strings.Split(hoverString(hov), "\n")
	for _, l := range lines {
		l = strings.TrimSpace(l)
		if idx := strings.Index(l, ":"); idx != -1 {
			t := strings.TrimSpace(l[idx+1:])
			if t != "" {
				return pasToMochiType(t)
			}
		}
	}
	return ""
}

func parsePasSignature(sig string) ([]pasParam, string) {
	sig = strings.TrimSpace(sig)
	sig = strings.TrimSuffix(sig, ";")
	lower := strings.ToLower(sig)
	if strings.HasPrefix(lower, "function") {
		sig = strings.TrimSpace(sig[len("function"):])
	} else if strings.HasPrefix(lower, "procedure") {
		sig = strings.TrimSpace(sig[len("procedure"):])
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	paramsPart := ""
	rest := ""
	if open != -1 && close != -1 && close > open {
		paramsPart = sig[open+1 : close]
		rest = strings.TrimSpace(sig[close+1:])
	}
	var params []pasParam
	for _, p := range strings.Split(paramsPart, ";") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		colon := strings.Index(p, ":")
		if colon == -1 {
			continue
		}
		names := strings.Split(p[:colon], ",")
		typ := pasToMochiType(strings.TrimSpace(p[colon+1:]))
		for _, n := range names {
			n = strings.TrimSpace(n)
			if n == "" {
				continue
			}
			params = append(params, pasParam{name: n, typ: typ})
		}
	}
	ret := ""
	if rest != "" {
		if strings.HasPrefix(rest, ":") {
			rest = strings.TrimSpace(rest[1:])
		}
		ret = pasToMochiType(strings.TrimSpace(rest))
	}
	return params, ret
}

func pasToMochiType(t string) string {
	t = strings.ToLower(strings.TrimSpace(t))
	switch t {
	case "", "void":
		return ""
	case "integer", "longint", "shortint", "byte", "smallint":
		return "int"
	case "double", "single", "real", "real64":
		return "float"
	case "boolean", "bool":
		return "bool"
	case "string", "ansistring", "widestring", "shortstring", "pchar", "char":
		return "string"
	}
	if strings.HasPrefix(t, "specialize tarray<") && strings.HasSuffix(t, ">") {
		inner := pasToMochiType(t[len("specialize tarray<") : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "array of ") {
		inner := pasToMochiType(strings.TrimPrefix(t, "array of "))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}
