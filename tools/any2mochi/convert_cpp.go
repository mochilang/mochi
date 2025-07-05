package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

type cppParam struct {
	name string
	typ  string
}

// ConvertCpp converts cpp source code to Mochi using the language server.
func ConvertCpp(src string) ([]byte, error) {
	ls := Servers["cpp"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeCppSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertCppFile reads the cpp file and converts it to Mochi.
func ConvertCppFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCpp(string(data))
}

func writeCppSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindNamespace, protocol.SymbolKindModule, protocol.SymbolKindPackage:
			writeCppSymbols(out, nameParts, s.Children, src, ls)
		case protocol.SymbolKindClass, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				t := ""
				if c.Detail != nil {
					t = mapCppType(*c.Detail)
				}
				if t == "" {
					t = cppFieldType(src, c, ls)
				}
				if t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
			var childSyms []protocol.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindField {
					childSyms = append(childSyms, c)
				}
			}
			if len(childSyms) > 0 {
				writeCppSymbols(out, nameParts, childSyms, src, ls)
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
				writeCppSymbols(out, nameParts, rest, src, ls)
			}
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			signature := ""
			if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
				if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
					lines := strings.Split(mc.Value, "\n")
					for i := len(lines) - 1; i >= 0 && signature == ""; i-- {
						l := strings.TrimSpace(lines[i])
						if strings.Contains(l, "(") && strings.Contains(l, ")") {
							signature = l
						}
					}
				}
			}
			var params []cppParam
			var ret string
			if signature != "" {
				params, ret = parseCppSignature(signature)
			} else {
				names, r := parseCppDetail(s.Detail)
				ret = r
				for _, n := range names {
					params = append(params, cppParam{name: n})
				}
			}
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
			ret = mapCppType(ret)
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := convertCppBody(src, s.Range)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, line := range body {
					out.WriteString("  ")
					out.WriteString(line)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if strings.HasPrefix(s.Name, "using ") || strings.Contains(s.Name, " ") {
				continue
			}
			out.WriteString("let ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('\n')
		}
	}
}

func parseCppDetail(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	if d == "" {
		return nil, ""
	}
	open := strings.Index(d, "(")
	close := strings.LastIndex(d, ")")
	if open == -1 || close == -1 || close < open {
		return nil, strings.TrimSpace(d)
	}
	ret := strings.TrimSpace(d[:open])
	paramsPart := d[open+1 : close]
	params := []string{}
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.Fields(p)
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		if eq := strings.Index(name, "="); eq != -1 {
			name = name[:eq]
		}
		name = strings.Trim(name, "*&")
		params = append(params, name)
	}
	return params, ret
}

func parseCppSignature(sig string) ([]cppParam, string) {
	sig = strings.TrimSpace(sig)
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, mapCppType(sig)
	}
	header := strings.TrimSpace(sig[:open])
	paramsPart := sig[open+1 : close]
	parts := strings.Fields(header)
	ret := ""
	if len(parts) > 1 {
		ret = mapCppType(strings.Join(parts[:len(parts)-1], " "))
	}
	paramsSplit := strings.Split(paramsPart, ",")
	params := make([]cppParam, 0, len(paramsSplit))
	for _, ps := range paramsSplit {
		ps = strings.TrimSpace(ps)
		if ps == "" || ps == "void" {
			continue
		}
		f := strings.Fields(ps)
		name := f[len(f)-1]
		typ := ""
		if len(f) > 1 {
			typ = mapCppType(strings.Join(f[:len(f)-1], " "))
		}
		if eq := strings.Index(name, "="); eq != -1 {
			name = name[:eq]
		}
		name = strings.Trim(name, "*&")
		params = append(params, cppParam{name: name, typ: typ})
	}
	return params, ret
}

func mapCppType(typ string) string {
	typ = strings.TrimSpace(typ)
	for strings.HasSuffix(typ, "*") || strings.HasSuffix(typ, "&") {
		typ = strings.TrimSpace(typ[:len(typ)-1])
	}
	typ = strings.TrimPrefix(typ, "const ")
	typ = strings.TrimPrefix(typ, "unsigned ")
	switch typ {
	case "", "void":
		return ""
	case "int", "size_t", "long", "short":
		return "int"
	case "float", "double":
		return "float"
	case "bool":
		return "bool"
	case "char", "char16_t", "char32_t", "std::string", "string":
		return "string"
	}
	if strings.HasPrefix(typ, "std::vector<") && strings.HasSuffix(typ, ">") {
		inner := mapCppType(typ[len("std::vector<") : len(typ)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return typ
}

// cppFieldType attempts to determine the type of a field using hover information
// from the language server when the document symbol does not include it.
func cppFieldType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			if idx := strings.Index(line, ":"); idx != -1 {
				t := strings.TrimSpace(line[idx+1:])
				if t != "" {
					return mapCppType(t)
				}
			}
		}
	}
	return ""
}

// convertCppBody converts the body of a function defined by r in src into a slice
// of Mochi statements. Only very basic constructs like prints, returns and
// simple assignments are handled.
func convertCppBody(src string, r protocol.Range) []string {
	lines := strings.Split(src, "\n")
	start := int(r.Start.Line)
	end := int(r.End.Line)
	if start >= len(lines) || end >= len(lines) {
		return nil
	}
	bodyLines := lines[start : end+1]
	if len(bodyLines) > 0 {
		bodyLines = bodyLines[1:]
	}
	if len(bodyLines) > 0 {
		bodyLines = bodyLines[:len(bodyLines)-1]
	}
	var out []string
	for _, l := range bodyLines {
		l = strings.TrimSpace(l)
		l = strings.TrimSuffix(l, ";")
		if l == "" {
			continue
		}
		switch {
		case strings.HasPrefix(l, "return"):
			out = append(out, l)
		case strings.Contains(l, "std::cout") || strings.HasPrefix(l, "cout <<"):
			l = strings.TrimPrefix(l, "std::cout <<")
			l = strings.TrimPrefix(l, "cout <<")
			l = strings.TrimSuffix(l, "<< std::endl")
			l = strings.TrimSuffix(l, "<< endl")
			l = strings.TrimSpace(l)
			out = append(out, "print("+l+")")
		default:
			for _, pre := range []string{"int ", "float ", "double ", "bool ", "std::string ", "string ", "auto "} {
				if strings.HasPrefix(l, pre) {
					l = strings.TrimPrefix(l, pre)
					break
				}
			}
			out = append(out, l)
		}
	}
	return out
}
