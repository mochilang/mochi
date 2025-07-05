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
			params, ret := getSwiftSignature(src, s.SelectionRange.Start, ls)
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
				if mt := swiftTypeToMochi(p.typ); mt != "" {
					out.WriteString(": ")
					out.WriteString(mt)
				}
			}
			out.WriteByte(')')
			if mt := swiftTypeToMochi(ret); mt != "" {
				out.WriteString(": ")
				out.WriteString(mt)
			}
			body := swiftFunctionBody(src, s)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, ln := range body {
					out.WriteString("  ")
					out.WriteString(ln)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case protocol.SymbolKindClass, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []protocol.DocumentSymbol{}
			rest := []protocol.DocumentSymbol{}
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindProperty || c.Kind == protocol.SymbolKindField {
					fields = append(fields, c)
				} else {
					rest = append(rest, c)
				}
			}
			if len(fields) == 0 && len(rest) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					out.WriteString("  ")
					typ := getSwiftVarType(src, f.SelectionRange.Start, ls)
					out.WriteString(f.Name)
					if mt := swiftTypeToMochi(typ); mt != "" {
						out.WriteString(": ")
						out.WriteString(mt)
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
				if len(rest) > 0 {
					writeSwiftSymbols(out, nameParts, rest, src, ls)
				}
			}
		case protocol.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindEnumMember {
					fmt.Fprintf(out, "  %s\n", c.Name)
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
				writeSwiftSymbols(out, nameParts, rest, src, ls)
			}
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				typ := getSwiftVarType(src, s.SelectionRange.Start, ls)
				if mt := swiftTypeToMochi(typ); mt != "" {
					out.WriteString(": ")
					out.WriteString(mt)
				}
				out.WriteByte('\n')
			}
		}
		if len(s.Children) > 0 {
			writeSwiftSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func getSwiftParams(src string, pos protocol.Position, ls LanguageServer) []string {
	params, _ := getSwiftSignature(src, pos, ls)
	names := make([]string, len(params))
	for i, p := range params {
		names[i] = p.name
	}
	return names
}

func getSwiftSignature(src string, pos protocol.Position, ls LanguageServer) ([]swiftParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return nil, ""
	}
	return parseSwiftSignature(mc.Value)
}

func getSwiftVarType(src string, pos protocol.Position, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return ""
	}
	_, typ := parseSwiftVarDecl(mc.Value)
	return typ
}

func parseSwiftVarDecl(sig string) (string, string) {
	sig = strings.TrimSpace(sig)
	if strings.HasPrefix(sig, "```swift") {
		sig = strings.TrimPrefix(sig, "```swift")
		if i := strings.LastIndex(sig, "```"); i != -1 {
			sig = sig[:i]
		}
	}
	sig = strings.TrimSpace(sig)
	if strings.HasPrefix(sig, "var ") {
		sig = sig[4:]
	} else if strings.HasPrefix(sig, "let ") {
		sig = sig[4:]
	}
	parts := strings.SplitN(sig, ":", 2)
	if len(parts) != 2 {
		return "", ""
	}
	name := strings.TrimSpace(parts[0])
	typ := strings.TrimSpace(parts[1])
	return name, typ
}

func swiftTypeToMochi(t string) string {
	switch strings.TrimSpace(t) {
	case "Int", "Int32", "Int64":
		return "int"
	case "Double", "Float":
		return "float"
	case "Bool":
		return "bool"
	case "String":
		return "string"
	default:
		return ""
	}
}

type swiftParam struct {
	name string
	typ  string
}

func parseSwiftSignature(sig string) ([]swiftParam, string) {
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
	var params []swiftParam
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		colon := strings.LastIndex(p, ":")
		var name, typ string
		if colon >= 0 {
			namePart := strings.TrimSpace(p[:colon])
			typ = strings.TrimSpace(p[colon+1:])
			fields := strings.Fields(namePart)
			if len(fields) > 1 {
				if fields[0] == "_" {
					name = fields[1]
				} else {
					name = fields[len(fields)-1]
				}
			} else if len(fields) == 1 {
				name = strings.TrimSuffix(fields[0], ":")
			}
		} else {
			name = strings.TrimSpace(p)
		}
		name = strings.TrimSuffix(name, ":")
		if name != "" {
			params = append(params, swiftParam{name: name, typ: typ})
		}
	}
	return params, ret
}

func swiftFunctionBody(src string, sym protocol.DocumentSymbol) []string {
	code := extractRangeText(src, sym.Range)
	start := strings.Index(code, "{")
	end := strings.LastIndex(code, "}")
	if start == -1 || end == -1 || end <= start {
		return nil
	}
	body := code[start+1 : end]
	return parseSwiftStatements(body)
}

func parseSwiftStatements(body string) []string {
	lines := strings.Split(body, "\n")
	var out []string
	indent := 1
	for _, line := range lines {
		l := strings.TrimSpace(line)
		if l == "" {
			continue
		}
		l = strings.TrimSuffix(l, ";")
		switch {
		case l == "}":
			if indent > 0 {
				indent--
			}
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(l, "if ") && strings.HasSuffix(l, "{"):
			cond := strings.TrimSpace(strings.TrimSuffix(l[3:], "{"))
			out = append(out, strings.Repeat("  ", indent)+"if "+cond+" {")
			indent++
		case l == "else {":
			if indent > 0 {
				indent--
			}
			out = append(out, strings.Repeat("  ", indent)+"else {")
			indent++
		case strings.HasPrefix(l, "for ") && strings.HasSuffix(l, "{"):
			head := strings.TrimSpace(strings.TrimSuffix(l[4:], "{"))
			out = append(out, strings.Repeat("  ", indent)+"for "+head+" {")
			indent++
		case strings.HasPrefix(l, "return "):
			expr := strings.TrimSpace(l[len("return "):])
			out = append(out, strings.Repeat("  ", indent)+"return "+expr)
		case strings.HasPrefix(l, "let ") || strings.HasPrefix(l, "var "):
			decl := strings.TrimPrefix(l, "let ")
			if decl == l {
				decl = strings.TrimPrefix(l, "var ")
			}
			parts := strings.SplitN(decl, "=", 2)
			if len(parts) == 2 {
				name := strings.TrimSpace(parts[0])
				if colon := strings.Index(name, ":"); colon != -1 {
					name = strings.TrimSpace(name[:colon])
				}
				expr := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+"let "+name+" = "+expr)
			}
		default:
			out = append(out, strings.Repeat("  ", indent)+l)
		}
	}
	return out
}

func extractRangeText(src string, r protocol.Range) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for i := int(r.Start.Line); i <= int(r.End.Line) && i < len(lines); i++ {
		line := lines[i]
		if i == int(r.Start.Line) && int(r.Start.Character) < len(line) {
			line = line[int(r.Start.Character):]
		}
		if i == int(r.End.Line) && int(r.End.Character) <= len(line) {
			line = line[:int(r.End.Character)]
		}
		out.WriteString(line)
		if i != int(r.End.Line) {
			out.WriteByte('\n')
		}
	}
	return out.String()
}

// ConvertSwiftFile reads the swift file and converts it to Mochi.
func ConvertSwiftFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSwift(string(data))
}
