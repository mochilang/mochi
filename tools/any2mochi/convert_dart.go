package any2mochi

import (
	"fmt"
	"os"
	"strings"
	"unicode"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertDart converts dart source code to Mochi using the language server.
func ConvertDart(src string) ([]byte, error) {
	ls := Servers["dart"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	syms = flattenSymbols(syms)

	var out strings.Builder
	matched := false
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindClass:
			convertDartClass(&out, src, s)
			matched = true
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			convertDartFunc(&out, s)
			matched = true
		}
	}

	if !matched {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertDartFile reads the dart file and converts it to Mochi.
func ConvertDartFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertDart(string(data))
}

func convertDartFunc(out *strings.Builder, s protocol.DocumentSymbol) {
	detail := ""
	if s.Detail != nil {
		detail = *s.Detail
	}
	params, ret := parseDartDetail(detail, s.Name)
	out.WriteString("fun ")
	out.WriteString(s.Name)
	out.WriteByte('(')
	if len(params) > 0 {
		out.WriteString(strings.Join(params, ", "))
	}
	out.WriteByte(')')
	if ret != "" && ret != "void" {
		out.WriteString(": ")
		out.WriteString(ret)
	}
	out.WriteString(" {}\n")
}

func convertDartClass(out *strings.Builder, src string, s protocol.DocumentSymbol) {
	if len(s.Children) == 0 {
		return
	}
	out.WriteString("type ")
	out.WriteString(s.Name)
	out.WriteString(" {\n")
	for _, f := range s.Children {
		if f.Kind != protocol.SymbolKindField && f.Kind != protocol.SymbolKindProperty {
			continue
		}
		typ := ""
		if f.Detail != nil {
			typ = parseDartFieldType(*f.Detail, snippetFromRange(src, f.Range))
		} else {
			typ = parseDartFieldType("", snippetFromRange(src, f.Range))
		}
		out.WriteString("  ")
		out.WriteString(f.Name)
		if typ != "" {
			out.WriteString(": ")
			out.WriteString(typ)
		}
		out.WriteByte('\n')
	}
	out.WriteString("}\n")
}

func flattenSymbols(in []protocol.DocumentSymbol) []protocol.DocumentSymbol {
	var out []protocol.DocumentSymbol
	var walk func([]protocol.DocumentSymbol)
	walk = func(list []protocol.DocumentSymbol) {
		for _, s := range list {
			out = append(out, s)
			if s.Children != nil {
				walk(s.Children)
			}
		}
	}
	walk(in)
	return out
}

func parseDartDetail(detail, name string) ([]string, string) {
	open := strings.Index(detail, "(")
	close := strings.LastIndex(detail, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := detail[open+1 : close]
	retPart := strings.TrimSpace(detail[close+1:])
	prefix := strings.TrimSpace(detail[:open])
	if retPart == "" && prefix != "" {
		if strings.HasSuffix(prefix, name) {
			retPart = strings.TrimSpace(prefix[:len(prefix)-len(name)])
		} else {
			retPart = prefix
		}
	}
	params := parseDartParams(paramsPart)
	return params, mapDartType(retPart)
}

func parseDartParams(paramsPart string) []string {
	params := []string{}
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if eq := strings.Index(p, "="); eq != -1 {
			p = strings.TrimSpace(p[:eq])
		}
		p = strings.TrimPrefix(p, "required ")
		fields := strings.FieldsFunc(p, func(r rune) bool { return unicode.IsSpace(r) || r == ':' })
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		typ := strings.Join(fields[:len(fields)-1], " ")
		t := mapDartType(typ)
		if t == "" {
			params = append(params, name)
		} else {
			params = append(params, fmt.Sprintf("%s: %s", name, t))
		}
	}
	return params
}

func parseDartFieldType(detail, snippet string) string {
	typ := strings.Fields(detail)
	if len(typ) > 0 {
		return mapDartType(typ[0])
	}
	fields := strings.Fields(snippet)
	if len(fields) > 1 {
		return mapDartType(fields[0])
	}
	return ""
}

func mapDartType(typ string) string {
	typ = strings.TrimSpace(typ)
	typ = strings.TrimSuffix(typ, ";")
	typ = strings.TrimSuffix(typ, "?")
	typ = strings.TrimPrefix(typ, "final ")
	typ = strings.TrimPrefix(typ, "const ")
	typ = strings.TrimPrefix(typ, "required ")
	switch typ {
	case "", "void":
		return ""
	case "int":
		return "int"
	case "double", "num":
		return "float"
	case "String":
		return "string"
	case "bool":
		return "bool"
	case "dynamic", "Object":
		return "any"
	}
	if strings.HasPrefix(typ, "List<") && strings.HasSuffix(typ, ">") {
		inner := mapDartType(typ[5 : len(typ)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(typ, "Map<") && strings.HasSuffix(typ, ">") {
		inside := typ[4 : len(typ)-1]
		parts := splitArgs(inside)
		if len(parts) == 2 {
			k := mapDartType(parts[0])
			v := mapDartType(parts[1])
			if k == "" {
				k = "any"
			}
			if v == "" {
				v = "any"
			}
			return fmt.Sprintf("map<%s,%s>", k, v)
		}
	}
	return typ
}
