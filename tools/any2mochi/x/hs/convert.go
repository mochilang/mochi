package hs

import (
	any2mochi "mochi/tools/any2mochi"

	"encoding/json"
	"fmt"
	"os"
	"strings"
)

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func diagnostics(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		start := int(d.Range.Start.Line)
		msg := d.Message
		line := ""
		if start < len(lines) {
			line = strings.TrimSpace(lines[start])
		}
		out.WriteString(fmt.Sprintf("line %d: %s\n  %s\n", start+1, msg, line))
	}
	return strings.TrimSpace(out.String())
}

func hoverString(h any2mochi.Hover) string {
	switch v := h.Contents.(type) {
	case any2mochi.MarkupContent:
		return v.Value
	case any2mochi.MarkedString:
		b, err := json.Marshal(v)
		if err == nil {
			var ms any2mochi.MarkedStringStruct
			if err := json.Unmarshal(b, &ms); err == nil {
				if ms.Value != "" {
					return ms.Value
				}
			}
			var s string
			if err := json.Unmarshal(b, &s); err == nil {
				return s
			}
		}
		return ""
	case []any2mochi.MarkedString:
		parts := make([]string, 0, len(v))
		for _, m := range v {
			parts = append(parts, hoverString(any2mochi.Hover{Contents: m}))
		}
		return strings.Join(parts, "\n")
	case string:
		return v
	default:
		return fmt.Sprint(v)
	}
}

// Convert converts Haskell source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	ls := any2mochi.Servers["hs"]
	syms, diags, err := any2mochi.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil || len(diags) > 0 {
		if items, err2 := parseCLI(src); err2 == nil {
			if out := convertItems(items); out != nil {
				return out, nil
			}
		}
		if out := parseSimple(src); out != nil {
			return out, nil
		}
		if err != nil {
			return nil, err
		}
		return nil, fmt.Errorf("%s", diagnostics(src, diags))
	}
	var out strings.Builder
	writeSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		if items, err2 := parseCLI(src); err2 == nil {
			if res := convertItems(items); res != nil {
				return res, nil
			}
		}
		if simple := parseSimple(src); simple != nil {
			return simple, nil
		}
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// parseCLI parses the source using the built-in parser and returns the items.
func parseCLI(src string) ([]Item, error) {
	return Parse(src), nil
}

func getSignature(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) ([]string, string) {
	if sym.Detail != nil {
		if params, ret := parseSigTypes(*sym.Detail); len(params) > 0 || ret != "" {
			return params, ret
		}
	}
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	return parseSigTypes(hoverString(hov))
}

func parseSigTypes(sig string) ([]string, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if i := strings.Index(sig, "::"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if i := strings.Index(sig, "=>"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if strings.HasPrefix(sig, "forall") {
		if i := strings.Index(sig, "."); i != -1 {
			sig = strings.TrimSpace(sig[i+1:])
		}
	}
	parts := strings.Split(sig, "->")
	for i, p := range parts {
		parts[i] = strings.TrimSpace(p)
	}
	if len(parts) == 0 {
		return nil, ""
	}
	ret := mapType(parts[len(parts)-1])
	params := make([]string, 0, len(parts)-1)
	for _, p := range parts[:len(parts)-1] {
		params = append(params, mapType(p))
	}
	return params, ret
}

func extractParams(sym any2mochi.DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == any2mochi.SymbolKindVariable && c.Range.Start.Line == start {
			params = append(params, c.Name)
		}
	}
	return params
}

func combineParams(names, types []string) []string {
	params := make([]string, 0, len(names))
	for i, n := range names {
		t := ""
		if i < len(types) {
			t = types[i]
		}
		if t != "" {
			params = append(params, fmt.Sprintf("%s: %s", n, t))
		} else {
			params = append(params, n)
		}
	}
	return params
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "()":
		return ""
	case "Int", "Integer":
		return "int"
	case "Float", "Double":
		return "float"
	case "String", "[Char]":
		return "string"
	case "Bool":
		return "bool"
	}
	if strings.HasPrefix(t, "[") && strings.HasSuffix(t, "]") {
		inner := mapType(strings.TrimSuffix(strings.TrimPrefix(t, "["), "]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

func extractBody(src string, sym any2mochi.DocumentSymbol) string {
	lines := strings.Split(src, "\n")
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start >= len(lines) {
		return ""
	}
	if end >= len(lines) {
		end = len(lines) - 1
	}
	snippet := strings.Join(lines[start:end+1], "\n")
	if i := strings.Index(snippet, "="); i != -1 {
		body := strings.TrimSpace(snippet[i+1:])
		if j := strings.Index(body, "\n"); j != -1 {
			body = strings.TrimSpace(body[:j])
		}
		return body
	}
	return ""
}

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindNamespace, any2mochi.SymbolKindModule, any2mochi.SymbolKindPackage:
			writeSymbols(out, nameParts, s.Children, src, ls)
			continue
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod:
			names := extractParams(s)
			types, ret := getSignature(src, s, ls)
			params := combineParams(names, types)
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteByte(')')
			if ret != "" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := extractBody(src, s)
			if body == "" {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" { ")
				out.WriteString(body)
				out.WriteString(" }\n")
			}
		case any2mochi.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == any2mochi.SymbolKindEnumMember || (c.Kind == any2mochi.SymbolKindEnum && len(c.Children) == 0) {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []any2mochi.DocumentSymbol
			for _, c := range s.Children {
				if !(c.Kind == any2mochi.SymbolKindEnumMember || (c.Kind == any2mochi.SymbolKindEnum && len(c.Children) == 0)) {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeSymbols(out, nameParts, rest, src, ls)
			}
			continue
		case any2mochi.SymbolKindStruct, any2mochi.SymbolKindClass, any2mochi.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			if len(s.Children) == 0 {
				out.WriteString(" {}\n")
				break
			}
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind != any2mochi.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				typ := fieldType(src, c, ls)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				typ := getVarType(src, s, ls)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
		if len(s.Children) > 0 && s.Kind != any2mochi.SymbolKindStruct && s.Kind != any2mochi.SymbolKindClass && s.Kind != any2mochi.SymbolKindInterface {
			writeSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func getVarType(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) string {
	if sym.Detail != nil {
		if _, ret := parseSigTypes(*sym.Detail); ret != "" {
			return ret
		}
	}
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	typ, _ := parseVarSig(hoverString(hov))
	return typ
}

func fieldType(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) string {
	if sym.Detail != nil && strings.TrimSpace(*sym.Detail) != "" {
		if t, _ := parseVarSig(*sym.Detail); t != "" {
			return t
		}
	}
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	t, _ := parseVarSig(hoverString(hov))
	return t
}

func parseVarSig(sig string) (string, bool) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if i := strings.Index(sig, "::"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if i := strings.Index(sig, "=>"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if strings.HasPrefix(sig, "forall") {
		if i := strings.Index(sig, "."); i != -1 {
			sig = strings.TrimSpace(sig[i+1:])
		}
	}
	sig = strings.TrimSpace(sig)
	if sig == "" {
		return "", false
	}
	return mapType(sig), true
}

// parseSimple converts trivial Haskell programs without relying on
// the language server. It handles single line function definitions and
// very small "main" functions using "putStrLn".
func parseSimple(src string) []byte {
	lines := strings.Split(src, "\n")
	for i, line := range lines {
		l := strings.TrimSpace(line)
		if strings.HasPrefix(l, "main =") {
			body := strings.TrimSpace(strings.TrimPrefix(l, "main ="))
			if body == "do" && i+1 < len(lines) {
				next := strings.TrimSpace(lines[i+1])
				if strings.HasPrefix(next, "putStrLn") {
					arg := strings.TrimSpace(strings.TrimPrefix(next, "putStrLn"))
					arg = strings.Trim(arg, "()")
					return []byte("print(" + arg + ")")
				}
			} else if strings.HasPrefix(body, "putStrLn") {
				arg := strings.TrimSpace(strings.TrimPrefix(body, "putStrLn"))
				arg = strings.Trim(arg, "()")
				return []byte("print(" + arg + ")")
			}
		}

		if parts := strings.SplitN(l, "=", 2); len(parts) == 2 {
			left := strings.Fields(strings.TrimSpace(parts[0]))
			if len(left) == 0 {
				continue
			}
			name := left[0]
			params := left[1:]
			body := strings.TrimSpace(parts[1])
			if strings.HasPrefix(body, "do") {
				continue
			}
			var out strings.Builder
			out.WriteString("fun ")
			out.WriteString(name)
			out.WriteByte('(')
			for j, p := range params {
				if j > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteString(") { ")
			out.WriteString(body)
			out.WriteString(" }")
			return []byte(out.String())
		}
	}
	return nil
}
