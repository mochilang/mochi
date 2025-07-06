package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// ConvertHs converts hs source code to Mochi using the language server.
func ConvertHs(src string) ([]byte, error) {
	ls := Servers["hs"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil || len(diags) > 0 {
		if items, err2 := parseHsCLI(src); err2 == nil {
			if out := convertHsItems(items); out != nil {
				return out, nil
			}
		}
		if out := parseSimpleHs(src); out != nil {
			return out, nil
		}
		if err != nil {
			return nil, err
		}
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeHsSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		if items, err2 := parseHsCLI(src); err2 == nil {
			if res := convertHsItems(items); res != nil {
				return res, nil
			}
		}
		if simple := parseSimpleHs(src); simple != nil {
			return simple, nil
		}
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertHsFile reads the hs file and converts it to Mochi.
func ConvertHsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertHs(string(data))
}

// parseHsCLI invokes the hsast CLI to parse the Haskell source into hsItems.
func parseHsCLI(src string) ([]hsItem, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	path, err := exec.LookPath("hsast")
	if err != nil {
		path = "go"
	}
	var args []string
	if path == "go" {
		args = []string{"run", "./tools/any2mochi/cmd/hsast"}
	}
	cmd := exec.CommandContext(ctx, path, args...)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var items []hsItem
	if err := json.Unmarshal(out.Bytes(), &items); err != nil {
		return nil, err
	}
	return items, nil
}

func getHsSignature(src string, sym DocumentSymbol, ls LanguageServer) ([]string, string) {
	if sym.Detail != nil {
		if params, ret := parseHsSigTypes(*sym.Detail); len(params) > 0 || ret != "" {
			return params, ret
		}
	}
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	return parseHsSigTypes(hoverString(hov))
}

func parseHsSigTypes(sig string) ([]string, string) {
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
	ret := mapHsType(parts[len(parts)-1])
	params := make([]string, 0, len(parts)-1)
	for _, p := range parts[:len(parts)-1] {
		params = append(params, mapHsType(p))
	}
	return params, ret
}

func extractHsParams(sym DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == SymbolKindVariable && c.Range.Start.Line == start {
			params = append(params, c.Name)
		}
	}
	return params
}

func combineHsParams(names, types []string) []string {
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

func mapHsType(t string) string {
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
		inner := mapHsType(strings.TrimSuffix(strings.TrimPrefix(t, "["), "]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

func extractHsBody(src string, sym DocumentSymbol) string {
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

func writeHsSymbols(out *strings.Builder, prefix []string, syms []DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case SymbolKindNamespace, SymbolKindModule, SymbolKindPackage:
			writeHsSymbols(out, nameParts, s.Children, src, ls)
			continue
		case SymbolKindFunction, SymbolKindMethod:
			names := extractHsParams(s)
			types, ret := getHsSignature(src, s, ls)
			params := combineHsParams(names, types)
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
			body := extractHsBody(src, s)
			if body == "" {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" { ")
				out.WriteString(body)
				out.WriteString(" }\n")
			}
		case SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == SymbolKindEnumMember || (c.Kind == SymbolKindEnum && len(c.Children) == 0) {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []DocumentSymbol
			for _, c := range s.Children {
				if !(c.Kind == SymbolKindEnumMember || (c.Kind == SymbolKindEnum && len(c.Children) == 0)) {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeHsSymbols(out, nameParts, rest, src, ls)
			}
			continue
		case SymbolKindStruct, SymbolKindClass, SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			if len(s.Children) == 0 {
				out.WriteString(" {}\n")
				break
			}
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind != SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				typ := hsFieldType(src, c, ls)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case SymbolKindVariable, SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				typ := getHsVarType(src, s, ls)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
		if len(s.Children) > 0 && s.Kind != SymbolKindStruct && s.Kind != SymbolKindClass && s.Kind != SymbolKindInterface {
			writeHsSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func getHsVarType(src string, sym DocumentSymbol, ls LanguageServer) string {
	if sym.Detail != nil {
		if _, ret := parseHsSigTypes(*sym.Detail); ret != "" {
			return ret
		}
	}
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	typ, _ := parseHsVarSig(hoverString(hov))
	return typ
}

func hsFieldType(src string, sym DocumentSymbol, ls LanguageServer) string {
	if sym.Detail != nil && strings.TrimSpace(*sym.Detail) != "" {
		if t, _ := parseHsVarSig(*sym.Detail); t != "" {
			return t
		}
	}
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	t, _ := parseHsVarSig(hoverString(hov))
	return t
}

func parseHsVarSig(sig string) (string, bool) {
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
	return mapHsType(sig), true
}

// parseSimpleHs converts trivial Haskell programs without relying on
// the language server. It handles single line function definitions and
// very small "main" functions using "putStrLn".
func parseSimpleHs(src string) []byte {
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
