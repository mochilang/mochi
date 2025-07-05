package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertClj converts Clojure source code to Mochi using the language server.
func ConvertClj(src string) ([]byte, error) {
	ls := Servers["clj"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeCljSymbols(&out, nil, syms, ls, src)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func writeCljSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, ls LanguageServer, src string) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params, ret := parseCljParams(s.Detail)
			if len(params) == 0 || ret == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					p, r := parseCljHoverSignature(hov)
					if len(p) > 0 {
						params = p
					}
					if r != "" {
						ret = r
					}
				}
			}
			out.WriteByte('(')
			if len(params) > 0 {
				out.WriteString(strings.Join(params, ", "))
			}
			out.WriteByte(')')
			if ret != "" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := parseCljBody(src, s.Range)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, stmt := range body {
					out.WriteString("  ")
					out.WriteString(stmt)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case protocol.SymbolKindClass, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindField, protocol.SymbolKindProperty, protocol.SymbolKindConstant:
			out.WriteString("var ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString("\n")
		case protocol.SymbolKindNamespace, protocol.SymbolKindModule, protocol.SymbolKindPackage:
			if len(s.Children) > 0 {
				writeCljSymbols(out, nameParts, s.Children, ls, src)
			}
			continue
		}
		if len(s.Children) > 0 {
			writeCljSymbols(out, nameParts, s.Children, ls, src)
		}
	}
}

func parseCljHoverParams(h protocol.Hover) []string {
	var text string
	switch c := h.Contents.(type) {
	case protocol.MarkupContent:
		text = c.Value
	case protocol.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m protocol.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []protocol.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m protocol.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		o := strings.Index(line, "[")
		cidx := strings.Index(line, "]")
		if o == -1 || cidx == -1 || cidx <= o {
			continue
		}
		list := strings.TrimSpace(line[o+1 : cidx])
		if list == "" {
			continue
		}
		fields := strings.Fields(list)
		params := make([]string, 0, len(fields))
		for _, f := range fields {
			if strings.HasPrefix(f, "&") {
				f = strings.TrimPrefix(f, "&")
			}
			if i := strings.IndexAny(f, ":"); i != -1 {
				f = f[:i]
			}
			params = append(params, f)
		}
		if len(params) > 0 {
			return params
		}
	}
	return nil
}

func parseCljHoverSignature(h protocol.Hover) ([]string, string) {
	var text string
	switch c := h.Contents.(type) {
	case protocol.MarkupContent:
		text = c.Value
	case protocol.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m protocol.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []protocol.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m protocol.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		o := strings.Index(line, "[")
		cidx := strings.Index(line, "]")
		if o == -1 || cidx == -1 || cidx <= o {
			continue
		}
		list := strings.TrimSpace(line[o+1 : cidx])
		params := []string{}
		if list != "" {
			fields := strings.Fields(list)
			for _, f := range fields {
				if strings.HasPrefix(f, "&") {
					f = strings.TrimPrefix(f, "&")
				}
				if i := strings.IndexAny(f, ":"); i != -1 {
					f = f[:i]
				}
				params = append(params, f)
			}
		}
		ret := strings.TrimSpace(line[cidx+1:])
		if strings.HasPrefix(ret, "->") {
			ret = strings.TrimSpace(strings.TrimPrefix(ret, "->"))
		}
		return params, ret
	}
	return nil, ""
}

func parseCljParams(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := *detail
	start := strings.Index(d, "[")
	end := strings.Index(d, "]")
	if start == -1 || end == -1 || end <= start {
		return nil, ""
	}
	list := strings.TrimSpace(d[start+1 : end])
	if list == "" {
		return nil, ""
	}
	fields := strings.Fields(list)
	params := make([]string, 0, len(fields))
	for _, f := range fields {
		if strings.HasPrefix(f, "&") {
			f = strings.TrimPrefix(f, "&")
		}
		params = append(params, f)
	}
	ret := strings.TrimSpace(d[end+1:])
	if strings.HasPrefix(ret, "->") {
		ret = strings.TrimSpace(strings.TrimPrefix(ret, "->"))
	}
	return params, ret
}

// ConvertCljFile reads the Clojure file and converts it to Mochi.
func ConvertCljFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertClj(string(data))
}

// parseCljBody extracts and converts the body of a Clojure function defined
// within the given range of the source code. It performs a very small subset of
// Clojure parsing in pure Go and attempts to translate the forms into Mochi
// statements. Only a handful of constructs generated by the Mochi compiler are
// recognised. The implementation intentionally does not rely on regular
// expressions – it tokenises the source and builds a simple s-expression tree.
func parseCljBody(src string, r protocol.Range) []string {
	lines := strings.Split(src, "\n")
	if int(r.Start.Line) >= len(lines) || int(r.End.Line) >= len(lines) {
		return nil
	}
	var body strings.Builder
	for i := int(r.Start.Line); i <= int(r.End.Line); i++ {
		body.WriteString(lines[i])
		body.WriteByte('\n')
	}
	exprs, _ := parseClojure(body.String())
	if len(exprs) == 1 {
		if list, ok := exprs[0].([]sexprNode); ok && len(list) >= 3 {
			if head, ok := list[0].(string); ok && head == "defn" {
				exprs = list[3:]
			}
		}
	}
	var stmts []string
	for _, e := range exprs {
		if s := cljToMochi(e); s != "" {
			stmts = append(stmts, s)
		}
	}
	return stmts
}

type cljToken struct {
	kind int
	val  string
}

const (
	tLParen = iota
	tRParen
	tLBracket
	tRBracket
	tString
	tSymbol
)

// tokenize breaks src into minimal tokens.
func tokenize(src string) []cljToken {
	var toks []cljToken
	for i := 0; i < len(src); {
		switch src[i] {
		case '(', ')', '[', ']':
			switch src[i] {
			case '(':
				toks = append(toks, cljToken{tLParen, "("})
			case ')':
				toks = append(toks, cljToken{tRParen, ")"})
			case '[':
				toks = append(toks, cljToken{tLBracket, "["})
			case ']':
				toks = append(toks, cljToken{tRBracket, "]"})
			}
			i++
		case ' ', '\t', '\n', '\r':
			i++
		case '"':
			j := i + 1
			for j < len(src) && src[j] != '"' {
				if src[j] == '\\' && j+1 < len(src) {
					j += 2
				} else {
					j++
				}
			}
			if j < len(src) {
				toks = append(toks, cljToken{tString, src[i : j+1]})
				i = j + 1
			} else {
				i = j
			}
		default:
			j := i
			for j < len(src) && !strings.ContainsRune("()[] \n\t\r", rune(src[j])) {
				j++
			}
			toks = append(toks, cljToken{tSymbol, src[i:j]})
			i = j
		}
	}
	return toks
}

type sexprNode interface{}

// parseClojure parses a sequence of s-expressions.
func parseClojure(src string) ([]sexprNode, int) {
	toks := tokenize(src)
	var pos int
	var list []sexprNode
	for pos < len(toks) {
		n, p := parseForm(toks, pos)
		if p == pos {
			break
		}
		pos = p
		if n != nil {
			list = append(list, n)
		}
	}
	return list, pos
}

func parseForm(toks []cljToken, i int) (sexprNode, int) {
	if i >= len(toks) {
		return nil, i
	}
	switch toks[i].kind {
	case tLParen:
		var list []sexprNode
		i++
		for i < len(toks) && toks[i].kind != tRParen {
			var n sexprNode
			n, i = parseForm(toks, i)
			if n != nil {
				list = append(list, n)
			}
		}
		if i < len(toks) && toks[i].kind == tRParen {
			i++
		}
		return list, i
	case tString, tSymbol:
		val := toks[i].val
		i++
		return val, i
	case tLBracket:
		var list []sexprNode
		i++
		for i < len(toks) && toks[i].kind != tRBracket {
			var n sexprNode
			n, i = parseForm(toks, i)
			if n != nil {
				list = append(list, n)
			}
		}
		if i < len(toks) && toks[i].kind == tRBracket {
			i++
		}
		return list, i
	default:
		i++
	}
	return nil, i
}

// cljToMochi converts a parsed s-expression into a Mochi statement or
// expression. Only a subset of constructs are supported.
func cljToMochi(n sexprNode) string {
	switch v := n.(type) {
	case string:
		return v
	case []sexprNode:
		if len(v) == 0 {
			return ""
		}
		head, ok := v[0].(string)
		if !ok {
			return ""
		}
		switch head {
		case "println":
			var args []string
			for _, a := range v[1:] {
				args = append(args, cljToMochi(a))
			}
			return fmt.Sprintf("print(%s)", strings.Join(args, ", "))
		case "def":
			if len(v) >= 3 {
				return fmt.Sprintf("let %s = %s", cljToMochi(v[1]), cljToMochi(v[2]))
			}
		case "+", "-", "*", "/", "mod", "quot":
			if len(v) == 3 {
				op := head
				if op == "mod" {
					op = "%"
				} else if op == "quot" {
					op = "/"
				}
				return fmt.Sprintf("%s %s %s", cljToMochi(v[1]), op, cljToMochi(v[2]))
			}
		case "throw":
			if len(v) == 2 {
				if l2, ok := v[1].([]sexprNode); ok && len(l2) >= 3 {
					if s, ok := l2[0].(string); ok && s == "ex-info" {
						if str, ok := l2[1].(string); ok && strings.Trim(str, "\"") == "return" {
							if mp, ok := l2[2].([]sexprNode); ok && len(mp) >= 2 {
								if key, ok := mp[0].(string); ok && key == ":value" {
									return fmt.Sprintf("return %s", cljToMochi(mp[1]))
								}
							}
						}
					}
				}
			}
		default:
			var args []string
			for _, a := range v[1:] {
				args = append(args, cljToMochi(a))
			}
			return fmt.Sprintf("%s(%s)", head, strings.Join(args, ", "))
		}
	}
	return ""
}
