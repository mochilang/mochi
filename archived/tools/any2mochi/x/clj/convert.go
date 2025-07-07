package clj

import (
	any2mochi "mochi/tools/any2mochi"

	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
)

type Program struct {
	Forms []form `json:"forms"`
}

type form struct {
	Type    string   `json:"type"`
	Name    string   `json:"name,omitempty"`
	Params  []string `json:"params,omitempty"`
	Doc     string   `json:"doc,omitempty"`
	Body    []node   `json:"body,omitempty"`
	Value   node     `json:"value,omitempty"`
	Line    int      `json:"line,omitempty"`
	Col     int      `json:"col,omitempty"`
	EndLine int      `json:"end-line,omitempty"`
	EndCol  int      `json:"end-col,omitempty"`
}

type node struct {
	Atom    string `json:"atom,omitempty"`
	List    []node `json:"list,omitempty"`
	Line    int    `json:"line,omitempty"`
	Col     int    `json:"col,omitempty"`
	EndLine int    `json:"end-line,omitempty"`
	EndCol  int    `json:"end-col,omitempty"`
}

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
		ln := int(d.Range.Start.Line)
		col := int(d.Range.Start.Character)
		msg := d.Message
		if ln >= len(lines) {
			out.WriteString(fmt.Sprintf("line %d:%d: %s\n", ln+1, col+1, msg))
			continue
		}
		out.WriteString(fmt.Sprintf("line %d:%d: %s\n", ln+1, col+1, msg))
		start := ln - 1
		if start < 0 {
			start = 0
		}
		end := ln + 1
		if end >= len(lines) {
			end = len(lines) - 1
		}
		for i := start; i <= end; i++ {
			out.WriteString(fmt.Sprintf("%3d | %s\n", i+1, lines[i]))
			if i == ln {
				pointer := strings.Repeat(" ", col) + "^"
				out.WriteString("    | " + pointer + "\n")
			}
		}
	}
	return strings.TrimSpace(out.String())
}

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}

// sanitizeName converts Clojure identifiers into valid Mochi identifiers.
// Leading dashes are replaced with an underscore and special characters
// like '-' or '?' are mapped to underscores or readable suffixes.
func sanitizeName(s string) string {
	s = strings.TrimLeft(s, "-")
	s = strings.ReplaceAll(s, "->", "_to_")
	s = strings.ReplaceAll(s, "-", "_")
	s = strings.ReplaceAll(s, "?", "_p")
	s = strings.ReplaceAll(s, "!", "_bang")
	s = strings.ReplaceAll(s, "/", "_")
	s = strings.ReplaceAll(s, ".", "_")
	if s == "" {
		return "_"
	}
	if s[0] >= '0' && s[0] <= '9' {
		s = "_" + s
	}
	return s
}

func isNumber(s string) bool {
	if _, err := strconv.ParseFloat(s, 64); err == nil {
		return true
	}
	return false
}

// Convert converts Clojure source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	if prog, err := parseCLI(src); err == nil {
		return programToMochi(prog, src)
	}
	ls := any2mochi.Servers["clj"]
	syms, diags, err := any2mochi.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return convertFallback(src)
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", diagnostics(src, diags))
	}
	var out strings.Builder
	writeCljSymbols(&out, nil, syms, ls, src)
	if out.Len() == 0 {
		return convertFallback(src)
	}
	return []byte(out.String()), nil
}

func writeCljSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, ls any2mochi.LanguageServer, src string) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, sanitizeName(s.Name))
		}
		switch s.Kind {
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params, ret := parseParams(s.Detail)
			if len(params) == 0 || ret == "" {
				if hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					p, r := parseHoverSignature(hov)
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
			body := parseBody(src, s.Range)
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
		case any2mochi.SymbolKindClass, any2mochi.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindField, any2mochi.SymbolKindProperty, any2mochi.SymbolKindConstant:
			out.WriteString("var ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString("\n")
		case any2mochi.SymbolKindNamespace, any2mochi.SymbolKindModule, any2mochi.SymbolKindPackage:
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

func parseHoverParams(h any2mochi.Hover) []string {
	var text string
	switch c := h.Contents.(type) {
	case any2mochi.MarkupContent:
		text = c.Value
	case any2mochi.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m any2mochi.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []any2mochi.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m any2mochi.MarkedStringStruct
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

func parseHoverSignature(h any2mochi.Hover) ([]string, string) {
	var text string
	switch c := h.Contents.(type) {
	case any2mochi.MarkupContent:
		text = c.Value
	case any2mochi.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m any2mochi.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []any2mochi.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m any2mochi.MarkedStringStruct
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

func parseParams(detail *string) ([]string, string) {
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

// ConvertFile reads the Clojure file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func parseCLI(src string) (*Program, error) {
	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "cljsrc_*.clj")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	script := filepath.Join(root, "tools", "any2mochi", "x", "clj", "parse.clj")
	cmd := exec.Command("clojure", script, tmp.Name())
	if _, err := exec.LookPath("clojure"); err != nil {
		cmd = exec.Command("go", "run", filepath.Join(root, "tools", "cljast"), tmp.Name())
	}
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("clojure: %s", msg)
	}
	var ast Program
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return &ast, nil
}

func nodeToSexpr(n node) sexprNode {
	if len(n.List) > 0 {
		var list []sexprNode
		for _, c := range n.List {
			list = append(list, nodeToSexpr(c))
		}
		return list
	}
	return n.Atom
}

func paramList(n sexprNode) []string {
	list, ok := n.([]sexprNode)
	if !ok {
		return nil
	}
	out := make([]string, 0, len(list))
	for _, p := range list {
		if s, ok := p.(string); ok {
			out = append(out, s)
		}
	}
	return out
}

func convertFallback(src string) ([]byte, error) {
	exprs, _ := parseClojure(src)
	var out strings.Builder
	for _, e := range exprs {
		if list, ok := e.([]sexprNode); ok && len(list) > 0 {
			head, _ := list[0].(string)
			switch head {
			case "defn":
				if len(list) >= 3 {
					name, _ := list[1].(string)
					params := paramList(list[2])
					out.WriteString("fun ")
					out.WriteString(sanitizeName(name))
					out.WriteByte('(')
					for i, p := range params {
						if i > 0 {
							out.WriteString(", ")
						}
						out.WriteString(sanitizeName(p))
					}
					out.WriteByte(')')
					if len(list) == 3 {
						out.WriteString(" {}\n")
					} else {
						out.WriteString(" {\n")
						for _, b := range list[3:] {
							if s := cljToMochi(b); s != "" {
								out.WriteString("  ")
								out.WriteString(s)
								out.WriteByte('\n')
							}
						}
						out.WriteString("}\n")
					}
					continue
				}
			case "def":
				if len(list) >= 3 {
					out.WriteString("let ")
					name := cljToMochi(list[1])
					out.WriteString(sanitizeName(name))
					if v := cljToMochi(list[2]); v != "" {
						out.WriteString(" = ")
						out.WriteString(v)
					}
					out.WriteByte('\n')
					continue
				}
			}
		}
		if s := cljToMochi(e); s != "" {
			out.WriteString(s)
			out.WriteByte('\n')
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

func programToMochi(p *Program, src string) ([]byte, error) {
	var out strings.Builder
	for _, f := range p.Forms {
		switch f.Type {
		case "defn":
			if f.Doc != "" {
				out.WriteString("// ")
				out.WriteString(f.Doc)
				out.WriteByte('\n')
			}
			out.WriteString("fun ")
			out.WriteString(sanitizeName(f.Name))
			out.WriteByte('(')
			for i, p := range f.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(sanitizeName(p))
			}
			out.WriteByte(')')
			if len(f.Body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, b := range f.Body {
					if s := cljToMochi(nodeToSexpr(b)); s != "" {
						out.WriteString("  ")
						out.WriteString(s)
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
			}
		case "def":
			out.WriteString("let ")
			out.WriteString(sanitizeName(f.Name))
			if s := cljToMochi(nodeToSexpr(f.Value)); s != "" {
				out.WriteString(" = ")
				out.WriteString(s)
			}
			out.WriteByte('\n')
		case "expr":
			if len(f.Body) == 1 {
				if s := cljToMochi(nodeToSexpr(f.Body[0])); s != "" {
					out.WriteString(s)
					out.WriteByte('\n')
				}
			}
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// parseBody extracts and converts the body of a Clojure function defined
// within the given range of the source code. It performs a very small subset of
// Clojure parsing in pure Go and attempts to translate the forms into Mochi
// statements. Only a handful of constructs generated by the Mochi compiler are
// recognised. The implementation intentionally does not rely on regular
// expressions – it tokenises the source and builds a simple s-expression tree.
func parseBody(src string, r any2mochi.Range) []string {
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

type token struct {
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
func tokenize(src string) []token {
	var toks []token
	for i := 0; i < len(src); {
		switch src[i] {
		case '(', ')', '[', ']':
			switch src[i] {
			case '(':
				toks = append(toks, token{tLParen, "("})
			case ')':
				toks = append(toks, token{tRParen, ")"})
			case '[':
				toks = append(toks, token{tLBracket, "["})
			case ']':
				toks = append(toks, token{tRBracket, "]"})
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
				toks = append(toks, token{tString, src[i : j+1]})
				i = j + 1
			} else {
				i = j
			}
		default:
			j := i
			for j < len(src) && !strings.ContainsRune("()[] \n\t\r", rune(src[j])) {
				j++
			}
			toks = append(toks, token{tSymbol, src[i:j]})
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

func parseForm(toks []token, i int) (sexprNode, int) {
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
		if strings.HasPrefix(v, "\"") && strings.HasSuffix(v, "\"") {
			return v
		}
		if isNumber(v) {
			return v
		}
		if strings.HasPrefix(v, ":") {
			v = strings.TrimPrefix(v, ":")
		}
		return sanitizeName(v)
	case []sexprNode:
		if len(v) == 0 {
			return ""
		}
		head, ok := v[0].(string)
		if !ok {
			return ""
		}
		switch head {
		case "ns", "declare":
			return ""
		case "println":
			var args []string
			for _, a := range v[1:] {
				args = append(args, cljToMochi(a))
			}
			return fmt.Sprintf("print(%s)", strings.Join(args, ", "))
		case "if":
			if len(v) >= 3 {
				cond := cljToMochi(v[1])
				thenPart := cljToMochi(v[2])
				elsePart := ""
				if len(v) >= 4 {
					elsePart = cljToMochi(v[3])
				}
				if elsePart == "" {
					return fmt.Sprintf("if %s { %s }", cond, thenPart)
				}
				return fmt.Sprintf("if %s { %s } else { %s }", cond, thenPart, elsePart)
			}
		case "when":
			if len(v) >= 3 {
				cond := cljToMochi(v[1])
				var body []string
				for _, b := range v[2:] {
					if s := cljToMochi(b); s != "" {
						body = append(body, s)
					}
				}
				if len(body) == 0 {
					return ""
				} else if len(body) == 1 {
					return fmt.Sprintf("if %s { %s }", cond, body[0])
				}
				var sb strings.Builder
				sb.WriteString(fmt.Sprintf("if %s {\n", cond))
				for _, b := range body {
					sb.WriteString("  ")
					sb.WriteString(b)
					sb.WriteByte('\n')
				}
				sb.WriteString("}")
				return sb.String()
			}
		case "let":
			if len(v) >= 3 {
				if bind, ok := v[1].([]sexprNode); ok && len(bind) >= 2 {
					name := cljToMochi(bind[0])
					val := cljToMochi(bind[1])
					body := cljToMochi(v[2])
					return fmt.Sprintf("(fun(%s) => %s)(%s)", name, body, val)
				}
			}
		case "def":
			if len(v) >= 3 {
				return fmt.Sprintf("let %s = %s", cljToMochi(v[1]), cljToMochi(v[2]))
			}
		case "and":
			if len(v) >= 3 {
				parts := make([]string, 0, len(v)-1)
				for _, a := range v[1:] {
					parts = append(parts, cljToMochi(a))
				}
				return strings.Join(parts, " && ")
			}
		case "or":
			if len(v) >= 3 {
				parts := make([]string, 0, len(v)-1)
				for _, a := range v[1:] {
					parts = append(parts, cljToMochi(a))
				}
				return strings.Join(parts, " || ")
			}
		case "not":
			if len(v) == 2 {
				return fmt.Sprintf("!%s", cljToMochi(v[1]))
			}
		case "+", "-", "*", "/", "mod", "quot":
			if len(v) == 2 && head == "-" {
				return fmt.Sprintf("(-%s)", cljToMochi(v[1]))
			}
			if len(v) == 3 {
				op := head
				if op == "mod" {
					op = "%"
				} else if op == "quot" {
					op = "/"
				}
				return fmt.Sprintf("%s %s %s", cljToMochi(v[1]), op, cljToMochi(v[2]))
			}
		case "<", "<=", ">", ">=", "=":
			if len(v) == 3 {
				op := head
				if op == "=" {
					op = "=="
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
			if isNumber(head) || strings.HasPrefix(head, "\"") || strings.HasPrefix(head, ":") {
				elems := make([]string, 0, len(v))
				for _, e := range v {
					elems = append(elems, cljToMochi(e))
				}
				return "[" + strings.Join(elems, ", ") + "]"
			}
			var args []string
			for _, a := range v[1:] {
				args = append(args, cljToMochi(a))
			}
			name := sanitizeName(head)
			return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", "))
		}
	}
	return ""
}
