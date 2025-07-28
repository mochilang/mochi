package clj

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Program represents the top-level forms returned by parse.clj.
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

// Parse reads Clojure source code and returns the parsed Program using parse.clj.
func Parse(src string) (*Program, error) {
	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "clj-src-*.clj")
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

	script := filepath.Join(root, "tools", "a2mochi", "x", "clj", "parse.clj")
	cmd := exec.Command("clojure", script, tmp.Name())
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
	var prog Program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}

// ConvertSource converts a parsed Program into Mochi source code.
func ConvertSource(p *Program) (string, error) {
	b, err := programToMochi(p, "")
	if err != nil {
		return "", err
	}
	return string(b), nil
}

// Convert converts a parsed Program to a Mochi AST node.
func Convert(p *Program) (*ast.Node, error) {
	src, err := ConvertSource(p)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
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
