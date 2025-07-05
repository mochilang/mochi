package any2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

const cljParseScript = `(require '[cheshire.core :as json])
 (require '[clojure.tools.reader :as tr])
 (require '[clojure.tools.reader.reader-types :as rt])
 (let [r (rt/push-back-reader (java.io.InputStreamReader. System/in))
       forms (loop [acc []]
                (let [f (try (tr/read {:eof ::eof} r)
                            (catch Exception _ ::eof))]
                  (if (= f ::eof) acc (recur (conj acc f)))))]
   (println (json/generate-string forms)))`

// ConvertClj converts Clojure source code to Mochi without using a language server.
func ConvertClj(src string) ([]byte, error) {
	funcs := parseCljFunctions(src)
	if len(funcs) == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	var out strings.Builder
	for _, fn := range funcs {
		out.WriteString("fun ")
		out.WriteString(fn.name)
		out.WriteByte('(')
		if len(fn.params) > 0 {
			out.WriteString(strings.Join(fn.params, ", "))
		}
		out.WriteByte(')')
		body := cljFormsToMochi(fn.body)
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
	}
	return []byte(out.String()), nil
}

// ConvertCljFile reads the Clojure file and converts it to Mochi.
func ConvertCljFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertClj(string(data))
}

func parseClojureCLI(src string) ([]sexprNode, error) {
	if err := EnsureServer("bb"); err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "cljparse*.clj")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(cljParseScript); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	cmd := exec.Command("bb", tmp.Name())
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var nodes []sexprNode
	if err := json.Unmarshal(out.Bytes(), &nodes); err != nil {
		return nil, err
	}
	return nodes, nil
}

// parseCljFunctions returns a list of top level functions defined in src.
// It uses a simple parser based on tokenization and string matching.
type cljFunc struct {
	name   string
	params []string
	body   []sexprNode
}

func parseCljFunctions(src string) []cljFunc {
	exprs, err := parseClojureCLI(src)
	if err != nil {
		exprs, _ = parseClojure(src)
	}
	var fns []cljFunc
	for _, e := range exprs {
		list, ok := e.([]sexprNode)
		if !ok || len(list) < 2 {
			continue
		}
		head, ok := list[0].(string)
		if !ok || head != "defn" {
			continue
		}
		name, ok := list[1].(string)
		if !ok {
			continue
		}
		idx := 2
		for idx < len(list) && !isVector(list[idx]) {
			idx++
		}
		if idx >= len(list) {
			continue
		}
		paramsVec, _ := list[idx].([]sexprNode)
		idx++
		var params []string
		for _, p := range paramsVec {
			if s, ok := p.(string); ok {
				s = strings.TrimPrefix(s, "&")
				if i := strings.IndexAny(s, ":"); i != -1 {
					s = s[:i]
				}
				params = append(params, s)
			}
		}
		body := list[idx:]
		fns = append(fns, cljFunc{name: name, params: params, body: body})
	}
	return fns
}

func isVector(n sexprNode) bool {
	_, ok := n.([]sexprNode)
	return ok
}

func cljFormsToMochi(forms []sexprNode) []string {
	var stmts []string
	for _, e := range forms {
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
