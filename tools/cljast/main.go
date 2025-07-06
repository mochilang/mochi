package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"
)

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

// --- AST structures ---

type node struct {
	Atom string `json:"atom,omitempty"`
	List []node `json:"list,omitempty"`
}

type form struct {
	Type   string   `json:"type"`
	Name   string   `json:"name,omitempty"`
	Params []string `json:"params,omitempty"`
	Body   []node   `json:"body,omitempty"`
	Value  node     `json:"value,omitempty"`
}

type ast struct {
	Forms []form `json:"forms"`
}

func toNode(n sexprNode) node {
	switch v := n.(type) {
	case string:
		return node{Atom: v}
	case []sexprNode:
		var children []node
		for _, c := range v {
			children = append(children, toNode(c))
		}
		return node{List: children}
	default:
		return node{Atom: fmt.Sprintf("%v", v)}
	}
}

func paramList(n sexprNode) []string {
	list, ok := n.([]sexprNode)
	if !ok {
		return nil
	}
	var out []string
	for _, p := range list {
		if s, ok := p.(string); ok {
			out = append(out, s)
		}
	}
	return out
}

func buildForm(n sexprNode) *form {
	list, ok := n.([]sexprNode)
	if !ok || len(list) == 0 {
		return &form{Type: "expr", Body: []node{toNode(n)}}
	}
	head, ok := list[0].(string)
	if !ok {
		return &form{Type: "expr", Body: []node{toNode(n)}}
	}
	switch head {
	case "defn":
		if len(list) >= 3 {
			name, _ := list[1].(string)
			params := paramList(list[2])
			var body []node
			for _, b := range list[3:] {
				body = append(body, toNode(b))
			}
			return &form{Type: "defn", Name: name, Params: params, Body: body}
		}
	case "def":
		if len(list) >= 3 {
			name, _ := list[1].(string)
			return &form{Type: "def", Name: name, Value: toNode(list[2])}
		}
	default:
		return &form{Type: "expr", Body: []node{toNode(n)}}
	}
	return nil
}

func parseForms(src string) ast {
	exprs, _ := parseClojure(src)
	var a ast
	for _, e := range exprs {
		if f := buildForm(e); f != nil {
			a.Forms = append(a.Forms, *f)
		}
	}
	return a
}

func main() {
	var data []byte
	var err error
	if len(os.Args) > 1 && os.Args[1] != "-" {
		data, err = os.ReadFile(os.Args[1])
	} else {
		data, err = io.ReadAll(os.Stdin)
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	ast := parseForms(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(ast); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
