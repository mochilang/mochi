package cljast

import (
	"strings"
)

type cljToken struct {
	kind int
	val  string
	line int
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
	line := 1
	for i := 0; i < len(src); {
		switch src[i] {
		case '(', ')', '[', ']':
			switch src[i] {
			case '(':
				toks = append(toks, cljToken{tLParen, "(", line})
			case ')':
				toks = append(toks, cljToken{tRParen, ")", line})
			case '[':
				toks = append(toks, cljToken{tLBracket, "[", line})
			case ']':
				toks = append(toks, cljToken{tRBracket, "]", line})
			}
			i++
		case ' ', '\t', '\r':
			i++
		case '\n':
			line++
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
				toks = append(toks, cljToken{tString, src[i : j+1], line})
				i = j + 1
			} else {
				i = j
			}
		default:
			j := i
			for j < len(src) && !strings.ContainsRune("()[] \n\t\r", rune(src[j])) {
				j++
			}
			toks = append(toks, cljToken{tSymbol, src[i:j], line})
			i = j
		}
	}
	return toks
}

func parseClojure(src string) ([]node, int) {
	toks := tokenize(src)
	var pos int
	var list []node
	for pos < len(toks) {
		n, p := parseForm(toks, pos)
		if p == pos {
			break
		}
		pos = p
		if n != nil {
			list = append(list, *n)
		}
	}
	return list, pos
}

func parseForm(toks []cljToken, i int) (*node, int) {
	if i >= len(toks) {
		return nil, i
	}
	switch toks[i].kind {
	case tLParen:
		var list []node
		line := toks[i].line
		i++
		for i < len(toks) && toks[i].kind != tRParen {
			var n *node
			n, i = parseForm(toks, i)
			if n != nil {
				list = append(list, *n)
			}
		}
		if i < len(toks) && toks[i].kind == tRParen {
			i++
		}
		return &node{Line: line, List: list}, i
	case tString, tSymbol:
		val := toks[i].val
		line := toks[i].line
		i++
		return &node{Line: line, Atom: val}, i
	case tLBracket:
		var list []node
		line := toks[i].line
		i++
		for i < len(toks) && toks[i].kind != tRBracket {
			var n *node
			n, i = parseForm(toks, i)
			if n != nil {
				list = append(list, *n)
			}
		}
		if i < len(toks) && toks[i].kind == tRBracket {
			i++
		}
		return &node{Line: line, List: list}, i
	default:
		i++
	}
	return nil, i
}

// --- AST structures ---

type Node struct {
	Line int    `json:"line,omitempty"`
	Atom string `json:"atom,omitempty"`
	List []Node `json:"list,omitempty"`
}

type Form struct {
	Type   string   `json:"type"`
	Name   string   `json:"name,omitempty"`
	Params []string `json:"params,omitempty"`
	Body   []Node   `json:"body,omitempty"`
	Value  Node     `json:"value,omitempty"`
	Line   int      `json:"line,omitempty"`
}

type AST struct {
	Forms []Form `json:"forms"`
}

func toNode(n Node) Node { return n }

func paramList(n Node) []string {
	var out []string
	for _, p := range n.List {
		if p.Atom != "" {
			out = append(out, p.Atom)
		}
	}
	return out
}

func buildForm(n Node) *Form {
	if len(n.List) == 0 {
		return &Form{Type: "expr", Body: []Node{toNode(n)}}
	}
	head := n.List[0].Atom
	if head == "" {
		return &form{Type: "expr", Body: []node{toNode(n)}}
	}
	switch head {
	case "defn":
		if len(n.List) >= 3 {
			name := n.List[1].Atom
			params := paramList(n.List[2])
			var body []Node
			for _, b := range n.List[3:] {
				body = append(body, toNode(b))
			}
			return &Form{Type: "defn", Name: name, Params: params, Body: body}
		}
	case "def":
		if len(n.List) >= 3 {
			name := n.List[1].Atom
			return &Form{Type: "def", Name: name, Value: toNode(n.List[2])}
		}
	default:
		return &Form{Type: "expr", Body: []Node{toNode(n)}}
	}
	return nil
}

func Parse(src string) AST {
	exprs, _ := parseClojure(src)
	var a AST
	for _, e := range exprs {
		if f := buildForm(e); f != nil {
			a.Forms = append(a.Forms, *f)
		}
	}
	return a
}
