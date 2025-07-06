package scheme

import "strings"

// SchemeItem represents a top level Scheme definition extracted from the source.
// Only a minimal subset is supported: function definitions with parameter names
// and simple variable definitions.
type Item struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	Params []string `json:"params,omitempty"`
}

// ParseSchemeItems parses Scheme source and returns a slice of SchemeItem.
// The parser handles a tiny subset of Scheme syntax sufficient for the
// conversion tests.
func ParseItems(src string) ([]Item, error) {
	toks := tokenize(src)
	nodes, _, err := parseList(toks, 0)
	if err != nil {
		return nil, err
	}
	var items []Item
	for _, n := range nodes {
		if len(n.list) == 0 {
			continue
		}
		if n.list[0].atom != "define" {
			continue
		}
		if len(n.list) < 3 {
			continue
		}
		def := n.list[1]
		if len(def.list) > 0 {
			// function definition
			name := def.list[0].atom
			var params []string
			for _, p := range def.list[1:] {
				if p.atom != "" {
					params = append(params, p.atom)
				}
			}
			items = append(items, Item{Kind: "func", Name: name, Params: params})
		} else if def.atom != "" {
			// variable definition
			items = append(items, Item{Kind: "var", Name: def.atom})
		}
	}
	return items, nil
}

type token struct {
	typ int
	val string
}

const (
	scTokLParen = iota
	scTokRParen
	scTokAtom
	scTokString
)

func tokenize(src string) []token {
	var toks []token
	i := 0
	for i < len(src) {
		c := src[i]
		switch c {
		case ';':
			for i < len(src) && src[i] != '\n' {
				i++
			}
		case ' ', '\t', '\n', '\r':
			i++
		case '(':
			toks = append(toks, token{scTokLParen, "("})
			i++
		case ')':
			toks = append(toks, token{scTokRParen, ")"})
			i++
		case '"':
			j := i + 1
			for j < len(src) {
				if src[j] == '\\' && j+1 < len(src) {
					j += 2
					continue
				}
				if src[j] == '"' {
					j++
					break
				}
				j++
			}
			if j <= len(src) {
				toks = append(toks, token{scTokString, src[i:j]})
			} else {
				toks = append(toks, token{scTokString, src[i:]})
			}
			i = j
		default:
			j := i
			for j < len(src) && !strings.ContainsRune("()\n\t\r ", rune(src[j])) {
				j++
			}
			toks = append(toks, token{scTokAtom, src[i:j]})
			i = j
		}
	}
	return toks
}

type node struct {
	atom string
	list []node
}

func parseList(toks []token, i int) ([]node, int, error) {
	var nodes []node
	for i < len(toks) {
		tok := toks[i]
		switch tok.typ {
		case scTokRParen:
			return nodes, i, nil
		case scTokLParen:
			lst, j, err := parseList(toks, i+1)
			if err != nil {
				return nil, 0, err
			}
			nodes = append(nodes, node{list: lst})
			i = j + 1
		case scTokAtom, scTokString:
			nodes = append(nodes, node{atom: tok.val})
			i++
		default:
			i++
		}
	}
	return nodes, i, nil
}
