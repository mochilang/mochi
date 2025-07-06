package any2mochi

import "strings"

// SchemeItem represents a top level Scheme definition extracted from the source.
// Only a minimal subset is supported: function definitions with parameter names
// and simple variable definitions.
type SchemeItem struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	Params []string `json:"params,omitempty"`
}

// ParseSchemeItems parses Scheme source and returns a slice of SchemeItem.
// The parser handles a tiny subset of Scheme syntax sufficient for the
// conversion tests.
func ParseSchemeItems(src string) ([]SchemeItem, error) {
	toks := tokenizeScheme(src)
	nodes, _, err := parseSchemeList(toks, 0)
	if err != nil {
		return nil, err
	}
	var items []SchemeItem
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
			items = append(items, SchemeItem{Kind: "func", Name: name, Params: params})
		} else if def.atom != "" {
			// variable definition
			items = append(items, SchemeItem{Kind: "var", Name: def.atom})
		}
	}
	return items, nil
}

type schemeToken struct {
	typ int
	val string
}

const (
	scTokLParen = iota
	scTokRParen
	scTokAtom
	scTokString
)

func tokenizeScheme(src string) []schemeToken {
	var toks []schemeToken
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
			toks = append(toks, schemeToken{scTokLParen, "("})
			i++
		case ')':
			toks = append(toks, schemeToken{scTokRParen, ")"})
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
				toks = append(toks, schemeToken{scTokString, src[i:j]})
			} else {
				toks = append(toks, schemeToken{scTokString, src[i:]})
			}
			i = j
		default:
			j := i
			for j < len(src) && !strings.ContainsRune("()\n\t\r ", rune(src[j])) {
				j++
			}
			toks = append(toks, schemeToken{scTokAtom, src[i:j]})
			i = j
		}
	}
	return toks
}

type schemeNode struct {
	atom string
	list []schemeNode
}

func parseSchemeList(toks []schemeToken, i int) ([]schemeNode, int, error) {
	var nodes []schemeNode
	for i < len(toks) {
		tok := toks[i]
		switch tok.typ {
		case scTokRParen:
			return nodes, i, nil
		case scTokLParen:
			lst, j, err := parseSchemeList(toks, i+1)
			if err != nil {
				return nil, 0, err
			}
			nodes = append(nodes, schemeNode{list: lst})
			i = j + 1
		case scTokAtom, scTokString:
			nodes = append(nodes, schemeNode{atom: tok.val})
			i++
		default:
			i++
		}
	}
	return nodes, i, nil
}
