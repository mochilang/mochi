//go:build slow

package scheme

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Item represents a top-level Scheme definition.
type Item struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	Params []string `json:"params,omitempty"`
}

// Parse parses a subset of Scheme source and returns top level Items.
func Parse(src string) ([]Item, error) {
	toks := tokenize(src)
	nodes, i, err := parseList(toks, 0)
	if err != nil {
		return nil, err
	}
	if i != len(toks) {
		return nil, fmt.Errorf("unexpected token")
	}
	var items []Item
	for _, n := range nodes {
		if len(n.list) < 3 || n.list[0].atom != "define" {
			continue
		}
		def := n.list[1]
		if len(def.list) > 0 {
			name := def.list[0].atom
			var params []string
			for _, p := range def.list[1:] {
				if p.atom != "" {
					params = append(params, p.atom)
				}
			}
			items = append(items, Item{Kind: "func", Name: name, Params: params})
		} else if def.atom != "" {
			items = append(items, Item{Kind: "var", Name: def.atom})
		}
	}
	return items, nil
}

// ConvertSource converts Items into Mochi source code.
func ConvertSource(items []Item) (string, error) {
	var b strings.Builder
	for _, it := range items {
		switch it.Kind {
		case "func":
			b.WriteString("fun ")
			b.WriteString(sanitizeName(it.Name))
			b.WriteByte('(')
			for i, p := range it.Params {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(sanitizeName(p))
				b.WriteString(": any")
			}
			b.WriteString(") {}\n")
		case "var":
			b.WriteString("let ")
			b.WriteString(sanitizeName(it.Name))
			b.WriteByte('\n')
		}
	}
	return b.String(), nil
}

// Convert converts parsed Items into a Mochi AST node.
func Convert(items []Item) (*ast.Node, error) {
	src, err := ConvertSource(items)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// --- Parser ---

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
		case ' ', '\t', '\r':
			i++
		case '\n':
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
			toks = append(toks, token{scTokString, src[i:j]})
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
	start := i
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
	if start != 0 {
		return nil, i, fmt.Errorf("missing ')'")
	}
	return nodes, i, nil
}

// sanitizeName converts Scheme identifiers into Mochi compatible ones.
func sanitizeName(s string) string {
	s = strings.ReplaceAll(s, "->", "_to_")
	s = strings.ReplaceAll(s, "-", "_")
	s = strings.ReplaceAll(s, "?", "_p")
	s = strings.ReplaceAll(s, "!", "_bang")
	return s
}
