//go:build slow

package scheme

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
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
		switch {
		case len(n.list) >= 3 && n.list[0].atom == "define":
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
				// Support (define name (lambda (params...) ...)) forms
				if len(n.list) > 2 && len(n.list[2].list) > 1 && n.list[2].list[0].atom == "lambda" {
					lam := n.list[2].list[1]
					var params []string
					for _, p := range lam.list {
						if p.atom != "" {
							params = append(params, p.atom)
						}
					}
					items = append(items, Item{Kind: "func", Name: def.atom, Params: params})
				} else {
					items = append(items, Item{Kind: "var", Name: def.atom})
				}
			}
		case len(n.list) == 3 && n.list[0].atom == "set!" && n.list[1].atom != "":
			items = append(items, Item{Kind: "var", Name: n.list[1].atom})
		case len(n.list) >= 2 && (n.list[0].atom == "import" || n.list[0].atom == "require"):
			for _, mod := range n.list[1:] {
				if len(mod.list) > 0 {
					var parts []string
					for _, p := range mod.list {
						if p.atom != "" {
							parts = append(parts, sanitizeName(p.atom))
						}
					}
					if len(parts) > 0 {
						items = append(items, Item{Kind: "import", Name: strings.Join(parts, "_")})
					}
				} else if mod.atom != "" {
					items = append(items, Item{Kind: "import", Name: sanitizeName(mod.atom)})
				}
			}
		}
	}
	return items, nil
}

// ConvertSource converts Items into Mochi source code.
func ConvertSource(items []Item, src string) (string, error) {
	var b strings.Builder
	b.WriteString(string(meta.Header("//")))
	b.WriteString("/*\n")
	b.WriteString(src)
	if !strings.HasSuffix(src, "\n") {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
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
		case "import":
			b.WriteString("use ")
			b.WriteString(it.Name)
			b.WriteByte('\n')
		}
	}
	return b.String(), nil
}

// Convert converts parsed Items into a Mochi AST node.
func Convert(items []Item, srcStr string) (*ast.Node, error) {
	src, err := ConvertSource(items, srcStr)
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
