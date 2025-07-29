//go:build slow

package clj

import (
	"fmt"
	"strings"
)

// Program mirrors the structure produced by the original Clojure helper script.
type Program struct {
	Forms  []form `json:"forms"`
	Source string `json:"-"`
}

type form struct {
	Type   string   `json:"type"`
	Name   string   `json:"name,omitempty"`
	Params []string `json:"params,omitempty"`
	Doc    string   `json:"doc,omitempty"`
	Body   []node   `json:"body,omitempty"`
	Value  node     `json:"value,omitempty"`
}

type node struct {
	Atom string   `json:"atom,omitempty"`
	List []node   `json:"list,omitempty"`
	Map  [][]node `json:"map,omitempty"`
}

// Parse reads Clojure source into an intermediate Program representation. It
// implements a small S-expression parser in Go so that the tests do not depend
// on an external Clojure installation.
func Parse(src string) (*Program, error) {
	l := lexer{src: src}
	p := parser{lex: l.lex()}
	var prog Program
	for p.peek().typ != tokenEOF {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		prog.Forms = append(prog.Forms, buildForm(expr))
	}
	prog.Source = src
	return &prog, nil
}

// buildForm converts the raw node into a top-level form record.
func buildForm(n node) form {
	if len(n.List) > 0 {
		head := atom(n.List[0])
		if head == "defn" && len(n.List) >= 3 {
			params := n.List[2]
			var ps []string
			for _, p := range params.List {
				ps = append(ps, atom(p))
			}
			f := form{Type: "defn", Name: atom(n.List[1]), Params: ps}
			if len(n.List) > 3 {
				f.Body = n.List[3:]
			}
			return f
		}
		if head == "def" && len(n.List) == 3 {
			return form{Type: "def", Name: atom(n.List[1]), Value: n.List[2]}
		}
	}
	return form{Type: "expr", Body: []node{n}}
}

// atom extracts the Atom field from a node.
func atom(n node) string {
	return n.Atom
}

// lexer/token implementation -------------------------------------------------

type tokenType int

const (
	tokenEOF tokenType = iota
	tokenLParen
	tokenRParen
	tokenLBrack
	tokenRBrack
	tokenLBrace
	tokenRBrace
	tokenQuote
	tokenString
	tokenSymbol
)

type token struct {
	typ tokenType
	val string
}

type lexer struct {
	src string
	pos int
}

func (l *lexer) next() rune {
	if l.pos >= len(l.src) {
		return 0
	}
	r := rune(l.src[l.pos])
	l.pos++
	return r
}

func (l *lexer) peek() rune {
	if l.pos >= len(l.src) {
		return 0
	}
	return rune(l.src[l.pos])
}

func (l *lexer) skipWhitespace() {
	for {
		r := l.peek()
		if r == ';' {
			for r != '\n' && r != 0 {
				l.next()
				r = l.peek()
			}
		}
		if r == ' ' || r == '\t' || r == '\n' || r == '\r' {
			l.next()
			continue
		}
		break
	}
}

func (l *lexer) readString() (string, error) {
	var b strings.Builder
	for {
		r := l.next()
		if r == 0 {
			return "", fmt.Errorf("unterminated string")
		}
		if r == '\\' {
			nr := l.next()
			if nr == 0 {
				return "", fmt.Errorf("unterminated string")
			}
			b.WriteRune(nr)
			continue
		}
		if r == '"' {
			break
		}
		b.WriteRune(r)
	}
	return b.String(), nil
}

func (l *lexer) readSymbol(first rune) string {
	var b strings.Builder
	b.WriteRune(first)
	for {
		r := l.peek()
		if r == 0 || strings.ContainsRune(" \t\n\r()[]{}'", r) {
			break
		}
		b.WriteRune(l.next())
	}
	return b.String()
}

func (l *lexer) lex() []token {
	var toks []token
	for {
		l.skipWhitespace()
		r := l.next()
		switch r {
		case 0:
			toks = append(toks, token{typ: tokenEOF})
			return toks
		case '(':
			toks = append(toks, token{typ: tokenLParen})
		case ')':
			toks = append(toks, token{typ: tokenRParen})
		case '[':
			toks = append(toks, token{typ: tokenLBrack})
		case ']':
			toks = append(toks, token{typ: tokenRBrack})
		case '{':
			toks = append(toks, token{typ: tokenLBrace})
		case '}':
			toks = append(toks, token{typ: tokenRBrace})
		case '\'':
			toks = append(toks, token{typ: tokenQuote})
		case '"':
			str, err := l.readString()
			if err != nil {
				toks = append(toks, token{typ: tokenEOF})
				return toks
			}
			toks = append(toks, token{typ: tokenString, val: str})
		default:
			if r == 0 {
				toks = append(toks, token{typ: tokenEOF})
				return toks
			}
			toks = append(toks, token{typ: tokenSymbol, val: l.readSymbol(r)})
		}
	}
}

// parser --------------------------------------------------------------------

type parser struct {
	lex []token
	pos int
}

func (p *parser) peek() token {
	if p.pos >= len(p.lex) {
		return token{typ: tokenEOF}
	}
	return p.lex[p.pos]
}

func (p *parser) next() token {
	t := p.peek()
	if p.pos < len(p.lex) {
		p.pos++
	}
	return t
}

func (p *parser) parseExpr() (node, error) {
	tok := p.next()
	switch tok.typ {
	case tokenEOF:
		return node{}, fmt.Errorf("unexpected EOF")
	case tokenQuote:
		expr, err := p.parseExpr()
		if err != nil {
			return node{}, err
		}
		return node{List: []node{{Atom: "quote"}, expr}}, nil
	case tokenLParen:
		var items []node
		for p.peek().typ != tokenRParen && p.peek().typ != tokenEOF {
			n, err := p.parseExpr()
			if err != nil {
				return node{}, err
			}
			items = append(items, n)
		}
		if p.peek().typ == tokenRParen {
			p.next()
		}
		return node{List: items}, nil
	case tokenLBrack:
		var items []node
		for p.peek().typ != tokenRBrack && p.peek().typ != tokenEOF {
			n, err := p.parseExpr()
			if err != nil {
				return node{}, err
			}
			items = append(items, n)
		}
		if p.peek().typ == tokenRBrack {
			p.next()
		}
		return node{List: items}, nil
	case tokenLBrace:
		var pairs [][]node
		for p.peek().typ != tokenRBrace && p.peek().typ != tokenEOF {
			k, err := p.parseExpr()
			if err != nil {
				return node{}, err
			}
			v, err := p.parseExpr()
			if err != nil {
				return node{}, err
			}
			pairs = append(pairs, []node{k, v})
		}
		if p.peek().typ == tokenRBrace {
			p.next()
		}
		return node{Map: pairs}, nil
	case tokenString, tokenSymbol:
		return node{Atom: tok.val}, nil
	default:
		return node{}, fmt.Errorf("unexpected token: %v", tok)
	}
}
