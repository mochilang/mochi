package scheme

import (
	"fmt"
	"strings"
)

// Node represents a parsed Scheme s-expression. When Atom is empty the node
// contains a list of child nodes. Line and Col store the 1-indexed location of
// the first token and EndLine/EndCol mark the closing token for better
// diagnostics.
type Node struct {
	Atom     string
	List     []Node
	Line     int
	Col      int
	EndLine  int
	EndCol   int
	StartPos int    // byte offset of first token
	EndPos   int    // byte offset after last token
	Text     string // exact source snippet
}

// ParseError represents a syntax error with location context.
type ParseError struct {
	Line int
	Col  int
	Msg  string
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("line %d:%d: %s", e.Line, e.Col, e.Msg)
}

// String reconstructs the Scheme code represented by the node. It mirrors the
// behaviour of the internal node type and is primarily used for error messages.
func (n Node) String() string {
	if n.Atom != "" {
		return n.Atom
	}
	parts := make([]string, 0, len(n.List))
	for _, c := range n.List {
		parts = append(parts, c.String())
	}
	return "(" + strings.Join(parts, " ") + ")"
}

// SchemeItem represents a top level Scheme definition extracted from the source.
// Only a minimal subset is supported: function definitions with parameter names
// and simple variable definitions.
// Item represents a top level definition discovered in the Scheme source.
// In addition to the kind and name of the definition it optionally exposes
// the raw expression for simple variable definitions.  The expression string
// is a very small subset of Scheme and is only used for the conversion tests
// contained in this repository.
type Item struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	Params []string `json:"params,omitempty"`
	Expr   string   `json:"expr,omitempty"`
}

// ParseSchemeItems parses Scheme source and returns a slice of SchemeItem.
// The parser handles a tiny subset of Scheme syntax sufficient for the
// conversion tests.
func ParseItems(src string) ([]Item, error) {
	toks := tokenize(src)
	nodes, i, err := parseList(toks, 0)
	if err != nil {
		if pe, ok := err.(*ParseError); ok {
			pe.Msg += "\n" + snippetAround(src, pe.Line, pe.Col)
		}
		return nil, err
	}
	if i != len(toks) {
		tok := toks[i]
		return nil, &ParseError{Line: tok.line, Col: tok.col, Msg: "unexpected token"}
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
			expr := ""
			if len(n.list) >= 3 {
				expr = n.list[2].String()
			}
			items = append(items, Item{Kind: "var", Name: def.atom, Expr: expr})
		}
	}
	return items, nil
}

// Parse parses Scheme source and returns the root nodes of the AST.
func Parse(src string) ([]Node, error) {
	toks := tokenize(src)
	n, i, err := parseList(toks, 0)
	if err != nil {
		if pe, ok := err.(*ParseError); ok {
			pe.Msg += "\n" + snippetAround(src, pe.Line, pe.Col)
		}
		return nil, err
	}
	if i != len(toks) {
		tok := toks[i]
		return nil, &ParseError{Line: tok.line, Col: tok.col, Msg: "unexpected token"}
	}
	out := make([]Node, len(n))
	for i, nd := range n {
		out[i] = toPublic(nd)
	}
	return out, nil
}

type token struct {
	typ  int
	val  string
	line int
	col  int
	pos  int
	end  int
}

const (
	scTokLParen = iota
	scTokRParen
	scTokAtom
	scTokString
)

func tokenize(src string) []token {
	var toks []token
	line, col := 1, 1
	i := 0
	for i < len(src) {
		c := src[i]
		switch c {
		case ';':
			for i < len(src) && src[i] != '\n' {
				i++
				col++
			}
		case ' ', '\t':
			i++
			col++
		case '\n':
			i++
			line++
			col = 1
		case '\r':
			i++
		case '(':
			toks = append(toks, token{scTokLParen, "(", line, col, i, i + 1})
			i++
			col++
		case ')':
			toks = append(toks, token{scTokRParen, ")", line, col, i, i + 1})
			i++
			col++
		case '"':
			j := i + 1
			jcol := col + 1
			for j < len(src) {
				if src[j] == '\\' && j+1 < len(src) {
					j += 2
					jcol += 2
					continue
				}
				if src[j] == '"' {
					j++
					jcol++
					break
				}
				j++
				jcol++
			}
			if j <= len(src) {
				toks = append(toks, token{scTokString, src[i:j], line, col, i, j})
			} else {
				toks = append(toks, token{scTokString, src[i:], line, col, i, len(src)})
			}
			col = jcol
			i = j
		default:
			j := i
			jcol := col
			for j < len(src) && !strings.ContainsRune("()\n\t\r ", rune(src[j])) {
				j++
				jcol++
			}
			toks = append(toks, token{scTokAtom, src[i:j], line, col, i, j})
			col = jcol
			i = j
		}
	}
	return toks
}

type node struct {
	atom    string
	list    []node
	line    int
	col     int
	endLine int
	endCol  int
	pos     int
	end     int
}

// String reconstructs the Scheme code represented by the node. It is not
// intended to be perfectly faithful but is sufficient for the simple conversion
// logic used in tests.
func (n node) String() string {
	if n.atom != "" {
		return n.atom
	}
	parts := make([]string, 0, len(n.list))
	for _, c := range n.list {
		parts = append(parts, c.String())
	}
	return "(" + strings.Join(parts, " ") + ")"
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
			nd := node{list: lst, line: tok.line, col: tok.col, pos: tok.pos}
			if j < len(toks) {
				nd.endLine = toks[j].line
				nd.endCol = toks[j].col + 1
				nd.end = toks[j].end
				i = j + 1
			} else {
				i = j
			}
			nodes = append(nodes, nd)
		case scTokAtom, scTokString:
			nodes = append(nodes, node{atom: tok.val, line: tok.line, col: tok.col, endLine: tok.line, endCol: tok.col + len(tok.val), pos: tok.pos, end: tok.end})
			i++
		default:
			i++
		}
	}
	if start != 0 && i >= len(toks) {
		// allow unterminated list at EOF to support partial snippets
		return nodes, len(toks), nil
	}
	if start != 0 {
		tok := toks[start-1]
		return nil, i, &ParseError{Line: tok.line, Col: tok.col, Msg: "missing ')'"}
	}
	return nodes, i, nil
}

func snippetAround(src string, line, col int) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	var b strings.Builder
	for i := start; i < end; i++ {
		prefix := "  "
		if i+1 == line {
			prefix = "->"
		}
		b.WriteString(fmt.Sprintf("%s%3d| %s\n", prefix, i+1, lines[i]))
		if i+1 == line {
			if col < 1 {
				col = 1
			}
			b.WriteString("     " + strings.Repeat(" ", col-1) + "^\n")
		}
	}
	return strings.TrimRight(b.String(), "\n")
}

func toPublic(n node) Node {
	out := Node{Atom: n.atom, Line: n.line, Col: n.col, EndLine: n.endLine, EndCol: n.endCol, StartPos: n.pos, EndPos: n.end, Text: n.String()}
	if len(n.list) > 0 {
		out.List = make([]Node, len(n.list))
		for i, c := range n.list {
			out.List[i] = toPublic(c)
		}
	}
	return out
}
