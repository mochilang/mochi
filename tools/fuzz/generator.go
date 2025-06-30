package fuzz

import (
	"fmt"
	"strconv"
	"strings"

	part "github.com/alecthomas/participle/v2/ebnf"
	"mochi/parser"
)

// Generator systematically enumerates small Mochi programs by
// walking the parser grammar. Each call to Next returns the source
// for one program until exhaustion.
type Generator struct {
	programs []string
	idx      int
}

// NewGenerator parses the Mochi grammar and expands the Statement rule
// to produce a corpus of example programs covering most AST nodes.
func NewGenerator() *Generator {
	g := &grammarGen{rules: loadGrammar()}
	stmts := g.expandRule("Statement", 0)
	programs := make([]string, len(stmts))
	for i, s := range stmts {
		programs[i] = strings.TrimSpace(s)
	}
	return &Generator{programs: programs}
}

// QueryGenerator enumerates query expressions using the parser grammar.
// Each item returned is a standalone query expression string.
type QueryGenerator struct {
	queries []string
	idx     int
}

// NewQueryGenerator expands the `QueryExpr` rule from the grammar to build
// a corpus of query strings.
func NewQueryGenerator() *QueryGenerator {
	g := &grammarGen{rules: loadGrammar()}
	qs := g.expandRule("QueryExpr", 0)
	out := make([]string, len(qs))
	for i, q := range qs {
		out[i] = strings.TrimSpace(q)
	}
	return &QueryGenerator{queries: out}
}

// Next returns the next query string. ok is false once exhausted.
func (q *QueryGenerator) Next() (string, bool) {
	if q.idx >= len(q.queries) {
		return "", false
	}
	s := q.queries[q.idx]
	q.idx++
	return s, true
}

// Next returns the next program source. ok is false when all programs
// have been exhausted.
func (g *Generator) Next() (src string, ok bool) {
	if g.idx >= len(g.programs) {
		return "", false
	}
	src = g.programs[g.idx]
	g.idx++
	return src, true
}

// --- grammar based generation ---

// grammarGen recursively expands EBNF productions.
type grammarGen struct {
	rules map[string]*part.Expression
	ident int
	num   int
}

func loadGrammar() map[string]*part.Expression {
	eb, _ := part.ParseString(parser.Parser.String())
	m := map[string]*part.Expression{}
	for _, p := range eb.Productions {
		m[p.Production] = p.Expression
	}
	return m
}

func (g *grammarGen) expandRule(name string, depth int) []string {
	expr := g.rules[name]
	if expr == nil || depth > 3 {
		return []string{""}
	}
	return g.expandExpr(expr, depth)
}

func (g *grammarGen) expandExpr(expr *part.Expression, depth int) []string {
	var out []string
	for _, seq := range expr.Alternatives {
		out = append(out, g.expandSeq(seq, depth+1)...)
	}
	return out
}

func (g *grammarGen) expandSeq(seq *part.Sequence, depth int) []string {
	res := []string{""}
	for _, term := range seq.Terms {
		t := g.expandTerm(term, depth+1)
		res = cross(res, t)
	}
	return res
}

func (g *grammarGen) expandTerm(term *part.Term, depth int) []string {
	base := []string{""}
	switch {
	case term.Name != "":
		base = g.expandRule(term.Name, depth+1)
	case term.Literal != "":
		base = []string{term.Literal}
	case term.Token != "":
		tok := strings.Trim(term.Token, "<>")
		base = []string{g.token(tok)}
	case term.Group != nil:
		base = g.expandExpr(term.Group.Expr, depth+1)
	}

	switch term.Repetition {
	case "?":
		return append(base, "")
	case "*":
		return append([]string{""}, base...)
	default:
		return base
	}
}

func (g *grammarGen) token(tok string) string {
	switch tok {
	case "ident":
		g.ident++
		return fmt.Sprintf("v%d", g.ident)
	case "int":
		g.num++
		return strconv.Itoa(g.num)
	case "float":
		return "1.0"
	case "string":
		g.ident++
		return fmt.Sprintf("\"s%d\"", g.ident)
	default:
		return tok
	}
}

func cross(a, b []string) []string {
	var out []string
	for _, x := range a {
		for _, y := range b {
			xs := strings.TrimSpace(x)
			ys := strings.TrimSpace(y)
			switch {
			case xs == "" && ys == "":
				out = append(out, "")
			case xs == "":
				out = append(out, ys)
			case ys == "":
				out = append(out, xs)
			default:
				out = append(out, xs+" "+ys)
			}
		}
	}
	return out
}
