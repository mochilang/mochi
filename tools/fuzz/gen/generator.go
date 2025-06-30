package gen

import (
	"math/rand"
	"strings"
)

// Generator produces random Mochi programs.
type Generator struct {
	r *rand.Rand
}

// New returns a new generator seeded with r.
func New(r *rand.Rand) *Generator {
	return &Generator{r: r}
}

// Program returns a randomly generated program string with the given depth.
// Increasing depth yields larger programs.
func (g *Generator) Program(depth int) string {
	if depth <= 0 {
		depth = 1
	}
	n := 1 + g.r.Intn(depth+1)
	stmts := make([]string, n)
	for i := 0; i < n; i++ {
		stmts[i] = g.statement(depth)
	}
	return strings.Join(stmts, "\n")
}

func (g *Generator) statement(depth int) string {
	if depth <= 0 {
		return g.simpleStmt()
	}
	switch g.r.Intn(14) {
	case 0:
		return "let " + g.ident() + " = " + g.expr(depth-1)
	case 1:
		return "var " + g.ident() + " = " + g.expr(depth-1)
	case 2:
		return g.ident() + " = " + g.expr(depth-1)
	case 3:
		return "fun " + g.ident() + "(" + g.ident() + ": int) { return " + g.expr(depth-1) + " }"
	case 4:
		return "return " + g.expr(depth-1)
	case 5:
		return "if " + g.expr(depth-1) + " { " + g.statement(depth-1) + " } else { " + g.statement(depth-1) + " }"
	case 6:
		return "while " + g.expr(depth-1) + " { " + g.statement(depth-1) + " }"
	case 7:
		return "for " + g.ident() + " in [" + g.expr(depth-1) + "] { " + g.statement(depth-1) + " }"
	case 8:
		return "break"
	case 9:
		return "continue"
	case 10:
		return "type " + g.ident() + " { " + g.ident() + ": int }"
	case 11:
		return "stream " + g.ident() + " { " + g.ident() + ": int }"
	case 12:
		return "emit " + g.ident() + " { " + g.ident() + ": " + g.expr(depth-1) + " }"
	case 13:
		return "agent " + g.ident() + " { intent " + g.ident() + "() { return " + g.expr(depth-1) + " } }"
	default:
		return g.simpleStmt()
	}
}

func (g *Generator) simpleStmt() string {
	return "print(" + g.expr(0) + ")"
}

func (g *Generator) expr(depth int) string {
	if depth <= 0 {
		return g.atom()
	}
	switch g.r.Intn(12) {
	case 0:
		return g.expr(depth-1) + " + " + g.expr(depth-1)
	case 1:
		return "-" + g.expr(depth-1)
	case 2:
		return g.ident() + "(" + g.expr(depth-1) + ")"
	case 3:
		return g.ident() + "." + g.ident()
	case 4:
		return "[" + g.expr(depth-1) + ", " + g.expr(depth-1) + "]"
	case 5:
		return "{" + g.ident() + ": " + g.expr(depth-1) + "}"
	case 6:
		return "fun(" + g.ident() + ": int) { return " + g.expr(depth-1) + " }"
	case 7:
		return g.expr(depth-1) + "[" + g.expr(0) + "]"
	case 8:
		return g.expr(depth-1) + " as int"
	case 9:
		return "from " + g.ident() + " in " + g.expr(depth-1) + " select " + g.expr(depth-1)
	case 10:
		return "match " + g.expr(depth-1) + " { " + g.expr(depth-1) + " => " + g.expr(depth-1) + " }"
	case 11:
		return "generate text { prompt: " + g.expr(depth-1) + " }"
	default:
		return g.atom()
	}
}

func (g *Generator) atom() string {
	switch g.r.Intn(4) {
	case 0:
		return "1"
	case 1:
		return "true"
	case 2:
		return "\"str\""
	default:
		return g.ident()
	}
}

func (g *Generator) ident() string {
	letters := []rune("abcdefghijklmnopqrstuvwxyz")
	l := 1 + g.r.Intn(3)
	b := make([]rune, l)
	for i := range b {
		b[i] = letters[g.r.Intn(len(letters))]
	}
	return string(b)
}
