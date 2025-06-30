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
	switch g.r.Intn(22) {
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
		if g.r.Intn(2) == 0 {
			return "for " + g.ident() + " in [" + g.expr(depth-1) + "] { " + g.statement(depth-1) + " }"
		}
		return "for " + g.ident() + " in " + g.expr(0) + ".." + g.expr(0) + " { " + g.statement(depth-1) + " }"
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
	case 14:
		return "import \"" + g.ident() + "\""
	case 15:
		return "test \"" + g.ident() + "\" { expect " + g.expr(depth-1) + " }"
	case 16:
		return "expect " + g.expr(depth-1)
	case 17:
		return "model " + g.ident() + " { provider: \"openai\" }"
	case 18:
		return "fetch " + g.expr(depth-1) + " into " + g.ident()
	case 19:
		return "on " + g.ident() + " as e { " + g.statement(depth-1) + " }"
	case 20:
		return "let " + g.ident() + " = " + g.Dataset()
	case 21:
		return "let " + g.ident() + " = " + g.Query(depth-1)
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
	switch g.r.Intn(20) {
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
	case 12:
		return g.expr(depth-1) + " && " + g.expr(depth-1)
	case 13:
		return g.expr(depth-1) + " || " + g.expr(depth-1)
	case 14:
		return g.expr(depth-1) + " == " + g.expr(depth-1)
	case 15:
		return g.expr(depth-1) + " union " + g.expr(depth-1)
	case 16:
		return "fetch \"url\""
	case 17:
		return "load \"file\" as " + g.ident()
	case 18:
		return g.Query(depth - 1)
	case 19:
		return g.Dataset()
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

// dataset generates a simple dataset expression.
func (g *Generator) Dataset() string {
	if g.r.Intn(2) == 0 {
		return g.datasetLiteral()
	}
	return "load \"file.csv\" as " + g.ident()
}

func (g *Generator) datasetLiteral() string {
	n := 1 + g.r.Intn(3)
	recs := make([]string, n)
	for i := 0; i < n; i++ {
		recs[i] = g.datasetRecord()
	}
	return "[" + strings.Join(recs, ", ") + "]"
}

func (g *Generator) datasetRecord() string {
	n := 1 + g.r.Intn(3)
	fields := make([]string, n)
	for i := 0; i < n; i++ {
		fields[i] = g.ident() + ": " + g.atom()
	}
	return "{" + strings.Join(fields, ", ") + "}"
}

func (g *Generator) Query(depth int) string {
	v := g.ident()
	q := strings.Builder{}
	q.WriteString("from " + v + " in " + g.Dataset())
	if g.r.Intn(2) == 0 {
		q.WriteString(" where " + g.expr(depth-1))
	}
	if g.r.Intn(2) == 0 {
		q.WriteString(" sort by " + g.expr(depth-1))
	}
	if g.r.Intn(2) == 0 {
		q.WriteString(" skip " + g.expr(0))
	}
	if g.r.Intn(2) == 0 {
		q.WriteString(" take " + g.expr(0))
	}
	q.WriteString(" select " + g.expr(depth-1))
	return q.String()
}
