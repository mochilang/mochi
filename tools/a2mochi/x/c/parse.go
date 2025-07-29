package c

import (
	"regexp"
	"strconv"
	"strings"
)

// Program is a very simplified representation of a parsed C program.
type Program struct {
	Stmts  []Stmt
	Source string
}

type Stmt interface{}

type VarDecl struct {
	Name  string
	Value string
}
type Assign struct {
	Name string
	Expr string
}
type PrintStmt struct{ Expr string }
type While struct {
	Cond string
	Body []Stmt
}
type For struct {
	Var   string
	Start string
	End   string
	Body  []Stmt
}
type If struct {
	Cond string
	Then []Stmt
	Else []Stmt
}

var (
	reVar    = regexp.MustCompile(`^int\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([^;]+);`)
	reAssign = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([^;]+);`)
	reFor    = regexp.MustCompile(`^for\s*\(int\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([^;]+);\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*<\s*([^;]+);.*\)\s*{`)
	reWhile  = regexp.MustCompile(`^while\s*\(([^\)]+)\)\s*{`)
	reIf     = regexp.MustCompile(`^if\s*\(([^\)]+)\)\s*{`)
	rePrintf = regexp.MustCompile(`printf\s*\((.*)\);`)
	rePuts   = regexp.MustCompile(`puts\s*\("([^\"]*)"\);`)
)

// Parse parses a limited subset of C needed for simple tests.
func Parse(src string) (*Program, error) {
	lines := strings.Split(src, "\n")
	var body []string
	started := false
	braces := 0
	for _, ln := range lines {
		if !started {
			if strings.Contains(ln, "main") && strings.Contains(ln, "{") {
				started = true
				braces = strings.Count(ln, "{") - strings.Count(ln, "}")
				continue
			}
			continue
		}
		braces += strings.Count(ln, "{")
		braces -= strings.Count(ln, "}")
		if braces < 0 {
			break
		}
		body = append(body, ln)
		if braces == 0 {
			break
		}
	}
	idx := 0
	stmts := parseBlock(body, &idx)
	return &Program{Stmts: stmts, Source: src}, nil
}

func parseBlock(lines []string, idx *int) []Stmt {
	var out []Stmt
	for *idx < len(lines) {
		ln := strings.TrimSpace(lines[*idx])
		if ln == "" || strings.HasPrefix(ln, "//") {
			(*idx)++
			continue
		}
		if ln == "}" {
			(*idx)++
			break
		}
		if m := reVar.FindStringSubmatch(ln); m != nil {
			out = append(out, VarDecl{Name: m[1], Value: m[2]})
			(*idx)++
			continue
		}
		if m := reAssign.FindStringSubmatch(ln); m != nil {
			out = append(out, Assign{Name: m[1], Expr: m[2]})
			(*idx)++
			continue
		}
		if m := reWhile.FindStringSubmatch(ln); m != nil {
			(*idx)++
			body := parseBlock(lines, idx)
			out = append(out, While{Cond: m[1], Body: body})
			continue
		}
		if m := reFor.FindStringSubmatch(ln); m != nil {
			(*idx)++
			body := parseBlock(lines, idx)
			name := m[1]
			condVar := m[3]
			if condVar != name {
				condVar = name
			}
			out = append(out, For{Var: name, Start: m[2], End: m[4], Body: body})
			continue
		}
		if m := reIf.FindStringSubmatch(ln); m != nil {
			(*idx)++
			thenBody := parseBlock(lines, idx)
			var elseBody []Stmt
			if *idx < len(lines) {
				next := strings.TrimSpace(lines[*idx])
				if strings.HasPrefix(next, "else") {
					(*idx)++
					elseBody = parseBlock(lines, idx)
				}
			}
			out = append(out, If{Cond: m[1], Then: thenBody, Else: elseBody})
			continue
		}
		if m := rePuts.FindStringSubmatch(ln); m != nil {
			out = append(out, PrintStmt{Expr: strconv.Quote(m[1])})
			(*idx)++
			continue
		}
		if m := rePrintf.FindStringSubmatch(ln); m != nil {
			args := strings.Split(m[1], ",")
			if len(args) > 1 {
				arg := strings.TrimSpace(args[len(args)-1])
				out = append(out, PrintStmt{Expr: arg})
			} else {
				out = append(out, PrintStmt{Expr: args[0]})
			}
			(*idx)++
			continue
		}
		(*idx)++
	}
	return out
}
