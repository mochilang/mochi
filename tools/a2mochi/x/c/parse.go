//go:build slow

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
type PrintStmt struct {
	Exprs   []string
	Newline bool
}
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
type Return struct {
	Expr string
}
type FunDecl struct {
	Name   string
	Ret    string
	Params []Param
	Body   []Stmt
}

type Param struct {
	Name string
	Type string
}
type Break struct{}
type Continue struct{}

var (
	reVar    = regexp.MustCompile(`^(?:[a-zA-Z_][a-zA-Z0-9_]*|int|long\s+long|size_t|const\s+char\s*\*)\s+([a-zA-Z_][a-zA-Z0-9_]*)(?:\[\])?\s*=\s*([^;]+);`)
	reAssign = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([^;]+);`)
	reFor    = regexp.MustCompile(`^for\s*\((?:int|size_t)\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([^;]+);\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*<\s*([^;]+);.*\)\s*{`)
	reWhile  = regexp.MustCompile(`^while\s*\(([^\)]+)\)\s*{`)
	reIf     = regexp.MustCompile(`^if\s*\((.*)\)\s*{`)
	rePrintf = regexp.MustCompile(`printf\s*\((.*)\);`)
	rePuts   = regexp.MustCompile(`puts\s*\("([^\"]*)"\);`)
	reFunc   = regexp.MustCompile(`^(int|void)\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^\)]*)\)\s*{`)
)

// Parse parses a limited subset of C needed for simple tests.
func Parse(src string) (*Program, error) {
	lines := strings.Split(src, "\n")
	var body []string
	var preStmts []Stmt
	started := false
	braces := 0
	for i := 0; i < len(lines); i++ {
		ln := lines[i]
		trimmed := strings.TrimSpace(ln)
		if !started {
			if strings.Contains(ln, "main") && strings.Contains(ln, "{") {
				started = true
				braces = strings.Count(ln, "{") - strings.Count(ln, "}")
				continue
			}
			if m := reFunc.FindStringSubmatch(trimmed); m != nil {
				params := parseParams(m[3])
				i++
				body := parseBlock(lines, &i)
				preStmts = append(preStmts, FunDecl{Name: m[2], Ret: m[1], Params: params, Body: body})
				continue
			}
			if m := reVar.FindStringSubmatch(trimmed); m != nil {
				preStmts = append(preStmts, VarDecl{Name: m[1], Value: m[2]})
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
	stmts = append(preStmts, stmts...)
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
		if ln == "break;" {
			out = append(out, Break{})
			(*idx)++
			continue
		}
		if ln == "continue;" {
			out = append(out, Continue{})
			(*idx)++
			continue
		}
		if strings.HasPrefix(ln, "return") {
			expr := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(ln, "return"), ";"))
			out = append(out, Return{Expr: expr})
			(*idx)++
			continue
		}
		if m := rePuts.FindStringSubmatch(ln); m != nil {
			out = append(out, PrintStmt{Exprs: []string{strconv.Quote(m[1])}, Newline: true})
			(*idx)++
			continue
		}
		if m := rePrintf.FindStringSubmatch(ln); m != nil {
			args := parseArgs(m[1])
			newline := false
			if len(args) > 0 {
				fmtStr := strings.TrimSpace(args[0])
				if strings.HasSuffix(fmtStr, `\n"`) {
					newline = true
				}
				args = args[1:]
			}
			for i := range args {
				args[i] = strings.TrimSpace(args[i])
			}
			out = append(out, PrintStmt{Exprs: args, Newline: newline})
			(*idx)++
			continue
		}
		(*idx)++
	}
	return out
}

func parseArgs(s string) []string {
	var args []string
	depth := 0
	inStr := false
	escape := false
	start := 0
	for i, r := range s {
		if escape {
			escape = false
			continue
		}
		switch r {
		case '\\':
			if inStr {
				escape = true
			}
		case '"':
			inStr = !inStr
		case '(':
			if !inStr {
				depth++
			}
		case ')':
			if !inStr && depth > 0 {
				depth--
			}
		case ',':
			if !inStr && depth == 0 {
				args = append(args, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start <= len(s) {
		args = append(args, strings.TrimSpace(s[start:]))
	}
	return args
}

func parseParams(s string) []Param {
	s = strings.TrimSpace(s)
	s = strings.TrimPrefix(s, "(")
	s = strings.TrimSuffix(s, ")")
	if s == "" || s == "void" {
		return nil
	}
	parts := parseArgs(s)
	var out []Param
	for _, p := range parts {
		fields := strings.Fields(p)
		if len(fields) > 0 {
			name := fields[len(fields)-1]
			out = append(out, Param{Name: name, Type: fields[0]})
		}
	}
	return out
}
