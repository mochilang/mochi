package scala

import (
	"fmt"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

func program(children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: "program", Children: children}
}

func let(name string) *ast.Node { return &ast.Node{Kind: "let", Value: name} }

func param(name string) *ast.Node { return &ast.Node{Kind: "param", Value: name} }

func fun(name string, params ...string) *ast.Node {
	n := &ast.Node{Kind: "fun", Value: name}
	for _, p := range params {
		n.Children = append(n.Children, param(p))
	}
	return n
}

var lambdaRE = regexp.MustCompile(`^\(\(([^:]+)(:[^)]+)?\)\s*=>\s*(.+)\)$`)

func convertLambda(expr string) string {
	expr = strings.TrimSpace(expr)
	m := lambdaRE.FindStringSubmatch(expr)
	if len(m) == 4 {
		return fmt.Sprintf("fun (%s) => %s", strings.TrimSpace(m[1]), strings.TrimSpace(m[3]))
	}
	return expr
}

func convertIfExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "if (") {
		inner := strings.TrimPrefix(expr, "if (")
		idx := strings.Index(inner, ")")
		if idx > -1 {
			cond := strings.TrimSpace(inner[:idx])
			rest := strings.TrimSpace(inner[idx+1:])
			if strings.HasPrefix(rest, "\"") {
				end := strings.Index(rest[1:], "\"")
				if end > -1 {
					thenPart := rest[:end+2]
					rest2 := strings.TrimSpace(rest[end+2:])
					if strings.HasPrefix(rest2, "else ") {
						elsePart := strings.TrimPrefix(rest2, "else ")
						elsePart = convertIfExpr(elsePart)
						if !strings.HasPrefix(elsePart, "if ") && !strings.HasPrefix(elsePart, "\"") {
							elsePart = fmt.Sprintf("\"%s\"", strings.Trim(elsePart, "\""))
						}
						return fmt.Sprintf("if %s then %s else %s", cond, thenPart, elsePart)
					}
					return fmt.Sprintf("if %s then %s", cond, thenPart)
				}
			}
		}
	}
	return expr
}

var (
	forRangeRE = regexp.MustCompile(`^for \(([^ ]+) <- ([0-9]+) until ([0-9]+)\) {?$`)
	forCollRE  = regexp.MustCompile(`^for \(([^ ]+) <- (.+)\)\s*{?$`)
	valLineRE  = regexp.MustCompile(`^val ([^:=]+)(:[^=]+)?= (.+)$`)
	varLineRE  = regexp.MustCompile(`^var ([^:=]+)(:[^=]+)?= (.+)$`)
	whileRE    = regexp.MustCompile(`^while \((.+)\) {`)
	ifRE       = regexp.MustCompile(`^if \((.+)\) {`)
	elseIfRE   = regexp.MustCompile(`^else if \((.+)\) {`)
	arrayBufRE = regexp.MustCompile(`ArrayBuffer\(([^)]*)\)`)      // to slice
	appendOpRE = regexp.MustCompile(`([A-Za-z0-9_]+) :\+ (.+)`)    // a :+ b
	sumCallRE  = regexp.MustCompile(`([A-Za-z0-9_\[\], ]+)\.sum`)  // x.sum
	sizeCallRE = regexp.MustCompile(`([A-Za-z0-9_\[\], ]+)\.size`) // x.size
)

func convertExpr(expr string) string {
	expr = convertLambda(strings.TrimSpace(expr))
	expr = convertIfExpr(expr)
	expr = arrayBufRE.ReplaceAllString(expr, "[$1]")
	expr = appendOpRE.ReplaceAllString(expr, "append($1, $2)")
	expr = sumCallRE.ReplaceAllString(expr, "sum($1)")
	expr = sizeCallRE.ReplaceAllString(expr, "len($1)")
	return expr
}

func convertLine(line string) string {
	line = strings.TrimSpace(line)
	if strings.HasPrefix(line, "println(") {
		inner := strings.TrimSuffix(strings.TrimPrefix(line, "println("), ")")
		return "print(" + convertExpr(inner) + ")"
	}
	if strings.HasPrefix(line, "return ((") {
		return "return " + convertExpr(strings.TrimPrefix(line, "return "))
	}
	if m := forRangeRE.FindStringSubmatch(line); len(m) == 4 {
		return fmt.Sprintf("for %s in %s..%s {", m[1], m[2], m[3])
	}
	if m := forCollRE.FindStringSubmatch(line); len(m) == 3 {
		return fmt.Sprintf("for %s in %s {", m[1], m[2])
	}
	if m := valLineRE.FindStringSubmatch(line); len(m) == 4 {
		return fmt.Sprintf("let %s = %s", strings.TrimSpace(m[1]), convertExpr(m[3]))
	}
	if m := varLineRE.FindStringSubmatch(line); len(m) == 4 {
		return fmt.Sprintf("var %s = %s", strings.TrimSpace(m[1]), convertExpr(m[3]))
	}
	if m := whileRE.FindStringSubmatch(line); len(m) == 2 {
		return fmt.Sprintf("while %s {", m[1])
	}
	if m := elseIfRE.FindStringSubmatch(line); len(m) == 2 {
		return fmt.Sprintf("} else if %s {", m[1])
	}
	if m := ifRE.FindStringSubmatch(line); len(m) == 2 {
		return fmt.Sprintf("if %s {", m[1])
	}
	if line == "}" || strings.HasPrefix(line, "else {") {
		return line
	}
	return convertExpr(line)
}

// Transform converts a parsed Scala Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	var b strings.Builder
	for _, d := range p.Decls {
		switch d.Kind {
		case "val":
			rhs := convertExpr(d.RHS)
			if rhs != "" {
				fmt.Fprintf(&b, "let %s = %s\n", d.Name, rhs)
			} else {
				fmt.Fprintf(&b, "let %s\n", d.Name)
			}
		case "var":
			rhs := convertExpr(d.RHS)
			if rhs != "" {
				fmt.Fprintf(&b, "var %s = %s\n", d.Name, rhs)
			} else {
				fmt.Fprintf(&b, "var %s\n", d.Name)
			}
		case "def":
			if d.Name == "main" {
				for _, line := range strings.Split(d.Body, "\n") {
					line = convertLine(line)
					if line == "" {
						continue
					}
					b.WriteString(line + "\n")
				}
				continue
			}
			fmt.Fprintf(&b, "fun %s(", d.Name)
			for i, prm := range d.Params {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(prm.Name)
			}
			b.WriteString(") {\n")
			if d.Body != "" {
				for _, line := range strings.Split(d.Body, "\n") {
					line = convertLine(line)
					if line == "" {
						continue
					}
					b.WriteString("  " + line + "\n")
				}
			}
			b.WriteString("}\n")
		}
	}
	prog, err := parser.ParseString(b.String())
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}
