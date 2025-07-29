//go:build slow

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

func convertType(t string) string {
	t = strings.TrimSpace(strings.TrimPrefix(t, ":"))
	if strings.Contains(t, "[") {
		return ""
	}
	switch t {
	case "Int":
		return "int"
	case "String":
		return "string"
	case "Boolean", "Bool":
		return "bool"
	default:
		return strings.ToLower(t)
	}
}

var (
	forRangeRE     = regexp.MustCompile(`^for \(([^ ]+) <- (.+) until (.+)\)\s*{?$`)
	forRangeToRE   = regexp.MustCompile(`^for \(([^ ]+) <- (.+) to (.+)\)\s*{?$`)
	forCollRE      = regexp.MustCompile(`^for \(([^ ]+) <- (.+)\)\s*{?$`)
	valLineRE      = regexp.MustCompile(`^val ([^:=]+)(:[^=]+)?= (.+)$`)
	varLineRE      = regexp.MustCompile(`^var ([^:=]+)(:[^=]+)?= (.+)$`)
	indexSetRE     = regexp.MustCompile(`^([A-Za-z0-9_]+)\(([^)]+)\) = (.+)$`)
	whileRE        = regexp.MustCompile(`^while \((.+)\) {`)
	ifRE           = regexp.MustCompile(`^if \((.+)\) {`)
	elseIfRE       = regexp.MustCompile(`^else if \((.+)\) {`)
	arrayBufRE     = regexp.MustCompile(`ArrayBuffer\(([^)]*)\)`) // to slice
	listMkStrRE    = regexp.MustCompile(`List\(([^)]*)\)\.mkString\(([^)]*)\)`)
	appendOpRE     = regexp.MustCompile(`([A-Za-z0-9_]+) :\+ (.+)`)   // a :+ b
	sumCallRE      = regexp.MustCompile(`([A-Za-z0-9_\[\], ]+)\.sum`) // x.sum
	sizeCallRE     = regexp.MustCompile(`(\[[^\]]+\]|[A-Za-z0-9_\.]+)\.size`)
	mapCallRE      = regexp.MustCompile(`([A-Za-z0-9_\[\].]+)\.map\(([^)]+)\)`) // x.map(f)
	mapLitRE       = regexp.MustCompile(`Map\(([^)]*)\)`)
	nonEmptyRE     = regexp.MustCompile(`(.+)\.nonEmpty$`)
	keysCallRE     = regexp.MustCompile(`([A-Za-z0-9_]+)\.keys`)
	dropCallRE     = regexp.MustCompile(`([A-Za-z0-9_\[\].]+)\.drop\(([^)]+)\)`)
	takeCallRE     = regexp.MustCompile(`([A-Za-z0-9_\[\].]+)\.take\(([^)]+)\)`)
	sortByCallRE   = regexp.MustCompile(`([A-Za-z0-9_\[\].]+)\.sortBy\(([^)]+)\)`)
	containsCallRE = regexp.MustCompile(`([A-Za-z0-9_\[\].]+)\.contains\(([^)]+)\)`)
	distinctCallRE = regexp.MustCompile(`([A-Za-z0-9_\[\].]+)\.distinct`)
	filterCallRE   = regexp.MustCompile(`([A-Za-z0-9_\[\].]+)\.filter\(([^)]+)\)`)
	unionCallRE    = regexp.MustCompile(`([A-Za-z0-9_\[\].]+) \+\+ ([A-Za-z0-9_\[\].]+)`)
	indexCallRE    = regexp.MustCompile(`^([A-Za-z_][A-Za-z0-9_]*)\(([^,()]+)\)$`)
)

var indexDisallow = map[string]bool{
	"len":      true,
	"sum":      true,
	"map":      true,
	"append":   true,
	"join":     true,
	"sort_by":  true,
	"take":     true,
	"drop":     true,
	"contains": true,
	"filter":   true,
	"concat":   true,
	"unique":   true,
	"keys":     true,
}

func convertExpr(expr string) string {
	expr = convertLambda(strings.TrimSpace(expr))
	expr = convertIfExpr(expr)
	expr = arrayBufRE.ReplaceAllString(expr, "[$1]")
	expr = listMkStrRE.ReplaceAllString(expr, "join([$1], $2)")
	expr = appendOpRE.ReplaceAllString(expr, "append($1, $2)")
	expr = sumCallRE.ReplaceAllString(expr, "sum($1)")
	expr = sizeCallRE.ReplaceAllStringFunc(expr, func(s string) string {
		m := sizeCallRE.FindStringSubmatch(s)
		return "len(" + strings.TrimSpace(m[1]) + ")"
	})
	expr = mapCallRE.ReplaceAllString(expr, "map($1, $2)")
	expr = mapLitRE.ReplaceAllStringFunc(expr, func(s string) string {
		inner := mapLitRE.FindStringSubmatch(s)[1]
		pairs := strings.Split(inner, ",")
		for i, p := range pairs {
			kv := strings.SplitN(strings.TrimSpace(p), "->", 2)
			if len(kv) == 2 {
				pairs[i] = fmt.Sprintf("%s: %s", strings.TrimSpace(kv[0]), strings.TrimSpace(kv[1]))
			} else {
				pairs[i] = strings.TrimSpace(p)
			}
		}
		return "{" + strings.Join(pairs, ", ") + "}"
	})
	expr = keysCallRE.ReplaceAllString(expr, "keys($1)")
	expr = dropCallRE.ReplaceAllString(expr, "drop($1, $2)")
	expr = takeCallRE.ReplaceAllString(expr, "take($1, $2)")
	expr = sortByCallRE.ReplaceAllString(expr, "sort_by($1, $2)")
	expr = containsCallRE.ReplaceAllString(expr, "contains($1, $2)")
	expr = distinctCallRE.ReplaceAllString(expr, "unique($1)")
	expr = filterCallRE.ReplaceAllString(expr, "filter($1, $2)")
	expr = unionCallRE.ReplaceAllString(expr, "concat($1, $2)")
	if m := nonEmptyRE.FindStringSubmatch(expr); len(m) == 2 {
		expr = fmt.Sprintf("len(%s) > 0", m[1])
	}
	if m := indexCallRE.FindStringSubmatch(expr); len(m) == 3 {
		if !indexDisallow[m[1]] {
			expr = fmt.Sprintf("%s[%s]", m[1], m[2])
		}
	}
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
	if m := forRangeToRE.FindStringSubmatch(line); len(m) == 4 {
		return fmt.Sprintf("for %s in %s..%s {", m[1], m[2], m[3])
	}
	if m := forCollRE.FindStringSubmatch(line); len(m) == 3 {
		return fmt.Sprintf("for %s in %s {", m[1], convertExpr(m[2]))
	}
	if m := indexSetRE.FindStringSubmatch(line); len(m) == 4 {
		return fmt.Sprintf("%s[%s] = %s", m[1], m[2], convertExpr(m[3]))
	}
	if m := valLineRE.FindStringSubmatch(line); len(m) == 4 {
		typ := convertType(m[2])
		rhs := convertExpr(m[3])
		name := strings.TrimSpace(m[1])
		if typ != "" {
			if rhs == "0" || rhs == "false" || rhs == "\"\"" {
				return fmt.Sprintf("let %s: %s", name, typ)
			}
			if rhs != "" {
				return fmt.Sprintf("let %s: %s = %s", name, typ, rhs)
			}
			return fmt.Sprintf("let %s: %s", name, typ)
		}
		return fmt.Sprintf("let %s = %s", name, rhs)
	}
	if m := varLineRE.FindStringSubmatch(line); len(m) == 4 {
		typ := convertType(m[2])
		rhs := convertExpr(m[3])
		name := strings.TrimSpace(m[1])
		if typ != "" {
			if rhs == "0" || rhs == "false" || rhs == "\"\"" {
				return fmt.Sprintf("var %s: %s", name, typ)
			}
			if rhs != "" {
				return fmt.Sprintf("var %s: %s = %s", name, typ, rhs)
			}
			return fmt.Sprintf("var %s: %s", name, typ)
		}
		return fmt.Sprintf("var %s = %s", name, rhs)
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
			typ := convertType(d.Ret)
			if typ != "" {
				if rhs == "0" || rhs == "false" || rhs == "\"\"" {
					fmt.Fprintf(&b, "let %s: %s\n", d.Name, typ)
				} else if rhs != "" {
					fmt.Fprintf(&b, "let %s: %s = %s\n", d.Name, typ, rhs)
				} else {
					fmt.Fprintf(&b, "let %s: %s\n", d.Name, typ)
				}
			} else {
				if rhs != "" {
					fmt.Fprintf(&b, "let %s = %s\n", d.Name, rhs)
				} else {
					fmt.Fprintf(&b, "let %s\n", d.Name)
				}
			}
		case "var":
			rhs := convertExpr(d.RHS)
			typ := convertType(d.Ret)
			if typ != "" {
				if rhs == "0" || rhs == "false" || rhs == "\"\"" {
					fmt.Fprintf(&b, "var %s: %s\n", d.Name, typ)
				} else if rhs != "" {
					fmt.Fprintf(&b, "var %s: %s = %s\n", d.Name, typ, rhs)
				} else {
					fmt.Fprintf(&b, "var %s: %s\n", d.Name, typ)
				}
			} else {
				if rhs != "" {
					fmt.Fprintf(&b, "var %s = %s\n", d.Name, rhs)
				} else {
					fmt.Fprintf(&b, "var %s\n", d.Name)
				}
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
		case "caseclass":
			fmt.Fprintf(&b, "type %s {\n", d.Name)
			for _, f := range d.Fields {
				fmt.Fprintf(&b, "  %s\n", f.Name)
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
