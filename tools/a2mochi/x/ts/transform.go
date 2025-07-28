//go:build slow

package ts

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Transform converts a Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	var b strings.Builder
	for _, d := range p.Nodes {
		switch d.Kind {
		case "var":
			emitVar(&b, d)
		case "funcvar":
			emitFuncVar(&b, d)
		case "func":
			emitFunc(&b, d)
		case "enum":
			emitEnum(&b, d)
		case "type":
			emitType(&b, d)
		case "alias":
			emitAlias(&b, d)
		case "print":
			emitPrint(&b, d)
		case "expr":
			emitExpr(&b, d)
		case "forof":
			emitForOf(&b, d)
		case "forin":
			emitForIn(&b, d)
		case "for":
			emitForRange(&b, d)
		case "while":
			emitWhile(&b, d)
		}
	}
	src := b.String()
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func emitVar(b *strings.Builder, d Node) {
	b.WriteString("let ")
	b.WriteString(d.Name)
	if d.Ret != "" {
		b.WriteString(": ")
		b.WriteString(toMochiType(d.Ret))
	}
	if d.Value != "" {
		b.WriteString(" = ")
		b.WriteString(d.Value)
	}
	b.WriteByte('\n')
}

func emitFuncVar(b *strings.Builder, d Node) {
	b.WriteString("let ")
	b.WriteString(d.Name)
	b.WriteString(" = ")
	emitFuncSignature(b, d)
}

func emitFunc(b *strings.Builder, d Node) {
	b.WriteString("fun ")
	b.WriteString(d.Name)
	emitFuncSignature(b, d)
}

func emitFuncSignature(b *strings.Builder, d Node) {
	b.WriteByte('(')
	for i, p := range d.Params {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(p.Name)
		if p.Typ != "" {
			b.WriteString(": ")
			b.WriteString(toMochiType(p.Typ))
		}
	}
	b.WriteByte(')')
	if d.Ret != "" && d.Ret != "void" {
		b.WriteString(": ")
		b.WriteString(toMochiType(d.Ret))
	}
	b.WriteString(" {\n}")
	b.WriteByte('\n')
}

func emitEnum(b *strings.Builder, d Node) {
	b.WriteString("type ")
	b.WriteString(d.Name)
	b.WriteString(" {\n")
	for _, v := range d.Variants {
		b.WriteString("  ")
		b.WriteString(v)
		b.WriteByte('\n')
	}
	b.WriteString("}\n")
}

func emitType(b *strings.Builder, d Node) {
	b.WriteString("type ")
	b.WriteString(d.Name)
	b.WriteString(" {\n")
	for _, f := range d.Fields {
		b.WriteString("  ")
		b.WriteString(f.Name)
		if f.Typ != "" {
			b.WriteString(": ")
			b.WriteString(toMochiType(f.Typ))
		}
		b.WriteByte('\n')
	}
	b.WriteString("}\n")
}

func emitAlias(b *strings.Builder, d Node) {
	b.WriteString("type ")
	b.WriteString(d.Name)
	b.WriteString(" = ")
	b.WriteString(toMochiType(d.Alias))
	b.WriteByte('\n')
}

func emitPrint(b *strings.Builder, d Node) {
	b.WriteString("print(")
	b.WriteString(d.Body)
	b.WriteString(")\n")
}

func emitExpr(b *strings.Builder, d Node) {
	b.WriteString(convertExpr(d.Expr))
	b.WriteByte('\n')
}

func emitForOf(b *strings.Builder, d Node) {
	body := replaceConsoleLogs(d.Body)
	list := replaceObjectValues(d.List)
	fmt.Fprintf(b, "for %s in %s {%s}\n", d.Iter, list, body)
}

func emitForIn(b *strings.Builder, d Node) {
	body := replaceConsoleLogs(d.Body)
	fmt.Fprintf(b, "for %s in %s {%s}\n", d.Iter, d.List, body)
}

func emitForRange(b *strings.Builder, d Node) {
	body := replaceConsoleLogs(d.Body)
	fmt.Fprintf(b, "for %s in %s..%s {%s}\n", d.Iter, d.StartVal, d.EndVal, body)
}

func emitWhile(b *strings.Builder, d Node) {
	body := replaceConsoleLogs(d.Body)
	fmt.Fprintf(b, "while %s {%s}\n", d.Cond, body)
}

// --- Helpers copied from archived any2mochi ---

func findMatch(s string, openIdx int, open, close rune) int {
	depth := 0
	for i, r := range s[openIdx:] {
		if r == open {
			depth++
		} else if r == close {
			depth--
			if depth == 0 {
				return openIdx + i
			}
		}
	}
	return len(s)
}

func parseFilterMap(expr, lhs string) []string {
	filterIdx := strings.Index(expr, ".filter(")
	mapIdx := strings.Index(expr, ".map(")
	if filterIdx == -1 || mapIdx == -1 || mapIdx < filterIdx {
		return nil
	}
	list := strings.TrimSpace(expr[:filterIdx])
	fStart := filterIdx + len(".filter(")
	fEnd := findMatch(expr, fStart-1, '(', ')')
	if fEnd <= fStart {
		return nil
	}
	fPart := expr[fStart:fEnd]
	arrow := strings.Index(fPart, "=>")
	if arrow == -1 {
		return nil
	}
	iter := strings.TrimSpace(strings.Trim(fPart[:arrow], "() "))
	cond := strings.TrimSpace(strings.Trim(fPart[arrow+2:], "() "))
	mStart := mapIdx + len(".map(")
	mEnd := findMatch(expr, mStart-1, '(', ')')
	if mEnd <= mStart {
		return nil
	}
	mPart := expr[mStart:mEnd]
	arrow = strings.Index(mPart, "=>")
	if arrow == -1 {
		return nil
	}
	iter2 := strings.TrimSpace(strings.Trim(mPart[:arrow], "() "))
	body := strings.TrimSpace(strings.Trim(mPart[arrow+2:], "() "))
	if iter2 != "" {
		iter = iter2
	}
	var out []string
	out = append(out, lhs+" = from "+iter+" in "+list)
	out = append(out, "             where "+cond)
	out = append(out, "             select "+body)
	return out
}

func replaceObjectValues(s string) string {
	const prefix = "Object.values("
	if strings.HasPrefix(s, prefix) {
		return "values(" + s[len(prefix):]
	}
	return s
}

func replaceConsoleLogs(body string) string {
	prefix := "console.log(String("
	for {
		idx := strings.Index(body, prefix)
		if idx == -1 {
			break
		}
		end := findMatch(body, idx+len(prefix)-1, '(', ')')
		if end == len(body) {
			break
		}
		expr := body[idx+len(prefix) : end]
		tail := end + 1
		if strings.HasPrefix(body[tail:], ");") {
			tail += 2
		}
		if tail < len(body) && body[tail] == ';' {
			tail++
		}
		body = body[:idx] + "print(" + expr + ")" + body[tail:]
	}
	return body
}

func convertExpr(expr string) string {
	if strings.HasPrefix(expr, "console.log(") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "console.log("), ")")
		if strings.HasPrefix(inner, "String(") && strings.HasSuffix(inner, ")") {
			inner = inner[len("String(") : len(inner)-1]
		}
		return "print(" + inner + ")"
	}
	if assignIdx := strings.Index(expr, "="); assignIdx > 0 {
		lhs := strings.TrimSpace(expr[:assignIdx])
		rhs := strings.TrimSpace(expr[assignIdx+1:])
		if q := parseFilterMap(rhs, lhs); q != nil {
			return strings.Join(q, "\n")
		}
	}
	return expr
}

func toMochiType(t string) string {
	t = strings.TrimSpace(t)
	if strings.Contains(t, "|") {
		parts := strings.Split(t, "|")
		filtered := make([]string, 0, len(parts))
		for _, p := range parts {
			p = strings.TrimSpace(p)
			if p == "null" || p == "undefined" {
				continue
			}
			mp := toMochiType(p)
			if mp != "" {
				filtered = append(filtered, mp)
			}
		}
		if len(filtered) == 1 {
			return filtered[0]
		}
		if len(filtered) > 1 {
			return "any"
		}
		return ""
	}
	switch t {
	case "", "any", "unknown", "object":
		return "any"
	case "number":
		return "int"
	case "string":
		return "string"
	case "boolean":
		return "bool"
	case "void", "undefined", "null":
		return ""
	}
	if strings.HasSuffix(t, "[]") {
		inner := toMochiType(t[:len(t)-2])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Array<") && strings.HasSuffix(t, ">") {
		inner := toMochiType(t[6 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Record<") && strings.HasSuffix(t, ">") {
		inner := t[7 : len(t)-1]
		comma := strings.Index(inner, ",")
		var k, v string
		if comma >= 0 {
			k = strings.TrimSpace(inner[:comma])
			v = strings.TrimSpace(inner[comma+1:])
		} else {
			k = strings.TrimSpace(inner)
		}
		key := toMochiType(k)
		val := toMochiType(v)
		if key == "" {
			key = "any"
		}
		if val == "" {
			val = "any"
		}
		return "map<" + key + "," + val + ">"
	}
	return t
}
