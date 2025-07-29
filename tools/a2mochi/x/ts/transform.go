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
	root := &ast.Node{Kind: "program"}
	for _, d := range p.Nodes {
		st, err := nodeFromDecl(d)
		if err != nil {
			return nil, err
		}
		if st != nil {
			root.Children = append(root.Children, st)
		}
	}
	return root, nil
}

func nodeFromDecl(d Node) (*ast.Node, error) {
	var src string
	switch d.Kind {
	case "var":
		src = emitVar(d)
	case "funcvar":
		src = emitFuncVar(d)
	case "func":
		src = emitFunc(d)
	case "enum":
		src = emitEnum(d)
	case "type":
		src = emitType(d)
	case "alias":
		src = emitAlias(d)
	case "print":
		src = emitPrint(d)
	case "expr":
		src = emitExpr(d)
	case "return":
		src = emitReturn(d)
	case "break":
		src = "break\n"
	case "continue":
		src = "continue\n"
	case "forof":
		src = emitForOf(d)
	case "forin":
		src = emitForIn(d)
	case "for":
		src = emitForRange(d)
	case "while":
		src = emitWhile(d)
	case "do":
		src = emitDoWhile(d)
	case "if":
		src = emitIf(d)
	}
	if src == "" {
		return nil, nil
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	if len(prog.Statements) == 0 {
		return nil, nil
	}
	return ast.FromStatement(prog.Statements[0]), nil
}

func emitVar(d Node) string {
	var sb strings.Builder
	sb.WriteString("var ")
	sb.WriteString(d.Name)
	if d.Ret != "" {
		sb.WriteString(": ")
		sb.WriteString(toMochiType(d.Ret))
	}
	if d.Value != "" {
		sb.WriteString(" = ")
		sb.WriteString(d.Value)
	}
	sb.WriteByte('\n')
	return sb.String()
}

func emitFuncVar(d Node) string {
	var sb strings.Builder
	sb.WriteString("let ")
	sb.WriteString(d.Name)
	sb.WriteString(" = ")
	emitFuncSignature(&sb, d)
	return sb.String()
}

func emitFunc(d Node) string {
	var sb strings.Builder
	sb.WriteString("fun ")
	sb.WriteString(d.Name)
	emitFuncSignature(&sb, d)
	return sb.String()
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
	b.WriteString(" {\n")
	if len(d.BodyNodes) > 0 {
		b.WriteString(emitNodes(d.BodyNodes))
	}
	b.WriteString("}\n")
	b.WriteByte('\n')
}

func emitEnum(d Node) string {
	var sb strings.Builder
	sb.WriteString("type ")
	sb.WriteString(d.Name)
	sb.WriteString(" {\n")
	for _, v := range d.Variants {
		sb.WriteString("  ")
		sb.WriteString(v)
		sb.WriteByte('\n')
	}
	sb.WriteString("}\n")
	return sb.String()
}

func emitType(d Node) string {
	var sb strings.Builder
	sb.WriteString("type ")
	sb.WriteString(d.Name)
	sb.WriteString(" {\n")
	for _, f := range d.Fields {
		sb.WriteString("  ")
		sb.WriteString(f.Name)
		if f.Typ != "" {
			sb.WriteString(": ")
			sb.WriteString(toMochiType(f.Typ))
		}
		sb.WriteByte('\n')
	}
	sb.WriteString("}\n")
	return sb.String()
}

func emitAlias(d Node) string {
	var sb strings.Builder
	sb.WriteString("type ")
	sb.WriteString(d.Name)
	sb.WriteString(" = ")
	sb.WriteString(toMochiType(d.Alias))
	sb.WriteByte('\n')
	return sb.String()
}

func emitPrint(d Node) string {
	return fmt.Sprintf("print(%s)\n", d.Body)
}

func emitExpr(d Node) string {
	return convertExpr(d.Expr) + "\n"
}

func emitReturn(d Node) string {
	if d.Expr != "" {
		return fmt.Sprintf("return %s\n", convertExpr(d.Expr))
	}
	return "return\n"
}

func emitForOf(d Node) string {
	var body string
	if len(d.BodyNodes) > 0 {
		body = emitNodes(d.BodyNodes)
	} else {
		body = replaceConsoleLogs(d.Body)
	}
	list := replaceObjectValues(d.List)
	return fmt.Sprintf("for %s in %s {%s}\n", d.Iter, list, body)
}

func emitForIn(d Node) string {
	var body string
	if len(d.BodyNodes) > 0 {
		body = emitNodes(d.BodyNodes)
	} else {
		body = replaceConsoleLogs(d.Body)
	}
	return fmt.Sprintf("for %s in %s {%s}\n", d.Iter, d.List, body)
}

func emitForRange(d Node) string {
	var body string
	if len(d.BodyNodes) > 0 {
		body = emitNodes(d.BodyNodes)
	} else {
		body = replaceConsoleLogs(d.Body)
	}
	return fmt.Sprintf("for %s in %s..%s {%s}\n", d.Iter, d.StartVal, d.EndVal, body)
}

func emitWhile(d Node) string {
	var body string
	if len(d.BodyNodes) > 0 {
		body = emitNodes(d.BodyNodes)
	} else {
		body = replaceConsoleLogs(d.Body)
	}
	return fmt.Sprintf("while %s {%s}\n", d.Cond, body)
}

func emitDoWhile(d Node) string {
	var body string
	if len(d.BodyNodes) > 0 {
		body = emitNodes(d.BodyNodes)
	} else {
		body = replaceConsoleLogs(d.Body)
	}
	return fmt.Sprintf("do {%s} while %s\n", body, d.Cond)
}

func emitIf(d Node) string {
	var thenBody, elseBody string
	if len(d.BodyNodes) > 0 {
		thenBody = emitNodes(d.BodyNodes)
	} else {
		thenBody = replaceConsoleLogs(d.Body)
	}
	if len(d.ElseNodes) > 0 {
		elseBody = emitNodes(d.ElseNodes)
	} else {
		elseBody = replaceConsoleLogs(d.Else)
	}
	if d.Else != "" {
		return fmt.Sprintf("if %s {%s} else {%s}\n", d.Cond, thenBody, elseBody)
	}
	return fmt.Sprintf("if %s {%s}\n", d.Cond, thenBody)
}

func emitNode(d Node) string {
	switch d.Kind {
	case "var":
		return emitVar(d)
	case "funcvar":
		return emitFuncVar(d)
	case "func":
		return emitFunc(d)
	case "enum":
		return emitEnum(d)
	case "type":
		return emitType(d)
	case "alias":
		return emitAlias(d)
	case "print":
		return emitPrint(d)
	case "expr":
		return emitExpr(d)
	case "return":
		return emitReturn(d)
	case "break":
		return "break\n"
	case "continue":
		return "continue\n"
	case "forof":
		return emitForOf(d)
	case "forin":
		return emitForIn(d)
	case "for":
		return emitForRange(d)
	case "while":
		return emitWhile(d)
	case "do":
		return emitDoWhile(d)
	case "if":
		return emitIf(d)
	}
	return ""
}

func emitNodes(nodes []Node) string {
	var sb strings.Builder
	for _, n := range nodes {
		sb.WriteString(emitNode(n))
	}
	return sb.String()
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
	prefix := "console.log("
	for {
		idx := strings.Index(body, prefix)
		if idx == -1 {
			break
		}
		end := findMatch(body, idx+len(prefix)-1, '(', ')')
		if end == len(body) {
			break
		}
		expr := body[idx : end+1]
		tail := end + 1
		if strings.HasPrefix(body[tail:], ".trim()") {
			expr += ".trim()"
			tail += len(".trim()")
		}
		if strings.HasPrefix(body[tail:], ");") {
			tail += 2
		}
		if tail < len(body) && body[tail] == ';' {
			tail++
		}
		body = body[:idx] + convertConsole(expr) + body[tail:]
	}
	return body
}

func convertConsole(expr string) string {
	if !strings.HasPrefix(expr, "console.log(") {
		return expr
	}
	inner := strings.TrimSuffix(strings.TrimPrefix(expr, "console.log("), ")")
	postfix := ""
	if strings.HasSuffix(inner, ".trim()") {
		inner = strings.TrimSuffix(inner, ".trim()")
		postfix = ".trim()"
	}
	inner = strings.TrimSpace(inner)
	if strings.HasPrefix(inner, "(") && strings.HasSuffix(inner, ")") {
		inner = strings.TrimSuffix(strings.TrimPrefix(inner, "("), ")")
	}
	if strings.HasPrefix(inner, "String(") && strings.HasSuffix(inner, ")") {
		inner = inner[len("String(") : len(inner)-1]
	}
	inner = removeStringCasts(inner)
	if strings.HasPrefix(inner, "\"[\"") && strings.Contains(inner, "]).join") && strings.HasSuffix(inner, "]\"") {
		start := strings.Index(inner, "[...")
		end := strings.Index(inner, "]")
		if start >= 0 && end > start {
			part := inner[start+4 : end]
			pieces := strings.SplitN(part, ",", 2)
			if len(pieces) == 2 {
				list := strings.TrimSpace(pieces[0])
				val := strings.TrimSpace(pieces[1])
				conv := fmt.Sprintf("append(%s, %s)", list, val)
				if postfix != "" {
					return "print((" + conv + ")" + postfix + ")"
				}
				return "print(" + conv + ")"
			}
		}
	}
	inner = convertExprSimple(inner)
	if postfix != "" {
		return "print((" + inner + ")" + postfix + ")"
	}
	return "print(" + inner + ")"
}

func removeStringCasts(s string) string {
	for {
		idx := strings.Index(s, "String(")
		if idx == -1 {
			break
		}
		end := findMatch(s, idx+len("String(")-1, '(', ')')
		if end == len(s) {
			break
		}
		inner := s[idx+len("String(") : end]
		s = s[:idx] + inner + s[end+1:]
	}
	return s
}

func convertExpr(expr string) string {
	if strings.HasPrefix(expr, "console.log(") {
		return convertConsole(expr)
	}
	if assignIdx := strings.Index(expr, "="); assignIdx > 0 {
		lhs := strings.TrimSpace(expr[:assignIdx])
		rhs := strings.TrimSpace(expr[assignIdx+1:])
		if q := parseFilterMap(rhs, lhs); q != nil {
			return strings.Join(q, "\n")
		}
	}
	return convertExprSimple(expr)
}

func convertExprSimple(expr string) string {
	expr = strings.TrimSpace(expr)
	expr = removeStringCasts(expr)
	expr = strings.ReplaceAll(expr, "'undefined'", "\"undefined\"")
	if strings.HasPrefix(expr, "\"[\"") && strings.Contains(expr, "].join") && strings.HasSuffix(expr, "]\"") {
		start := strings.Index(expr, "[...")
		end := strings.Index(expr, "]")
		if start >= 0 && end > start {
			inner := expr[start+4 : end]
			parts := strings.SplitN(inner, ",", 2)
			if len(parts) == 2 {
				list := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				return fmt.Sprintf("append(%s, %s)", list, val)
			}
		}
	}
	if strings.HasPrefix(expr, "[...") && strings.HasSuffix(expr, "]") {
		inner := strings.TrimSpace(expr[1 : len(expr)-1])
		parts := strings.SplitN(inner, ",", 2)
		if len(parts) == 2 {
			head := strings.TrimSpace(parts[0])
			if strings.HasPrefix(head, "...") {
				list := strings.TrimSpace(head[3:])
				tail := strings.TrimSpace(parts[1])
				if list != "" && tail != "" {
					return fmt.Sprintf("append(%s, %s)", list, tail)
				}
			}
		}
	}
	if strings.HasPrefix(expr, "+") {
		inner := strings.TrimSpace(expr[1:])
		// ignore unary plus on boolean expressions
		return convertExprSimple(inner)
	}
	if q := strings.Index(expr, "?"); q >= 0 {
		if colon := strings.LastIndex(expr, ":"); colon > q {
			cond := strings.TrimSpace(expr[:q])
			thenPart := strings.TrimSpace(expr[q+1 : colon])
			elsePart := strings.TrimSpace(expr[colon+1:])
			return fmt.Sprintf("match %s { true => %s false => %s }", cond, thenPart, elsePart)
		}
	}
	if strings.HasPrefix(expr, "String(") && strings.HasSuffix(expr, ")") {
		return strings.TrimSpace(expr[len("String(") : len(expr)-1])
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
