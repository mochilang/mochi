//go:build slow

package hs

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

var traceNames map[string]bool

func replaceOutsideQuotes(s, name, repl string) string {
	parts := strings.Split(s, "\"")
	for i := 0; i < len(parts); i += 2 {
		parts[i] = replaceWord(parts[i], name, repl)
	}
	return strings.Join(parts, "\"")
}

func replaceWord(s, name, repl string) string {
	var out strings.Builder
	for {
		idx := strings.Index(s, name)
		if idx == -1 {
			out.WriteString(s)
			break
		}
		before := idx == 0 || !isIdentChar(rune(s[idx-1]))
		after := idx+len(name) == len(s) || !isIdentChar(rune(s[idx+len(name)]))
		if before && after {
			out.WriteString(s[:idx])
			out.WriteString(repl)
			s = s[idx+len(name):]
		} else {
			out.WriteString(s[:idx+1])
			s = s[idx+1:]
		}
	}
	return out.String()
}

func isIdentChar(r rune) bool {
	if r >= '0' && r <= '9' {
		return true
	}
	if r >= 'A' && r <= 'Z' {
		return true
	}
	if r >= 'a' && r <= 'z' {
		return true
	}
	return r == '_' || r == '\''
}

// parseMapM parses a line using mapM_ loop syntax and returns an Item describing a for loop.
func parseMapM(line string, lineNum int) (Item, bool) {
	l := strings.TrimSpace(strings.TrimPrefix(line, "mapM_"))
	if !strings.HasPrefix(l, "(\\") {
		return Item{}, false
	}
	idx := strings.Index(l, "->")
	if idx == -1 {
		return Item{}, false
	}
	varName := strings.TrimSpace(l[2:idx])
	rest := strings.TrimSpace(l[idx+2:])
	depth := 1
	close := -1
	for i := 0; i < len(rest); i++ {
		switch rest[i] {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				close = i
				i = len(rest)
				break
			}
		}
	}
	if close == -1 {
		return Item{}, false
	}
	body := strings.TrimSpace(rest[:close])
	if strings.HasPrefix(body, "do") {
		body = strings.TrimSpace(strings.TrimPrefix(body, "do"))
	}
	src := strings.TrimSpace(rest[close+1:])
	if strings.HasPrefix(src, "(") && strings.HasSuffix(src, ")") {
		src = strings.TrimSuffix(strings.TrimPrefix(src, "("), ")")
	}
	if strings.HasPrefix(src, "[") && strings.HasSuffix(src, "]") {
		s := strings.TrimSuffix(strings.TrimPrefix(src, "["), "]")
		if parts := strings.SplitN(s, "..", 2); len(parts) == 2 {
			start := strings.TrimSpace(parts[0])
			end := strings.TrimSpace(parts[1])
			end = strings.TrimSuffix(end, "- 1")
			end = strings.TrimSpace(end)
			end = "(" + end + " + 1)"
			return Item{Kind: "for", Name: varName, Start: start, End: end, Body: convertBody(body), Line: lineNum}, true
		}
	}
	if strings.HasPrefix(src, "Map.keys ") {
		src = strings.TrimPrefix(src, "Map.keys ")
	}
	return Item{Kind: "for", Name: varName, Collection: src, Body: convertBody(body), Line: lineNum}, true
}

func convertBody(body string) string {
	if strings.HasPrefix(body, "print") {
		arg := strings.TrimSpace(strings.TrimPrefix(body, "print"))
		arg = strings.Trim(arg, "()")
		return "print(" + convertExpr(arg) + ")"
	}
	if strings.HasPrefix(body, "putStrLn") {
		arg := strings.TrimSpace(strings.TrimPrefix(body, "putStrLn"))
		arg = strings.Trim(arg, "()")
		if strings.HasPrefix(arg, "show ") {
			arg = "str(" + strings.TrimSpace(strings.TrimPrefix(arg, "show ")) + ")"
		}
		return "print(" + convertExpr(arg) + ")"
	}
	return convertExpr(body)
}

// pattern matching helpers removed; simple string processing is used instead.

// fixFuncCalls converts Haskell-style function application to Mochi calls.
func fixFuncCalls(expr string) string {
	// simplistic: just collapse spaces between identifiers and argument lists
	var out strings.Builder
	i := 0
	for i < len(expr) {
		if expr[i] == ' ' && i+1 < len(expr) && (expr[i+1] == '(' || expr[i+1] == '[') {
			i++
			continue
		}
		out.WriteByte(expr[i])
		i++
	}
	res := out.String()
	// convert calls with list literals like "sum[1,2]" to "sum([1,2])"
	var b strings.Builder
	b.Reset()
	i = 0
	for i < len(res) {
		if isIdentChar(rune(res[i])) {
			start := i
			for i < len(res) && isIdentChar(rune(res[i])) {
				i++
			}
			if i < len(res) && res[i] == '[' {
				b.WriteString(res[start:i])
				b.WriteString("([")
				depth := 1
				i++
				argStart := i
				for i < len(res) && depth > 0 {
					switch res[i] {
					case '[':
						depth++
					case ']':
						depth--
					}
					i++
				}
				arg := res[argStart : i-1]
				b.WriteString(arg)
				b.WriteString("])")
				continue
			}
			b.WriteString(res[start:i])
			continue
		}
		b.WriteByte(res[i])
		i++
	}
	return b.String()
}

// trimOuterParens removes a single pair of wrapping parentheses if they
// encompass the entire expression.
func trimOuterParens(s string) string {
	for {
		if len(s) >= 2 && strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
			depth := 0
			ok := true
			for i, ch := range s {
				switch ch {
				case '(':
					depth++
				case ')':
					depth--
					if depth == 0 && i != len(s)-1 {
						ok = false
						break
					}
				}
			}
			if ok && depth == 0 {
				s = strings.TrimSpace(s[1 : len(s)-1])
				continue
			}
		}
		break
	}
	return s
}

// reorderVars moves variable declarations so that referenced variables are
// defined before use. This handles simple dependencies between top-level vars.
func reorderVars(items []Item) []Item {
	nameIdx := map[string]int{}
	for i, it := range items {
		if it.Kind == "var" || it.Kind == "func" {
			nameIdx[it.Name] = i
		}
	}
	for i := 0; i < len(items); i++ {
		it := items[i]
		if it.Kind != "var" {
			continue
		}
		for name, j := range nameIdx {
			if j > i && strings.Contains(it.Body, name) {
				items[i], items[j] = items[j], items[i]
				nameIdx[it.Name], nameIdx[name] = j, i
			}
		}
	}
	return items
}

func convertExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	expr = fixFuncCalls(expr)
	expr = trimOuterParens(expr)
	if strings.HasPrefix(expr, "\\") && strings.Contains(expr, "->") {
		idx := strings.Index(expr, "->")
		params := strings.Fields(strings.TrimSpace(expr[1:idx]))
		body := strings.TrimSpace(expr[idx+2:])
		var b strings.Builder
		b.WriteString("fun(")
		for i, p := range params {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(p)
			b.WriteString(": int")
		}
		b.WriteString("): int => ")
		b.WriteString(convertExpr(body))
		return b.String()
	}
	if strings.HasPrefix(expr, "trace ") {
		rest := strings.TrimSpace(strings.TrimPrefix(expr, "trace "))
		if i := strings.Index(rest, " "); i != -1 {
			msg := trimOuterParens(rest[:i])
			val := strings.TrimSpace(rest[i+1:])
			return "__trace(" + msg + ", " + convertExpr(val) + ")"
		}
	}
	if strings.HasPrefix(expr, "if ") && strings.Contains(expr, " then ") && strings.Contains(expr, " else ") {
		condEnd := strings.Index(expr, " then ")
		elseIdx := strings.LastIndex(expr, " else ")
		if condEnd != -1 && elseIdx > condEnd {
			cond := strings.TrimSpace(expr[len("if "):condEnd])
			thenPart := strings.TrimSpace(expr[condEnd+len(" then ") : elseIdx])
			elsePart := strings.TrimSpace(expr[elseIdx+len(" else "):])
			return "if " + convertExpr(cond) + " then " + convertExpr(thenPart) + " else " + convertExpr(elsePart)
		}
	}
	if strings.HasPrefix(expr, "fromIntegral(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "fromIntegral("), ")")
		return "float(" + convertExpr(inner) + ")"
	}
	if strings.HasPrefix(expr, "fromIntegral ") {
		inner := strings.TrimSpace(strings.TrimPrefix(expr, "fromIntegral "))
		return "float(" + convertExpr(inner) + ")"
	}
	if strings.HasPrefix(expr, "fromEnum(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "fromEnum("), ")")
		return convertExpr(inner)
	}
	if strings.HasPrefix(expr, "fromEnum ") {
		inner := strings.TrimSpace(strings.TrimPrefix(expr, "fromEnum "))
		return convertExpr(inner)
	}
	if strings.HasPrefix(expr, "read ") && strings.Contains(expr, "::") {
		parts := strings.SplitN(strings.TrimPrefix(expr, "read "), "::", 2)
		val := strings.TrimSpace(parts[0])
		typ := mapType(strings.TrimSpace(parts[1]))
		switch typ {
		case "int", "float", "string", "bool":
			return typ + "(" + val + ")"
		}
	}
	// mapFromList pattern not supported without regex
	if strings.HasPrefix(expr, "[") && strings.HasSuffix(expr, "]") {
		return expr
	}
	if strings.Contains(expr, "++") {
		parts := strings.Split(expr, "++")
		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			if strings.HasPrefix(right, "[") && strings.HasSuffix(right, "]") {
				val := strings.TrimSuffix(strings.TrimPrefix(right, "["), "]")
				return "append(" + left + ", " + strings.TrimSpace(val) + ")"
			}
			if (strings.HasPrefix(left, "\"") && strings.HasSuffix(left, "\"")) ||
				(strings.HasPrefix(right, "\"") && strings.HasSuffix(right, "\"")) {
				return convertExpr(left) + " + " + convertExpr(right)
			}
		}
	}
	if strings.Contains(expr, "!!") {
		parts := strings.Split(expr, "!!")
		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			idx := strings.TrimSpace(parts[1])
			idx = strings.TrimPrefix(idx, "(")
			idx = strings.TrimSuffix(idx, ")")
			return convertExpr(left) + "[" + convertExpr(idx) + "]"
		}
	}
	if strings.HasPrefix(expr, "isInfixOf ") {
		rest := strings.TrimSpace(strings.TrimPrefix(expr, "isInfixOf "))
		fields := strings.Fields(rest)
		if len(fields) >= 2 {
			pat := convertExpr(fields[0])
			obj := convertExpr(strings.Join(fields[1:], " "))
			return pat + " in " + obj
		}
	}
	if strings.HasPrefix(expr, "Map.elems ") {
		m := strings.TrimSpace(strings.TrimPrefix(expr, "Map.elems "))
		m = strings.TrimSuffix(strings.TrimPrefix(m, "("), ")")
		return "values(" + m + ")"
	}
	if strings.HasPrefix(expr, "Map.elems(") && strings.HasSuffix(expr, ")") {
		m := strings.TrimSuffix(strings.TrimPrefix(expr, "Map.elems("), ")")
		return "values(" + m + ")"
	}
	if strings.HasPrefix(expr, "div ") {
		parts := strings.Fields(expr)
		if len(parts) == 3 {
			return parts[1] + " / " + parts[2]
		}
	}
	if strings.HasPrefix(expr, "length ") {
		arg := strings.TrimSpace(expr[len("length "):])
		return "len(" + convertExpr(arg) + ")"
	}
	if strings.HasPrefix(expr, "length(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "length("), ")")
		return "len(" + inner + ")"
	}
	if strings.Contains(expr, "`mod`") {
		parts := strings.Split(expr, "`mod`")
		if len(parts) == 2 {
			return strings.TrimSpace(parts[0]) + " % " + strings.TrimSpace(parts[1])
		}
	}
	expr = strings.ReplaceAll(expr, "True", "true")
	expr = strings.ReplaceAll(expr, "False", "false")
	if strings.ContainsAny(expr, "()") {
		return expr
	}
	parts := strings.Fields(expr)
	if len(parts) > 1 {
		ops := map[string]bool{"+": true, "-": true, "*": true, "/": true, "%": true, "&&": true, "||": true, "==": true, "<": true, ">": true, "<=": true, ">=": true}
		hasOp := false
		for _, p := range parts[1:] {
			if ops[p] {
				hasOp = true
				break
			}
		}
		if !hasOp {
			for i := 1; i < len(parts); i++ {
				parts[i] = strings.TrimSuffix(parts[i], ",")
			}
			return parts[0] + "(" + strings.Join(parts[1:], ", ") + ")"
		}
	}
	return expr
}

func parseSigTypes(sig string) ([]string, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if i := strings.Index(sig, "::"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if i := strings.Index(sig, "=>"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if strings.HasPrefix(sig, "forall") {
		if i := strings.Index(sig, "."); i != -1 {
			sig = strings.TrimSpace(sig[i+1:])
		}
	}
	parts := strings.Split(sig, "->")
	for i, p := range parts {
		parts[i] = strings.TrimSpace(p)
	}
	if len(parts) == 0 {
		return nil, ""
	}
	ret := mapType(parts[len(parts)-1])
	params := make([]string, 0, len(parts)-1)
	for _, p := range parts[:len(parts)-1] {
		params = append(params, mapType(p))
	}
	return params, ret
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "()":
		return ""
	case "Int", "Integer":
		return "int"
	case "Float", "Double":
		return "float"
	case "String", "[Char]":
		return "string"
	case "Bool":
		return "bool"
	}
	if strings.HasPrefix(t, "[") && strings.HasSuffix(t, "]") {
		inner := mapType(strings.TrimSuffix(strings.TrimPrefix(t, "["), "]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

func parseVarSig(sig string) (string, bool) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if i := strings.Index(sig, "::"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if i := strings.Index(sig, "=>"); i != -1 {
		sig = strings.TrimSpace(sig[i+2:])
	}
	if strings.HasPrefix(sig, "forall") {
		if i := strings.Index(sig, "."); i != -1 {
			sig = strings.TrimSpace(sig[i+1:])
		}
	}
	sig = strings.TrimSpace(sig)
	if sig == "" {
		return "", false
	}
	return mapType(sig), true
}

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	dup := make([]Item, len(p.Items))
	copy(dup, p.Items)
	return buildProgramNodeFromItems(dup)
}

func buildProgramNodeFromItems(items []Item) (*ast.Node, error) {
	items = reorderVars(items)
	root := &ast.Node{Kind: "program"}
	for _, it := range items {
		if it.Name == "main" && it.Kind == "var" {
			body := strings.TrimSpace(it.Body)
			if strings.HasPrefix(body, "do") {
				body = strings.TrimSpace(strings.TrimPrefix(body, "do"))
			}
			body = strings.ReplaceAll(body, "\n", " ")
			line := strings.TrimSpace(body)
			if pitem, ok := parseMapM(line, it.Line); ok {
				node, err := itemToNode(pitem)
				if err != nil {
					return nil, err
				}
				if node != nil {
					root.Children = append(root.Children, node)
				}
				continue
			}
			if strings.HasPrefix(line, "print") || strings.HasPrefix(line, "putStrLn") {
				arg := strings.TrimSpace(strings.TrimPrefix(line, "print"))
				if strings.HasPrefix(line, "putStrLn") {
					arg = strings.TrimSpace(strings.TrimPrefix(line, "putStrLn"))
					if strings.HasPrefix(arg, "show ") {
						arg = strings.TrimSpace(strings.TrimPrefix(arg, "show "))
						arg = "str(" + arg + ")"
					}
				}
				arg = strings.Trim(arg, "()")
				pitem := Item{Kind: "print", Body: arg}
				node, err := itemToNode(pitem)
				if err != nil {
					return nil, err
				}
				if node != nil {
					root.Children = append(root.Children, node)
				}
				continue
			}
			continue
		}
		if it.Name == "main" && it.Kind == "sig" {
			continue
		}
		node, err := itemToNode(it)
		if err != nil {
			return nil, err
		}
		if node != nil {
			root.Children = append(root.Children, node)
		}
	}
	if len(root.Children) == 0 {
		return nil, fmt.Errorf("no output")
	}
	return root, nil
}

func itemToNode(it Item) (*ast.Node, error) {
	switch it.Kind {
	case "print":
		expr := "print(" + convertExpr(it.Body) + ")"
		return parseExprNode(expr)
	case "json":
		expr := "json(" + convertExpr(it.Body) + ")"
		return parseExprNode(expr)
	case "func":
		var b strings.Builder
		b.WriteString("fun ")
		b.WriteString(it.Name)
		b.WriteByte('(')
		for i, p := range it.Params {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(p)
		}
		b.WriteString(") {")
		if strings.TrimSpace(it.Body) != "" {
			b.WriteString(" return ")
			b.WriteString(convertExpr(it.Body))
		}
		b.WriteString(" }")
		return parseStmtNode(b.String())
	case "var":
		var b strings.Builder
		b.WriteString("let ")
		b.WriteString(it.Name)
		if strings.TrimSpace(it.Body) != "" {
			b.WriteString(" = ")
			b.WriteString(convertExpr(it.Body))
		}
		return parseStmtNode(b.String())
	case "for":
		var b strings.Builder
		b.WriteString("for ")
		b.WriteString(it.Name)
		b.WriteString(" in ")
		if it.Collection != "" {
			b.WriteString(it.Collection)
		} else {
			b.WriteString(it.Start)
			b.WriteString("..")
			b.WriteString(it.End)
		}
		b.WriteString(" { ")
		b.WriteString(convertExpr(it.Body))
		b.WriteString(" }")
		return parseStmtNode(b.String())
	case "struct":
		var b strings.Builder
		b.WriteString("type ")
		b.WriteString(it.Name)
		if len(it.Fields) == 0 {
			b.WriteString(" {}")
			return parseStmtNode(b.String())
		}
		b.WriteString(" {\n")
		for _, f := range it.Fields {
			b.WriteString("  ")
			b.WriteString(f.Name)
			if f.Type != "" {
				b.WriteString(": ")
				b.WriteString(mapType(f.Type))
			}
			b.WriteByte('\n')
		}
		b.WriteString("}")
		return parseStmtNode(b.String())
	}
	return nil, nil
}

func parseStmtNode(src string) (*ast.Node, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	if len(prog.Statements) != 1 {
		return nil, fmt.Errorf("unexpected statement")
	}
	return ast.FromStatement(prog.Statements[0]), nil
}

func parseExprNode(src string) (*ast.Node, error) {
	prog, err := parser.ParseString("expect " + src)
	if err != nil {
		return nil, err
	}
	if len(prog.Statements) != 1 || prog.Statements[0].Expect == nil {
		return nil, fmt.Errorf("unexpected expr")
	}
	return ast.FromExpr(prog.Statements[0].Expect.Value), nil
}
