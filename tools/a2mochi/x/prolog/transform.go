//go:build slow

package prolog

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

func node(kind string, value any, children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: kind, Value: value, Children: children}
}

// Transform parses Prolog source code and returns a Mochi AST node.
func Transform(src string) (*ast.Node, error) {
	p, err := Parse(src)
	if err != nil {
		return nil, err
	}
	return TransformProgram(p)
}

// TransformFile reads a Prolog file and converts it to a Mochi AST.
func TransformFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Transform(string(data))
}

// parseExpr converts a Mochi expression string into an AST node using the
// Mochi parser.
func parseExpr(expr string) (*ast.Node, error) {
	src := fmt.Sprintf("fun _(){ return %s }", expr)
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	ret := prog.Statements[0].Fun.Body[0].Return.Value
	return ast.FromExpr(ret), nil
}

// parseBodyNodes converts a Prolog clause body into Mochi AST statements.
func parseBodyNodes(body string) ([]*ast.Node, error) {
	clauses := splitClauses(body)
	var out []*ast.Node
	for i := 0; i < len(clauses); i++ {
		c := strings.TrimSpace(clauses[i])
		switch {
		case c == "" || c == "true":
			continue
		case strings.HasPrefix(c, "write("):
			arg := strings.TrimSuffix(strings.TrimPrefix(c, "write("), ")")
			arg = strings.ReplaceAll(arg, "'", "\"")
			arg = convertExpr(arg)
			expr, err := parseExpr(arg)
			if err != nil {
				return nil, err
			}
			out = append(out, node("call", "print", expr))
		case strings.HasPrefix(c, "writeln("):
			arg := strings.TrimSuffix(strings.TrimPrefix(c, "writeln("), ")")
			arg = strings.ReplaceAll(arg, "'", "\"")
			arg = convertExpr(arg)
			expr, err := parseExpr(arg)
			if err != nil {
				return nil, err
			}
			out = append(out, node("call", "print", expr))
		case c == "nl":
			continue
		case strings.HasPrefix(c, "(") && strings.Contains(c, "->") && strings.Contains(c, ";"):
			expr := strings.TrimSuffix(strings.TrimPrefix(c, "("), ")")
			parts := strings.SplitN(expr, "->", 2)
			condStr := convertExpr(strings.TrimSpace(parts[0]))
			rest := strings.TrimSpace(parts[1])
			parts = strings.SplitN(rest, ";", 2)
			thenPart := strings.TrimSpace(parts[0])
			elsePart := strings.TrimSpace(parts[1])
			condNode, err := parseExpr(condStr)
			if err != nil {
				return nil, err
			}
			thenNodes, err := parseBodyNodes(thenPart)
			if err != nil {
				return nil, err
			}
			elseNodes, err := parseBodyNodes(elsePart)
			if err != nil {
				return nil, err
			}
			if len(thenNodes) == 1 && thenNodes[0].Kind == "let" {
				varName := fmt.Sprint(thenNodes[0].Value)
				init, _ := parseExpr("\"\"")
				for i, n := range thenNodes {
					if n.Kind == "let" && n.Value == varName {
						thenNodes[i] = node("assign", varName, n.Children[0])
					}
				}
				if len(elseNodes) >= 1 && elseNodes[0].Kind == "var" && elseNodes[0].Value == varName {
					elseNodes = elseNodes[1:]
				}
				for i, n := range elseNodes {
					if n.Kind == "let" && n.Value == varName {
						elseNodes[i] = node("assign", varName, n.Children[0])
					}
				}
				out = append(out, node("var", varName, init))
				out = append(out, node("if", nil,
					condNode,
					node("block", nil, thenNodes...),
					node("block", nil, elseNodes...),
				))
			} else {
				out = append(out, node("if", nil,
					condNode,
					node("block", nil, thenNodes...),
					node("block", nil, elseNodes...),
				))
			}
		case strings.Contains(c, "->") && strings.Contains(c, ";"):
			parts := strings.SplitN(c, "->", 2)
			condStr := convertExpr(strings.TrimSpace(parts[0]))
			rest := strings.TrimSpace(parts[1])
			parts = strings.SplitN(rest, ";", 2)
			thenPart := strings.TrimSpace(parts[0])
			elsePart := strings.TrimSpace(parts[1])
			condNode, err := parseExpr(condStr)
			if err != nil {
				return nil, err
			}
			thenNodes, err := parseBodyNodes(thenPart)
			if err != nil {
				return nil, err
			}
			elseNodes, err := parseBodyNodes(elsePart)
			if err != nil {
				return nil, err
			}
			if len(thenNodes) == 1 && thenNodes[0].Kind == "let" {
				varName := fmt.Sprint(thenNodes[0].Value)
				init, _ := parseExpr("\"\"")
				for i, n := range thenNodes {
					if n.Kind == "let" && n.Value == varName {
						thenNodes[i] = node("assign", varName, n.Children[0])
					}
				}
				if len(elseNodes) >= 1 && elseNodes[0].Kind == "var" && elseNodes[0].Value == varName {
					elseNodes = elseNodes[1:]
				}
				for i, n := range elseNodes {
					if n.Kind == "let" && n.Value == varName {
						elseNodes[i] = node("assign", varName, n.Children[0])
					}
				}
				out = append(out, node("var", varName, init))
				out = append(out, node("if", nil,
					condNode,
					node("block", nil, thenNodes...),
					node("block", nil, elseNodes...),
				))
			} else {
				out = append(out, node("if", nil,
					condNode,
					node("block", nil, thenNodes...),
					node("block", nil, elseNodes...),
				))
			}
		case strings.HasPrefix(c, "(") && strings.Contains(c, "->"):
			expr := strings.TrimSuffix(strings.TrimPrefix(c, "("), ")")
			parts := strings.SplitN(expr, "->", 2)
			condStr := convertExpr(strings.TrimSpace(parts[0]))
			thenPart := strings.TrimSpace(parts[1])
			condNode, err := parseExpr(condStr)
			if err != nil {
				return nil, err
			}
			thenNodes, err := parseBodyNodes(thenPart)
			if err != nil {
				return nil, err
			}
			out = append(out, node("if", nil,
				condNode,
				node("block", nil, thenNodes...),
			))
		case strings.Contains(c, " is "):
			parts := strings.SplitN(c, " is ", 2)
			name := strings.TrimSpace(parts[0])
			exprStr := convertExpr(strings.TrimSpace(parts[1]))
			exprNode, err := parseExpr(exprStr)
			if err != nil {
				return nil, err
			}
			out = append(out, node("let", name, exprNode))
		case func() bool { _, _, _, _, ok := parseSubStringAssign(c); return ok }():
			src, start, length, outVar, _ := parseSubStringAssign(c)
			n1, err := parseExpr(src)
			if err != nil {
				return nil, err
			}
			n2, err := parseExpr(start)
			if err != nil {
				return nil, err
			}
			endExpr, err := parseExpr(start + " + " + length)
			if err != nil {
				return nil, err
			}
			call := node("call", "substr", n1, n2, endExpr)
			out = append(out, node("let", outVar, call))
		case func() bool { _, _, ok := parseSubStringContains(c); return ok }():
			src, sub, _ := parseSubStringContains(c)
			n1, err := parseExpr(src)
			if err != nil {
				return nil, err
			}
			n2, err := parseExpr("\"" + sub + "\"")
			if err != nil {
				return nil, err
			}
			call := node("call", "contains", n1, n2)
			out = append(out, node("call", "print", call))
		case func() bool { _, _, _, _, ok := parseNth04(c); return ok }():
			idx, list, _, rest, _ := parseNth04(c)
			if i+1 < len(clauses) {
				next := strings.TrimSpace(clauses[i+1])
				idx2, outList, val, rest2, ok2 := parseNth04(next)
				if ok2 && idx2 == idx && rest2 == rest {
					base, err := parseExpr(list)
					if err != nil {
						return nil, err
					}
					out = append(out, node("var", outList, base))
					idxNode, err := parseExpr(idx)
					if err != nil {
						return nil, err
					}
					valNode, err := parseExpr(val)
					if err != nil {
						return nil, err
					}
					tgt, err := parseExpr(outList)
					if err != nil {
						return nil, err
					}
					assign := node("assign", nil, node("index", nil, tgt, idxNode), valNode)
					out = append(out, assign)
					i++
					continue
				}
			}
			// unsupported 4-arg nth0
		case func() bool { _, _, _, ok := parseNth0(c); return ok }():
			idx, list, outVar, _ := parseNth0(c)
			n1, err := parseExpr(list)
			if err != nil {
				return nil, err
			}
			n2, err := parseExpr(idx)
			if err != nil {
				return nil, err
			}
			index := node("index", nil, n1, n2)
			out = append(out, node("let", outVar, index))
		case func() bool { _, _, _, ok := parseGetDict(c); return ok }():
			key, dict, outVar, _ := parseGetDict(c)
			n1, err := parseExpr(convertExpr(dict))
			if err != nil {
				return nil, err
			}
			n2, err := parseExpr(quoteAtom(key))
			if err != nil {
				return nil, err
			}
			idxNode := node("index", nil, n1, n2)
			out = append(out, node("let", outVar, idxNode))
		case func() bool { _, _, _, ok := parseCallAssign(c); return ok }():
			name, args, outVar, _ := parseCallAssign(c)
			var argNodes []*ast.Node
			for _, a := range args {
				n, err := parseExpr(a)
				if err != nil {
					return nil, err
				}
				argNodes = append(argNodes, n)
			}
			call := node("call", name, argNodes...)
			out = append(out, node("let", outVar, call))
		case func() bool { _, _, ok := parseSimpleCall(c); return ok }():
			name, args, _ := parseSimpleCall(c)
			var argNodes []*ast.Node
			for _, a := range args {
				n, err := parseExpr(a)
				if err != nil {
					return nil, err
				}
				argNodes = append(argNodes, n)
			}
			out = append(out, node("call", name, argNodes...))
		case strings.Contains(c, " = ") || (strings.Contains(c, "=") && !strings.Contains(c, "==") && !strings.Contains(c, "=:=") && !strings.Contains(c, "=\\")):
			parts := strings.SplitN(c, "=", 2)
			name := strings.TrimSpace(parts[0])
			exprStr := convertExpr(strings.TrimSpace(parts[1]))
			exprNode, err := parseExpr(exprStr)
			if err != nil {
				return nil, err
			}
			out = append(out, node("let", name, exprNode))
		default:
			// unsupported clause -> ignore
		}
	}
	return out, nil
}

func TransformProgram(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	if len(p.Clauses) == 0 {
		return nil, fmt.Errorf("no convertible symbols found")
	}
	root := node("program", nil)
	hasMain := false
	for _, c := range p.Clauses {
		if c.Name == ":-" {
			continue
		}
		fn := node("fun", c.Name)
		if c.Name == "main" {
			hasMain = true
		}
		for _, param := range c.Params {
			fn.Children = append(fn.Children, node("param", param))
		}
		body, err := parseBodyNodes(c.Body)
		if err != nil {
			return nil, err
		}
		fn.Children = append(fn.Children, body...)
		root.Children = append(root.Children, fn)
	}
	if hasMain {
		root.Children = append(root.Children, node("call", "main"))
	}
	if len(root.Children) == 0 {
		return nil, fmt.Errorf("no convertible symbols found")
	}
	return root, nil
}

// splitClauses splits a Prolog body into top-level comma-separated clauses.
func splitClauses(body string) []string {
	var clauses []string
	depth := 0
	brack := 0
	brace := 0
	start := 0
	for i, r := range body {
		switch r {
		case '(':
			depth++
		case ')':
			if depth > 0 {
				depth--
			}
		case '[':
			brack++
		case ']':
			if brack > 0 {
				brack--
			}
		case '{':
			brace++
		case '}':
			if brace > 0 {
				brace--
			}
		case ',':
			if depth == 0 && brack == 0 && brace == 0 {
				clauses = append(clauses, strings.TrimSpace(body[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(body) {
		clauses = append(clauses, strings.TrimSpace(body[start:]))
	}
	return clauses
}

func convertExpr(expr string) string {
	if key, dict, outVar, ok := parseGetDict(expr); ok {
		dict = convertExpr(dict)
		key = quoteAtom(key)
		if outVar == "_" {
			return "contains(" + dict + ", " + key + ")"
		}
		return dict + "[" + key + "]"
	}
	if list, elem, ok := parseNotMember(expr); ok {
		return "!(" + elem + " in " + list + ")"
	}
	if list, elem, ok := parseMember(expr); ok {
		return elem + " in " + list
	}
	if src, sub, ok := parseSubStringContains(expr); ok {
		return "\"" + sub + "\" in " + src
	}
	expr = strings.ReplaceAll(expr, "=:=", "==")
	expr = strings.ReplaceAll(expr, "=\\=", "!=")
	expr = strings.ReplaceAll(expr, "@>=", ">=")
	expr = strings.ReplaceAll(expr, "@=<", "<=")
	expr = strings.ReplaceAll(expr, "@>", ">")
	expr = strings.ReplaceAll(expr, "@<", "<")
	expr = strings.ReplaceAll(expr, " = ", " == ")
	eqRe := regexp.MustCompile(`([^<>!:=\-])=([^=])`)
	expr = eqRe.ReplaceAllString(expr, `$1==$2`)
	expr = strings.ReplaceAll(expr, "map{", "{")
	expr = strings.ReplaceAll(expr, "_{", "{")
	expr = regexp.MustCompile(`^[A-Za-z0-9_]+{`).ReplaceAllStringFunc(expr, func(s string) string {
		if strings.HasSuffix(s, "{") {
			return "{"
		}
		return s
	})
	// handle unary minus following an operator
	unaryRe := regexp.MustCompile(`([+\-*/])\s*-\s*([0-9]+(?:\.[0-9]+)?)`)
	expr = unaryRe.ReplaceAllString(expr, `$1(-$2)`)
	expr = replaceTopLevelCommas(expr)
	expr = quoteMapKeys(expr)
	return expr
}

func parseArgs(s string) []string {
	var args []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '(':
			depth++
		case ')':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				args = append(args, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		args = append(args, strings.TrimSpace(s[start:]))
	}
	return args
}

func parseSubStringAssign(s string) (src, start, length, outVar string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "sub_string(") {
		return
	}
	inner := strings.TrimSuffix(strings.TrimPrefix(s, "sub_string("), ")")
	parts := parseArgs(inner)
	if len(parts) != 5 {
		return
	}
	src = parts[0]
	start = parts[1]
	length = parts[2]
	outVar = parts[4]
	ok = true
	return
}

func parseSubStringContains(s string) (src, sub string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "sub_string(") {
		return
	}
	inner := strings.TrimSuffix(strings.TrimPrefix(s, "sub_string("), ")")
	parts := parseArgs(inner)
	if len(parts) != 5 {
		return
	}
	src = parts[0]
	sub = strings.Trim(parts[4], "'\"")
	ok = true
	return
}

func parseNth0(s string) (index, list, outVar string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "nth0(") {
		return
	}
	inner := strings.TrimSuffix(strings.TrimPrefix(s, "nth0("), ")")
	parts := parseArgs(inner)
	if len(parts) != 3 {
		return
	}
	index = parts[0]
	list = parts[1]
	outVar = parts[2]
	ok = true
	return
}

func parseNth04(s string) (index, list, elem, rest string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "nth0(") {
		return
	}
	inner := strings.TrimSuffix(strings.TrimPrefix(s, "nth0("), ")")
	parts := parseArgs(inner)
	if len(parts) != 4 {
		return
	}
	index = parts[0]
	list = parts[1]
	elem = parts[2]
	rest = parts[3]
	ok = true
	return
}

func parseCallAssign(s string) (name string, args []string, outVar string, ok bool) {
	s = strings.TrimSpace(s)
	open := strings.Index(s, "(")
	close := strings.LastIndex(s, ")")
	if open == -1 || close == -1 || close <= open {
		return
	}
	name = strings.TrimSpace(s[:open])
	inner := s[open+1 : close]
	parts := parseArgs(inner)
	if len(parts) < 2 {
		return
	}
	args = parts[:len(parts)-1]
	outVar = parts[len(parts)-1]
	ok = name != "" && outVar != ""
	return
}

func parseSimpleCall(s string) (name string, args []string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.Contains(s, "(") || !strings.HasSuffix(s, ")") {
		return
	}
	open := strings.Index(s, "(")
	if open == -1 {
		return
	}
	name = strings.TrimSpace(s[:open])
	if name == "" || strings.ContainsAny(name, " :=<>") {
		return
	}
	inner := s[open+1 : len(s)-1]
	args = parseArgs(inner)
	ok = true
	return
}

func parseMember(s string) (list, elem string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "member(") || !strings.HasSuffix(s, ")") {
		return
	}
	inner := strings.TrimSuffix(strings.TrimPrefix(s, "member("), ")")
	parts := parseArgs(inner)
	if len(parts) != 2 {
		return
	}
	elem = parts[0]
	list = parts[1]
	ok = true
	return
}

func parseNotMember(s string) (list, elem string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "\\+") {
		return
	}
	inner := strings.TrimPrefix(s, "\\+")
	if strings.HasPrefix(inner, "(") && strings.HasSuffix(inner, ")") {
		inner = strings.TrimSuffix(strings.TrimPrefix(inner, "("), ")")
	}
	for strings.HasPrefix(inner, "(") && strings.HasSuffix(inner, ")") {
		inner = strings.TrimSuffix(strings.TrimPrefix(inner, "("), ")")
	}
	if !strings.HasSuffix(inner, ")") {
		return
	}
	return parseMember(inner)
}

func parseGetDict(s string) (key, dict, outVar string, ok bool) {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "get_dict(") {
		return
	}
	inner := strings.TrimSuffix(strings.TrimPrefix(s, "get_dict("), ")")
	parts := parseArgs(inner)
	if len(parts) != 3 {
		return
	}
	key = parts[0]
	dict = parts[1]
	outVar = parts[2]
	ok = true
	return
}

func quoteAtom(s string) string {
	s = strings.Trim(s, " \t")
	if strings.HasPrefix(s, "'") && strings.HasSuffix(s, "'") {
		s = strings.Trim(s, "'")
	}
	if strings.HasPrefix(s, "\"") && strings.HasSuffix(s, "\"") {
		return s
	}
	if matched, _ := regexp.MatchString(`^[a-z][A-Za-z0-9_]*$`, s); matched {
		return "\"" + s + "\""
	}
	return s
}

func replaceTopLevelCommas(s string) string {
	var b strings.Builder
	depth := 0
	brack := 0
	brace := 0
	inSingle := false
	inDouble := false
	for _, r := range s {
		switch r {
		case '\'':
			if !inDouble {
				inSingle = !inSingle
			}
		case '"':
			if !inSingle {
				inDouble = !inDouble
			}
		case '(':
			if !inSingle && !inDouble {
				depth++
			}
		case ')':
			if !inSingle && !inDouble && depth > 0 {
				depth--
			}
		case '[':
			if !inSingle && !inDouble {
				brack++
			}
		case ']':
			if !inSingle && !inDouble && brack > 0 {
				brack--
			}
		case '{':
			if !inSingle && !inDouble {
				brace++
			}
		case '}':
			if !inSingle && !inDouble && brace > 0 {
				brace--
			}
		case ',':
			if !inSingle && !inDouble && depth == 0 && brack == 0 && brace == 0 {
				b.WriteString(" && ")
				continue
			}
		}
		b.WriteRune(r)
	}
	return b.String()
}

var mapKeyRe = regexp.MustCompile(`([,{]\s*)([a-zA-Z][A-Za-z0-9_]*)\s*:`)

func quoteMapKeys(s string) string {
	return mapKeyRe.ReplaceAllString(s, `$1"$2":`)
}
