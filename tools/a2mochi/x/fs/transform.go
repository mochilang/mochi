//go:build slow

package fs

import (
	"fmt"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

var (
	appendRe = regexp.MustCompile(`([a-zA-Z_][\w]*)\s*@\s*\[([^\]]+)\]`)
	eqRe     = regexp.MustCompile(`\s=\s`)
)

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	root := &ast.Node{Kind: "program"}

	for _, v := range p.Vars {
		n, err := varNode(v)
		if err != nil {
			return nil, err
		}
		root.Children = append(root.Children, n)
	}

	if len(p.Stmts) > 0 {
		ns, err := stmtsToNodes(p.Stmts)
		if err != nil {
			return nil, err
		}
		root.Children = append(root.Children, ns...)
	}

	for _, pr := range p.Prints {
		n, err := printNode(PrintStmt{Expr: pr})
		if err != nil {
			return nil, err
		}
		root.Children = append(root.Children, n)
	}

	if len(root.Children) == 0 {
		return nil, fmt.Errorf("unsupported")
	}
	return root, nil
}

func stmtsToNodes(stmts []Stmt) ([]*ast.Node, error) {
	var out []*ast.Node
	for _, s := range stmts {
		n, err := stmtNode(s)
		if err != nil {
			return nil, err
		}
		if n != nil {
			out = append(out, n)
		}
	}
	return out, nil
}

func stmtNode(s Stmt) (*ast.Node, error) {
	switch v := s.(type) {
	case Var:
		return varNode(v)
	case Assign:
		val, err := exprNode(v.Expr)
		if err != nil {
			return nil, err
		}
		target := v.Name
		if v.Index != "" {
			target += "[" + fixIndex(v.Index) + "]"
		}
		return &ast.Node{Kind: "assign", Value: target, Children: []*ast.Node{val}}, nil
	case PrintStmt:
		return printNode(v)
	case Expect:
		e, err := exprNode(v.Cond)
		if err != nil {
			return nil, err
		}
		return &ast.Node{Kind: "expect", Children: []*ast.Node{e}}, nil
	case ForRange:
		start, err := exprNode(v.Start)
		if err != nil {
			return nil, err
		}
		adjEnd := v.End + " + 1"
		end, err := exprNode(adjEnd)
		if err != nil {
			return nil, err
		}
		body, err := stmtsToNodes(v.Body)
		if err != nil {
			return nil, err
		}
		r := &ast.Node{Kind: "range", Children: []*ast.Node{start, end}}
		blk := &ast.Node{Kind: "block", Children: body}
		return &ast.Node{Kind: "for", Value: v.Var, Children: []*ast.Node{r, blk}}, nil
	case ForIn:
		src, err := exprNode(v.Expr)
		if err != nil {
			return nil, err
		}
		body, err := stmtsToNodes(v.Body)
		if err != nil {
			return nil, err
		}
		inN := &ast.Node{Kind: "in", Children: []*ast.Node{src}}
		blk := &ast.Node{Kind: "block", Children: body}
		return &ast.Node{Kind: "for", Value: v.Var, Children: []*ast.Node{inN, blk}}, nil
	case While:
		cond, err := exprNode(v.Cond)
		if err != nil {
			return nil, err
		}
		body, err := stmtsToNodes(v.Body)
		if err != nil {
			return nil, err
		}
		blk := &ast.Node{Kind: "block", Children: body}
		return &ast.Node{Kind: "while", Children: []*ast.Node{cond, blk}}, nil
	case If:
		cond, err := exprNode(v.Cond)
		if err != nil {
			return nil, err
		}
		thenNodes, err := stmtsToNodes(v.Then)
		if err != nil {
			return nil, err
		}
		n := &ast.Node{Kind: "if", Children: []*ast.Node{cond, {Kind: "block", Children: thenNodes}}}
		elseNodes, err := stmtsToNodes(v.Else)
		if err != nil {
			return nil, err
		}
		if len(elseNodes) > 0 {
			n.Children = append(n.Children, &ast.Node{Kind: "block", Children: elseNodes})
		}
		return n, nil
	case Return:
		e, err := exprNode(v.Expr)
		if err != nil {
			return nil, err
		}
		return &ast.Node{Kind: "return", Children: []*ast.Node{e}}, nil
	case Break:
		return &ast.Node{Kind: "break"}, nil
	case Continue:
		return &ast.Node{Kind: "continue"}, nil
	case Fun:
		n := &ast.Node{Kind: "fun", Value: v.Name}
		for _, p := range v.Params {
			pn := &ast.Node{Kind: "param", Value: p.Name}
			typ := "any"
			if p.Type != "" {
				if t := mapType(p.Type); t != "" {
					typ = t
				}
			}
			pn.Children = append(pn.Children, typeNode(typ))
			n.Children = append(n.Children, pn)
		}
		retType := "any"
		if v.Ret != "" {
			if t := mapType(v.Ret); t != "" {
				retType = t
			}
		}
		n.Children = append(n.Children, typeNode(retType))
		body, err := stmtsToNodes(v.Body)
		if err != nil {
			return nil, err
		}
		n.Children = append(n.Children, body...)
		return n, nil
	case TypeDecl:
		tn := &ast.Node{Kind: "type", Value: v.Name}
		for _, f := range v.Fields {
			typ := mapType(f.Type)
			if typ == "" {
				typ = "any"
			}
			tn.Children = append(tn.Children, &ast.Node{Kind: "field", Value: f.Name, Children: []*ast.Node{typeNode(typ)}})
		}
		// Variants ignored for simplicity
		return tn, nil
	default:
		return nil, fmt.Errorf("unsupported stmt")
	}
}

func varNode(v Var) (*ast.Node, error) {
	n := &ast.Node{Kind: "let", Value: v.Name}
	if v.Mutable {
		n.Kind = "var"
	}
	if v.Type != "" {
		if t := mapType(v.Type); t != "" {
			n.Children = append(n.Children, typeNode(t))
		}
	}
	if v.Expr != "" {
		e, err := exprNode(v.Expr)
		if err != nil {
			return nil, err
		}
		n.Children = append(n.Children, e)
	}
	return n, nil
}

func printNode(p PrintStmt) (*ast.Node, error) {
	e, err := exprNode(p.Expr)
	if err != nil {
		return nil, err
	}
	return &ast.Node{Kind: "call", Value: "print", Children: []*ast.Node{e}}, nil
}

func exprNode(src string) (*ast.Node, error) {
	src = fixIndex(src)
	prog, err := parser.ParseString("x = " + src)
	if err != nil {
		return nil, err
	}
	if len(prog.Statements) == 0 || prog.Statements[0].Assign == nil {
		return nil, fmt.Errorf("expr parse failed")
	}
	return ast.FromExpr(prog.Statements[0].Assign.Value), nil
}

func typeNode(t string) *ast.Node {
	if t == "" {
		return nil
	}
	return &ast.Node{Kind: "type", Value: t}
}

func fixIndex(expr string) string {
	expr = strings.ReplaceAll(expr, ".[", "[")
	expr = strings.ReplaceAll(expr, "[|", "[")
	expr = strings.ReplaceAll(expr, "|]", "]")
	expr = convertRecordFields(expr)
	expr = convertContains(expr)
	expr = convertListPrint(expr)
	expr = convertFunExpr(expr)
	expr = stripStringCall(expr)
	expr = convertBuiltins(expr)
	expr = stripStringCall(expr)
	expr = convertCallSyntax(expr)
	expr = convertEquality(expr)
	expr = strings.ReplaceAll(expr, ";", ",")
	return expr
}

// DebugFix exposes fixIndex for tests.
func DebugFix(expr string) string {
	return fixIndex(expr)
}

func convertRecordFields(expr string) string {
	var out strings.Builder
	inBraces := false
	for i := 0; i < len(expr); i++ {
		if expr[i] == '{' {
			inBraces = true
		} else if expr[i] == '}' {
			inBraces = false
		}
		if inBraces && expr[i] == '=' {
			out.WriteByte(':')
			continue
		}
		out.WriteByte(expr[i])
	}
	return out.String()
}

// convertCallSyntax converts simple F# space-separated function calls like
// "add 1 2" into the Mochi style "add(1, 2)". It only operates on top-level
// space separated tokens and ignores expressions containing operators.
func convertCallSyntax(expr string) string {
	expr = strings.TrimSpace(expr)
	if expr == "" {
		return expr
	}
	// quick check for operators
	if strings.ContainsAny(expr, "+-*/=<>&|%^") {
		return expr
	}
	parts := splitTopLevel(expr, ' ')
	if len(parts) <= 1 {
		return expr
	}
	name := parts[0]
	if !isIdent(name) {
		return expr
	}
	return name + "(" + strings.Join(parts[1:], ", ") + ")"
}

func splitTopLevel(s string, sep rune) []string {
	var parts []string
	var buf strings.Builder
	depth := 0
	for _, r := range s {
		switch r {
		case '(', '[', '{':
			depth++
		case ')', ']', '}':
			if depth > 0 {
				depth--
			}
		}
		if r == sep && depth == 0 {
			if buf.Len() > 0 {
				parts = append(parts, buf.String())
				buf.Reset()
			}
			continue
		}
		buf.WriteRune(r)
	}
	if buf.Len() > 0 {
		parts = append(parts, buf.String())
	}
	return parts
}

func isIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || r == '_' {
			// ok
		} else if i > 0 && r >= '0' && r <= '9' {
			// ok
		} else {
			return false
		}
	}
	return true
}

func convertContains(expr string) string {
	if i := strings.Index(expr, "if Map.containsKey"); i != -1 && strings.Contains(expr, "then 1 else 0") {
		rest := expr[i+len("if Map.containsKey"):]
		parts := strings.Fields(rest)
		if len(parts) >= 2 {
			return strings.TrimSpace(parts[0]) + " in " + strings.TrimSpace(parts[1])
		}
	}
	if i := strings.Index(expr, "List.contains"); i != -1 {
		rest := strings.TrimSpace(expr[i+len("List.contains"):])
		parts := strings.Fields(rest)
		if len(parts) >= 2 {
			return strings.Replace(expr, "List.contains "+parts[0]+" "+parts[1], parts[0]+" in "+parts[1], 1)
		}
	}
	if idx := strings.Index(expr, ".Contains("); idx != -1 {
		before := strings.TrimSpace(expr[:idx])
		rest := expr[idx+len(".Contains("):]
		if j := strings.Index(rest, ")"); j != -1 {
			arg := strings.TrimSpace(rest[:j])
			return arg + " in " + before + rest[j+1:]
		}
	}
	if i := strings.Index(expr, "Map.containsKey"); i != -1 {
		rest := strings.TrimSpace(expr[i+len("Map.containsKey"):])
		parts := strings.Fields(rest)
		if len(parts) >= 2 {
			return strings.Replace(expr, "Map.containsKey "+parts[0]+" "+parts[1], parts[0]+" in "+parts[1], 1)
		}
	}
	return expr
}

func convertListPrint(expr string) string {
	expr = strings.TrimSpace(expr)
	prefix := `(("[" + (String.concat ", " (List.map string `
	suffix := `))) + "]")`
	if strings.HasPrefix(expr, prefix) && strings.HasSuffix(expr, suffix) {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, prefix), suffix)
		return strings.TrimSpace(inner)
	}
	return expr
}

func convertFunExpr(expr string) string {
	trimmed := strings.TrimSpace(expr)
	if strings.HasPrefix(trimmed, "fun ") {
		rest := strings.TrimSpace(trimmed[4:])
		if idx := strings.Index(rest, "->"); idx != -1 {
			params := strings.Fields(strings.TrimSpace(rest[:idx]))
			body := strings.TrimSpace(rest[idx+2:])
			return "fun (" + strings.Join(params, ", ") + ") { " + body + " }"
		}
	}
	return expr
}

func convertBuiltins(expr string) string {
	if i := strings.Index(expr, "List.length"); i != -1 {
		arg := strings.TrimSpace(expr[i+len("List.length"):])
		expr = expr[:i] + "len(" + arg + ")"
	}
	if i := strings.Index(expr, "Seq.length"); i != -1 {
		arg := strings.TrimSpace(expr[i+len("Seq.length"):])
		expr = expr[:i] + "len(" + arg + ")"
	}
	if i := strings.Index(expr, "String.length"); i != -1 {
		arg := strings.TrimSpace(expr[i+len("String.length"):])
		expr = expr[:i] + "len(" + arg + ")"
	}
	if i := strings.Index(expr, "List.sum"); i != -1 {
		arg := strings.TrimSpace(expr[i+len("List.sum"):])
		expr = expr[:i] + "sum(" + arg + ")"
	}
	if i := strings.Index(expr, "List.averageBy float"); i != -1 {
		arg := strings.TrimSpace(expr[i+len("List.averageBy float"):])
		expr = expr[:i] + "avg(" + arg + ")"
	}
	if i := strings.Index(expr, "int ("); i != -1 && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSpace(expr[i+len("int (") : len(expr)-1])
		expr = expr[:i] + "(" + inner + " as int)"
	} else if strings.HasPrefix(expr, "int ") {
		inner := strings.TrimSpace(expr[len("int "):])
		expr = "(" + inner + " as int)"
	}
	if i := strings.Index(expr, "Map.ofList ["); i != -1 {
		rest := expr[i+len("Map.ofList ["):]
		if j := strings.Index(rest, "]"); j != -1 {
			list := rest[:j]
			after := rest[j+1:]
			items := strings.Split(list, ";")
			parts := make([]string, 0, len(items))
			for _, it := range items {
				it = strings.TrimSpace(it)
				it = strings.Trim(it, "()")
				kv := strings.SplitN(it, ",", 2)
				if len(kv) == 2 {
					parts = append(parts, strings.TrimSpace(kv[0])+": "+strings.TrimSpace(kv[1]))
				}
			}
			expr = expr[:i] + "{" + strings.Join(parts, ", ") + "}" + after
		}
	}
	expr = appendRe.ReplaceAllString(expr, "append($1, $2)")
	for _, name := range []string{"min", "max", "len", "sum"} {
		prefix := name + " "
		if idx := strings.Index(expr, prefix); idx != -1 {
			arg := strings.TrimSpace(expr[idx+len(prefix):])
			expr = expr[:idx] + name + "(" + arg + ")"
		}
	}
	if strings.HasPrefix(expr, "String.concat \" \" [") && strings.HasSuffix(expr, "]") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "String.concat \" \" ["), "]")
		parts := strings.Split(inner, ";")
		if len(parts) == 2 {
			a := toStr(strings.TrimSpace(parts[0]))
			b := toStr(strings.TrimSpace(parts[1]))
			expr = a + " + \" \" + " + b
		}
	}
	return expr
}

func toStr(part string) string {
	trimmed := strings.TrimSpace(part)
	if strings.HasPrefix(trimmed, "string (") && strings.HasSuffix(trimmed, ")") {
		inner := strings.TrimSpace(trimmed[len("string (") : len(trimmed)-1])
		if strings.HasPrefix(inner, "\"") && strings.HasSuffix(inner, "\"") {
			return inner
		}
		return "str(" + inner + ")"
	}
	if strings.HasPrefix(trimmed, "string(") && strings.HasSuffix(trimmed, ")") {
		inner := strings.TrimSpace(trimmed[len("string(") : len(trimmed)-1])
		if strings.HasPrefix(inner, "\"") && strings.HasSuffix(inner, "\"") {
			return inner
		}
		return "str(" + inner + ")"
	}
	if strings.HasPrefix(trimmed, "string ") {
		inner := strings.TrimSpace(trimmed[len("string "):])
		if strings.HasPrefix(inner, "\"") && strings.HasSuffix(inner, "\"") {
			return inner
		}
		return "str(" + inner + ")"
	}
	return trimmed
}

func stripStringCall(expr string) string {
	trimmed := strings.TrimSpace(expr)
	trimmed = stripOuterParens(trimmed)
	for {
		var changed bool
		if strings.HasPrefix(trimmed, "string (") && strings.HasSuffix(trimmed, ")") {
			trimmed = strings.TrimSpace(trimmed[len("string (") : len(trimmed)-1])
			changed = true
		} else if strings.HasPrefix(trimmed, "string(") && strings.HasSuffix(trimmed, ")") {
			trimmed = strings.TrimSpace(trimmed[len("string(") : len(trimmed)-1])
			changed = true
		} else if strings.HasPrefix(trimmed, "string ") {
			trimmed = strings.TrimSpace(trimmed[len("string "):])
			changed = true
		}
		if changed {
			trimmed = stripOuterParens(trimmed)
		} else {
			break
		}
	}
	return trimmed
}

func stripOuterParens(s string) string {
	for {
		t := strings.TrimSpace(s)
		if len(t) >= 2 && t[0] == '(' && t[len(t)-1] == ')' {
			if balancedParens(t[1 : len(t)-1]) {
				s = strings.TrimSpace(t[1 : len(t)-1])
				continue
			}
		}
		break
	}
	return s
}

func balancedParens(s string) bool {
	depth := 0
	for _, r := range s {
		if r == '(' {
			depth++
		} else if r == ')' {
			depth--
			if depth < 0 {
				return false
			}
		}
	}
	return depth == 0
}

func convertEquality(expr string) string {
	return eqRe.ReplaceAllString(expr, " == ")
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "unit":
		return ""
	case "int", "int32", "int64", "uint32", "uint64":
		return "int"
	case "float", "double", "decimal":
		return "float"
	case "bool":
		return "bool"
	case "string":
		return "string"
	}
	if strings.HasSuffix(t, " list") {
		inner := mapType(strings.TrimSuffix(t, " list"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := mapType(strings.TrimSuffix(t, " array"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return ""
}
