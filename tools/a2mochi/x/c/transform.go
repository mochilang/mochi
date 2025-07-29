//go:build slow

package c

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"mochi/ast"
)

func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}

	assigned := map[string]bool{}
	for _, st := range p.Stmts {
		switch v := st.(type) {
		case Assign:
			assigned[v.Name] = true
		case For:
			assigned[v.Var] = true
		}
	}

	root := &ast.Node{Kind: "program"}
	for _, st := range p.Structs {
		t := &ast.Node{Kind: "type", Value: st.Name}
		for _, f := range st.Fields {
			field := &ast.Node{Kind: "field", Value: f.Name}
			if typ := mapCType(f.Type); typ != "" {
				field.Children = append(field.Children, &ast.Node{Kind: "type", Value: typ})
			}
			t.Children = append(t.Children, field)
		}
		root.Children = append(root.Children, t)
	}
	for i := 0; i < len(p.Stmts); {
		st := p.Stmts[i]
		var next Stmt
		if i+1 < len(p.Stmts) {
			next = p.Stmts[i+1]
		}
		if ps, ok := st.(PrintStmt); ok {
			exprs := append([]string{}, ps.Exprs...)
			newline := ps.Newline
			j := i + 1
			for !newline && j < len(p.Stmts) {
				if next, ok := p.Stmts[j].(PrintStmt); ok {
					exprs = append(exprs, next.Exprs...)
					newline = next.Newline
					j++
				} else {
					break
				}
			}
			i = j
			call := &ast.Node{Kind: "call", Value: "print"}
			for _, e := range exprs {
				call.Children = append(call.Children, exprNode(e))
			}
			root.Children = append(root.Children, call)
			continue
		}
		i++
		if n := stmtNodeWithAssign(st, assigned, false, next); n != nil {
			root.Children = append(root.Children, n)
		}
	}
	return root, nil
}

func stmtNodeWithAssign(s Stmt, assigned map[string]bool, inFunc bool, next Stmt) *ast.Node {
	switch v := s.(type) {
	case VarDecl:
		kind := "let"
		if assigned[v.Name] {
			kind = "var"
		}
		if v.Value == "0" {
			if inFunc {
				n := &ast.Node{Kind: kind, Value: v.Name}
				n.Children = append(n.Children, &ast.Node{Kind: "type", Value: "int"})
				return n
			}
			if usesVarInNext(next, v.Name) {
				val := &ast.Node{Kind: "int", Value: 0}
				return &ast.Node{Kind: kind, Value: v.Name, Children: []*ast.Node{val}}
			}
			n := &ast.Node{Kind: kind, Value: v.Name}
			n.Children = append(n.Children, &ast.Node{Kind: "type", Value: "int"})
			return n
		}
		val := exprNode(v.Value)
		if name, ok := lenExpr(v.Value); ok {
			val = &ast.Node{Kind: "call", Value: "len", Children: []*ast.Node{exprNode(name)}}
		}
		return &ast.Node{Kind: kind, Value: v.Name, Children: []*ast.Node{val}}
	case Assign:
		val := exprNode(v.Expr)
		if name, ok := lenExpr(v.Expr); ok {
			val = &ast.Node{Kind: "call", Value: "len", Children: []*ast.Node{exprNode(name)}}
		}
		return &ast.Node{Kind: "assign", Value: v.Name, Children: []*ast.Node{val}}
	case PrintStmt:
		call := &ast.Node{Kind: "call", Value: "print"}
		for _, e := range v.Exprs {
			call.Children = append(call.Children, exprNode(e))
		}
		return call
	case While:
		cond := boolify(exprNode(v.Cond))
		body := blockNode(v.Body, assigned)
		return &ast.Node{Kind: "while", Children: []*ast.Node{cond, body}}
	case For:
		start := exprNode(v.Start)
		end := exprNode(v.End)
		rng := &ast.Node{Kind: "range", Children: []*ast.Node{start, end}}
		body := blockNode(v.Body, assigned)
		n := &ast.Node{Kind: "for", Value: v.Var, Children: []*ast.Node{rng, body}}
		return n
	case If:
		cond := boolify(exprNode(v.Cond))
		thenBlock := blockNode(v.Then, assigned)
		elseBlock := blockNode(v.Else, assigned)
		return &ast.Node{Kind: "if", Children: []*ast.Node{cond, thenBlock, elseBlock}}
	case Return:
		if !inFunc {
			return nil
		}
		return &ast.Node{Kind: "return", Children: []*ast.Node{exprNode(v.Expr)}}
	case FunDecl:
		body := blockNode(v.Body, assigned)
		n := &ast.Node{Kind: "fun", Value: v.Name}
		for _, p := range v.Params {
			param := &ast.Node{Kind: "param", Value: p.Name}
			if typ := mapCType(p.Type); typ != "" {
				param.Children = append(param.Children, &ast.Node{Kind: "type", Value: typ})
			}
			n.Children = append(n.Children, param)
		}
		if typ := mapCType(v.Ret); typ != "" {
			n.Children = append(n.Children, &ast.Node{Kind: "type", Value: typ})
		} else if v.Ret != "" && v.Ret != "void" {
			n.Children = append(n.Children, &ast.Node{Kind: "type", Value: v.Ret})
		}
		n.Children = append(n.Children, body)
		return n
	case Break:
		return &ast.Node{Kind: "break"}
	case Continue:
		return &ast.Node{Kind: "continue"}
	default:
		return nil
	}
}

func blockNode(stmts []Stmt, assigned map[string]bool) *ast.Node {
	blk := &ast.Node{Kind: "block"}
	for i := 0; i < len(stmts); i++ {
		var next Stmt
		if i+1 < len(stmts) {
			next = stmts[i+1]
		}
		s := stmts[i]
		if n := stmtNodeWithAssign(s, assigned, true, next); n != nil {
			blk.Children = append(blk.Children, n)
		}
	}
	return blk
}

var reBinary = regexp.MustCompile(`^(.+)\s*(==|!=|<=|>=|<|>|\+|\-|\*|/|%|&&|\|\|)\s*(.+)$`)
var reCall = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\((.*)\)$`)
var reList = regexp.MustCompile(`^\{\s*([^}]*)\s*\}$`)
var reIndex = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\[(.+)\]$`)
var reLen = regexp.MustCompile(`^sizeof\((\w+)\)\s*/\s*sizeof\((\w+)\[0\]\)$`)
var reStructLit = regexp.MustCompile(`^\(([A-Za-z_][A-Za-z0-9_]*)\)\s*\{(.*)\}$`)
var reField = regexp.MustCompile(`\.[A-Za-z_][A-Za-z0-9_]*\s*=\s*[^,}]+`)

func exprNode(expr string) *ast.Node {
	expr = strings.TrimSpace(expr)
	for strings.HasPrefix(expr, "(") && strings.HasSuffix(expr, ")") {
		inner := expr[1 : len(expr)-1]
		if parenBalanced(inner) {
			expr = strings.TrimSpace(inner)
		} else {
			break
		}
	}
	if strings.HasPrefix(expr, "\"") && strings.HasSuffix(expr, "\"") {
		if s, err := strconv.Unquote(expr); err == nil {
			if i, err2 := strconv.Atoi(s); err2 == nil {
				return &ast.Node{Kind: "int", Value: i}
			}
			if f, err2 := strconv.ParseFloat(s, 64); err2 == nil {
				if f == float64(int64(f)) {
					return &ast.Node{Kind: "int", Value: int(f)}
				}
				return &ast.Node{Kind: "float", Value: f}
			}
			return &ast.Node{Kind: "string", Value: s}
		}
	}
	if l, op, r, ok := splitBinary(expr); ok {
		left := exprNode(l)
		right := exprNode(r)
		if op == "&&" || op == "||" {
			left = boolify(left)
			right = boolify(right)
		}
		return &ast.Node{Kind: "binary", Value: op, Children: []*ast.Node{left, right}}
	}
	if m := reCall.FindStringSubmatch(expr); m != nil {
		args := splitArgs(m[2])
		call := &ast.Node{Kind: "call", Value: m[1]}
		for _, a := range args {
			a = strings.TrimSpace(a)
			if a == "" {
				continue
			}
			call.Children = append(call.Children, exprNode(a))
		}
		return call
	}
	if m := reIndex.FindStringSubmatch(expr); m != nil {
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(m[1]), exprNode(m[2])}}
	}
	if m := reStructLit.FindStringSubmatch(expr); m != nil {
		typ := m[1]
		fieldsPart := m[2]
		var entries []*ast.Node
		matches := reField.FindAllString(fieldsPart, -1)
		for _, f := range matches {
			parts := strings.SplitN(f, "=", 2)
			if len(parts) != 2 {
				continue
			}
			name := strings.TrimSpace(strings.TrimPrefix(parts[0], "."))
			val := strings.TrimSpace(parts[1])
			entry := &ast.Node{Kind: "entry", Children: []*ast.Node{&ast.Node{Kind: "string", Value: name}, exprNode(val)}}
			entries = append(entries, entry)
		}
		rec := &ast.Node{Kind: "map", Children: entries}
		return &ast.Node{Kind: "cast", Children: []*ast.Node{rec, &ast.Node{Kind: "type", Value: typ}}}
	}
	if m := reList.FindStringSubmatch(expr); m != nil {
		parts := splitArgs(m[1])
		n := &ast.Node{Kind: "list"}
		for _, p := range parts {
			p = strings.TrimSpace(p)
			if p == "" {
				continue
			}
			n.Children = append(n.Children, exprNode(p))
		}
		return n
	}
	if i, err := strconv.Atoi(expr); err == nil {
		return &ast.Node{Kind: "int", Value: i}
	}
	if f, err := strconv.ParseFloat(expr, 64); err == nil {
		if f == float64(int64(f)) {
			return &ast.Node{Kind: "int", Value: int(f)}
		}
		return &ast.Node{Kind: "float", Value: f}
	}
	return &ast.Node{Kind: "selector", Value: expr}
}

func lenExpr(s string) (string, bool) {
	if m := reLen.FindStringSubmatch(strings.ReplaceAll(s, " ", "")); m != nil {
		if m[1] == m[2] {
			return m[1], true
		}
	}
	return "", false
}

func boolify(n *ast.Node) *ast.Node {
	if n == nil {
		return &ast.Node{Kind: "bool", Value: false}
	}
	switch n.Kind {
	case "bool", "binary":
		return n
	case "int":
		if v, ok := n.Value.(int); ok {
			if v == 0 {
				return &ast.Node{Kind: "bool", Value: false}
			}
			if v == 1 {
				return &ast.Node{Kind: "bool", Value: true}
			}
		}
	}
	zero := &ast.Node{Kind: "int", Value: 0}
	return &ast.Node{Kind: "binary", Value: "!=", Children: []*ast.Node{n, zero}}
}

func usesVarInNext(s Stmt, name string) bool {
	switch v := s.(type) {
	case While:
		return strings.Contains(v.Cond, name)
	case For:
		if v.Var == name {
			return true
		}
		if strings.Contains(v.Start, name) || strings.Contains(v.End, name) {
			return true
		}
	}
	return false
}

func splitArgs(s string) []string {
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
				args = append(args, s[start:i])
				start = i + 1
			}
		}
	}
	if start <= len(s) {
		args = append(args, s[start:])
	}
	return args
}

func parenBalanced(s string) bool {
	depth := 0
	for _, r := range s {
		switch r {
		case '(':
			depth++
		case ')':
			depth--
			if depth < 0 {
				return false
			}
		}
	}
	return depth == 0
}

func splitBinary(s string) (string, string, string, bool) {
	groups := [][]string{
		{"||"},
		{"&&"},
		{"==", "!="},
		{"<=", ">=", "<", ">"},
		{"+", "-"},
		{"*", "/", "%"},
	}
	for _, ops := range groups {
		depth := 0
		for i := len(s) - 1; i >= 0; i-- {
			switch s[i] {
			case ')':
				depth++
			case '(':
				if depth > 0 {
					depth--
				}
			}
			if depth != 0 {
				continue
			}
			for _, op := range ops {
				if i-len(op)+1 < 0 {
					continue
				}
				if s[i-len(op)+1:i+1] == op {
					// ignore unary + or -
					if (op == "-" || op == "+") && (i-len(op)+1 == 0 || strings.ContainsRune("+-*/%(<>=", rune(s[i-len(op)]))) {
						continue
					}
					left := strings.TrimSpace(s[:i-len(op)+1])
					right := strings.TrimSpace(s[i+1:])
					return left, op, right, true
				}
			}
		}
	}
	return "", "", "", false
}

func mapCType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "int", "long", "long long", "size_t":
		return "int"
	case "const char*", "char*", "const char *":
		return "string"
	default:
		return ""
	}
}
