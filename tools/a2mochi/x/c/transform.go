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
	for i := 0; i < len(p.Stmts); {
		st := p.Stmts[i]
		if ps, ok := st.(PrintStmt); ok {
			exprs := []string{ps.Expr}
			newline := ps.Newline
			j := i + 1
			for !newline && j < len(p.Stmts) {
				if next, ok := p.Stmts[j].(PrintStmt); ok {
					exprs = append(exprs, next.Expr)
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
		if n := stmtNodeWithAssign(st, assigned, false); n != nil {
			root.Children = append(root.Children, n)
		}
	}
	return root, nil
}

func stmtNodeWithAssign(s Stmt, assigned map[string]bool, inFunc bool) *ast.Node {
	switch v := s.(type) {
	case VarDecl:
		kind := "let"
		if assigned[v.Name] {
			kind = "var"
		}
		if v.Value == "0" {
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
		return &ast.Node{Kind: "call", Value: "print", Children: []*ast.Node{exprNode(v.Expr)}}
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
		if v.Ret == "int" {
			n.Children = append(n.Children, &ast.Node{Kind: "type", Value: "int"})
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
	for _, s := range stmts {
		if n := stmtNodeWithAssign(s, assigned, true); n != nil {
			blk.Children = append(blk.Children, n)
		}
	}
	return blk
}

var reBinary = regexp.MustCompile(`^(.+)\s*(==|!=|<=|>=|<|>|\+|\-|\*|/|&&|\|\|)\s*(.+)$`)
var reCall = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\((.*)\)$`)
var reList = regexp.MustCompile(`^\{\s*([0-9]+(?:\s*,\s*[0-9]+)*)\s*\}$`)
var reIndex = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\[(.+)\]$`)
var reLen = regexp.MustCompile(`^sizeof\((\w+)\)\s*/\s*sizeof\((\w+)\[0\]\)$`)

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
	if m := reList.FindStringSubmatch(expr); m != nil {
		parts := strings.Split(m[1], ",")
		n := &ast.Node{Kind: "list"}
		for _, p := range parts {
			p = strings.TrimSpace(p)
			if i, err := strconv.Atoi(p); err == nil {
				n.Children = append(n.Children, &ast.Node{Kind: "int", Value: i})
			}
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
	ops := []string{"&&", "||", "==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/"}
	depth := 0
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '(':
			depth++
		case ')':
			if depth > 0 {
				depth--
			}
		default:
			if depth == 0 {
				for _, op := range ops {
					if strings.HasPrefix(s[i:], op) {
						left := strings.TrimSpace(s[:i])
						right := strings.TrimSpace(s[i+len(op):])
						return left, op, right, true
					}
				}
			}
		}
	}
	return "", "", "", false
}
