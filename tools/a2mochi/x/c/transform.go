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
	for _, st := range p.Stmts {
		n := stmtNodeWithAssign(st, assigned)
		if n != nil {
			root.Children = append(root.Children, n)
		}
	}
	return root, nil
}

func stmtNodeWithAssign(s Stmt, assigned map[string]bool) *ast.Node {
	switch v := s.(type) {
	case VarDecl:
		kind := "let"
		if assigned[v.Name] {
			kind = "var"
		}
		return &ast.Node{Kind: kind, Value: v.Name, Children: []*ast.Node{exprNode(v.Value)}}
	case Assign:
		return &ast.Node{Kind: "assign", Value: v.Name, Children: []*ast.Node{exprNode(v.Expr)}}
	case PrintStmt:
		return &ast.Node{Kind: "call", Value: "print", Children: []*ast.Node{exprNode(v.Expr)}}
	case While:
		cond := exprNode(v.Cond)
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
		cond := exprNode(v.Cond)
		thenBlock := blockNode(v.Then, assigned)
		elseBlock := blockNode(v.Else, assigned)
		return &ast.Node{Kind: "if", Children: []*ast.Node{cond, thenBlock, elseBlock}}
	default:
		return nil
	}
}

func blockNode(stmts []Stmt, assigned map[string]bool) *ast.Node {
	blk := &ast.Node{Kind: "block"}
	for _, s := range stmts {
		if n := stmtNodeWithAssign(s, assigned); n != nil {
			blk.Children = append(blk.Children, n)
		}
	}
	return blk
}

var reBinary = regexp.MustCompile(`^(.+)\s*(==|!=|<=|>=|<|>|\+|\-|\*|/)\s*(.+)$`)

func exprNode(expr string) *ast.Node {
	expr = strings.TrimSpace(expr)
	if m := reBinary.FindStringSubmatch(expr); m != nil {
		return &ast.Node{Kind: "binary", Value: m[2], Children: []*ast.Node{exprNode(m[1]), exprNode(m[3])}}
	}
	if strings.HasPrefix(expr, "\"") && strings.HasSuffix(expr, "\"") {
		if s, err := strconv.Unquote(expr); err == nil {
			return &ast.Node{Kind: "string", Value: s}
		}
	}
	if i, err := strconv.Atoi(expr); err == nil {
		return &ast.Node{Kind: "int", Value: i}
	}
	return &ast.Node{Kind: "selector", Value: expr}
}
