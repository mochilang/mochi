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
	root := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n := stmtNode(st)
		if n != nil {
			root.Children = append(root.Children, n)
		}
	}
	return root, nil
}

func stmtNode(s Stmt) *ast.Node {
	switch v := s.(type) {
	case VarDecl:
		return &ast.Node{Kind: "let", Value: v.Name, Children: []*ast.Node{exprNode(v.Value)}}
	case Assign:
		return &ast.Node{Kind: "assign", Value: v.Name, Children: []*ast.Node{exprNode(v.Expr)}}
	case PrintStmt:
		return &ast.Node{Kind: "call", Value: "print", Children: []*ast.Node{exprNode(v.Expr)}}
	case While:
		cond := exprNode(v.Cond)
		body := blockNode(v.Body)
		return &ast.Node{Kind: "while", Children: []*ast.Node{cond, body}}
	case For:
		start := exprNode(v.Start)
		end := exprNode(v.End)
		rng := &ast.Node{Kind: "range", Children: []*ast.Node{start, end}}
		body := blockNode(v.Body)
		n := &ast.Node{Kind: "for", Value: v.Var, Children: []*ast.Node{rng, body}}
		return n
	case If:
		cond := exprNode(v.Cond)
		thenBlock := blockNode(v.Then)
		elseBlock := blockNode(v.Else)
		return &ast.Node{Kind: "if", Children: []*ast.Node{cond, thenBlock, elseBlock}}
	default:
		return nil
	}
}

func blockNode(stmts []Stmt) *ast.Node {
	blk := &ast.Node{Kind: "block"}
	for _, s := range stmts {
		if n := stmtNode(s); n != nil {
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
