package plan

import (
	"mochi/ast"
	"mochi/parser"
)

// Node represents a typed logical query plan node.
type Node interface{ isNode() }

type Scan struct {
	Src   *parser.Expr
	Alias string
	Elem  any
}

func (*Scan) isNode() {}

type Select struct {
	Expr  *parser.Expr
	Input Node
}

func (*Select) isNode() {}

type Where struct {
	Cond  *parser.Expr
	Input Node
}

func (*Where) isNode() {}

type Join struct {
	Left     Node
	Right    Node
	On       *parser.Expr
	JoinType string
}

func (*Join) isNode() {}

type Group struct {
	By    []*parser.Expr
	Name  string
	Input Node
}

func (*Group) isNode() {}

type Sort struct {
	Key   *parser.Expr
	Input Node
}

func (*Sort) isNode() {}

type Limit struct {
	Skip  *parser.Expr
	Take  *parser.Expr
	Input Node
}

func (*Limit) isNode() {}

// nodeTree converts a plan Node to an AST node used for printing.
func nodeTree(pl Node) *ast.Node {
	switch p := pl.(type) {
	case *Scan:
		return &ast.Node{Kind: "scan", Value: p.Alias, Children: []*ast.Node{ast.FromExpr(p.Src)}}
	case *Select:
		return &ast.Node{Kind: "select", Children: []*ast.Node{ast.FromExpr(p.Expr), nodeTree(p.Input)}}
	case *Where:
		return &ast.Node{Kind: "where", Children: []*ast.Node{ast.FromExpr(p.Cond), nodeTree(p.Input)}}
	case *Join:
		kind := "join"
		if p.JoinType != "inner" {
			kind = p.JoinType + "_join"
		}
		n := &ast.Node{Kind: kind, Children: []*ast.Node{nodeTree(p.Left), nodeTree(p.Right)}}
		if p.On != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "on", Children: []*ast.Node{ast.FromExpr(p.On)}})
		}
		return n
	case *Group:
		n := &ast.Node{Kind: "group", Value: p.Name}
		for _, e := range p.By {
			n.Children = append(n.Children, ast.FromExpr(e))
		}
		n.Children = append(n.Children, nodeTree(p.Input))
		return n
	case *Sort:
		return &ast.Node{Kind: "sort", Children: []*ast.Node{ast.FromExpr(p.Key), nodeTree(p.Input)}}
	case *Limit:
		n := &ast.Node{Kind: "limit"}
		if p.Skip != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "skip", Children: []*ast.Node{ast.FromExpr(p.Skip)}})
		}
		if p.Take != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "take", Children: []*ast.Node{ast.FromExpr(p.Take)}})
		}
		n.Children = append(n.Children, nodeTree(p.Input))
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// String pretty prints a plan Node as a Lisp-like tree.
func String(pl Node) string {
	return nodeTree(pl).String()
}
