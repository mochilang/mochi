//go:build slow

package php

import (
	"fmt"

	"mochi/ast"
	"mochi/parser"
)

// ConvertError represents a conversion failure with line context.
type ConvertError struct {
	Path string
	Line int
	Msg  string
	Snip string
}

func (e *ConvertError) Error() string {
	loc := e.Path
	if e.Line > 0 {
		loc = fmt.Sprintf("%s:%d", e.Path, e.Line)
	}
	return fmt.Sprintf("%s: %s\n%s", loc, e.Msg, e.Snip)
}

// Transform converts a parsed PHP program to a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	n := &ast.Node{Kind: "program"}
	for _, c := range p.Classes {
		n.Children = append(n.Children, classNode(c))
	}
	for _, f := range p.Functions {
		n.Children = append(n.Children, funcNode(f))
	}
	for _, v := range p.Vars {
		node, err := varNode(v)
		if err != nil {
			return nil, err
		}
		n.Children = append(n.Children, node)
	}
	for _, pr := range p.Prints {
		expr, err := parseExpr(pr.Expr)
		if err != nil {
			return nil, &ConvertError{Line: pr.StartLine, Msg: err.Error()}
		}
		n.Children = append(n.Children, &ast.Node{Kind: "call", Value: "print", Children: []*ast.Node{expr}})
	}
	if len(n.Children) == 0 {
		return nil, fmt.Errorf("no convertible symbols found")
	}
	return n, nil
}

func classNode(c Class) *ast.Node {
	n := &ast.Node{Kind: "type", Value: c.Name}
	for _, f := range c.Fields {
		field := &ast.Node{Kind: "field", Value: f.Name}
		typ := typeNode(f.Type)
		field.Children = append(field.Children, typ)
		n.Children = append(n.Children, field)
	}
	for _, m := range c.Methods {
		n.Children = append(n.Children, funcNode(m))
	}
	return n
}

func funcNode(f Func) *ast.Node {
	n := &ast.Node{Kind: "fun", Value: f.Name}
	for _, p := range f.Params {
		pn := &ast.Node{Kind: "param", Value: p.Name}
		pn.Children = append(pn.Children, typeNode(p.Type))
		n.Children = append(n.Children, pn)
	}
	n.Children = append(n.Children, typeNode(f.Return))
	return n
}

func varNode(v Var) (*ast.Node, error) {
	n := &ast.Node{Kind: "let", Value: v.Name}
	if v.Value != "" {
		expr, err := parseExpr(v.Value)
		if err != nil {
			return nil, &ConvertError{Line: v.StartLine, Msg: err.Error()}
		}
		n.Children = append(n.Children, expr)
	}
	return n, nil
}

func typeNode(t string) *ast.Node {
	if t == "" {
		t = "any"
	}
	return &ast.Node{Kind: "type", Value: t}
}

// parseExpr parses a Mochi expression string to an AST node.
func parseExpr(expr string) (*ast.Node, error) {
	prog, err := parser.ParseString("let _ = " + expr)
	if err != nil {
		return nil, err
	}
	if len(prog.Statements) == 0 || prog.Statements[0].Let == nil {
		return nil, fmt.Errorf("invalid expression")
	}
	v := prog.Statements[0].Let.Value
	if v == nil {
		return nil, fmt.Errorf("invalid expression")
	}
	return ast.FromExpr(v), nil
}
