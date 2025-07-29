//go:build slow

package kt

import (
	"fmt"

	"mochi/ast"
)

// Transform builds a Mochi AST from a parsed Kotlin Program.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	root := &ast.Node{Kind: "program"}
	for _, d := range p.Nodes {
		var n *ast.Node
		switch d.Kind {
		case "var":
			n = varNode(d)
		case "func":
			n = funcNode(d)
		case "type":
			n = typeDeclNode(d)
		}
		if n != nil {
			root.Children = append(root.Children, n)
		}
	}
	return root, nil
}

func typeNode(t string) *ast.Node {
	mt := mapType(t)
	if mt == "" {
		return nil
	}
	return &ast.Node{Kind: "type", Value: mt}
}

func varNode(n Node) *ast.Node {
	kind := "let"
	if n.Kind == "var" {
		kind = "var"
	}
	v := &ast.Node{Kind: kind, Value: n.Name}
	if n.Ret != "" {
		if tn := typeNode(n.Ret); tn != nil {
			v.Children = append(v.Children, tn)
		}
	}
	return v
}

func funcNode(n Node) *ast.Node {
	fn := &ast.Node{Kind: "fun", Value: n.Name}
	for _, prm := range n.Params {
		pn := &ast.Node{Kind: "param", Value: prm.Name}
		if prm.Typ != "" {
			if tn := typeNode(prm.Typ); tn != nil {
				pn.Children = append(pn.Children, tn)
			}
		}
		fn.Children = append(fn.Children, pn)
	}
	if n.Ret != "" && n.Ret != "Unit" {
		if tn := typeNode(n.Ret); tn != nil {
			fn.Children = append(fn.Children, tn)
		}
	}
	return fn
}

func typeDeclNode(n Node) *ast.Node {
	typ := &ast.Node{Kind: "type", Value: n.Name}
	for _, f := range n.Fields {
		fld := &ast.Node{Kind: "field", Value: f.Name}
		if f.Typ != "" {
			if tn := typeNode(f.Typ); tn != nil {
				fld.Children = append(fld.Children, tn)
			}
		}
		typ.Children = append(typ.Children, fld)
	}
	return typ
}
