//go:build slow

package kt

import (
	"fmt"
	"strings"

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
	v := &ast.Node{Kind: kind, Value: sanitizeName(n.Name)}
	if n.Ret != "" {
		if tn := typeNode(n.Ret); tn != nil {
			v.Children = append(v.Children, tn)
		}
	}
	return v
}

func funcNode(n Node) *ast.Node {
	fn := &ast.Node{Kind: "fun", Value: sanitizeName(n.Name)}
	for _, prm := range n.Params {
		pn := &ast.Node{Kind: "param", Value: sanitizeName(prm.Name)}
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
	typ := &ast.Node{Kind: "type", Value: sanitizeName(n.Name)}
	for _, f := range n.Fields {
		fld := &ast.Node{Kind: "field", Value: sanitizeName(f.Name)}
		if f.Typ != "" {
			if tn := typeNode(f.Typ); tn != nil {
				fld.Children = append(fld.Children, tn)
			}
		}
		typ.Children = append(typ.Children, fld)
	}
	return typ
}

func sanitizeName(s string) string {
	s = strings.ReplaceAll(s, "-", "_")
	s = strings.ReplaceAll(s, "?", "_p")
	s = strings.ReplaceAll(s, "!", "_bang")
	if reserved[s] {
		s = "_" + s
	}
	if s == "" {
		return "_"
	}
	if s[0] >= '0' && s[0] <= '9' {
		s = "_" + s
	}
	return s
}

var reserved = map[string]bool{
	"expect":    true,
	"test":      true,
	"agent":     true,
	"intention": true,
	"stream":    true,
	"emit":      true,
	"type":      true,
	"fun":       true,
	"extern":    true,
	"import":    true,
	"return":    true,
	"break":     true,
	"continue":  true,
	"let":       true,
	"var":       true,
	"if":        true,
	"else":      true,
	"then":      true,
	"for":       true,
	"while":     true,
	"in":        true,
	"generate":  true,
	"match":     true,
	"fetch":     true,
	"load":      true,
	"save":      true,
	"package":   true,
	"export":    true,
	"fact":      true,
	"rule":      true,
	"all":       true,
	"null":      true,
}
