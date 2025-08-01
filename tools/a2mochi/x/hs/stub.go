//go:build !slow

package hs

import "mochi/ast"

type Field struct {
	Name string
	Type string
}

type Item struct {
	Kind       string
	Name       string
	Params     []string
	Body       string
	Type       string
	Fields     []Field
	Collection string
	Start      string
	End        string
	Line       int
}

type Program struct {
	Items  []Item
	Source string
}

func Parse(src string) (*Program, error) { return &Program{Source: src}, nil }

func Print(node *ast.Node) (string, error) { return "", nil }

func Transform(p *Program) (*ast.Node, error) { return nil, nil }
