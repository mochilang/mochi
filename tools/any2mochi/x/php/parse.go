package php

import (
	"fmt"

	pnode "github.com/z7zmey/php-parser/node"
	"github.com/z7zmey/php-parser/node/expr"
	"github.com/z7zmey/php-parser/node/stmt"
	"github.com/z7zmey/php-parser/php7"
)

type Program struct {
	Functions []Func  `json:"functions"`
	Classes   []Class `json:"classes"`
}

// ParseError represents a PHP parse error with line information.
type ParseError struct {
	Msg  string
	Line int
}

func (e *ParseError) Error() string { return fmt.Sprintf("%s at line %d", e.Msg, e.Line) }

type Func struct {
	Name      string   `json:"name"`
	Params    []string `json:"params"`
	StartLine int      `json:"start_line"`
	EndLine   int      `json:"end_line"`
}

type Class struct {
	Name      string   `json:"name"`
	Fields    []string `json:"fields"`
	Methods   []Func   `json:"methods"`
	StartLine int      `json:"start_line"`
	EndLine   int      `json:"end_line"`
}

// Parse parses PHP source using the php-parser library and returns a
// simplified AST representation.
func Parse(src string) (*Program, error) {
	parser := php7.NewParser([]byte(src), "7.4")
	parser.Parse()
	if errs := parser.GetErrors(); len(errs) > 0 {
		e := errs[0]
		line := 0
		if e.Pos != nil {
			line = e.Pos.StartLine
		}
		return nil, &ParseError{Msg: e.Msg, Line: line}
	}
	root, ok := parser.GetRootNode().(*pnode.Root)
	if !ok {
		return nil, fmt.Errorf("unexpected root")
	}
	prog := &Program{}
	for _, st := range root.Stmts {
		switch n := st.(type) {
		case *stmt.Function:
			prog.Functions = append(prog.Functions, funcFromFn(n))
		case *stmt.Class:
			prog.Classes = append(prog.Classes, classFromNode(n))
		}
	}
	return prog, nil
}

func funcFromFn(fn *stmt.Function) Func {
	name := identString(fn.FunctionName)
	var params []string
	for _, p := range fn.Params {
		if param, ok := p.(*pnode.Parameter); ok {
			params = append(params, paramName(param))
		}
	}
	pos := fn.GetPosition()
	return Func{Name: name, Params: params, StartLine: pos.StartLine, EndLine: pos.EndLine}
}

func classFromNode(c *stmt.Class) Class {
	pos := c.GetPosition()
	cl := Class{Name: identString(c.ClassName), StartLine: pos.StartLine, EndLine: pos.EndLine}
	for _, st := range c.Stmts {
		switch n := st.(type) {
		case *stmt.PropertyList:
			for _, p := range n.Properties {
				if prop, ok := p.(*stmt.Property); ok {
					cl.Fields = append(cl.Fields, propertyName(prop))
				}
			}
		case *stmt.ClassMethod:
			cl.Methods = append(cl.Methods, methodFromNode(n))
		}
	}
	return cl
}

func methodFromNode(m *stmt.ClassMethod) Func {
	name := identString(m.MethodName)
	var params []string
	for _, p := range m.Params {
		if param, ok := p.(*pnode.Parameter); ok {
			params = append(params, paramName(param))
		}
	}
	pos := m.GetPosition()
	return Func{Name: name, Params: params, StartLine: pos.StartLine, EndLine: pos.EndLine}
}

func paramName(p *pnode.Parameter) string {
	if v, ok := p.Variable.(*expr.Variable); ok {
		return identString(v.VarName)
	}
	return ""
}

func propertyName(p *stmt.Property) string {
	if v, ok := p.Variable.(*expr.Variable); ok {
		return identString(v.VarName)
	}
	return ""
}

func identString(n pnode.Node) string {
	if id, ok := n.(*pnode.Identifier); ok {
		return id.Value
	}
	return ""
}
