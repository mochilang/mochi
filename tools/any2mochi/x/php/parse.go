package php

import (
	"fmt"
	"strings"

	pnode "github.com/z7zmey/php-parser/node"
	"github.com/z7zmey/php-parser/node/expr"
	"github.com/z7zmey/php-parser/node/expr/assign"
	"github.com/z7zmey/php-parser/node/name"
	"github.com/z7zmey/php-parser/node/scalar"
	"github.com/z7zmey/php-parser/node/stmt"
	"github.com/z7zmey/php-parser/php7"
)

type Program struct {
	Functions []Func      `json:"functions"`
	Classes   []Class     `json:"classes"`
	Vars      []Var       `json:"vars"`
	Prints    []PrintStmt `json:"prints"`
}

// ParseError represents a PHP parse error with line information.
type ParseError struct {
	Msg  string
	Line int
}

func (e *ParseError) Error() string { return fmt.Sprintf("%s at line %d", e.Msg, e.Line) }

type Param struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

type Field struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

type Var struct {
	Name      string `json:"name"`
	Value     string `json:"value,omitempty"`
	StartLine int    `json:"start_line"`
	EndLine   int    `json:"end_line"`
}

type PrintStmt struct {
	Expr      string `json:"expr"`
	StartLine int    `json:"start_line"`
	EndLine   int    `json:"end_line"`
}

type Func struct {
	Name      string  `json:"name"`
	Params    []Param `json:"params"`
	Return    string  `json:"return,omitempty"`
	Doc       string  `json:"doc,omitempty"`
	StartLine int     `json:"start_line"`
	EndLine   int     `json:"end_line"`
}

type Class struct {
	Name      string  `json:"name"`
	Fields    []Field `json:"fields"`
	Methods   []Func  `json:"methods"`
	Doc       string  `json:"doc,omitempty"`
	StartLine int     `json:"start_line"`
	EndLine   int     `json:"end_line"`
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
		case *stmt.Expression:
			parseExprStmt(prog, n)
		}
	}
	return prog, nil
}

func funcFromFn(fn *stmt.Function) Func {
	name := identString(fn.FunctionName)
	var params []Param
	for _, p := range fn.Params {
		if param, ok := p.(*pnode.Parameter); ok {
			params = append(params, paramInfo(param))
		}
	}
	ret := typeString(fn.ReturnType)
	doc := strings.TrimSpace(fn.PhpDocComment)
	pos := fn.GetPosition()
	return Func{Name: name, Params: params, Return: ret, Doc: doc, StartLine: pos.StartLine, EndLine: pos.EndLine}
}

func classFromNode(c *stmt.Class) Class {
	pos := c.GetPosition()
	cl := Class{Name: identString(c.ClassName), Doc: strings.TrimSpace(c.PhpDocComment), StartLine: pos.StartLine, EndLine: pos.EndLine}
	for _, st := range c.Stmts {
		switch n := st.(type) {
		case *stmt.PropertyList:
			for _, p := range n.Properties {
				if prop, ok := p.(*stmt.Property); ok {
					cl.Fields = append(cl.Fields, fieldInfo(prop, n))
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
	var params []Param
	for _, p := range m.Params {
		if param, ok := p.(*pnode.Parameter); ok {
			params = append(params, paramInfo(param))
		}
	}
	ret := typeString(m.ReturnType)
	doc := strings.TrimSpace(m.PhpDocComment)
	pos := m.GetPosition()
	return Func{Name: name, Params: params, Return: ret, Doc: doc, StartLine: pos.StartLine, EndLine: pos.EndLine}
}

func parseExprStmt(p *Program, st *stmt.Expression) {
	switch e := st.Expr.(type) {
	case *assign.Assign:
		v, ok := e.Variable.(*expr.Variable)
		if !ok {
			return
		}
		val, ok := simpleExpr(e.Expression)
		if !ok {
			return
		}
		pos := e.GetPosition()
		p.Vars = append(p.Vars, Var{
			Name:      identString(v.VarName),
			Value:     val,
			StartLine: pos.StartLine,
			EndLine:   pos.EndLine,
		})
	case *expr.FunctionCall:
		name := nameString(e.Function)
		if name != "_print" || e.ArgumentList == nil || len(e.ArgumentList.Arguments) != 1 {
			return
		}
		argNode, ok := e.ArgumentList.Arguments[0].(*pnode.Argument)
		if !ok {
			return
		}
		val, ok := simpleExpr(argNode.Expr)
		if !ok {
			return
		}
		pos := e.GetPosition()
		p.Prints = append(p.Prints, PrintStmt{Expr: val, StartLine: pos.StartLine, EndLine: pos.EndLine})
	}
}

func nameString(n pnode.Node) string {
	switch v := n.(type) {
	case *name.Name:
		return joinNameParts(v.Parts)
	case *name.FullyQualified:
		return joinNameParts(v.Parts)
	case *name.Relative:
		return joinNameParts(v.Parts)
	case *name.NamePart:
		return v.Value
	case *expr.Variable:
		return identString(v.VarName)
	}
	if id, ok := n.(*pnode.Identifier); ok {
		return id.Value
	}
	return ""
}

func simpleExpr(n pnode.Node) (string, bool) {
	switch v := n.(type) {
	case *scalar.Lnumber:
		return v.Value, true
	case *scalar.Dnumber:
		return v.Value, true
	case *scalar.String:
		return fmt.Sprintf("\"%s\"", v.Value), true
	case *expr.Variable:
		return identString(v.VarName), true
	case *expr.UnaryMinus:
		val, ok := simpleExpr(v.Expr)
		if !ok {
			return "", false
		}
		return "-" + val, true
	case *expr.Array:
		return arrayExpr(v.Items)
	case *expr.ShortArray:
		return arrayExpr(v.Items)
	}
	return "", false
}

func arrayExpr(items []pnode.Node) (string, bool) {
	elems := make([]string, 0, len(items))
	for _, it := range items {
		ai, ok := it.(*expr.ArrayItem)
		if !ok {
			return "", false
		}
		val, ok := simpleExpr(ai.Val)
		if !ok {
			return "", false
		}
		elems = append(elems, val)
	}
	return "[" + strings.Join(elems, ", ") + "]", true
}

func paramInfo(p *pnode.Parameter) Param {
	name := ""
	if v, ok := p.Variable.(*expr.Variable); ok {
		name = identString(v.VarName)
	}
	typ := typeString(p.VariableType)
	return Param{Name: name, Type: typ}
}

func fieldInfo(p *stmt.Property, list *stmt.PropertyList) Field {
	name := ""
	if v, ok := p.Variable.(*expr.Variable); ok {
		name = identString(v.VarName)
	}
	typ := typeString(list.Type)
	return Field{Name: name, Type: typ}
}

func identString(n pnode.Node) string {
	if id, ok := n.(*pnode.Identifier); ok {
		return id.Value
	}
	return ""
}

func typeString(n pnode.Node) string {
	switch t := n.(type) {
	case *name.Name:
		return joinNameParts(t.Parts)
	case *name.FullyQualified:
		return joinNameParts(t.Parts)
	case *name.Relative:
		return joinNameParts(t.Parts)
	case *name.NamePart:
		return t.Value
	}
	return ""
}

func joinNameParts(parts []pnode.Node) string {
	var out []string
	for _, p := range parts {
		if np, ok := p.(*name.NamePart); ok {
			out = append(out, np.Value)
		}
	}
	return strings.Join(out, "\\")
}
