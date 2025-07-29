//go:build slow

package php

import (
	"fmt"
	"strconv"
	"strings"

	pnode "github.com/z7zmey/php-parser/node"
	"github.com/z7zmey/php-parser/node/expr"
	"github.com/z7zmey/php-parser/node/expr/assign"
	binary "github.com/z7zmey/php-parser/node/expr/binary"
	castexpr "github.com/z7zmey/php-parser/node/expr/cast"
	"github.com/z7zmey/php-parser/node/name"
	"github.com/z7zmey/php-parser/node/scalar"
	"github.com/z7zmey/php-parser/node/stmt"
	"github.com/z7zmey/php-parser/php7"

	"mochi/ast"
)

type Program struct {
	Functions []Func      `json:"functions"`
	Classes   []Class     `json:"classes"`
	Vars      []Var       `json:"vars"`
	Prints    []PrintStmt `json:"prints"`
	Stmts     []*ast.Node `json:"stmts"`
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
		case *stmt.Echo:
			parseExprStmt(prog, &stmt.Expression{Expr: n})
		case *stmt.For:
			if node := forNodeFromStmt(n, prog); node != nil {
				prog.Stmts = append(prog.Stmts, node)
			}
		case *stmt.Foreach:
			if node := foreachNodeFromStmt(n, prog); node != nil {
				prog.Stmts = append(prog.Stmts, node)
			}
		case *stmt.While:
			if node := whileNodeFromStmt(n, prog); node != nil {
				prog.Stmts = append(prog.Stmts, node)
			}
		case *stmt.If:
			if node := ifNodeFromStmt(n, prog); node != nil {
				prog.Stmts = append(prog.Stmts, node)
			}
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
	if ret == "" && boolReturn(fn.Stmts) {
		ret = "bool"
	}
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
	if ret == "" && boolReturnNode(m.Stmt) {
		ret = "bool"
	}
	doc := strings.TrimSpace(m.PhpDocComment)
	pos := m.GetPosition()
	return Func{Name: name, Params: params, Return: ret, Doc: doc, StartLine: pos.StartLine, EndLine: pos.EndLine}
}

func boolReturn(stmts []pnode.Node) bool {
	for _, st := range stmts {
		r, ok := st.(*stmt.Return)
		if !ok || r.Expr == nil {
			continue
		}
		if cf, ok := r.Expr.(*expr.ConstFetch); ok {
			name := nameString(cf.Constant)
			if name == "true" || name == "false" {
				return true
			}
		}
	}
	return false
}

func boolReturnNode(n pnode.Node) bool {
	if list, ok := n.(*stmt.StmtList); ok {
		return boolReturn(list.Stmts)
	}
	return false
}

func parseExprStmt(p *Program, st *stmt.Expression) *ast.Node {
	return parseExprStmtRec(p, st, true)
}

func parseExprStmtRec(p *Program, st *stmt.Expression, record bool) *ast.Node {
	switch e := st.Expr.(type) {
	case *assign.Assign:
		v, ok := e.Variable.(*expr.Variable)
		if !ok {
			return nil
		}
		val, ok := simpleExpr(e.Expression)
		if !ok {
			return nil
		}
		pos := e.GetPosition()
		if record {
			p.Vars = append(p.Vars, Var{
				Name:      identString(v.VarName),
				Value:     val,
				StartLine: pos.StartLine,
				EndLine:   pos.EndLine,
			})
		}
		exprNode, err := parseExpr(val)
		if err != nil {
			return nil
		}
		return &ast.Node{Kind: "let", Value: identString(v.VarName), Children: []*ast.Node{exprNode}}
	case *expr.FunctionCall:
		name := nameString(e.Function)
		if name != "_print" || e.ArgumentList == nil || len(e.ArgumentList.Arguments) != 1 {
			return nil
		}
		argNode, ok := e.ArgumentList.Arguments[0].(*pnode.Argument)
		if !ok {
			return nil
		}
		val, ok := simpleExpr(argNode.Expr)
		if !ok {
			return nil
		}
		pos := e.GetPosition()
		if record {
			p.Prints = append(p.Prints, PrintStmt{Expr: val, StartLine: pos.StartLine, EndLine: pos.EndLine})
		}
		exprNode, err := parseExpr(val)
		if err != nil {
			return nil
		}
		return &ast.Node{Kind: "call", Value: "print", Children: []*ast.Node{exprNode}}
	case *stmt.Echo:
		if len(e.Exprs) == 0 {
			return nil
		}
		val, ok := simpleExpr(e.Exprs[0])
		if !ok {
			return nil
		}
		pos := e.GetPosition()
		if record {
			p.Prints = append(p.Prints, PrintStmt{Expr: val, StartLine: pos.StartLine, EndLine: pos.EndLine})
		}
		exprNode, err := parseExpr(val)
		if err != nil {
			return nil
		}
		return &ast.Node{Kind: "call", Value: "print", Children: []*ast.Node{exprNode}}
	}
	return nil
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
		// v.Value already includes surrounding quotes
		if strings.HasPrefix(v.Value, "\"") && strings.HasSuffix(v.Value, "\"") {
			return v.Value, true
		}
		return strconv.Quote(v.Value), true
	case *expr.Variable:
		return identString(v.VarName), true
	case *expr.UnaryMinus:
		val, ok := simpleExpr(v.Expr)
		if !ok {
			return "", false
		}
		return "-" + val, true
	case *castexpr.Int:
		val, ok := simpleExpr(v.Expr)
		if !ok {
			return "", false
		}
		return fmt.Sprintf("(%s as int)", val), true
	case *expr.FunctionCall:
		name := nameString(v.Function)
		args := []string{}
		if v.ArgumentList != nil {
			for _, a := range v.ArgumentList.Arguments {
				arg, ok := a.(*pnode.Argument)
				if !ok {
					return "", false
				}
				val, ok := simpleExpr(arg.Expr)
				if !ok {
					return "", false
				}
				args = append(args, val)
			}
		}
		switch name {
		case "array_sum":
			name = "sum"
		case "count":
			name = "len"
		case "strlen":
			name = "len"
		case "substr":
			if len(args) == 3 {
				end := fmt.Sprintf("(%s + %s)", args[1], args[2])
				return fmt.Sprintf("substring(%s, %s, %s)", args[0], args[1], end), true
			}
		case "str_contains":
			if len(args) == 2 {
				return fmt.Sprintf("%s.contains(%s)", args[0], args[1]), true
			}
		case "array_merge":
			if len(args) == 2 && strings.HasPrefix(args[1], "[") && strings.HasSuffix(args[1], "]") {
				val := strings.TrimSuffix(strings.TrimPrefix(args[1], "["), "]")
				return fmt.Sprintf("append(%s, %s)", args[0], val), true
			}
		case "array_values":
			name = "values"
		case "intval":
			if len(args) == 1 {
				return fmt.Sprintf("(%s as int)", args[0]), true
			}
		}
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), true
	case *expr.Ternary:
		// Handle boolean ternary patterns and general case
		if v.IfTrue != nil && v.IfFalse != nil {
			// Pattern: is_float(x) ? json_encode(x, 1344) : x
			if condCall, ok := v.Condition.(*expr.FunctionCall); ok {
				if nameString(condCall.Function) == "is_float" && len(condCall.ArgumentList.Arguments) == 1 {
					arg := condCall.ArgumentList.Arguments[0]
					if trueCall, ok := v.IfTrue.(*expr.FunctionCall); ok && nameString(trueCall.Function) == "json_encode" && len(trueCall.ArgumentList.Arguments) >= 1 {
						if argTrue, ok := trueCall.ArgumentList.Arguments[0].(*pnode.Argument); ok {
							if argCond, ok := arg.(*pnode.Argument); ok {
								if simple, ok := simpleExpr(argTrue.Expr); ok && simpleExprEqual(simple, argCond.Expr) {
									return simpleExpr(v.IfFalse)
								}
							}
						}
					}
				}
			}
			t, ok1 := simpleExpr(v.IfTrue)
			f, ok2 := simpleExpr(v.IfFalse)
			c, ok3 := simpleExpr(v.Condition)
			if ok1 && ok2 && ok3 {
				if t == "\"True\"" && f == "\"False\"" {
					return c, true
				}
				return fmt.Sprintf("if %s then %s else %s", c, t, f), true
			}
		}
		if v.IfFalse != nil {
			return simpleExpr(v.IfFalse)
		}
		return "", false
	case *binary.Concat:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s + %s)", left, right), true
	case *binary.Equal:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s == %s)", left, right), true
	case *binary.NotEqual:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s != %s)", left, right), true
	case *binary.Greater:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s > %s)", left, right), true
	case *binary.GreaterOrEqual:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s >= %s)", left, right), true
	case *binary.Smaller:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s < %s)", left, right), true
	case *binary.SmallerOrEqual:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s <= %s)", left, right), true
	case *binary.BooleanAnd:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s && %s)", left, right), true
	case *binary.LogicalAnd:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s && %s)", left, right), true
	case *binary.BooleanOr:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s || %s)", left, right), true
	case *binary.LogicalOr:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s || %s)", left, right), true
	case *binary.Plus:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		if strings.HasPrefix(right, "-") {
			right = "(" + right + ")"
		}
		return fmt.Sprintf("(%s + %s)", left, right), true
	case *binary.Minus:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s - %s)", left, right), true
	case *binary.Mul:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s * %s)", left, right), true
	case *binary.Div:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s / %s)", left, right), true
	case *binary.Mod:
		left, ok1 := simpleExpr(v.Left)
		if !ok1 {
			return "", false
		}
		right, ok2 := simpleExpr(v.Right)
		if !ok2 {
			return "", false
		}
		return fmt.Sprintf("(%s %% %s)", left, right), true
	case *expr.ArrayDimFetch:
		base, ok := simpleExpr(v.Variable)
		if !ok {
			return "", false
		}
		dim, ok := simpleExpr(v.Dim)
		if !ok {
			return "", false
		}
		if _, nested := v.Variable.(*expr.ArrayDimFetch); nested {
			base = fmt.Sprintf("(%s as map<string, any>)", base)
		}
		return fmt.Sprintf("%s[%s]", base, dim), true
	case *expr.Array:
		return arrayExpr(v.Items)
	case *expr.ShortArray:
		return arrayExpr(v.Items)
	}
	return "", false
}

func arrayExpr(items []pnode.Node) (string, bool) {
	elems := make([]string, 0, len(items))
	isMap := false
	for _, it := range items {
		ai, ok := it.(*expr.ArrayItem)
		if !ok {
			return "", false
		}
		val, ok := simpleExpr(ai.Val)
		if !ok {
			return "", false
		}
		if ai.Key != nil {
			key, ok := simpleExpr(ai.Key)
			if !ok {
				return "", false
			}
			isMap = true
			elems = append(elems, fmt.Sprintf("%s: %s", key, val))
		} else {
			elems = append(elems, val)
		}
	}
	if isMap {
		// simple support for maps; mixed arrays are not handled
		return "{" + strings.Join(elems, ", ") + "}", true
	}
	return "[" + strings.Join(elems, ", ") + "]", true
}

func simpleExprEqual(s string, n pnode.Node) bool {
	other, ok := simpleExpr(n)
	return ok && other == s
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

func exprNode(exprStr string) (*ast.Node, error) {
	return parseExpr(exprStr)
}

func parseStmtList(list *stmt.StmtList, p *Program) []*ast.Node {
	var out []*ast.Node
	for _, s := range list.Stmts {
		out = append(out, parseStatement(s, p)...)
	}
	return out
}

func parseStatement(n pnode.Node, p *Program) []*ast.Node {
	switch s := n.(type) {
	case *stmt.Expression:
		if node := parseExprStmtRec(p, s, false); node != nil {
			return []*ast.Node{node}
		}
	case *stmt.Echo:
		if node := parseExprStmtRec(p, &stmt.Expression{Expr: s}, false); node != nil {
			return []*ast.Node{node}
		}
	case *stmt.For:
		if node := forNodeFromStmt(s, p); node != nil {
			return []*ast.Node{node}
		}
	case *stmt.Foreach:
		if node := foreachNodeFromStmt(s, p); node != nil {
			return []*ast.Node{node}
		}
	case *stmt.While:
		if node := whileNodeFromStmt(s, p); node != nil {
			return []*ast.Node{node}
		}
	case *stmt.If:
		if node := ifNodeFromStmt(s, p); node != nil {
			return []*ast.Node{node}
		}
	case *stmt.Break:
		return []*ast.Node{{Kind: "break"}}
	case *stmt.Continue:
		return []*ast.Node{{Kind: "continue"}}
	case *stmt.StmtList:
		return parseStmtList(s, p)
	}
	return nil
}

func forNodeFromStmt(f *stmt.For, p *Program) *ast.Node {
	if len(f.Init) != 1 || len(f.Cond) != 1 {
		return nil
	}
	init, ok := f.Init[0].(*assign.Assign)
	if !ok {
		return nil
	}
	v, ok := init.Variable.(*expr.Variable)
	if !ok {
		return nil
	}
	startStr, ok := simpleExpr(init.Expression)
	if !ok {
		return nil
	}
	var endStr string
	switch c := f.Cond[0].(type) {
	case *binary.Smaller:
		endStr, _ = simpleExpr(c.Right)
	case *binary.SmallerOrEqual:
		if val, ok := simpleExpr(c.Right); ok {
			endStr = fmt.Sprintf("(%s + 1)", val)
		}
	default:
		return nil
	}
	bodyList, ok := f.Stmt.(*stmt.StmtList)
	if !ok {
		return nil
	}
	body := parseStmtList(bodyList, p)
	startNode, err1 := exprNode(startStr)
	if err1 != nil {
		return nil
	}
	endNode, err2 := exprNode(endStr)
	if err2 != nil {
		return nil
	}
	rangeNode := &ast.Node{Kind: "range", Children: []*ast.Node{startNode, endNode}}
	block := &ast.Node{Kind: "block", Children: body}
	return &ast.Node{Kind: "for", Value: identString(v.VarName), Children: []*ast.Node{rangeNode, block}}
}

func foreachNodeFromStmt(f *stmt.Foreach, p *Program) *ast.Node {
	v, ok := f.Variable.(*expr.Variable)
	if !ok {
		return nil
	}
	srcStr, ok := simpleExpr(f.Expr)
	if !ok {
		return nil
	}
	bodyList, ok := f.Stmt.(*stmt.StmtList)
	if !ok {
		return nil
	}
	body := parseStmtList(bodyList, p)
	srcNode, err := exprNode(srcStr)
	if err != nil {
		return nil
	}
	inNode := &ast.Node{Kind: "in", Children: []*ast.Node{srcNode}}
	block := &ast.Node{Kind: "block", Children: body}
	return &ast.Node{Kind: "for", Value: identString(v.VarName), Children: []*ast.Node{inNode, block}}
}

func whileNodeFromStmt(w *stmt.While, p *Program) *ast.Node {
	condStr, ok := simpleExpr(w.Cond)
	if !ok {
		return nil
	}
	bodyList, ok := w.Stmt.(*stmt.StmtList)
	if !ok {
		return nil
	}
	body := parseStmtList(bodyList, p)
	condNode, err := exprNode(condStr)
	if err != nil {
		return nil
	}
	block := &ast.Node{Kind: "block", Children: body}
	return &ast.Node{Kind: "while", Children: []*ast.Node{condNode, block}}
}

func ifNodeFromStmt(i *stmt.If, p *Program) *ast.Node {
	condStr, ok := simpleExpr(i.Cond)
	if !ok {
		return nil
	}
	thenList, ok := i.Stmt.(*stmt.StmtList)
	if !ok {
		return nil
	}
	thenBlock := &ast.Node{Kind: "block", Children: parseStmtList(thenList, p)}
	condNode, err := exprNode(condStr)
	if err != nil {
		return nil
	}
	n := &ast.Node{Kind: "if", Children: []*ast.Node{condNode, thenBlock}}
	if i.Else != nil {
		if elseList, ok := i.Else.(*stmt.StmtList); ok {
			elseBlock := &ast.Node{Kind: "block", Children: parseStmtList(elseList, p)}
			n.Children = append(n.Children, elseBlock)
		}
	}
	return n
}
