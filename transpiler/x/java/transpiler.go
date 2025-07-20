//go:build slow

package javatr

import (
	"bytes"
	"fmt"
	"io"

	"mochi/parser"
	"mochi/types"
)

var varTypes map[string]string

func javaType(t string) string {
	switch t {
	case "int":
		return "int"
	case "bool":
		return "boolean"
	case "string":
		return "String"
	case "void":
		return "void"
	case "int[]":
		return "int[]"
	case "string[]":
		return "String[]"
	case "bool[]":
		return "boolean[]"
	case "map":
		return "java.util.Map<String, Integer>"
	default:
		return "int"
	}
}

func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		return "int"
	case *BoolLit:
		return "boolean"
	case *StringLit:
		return "string"
	case *SubstringExpr:
		return "string"
	case *IndexExpr:
		if isStringExpr(ex.Target) {
			return "string"
		}
	case *SliceExpr:
		if isStringExpr(ex.Value) {
			return "string"
		}
	case *ListLit:
		if len(ex.Elems) > 0 {
			t := inferType(ex.Elems[0])
			switch t {
			case "string":
				return "string[]"
			case "boolean":
				return "bool[]"
			}
		}
		return "int[]"
	case *MapLit:
		return "map"
	case *UnaryExpr:
		if ex.Op == "!" {
			return "boolean"
		}
		return inferType(ex.Value)
	case *BinaryExpr:
		switch ex.Op {
		case "+":
			if isStringExpr(ex.Left) || isStringExpr(ex.Right) {
				return "String"
			}
			return "int"
		case "-", "*", "/", "%":
			return "int"
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return "boolean"
		}
	case *TernaryExpr:
		t := inferType(ex.Then)
		if t == "" {
			t = inferType(ex.Else)
		}
		return t
	case *LenExpr:
		return "int"
	case *CallExpr:
		switch ex.Func {
		case "String.valueOf", "substring":
			return "String"
		case "Integer.parseInt":
			return "int"
		case "System.out.println":
			return "void"
		}
	case *MethodCallExpr:
		switch ex.Name {
		case "contains":
			return "boolean"
		}
	case *VarExpr:
		if t, ok := varTypes[ex.Name]; ok {
			return t
		}
	}
	return ""
}

func inferReturnType(body []Stmt) string {
	if len(body) == 0 {
		return "void"
	}
	if ret, ok := body[len(body)-1].(*ReturnStmt); ok {
		if ret.Expr == nil {
			return "void"
		}
		t := inferType(ret.Expr)
		if t == "" {
			return "void"
		}
		return t
	}
	return "void"
}

// --- Simple Java AST ---

type Program struct {
	Funcs []*Function
	Stmts []Stmt
}

type Param struct {
	Name string
	Type string
}

type Function struct {
	Name   string
	Params []Param
	Return string
	Body   []Stmt
}

type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"return")
	if r.Expr != nil {
		fmt.Fprint(w, " ")
		r.Expr.emit(w)
	}
	fmt.Fprint(w, ";\n")
}

type Stmt interface{ emit(io.Writer, string) }

type Expr interface{ emit(io.Writer) }

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"if (")
	s.Cond.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range s.Then {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}")
	if len(s.Else) > 0 {
		if len(s.Else) == 1 {
			if ei, ok := s.Else[0].(*IfStmt); ok {
				fmt.Fprint(w, " else ")
				ei.emit(w, indent)
				return
			}
		}
		fmt.Fprint(w, " else {\n")
		for _, st := range s.Else {
			st.emit(w, indent+"    ")
		}
		fmt.Fprint(w, indent+"}\n")
	} else {
		fmt.Fprint(w, "\n")
	}
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent)
	s.Expr.emit(w)
	fmt.Fprint(w, ";\n")
}

type LetStmt struct {
	Name string
	Type string
	Expr Expr
}

func (s *LetStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent)
	if indent == "    " {
		fmt.Fprint(w, "static ")
	}
	typ := s.Type
	if typ == "" && s.Expr != nil {
		typ = inferType(s.Expr)
	}
	if typ == "" {
		typ = "int"
	}
	fmt.Fprint(w, javaType(typ)+" "+s.Name)
	if s.Expr != nil {
		fmt.Fprint(w, " = ")
		s.Expr.emit(w)
	}
	fmt.Fprint(w, ";\n")
}

type VarStmt struct {
	Name string
	Type string
	Expr Expr
}

func (s *VarStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent)
	if indent == "    " {
		fmt.Fprint(w, "static ")
	}
	typ := s.Type
	if typ == "" && s.Expr != nil {
		typ = inferType(s.Expr)
	}
	if typ == "" {
		typ = "int"
	}
	fmt.Fprint(w, javaType(typ)+" "+s.Name)
	if s.Expr != nil {
		fmt.Fprint(w, " = ")
		s.Expr.emit(w)
	}
	fmt.Fprint(w, ";\n")
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (s *AssignStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+s.Name+" = ")
	s.Expr.emit(w)
	fmt.Fprint(w, ";\n")
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"while (")
	if wst.Cond != nil {
		wst.Cond.emit(w)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range wst.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}\n")
}

type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"for (int "+fr.Name+" = ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "; ")
	fmt.Fprint(w, fr.Name+" < ")
	fr.End.emit(w)
	fmt.Fprint(w, "; ")
	fmt.Fprint(w, fr.Name+"++")
	fmt.Fprint(w, ") {\n")
	for _, st := range fr.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}\n")
}

// ForEachStmt represents `for x in list {}` loops.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	IsMap    bool
}

func (fe *ForEachStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"for (var "+fe.Name+" : ")
	fe.Iterable.emit(w)
	if fe.IsMap {
		fmt.Fprint(w, ".keySet()")
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range fe.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}\n")
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer, indent string) { fmt.Fprint(w, indent+"break;\n") }

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer, indent string) { fmt.Fprint(w, indent+"continue;\n") }

// ListLit represents a list literal.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	arrType := "int"
	if len(l.Elems) > 0 {
		switch inferType(l.Elems[0]) {
		case "string":
			arrType = "String"
		case "boolean":
			arrType = "boolean"
		}
	}
	fmt.Fprintf(w, "new %s[]{", arrType)
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "}")
}

// MapLit represents a simple map literal with string keys and int values.
type MapLit struct {
	Keys   []Expr
	Values []Expr
}

func (m *MapLit) emit(w io.Writer) {
	fmt.Fprint(w, "java.util.Map.of(")
	for i := range m.Keys {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		m.Keys[i].emit(w)
		fmt.Fprint(w, ", ")
		m.Values[i].emit(w)
	}
	fmt.Fprint(w, ")")
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if isStringExpr(b.Left) && isStringExpr(b.Right) {
		switch b.Op {
		case "==", "!=":
			if b.Op == "!=" {
				fmt.Fprint(w, "!")
			}
			fmt.Fprint(w, "(")
			b.Left.emit(w)
			fmt.Fprint(w, ".equals(")
			b.Right.emit(w)
			fmt.Fprint(w, "))")
			return
		case "<", "<=", ">", ">=":
			fmt.Fprint(w, "(")
			b.Left.emit(w)
			fmt.Fprint(w, ".compareTo(")
			b.Right.emit(w)
			fmt.Fprint(w, ") ")
			switch b.Op {
			case "<":
				fmt.Fprint(w, "< 0")
			case "<=":
				fmt.Fprint(w, "<= 0")
			case ">":
				fmt.Fprint(w, "> 0")
			case ">=":
				fmt.Fprint(w, ">= 0")
			}
			fmt.Fprint(w, ")")
			return
		}
	}
	b.Left.emit(w)
	fmt.Fprint(w, " "+b.Op+" ")
	b.Right.emit(w)
}

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprint(w, i.Value) }

type VarExpr struct{ Name string }

func (v *VarExpr) emit(w io.Writer) { fmt.Fprint(w, v.Name) }

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (f *FieldExpr) emit(w io.Writer) {
	f.Target.emit(w)
	fmt.Fprint(w, "."+f.Name)
}

type LenExpr struct{ Value Expr }

func (l *LenExpr) emit(w io.Writer) {
	l.Value.emit(w)
	fmt.Fprint(w, ".length()")
}

type UnaryExpr struct {
	Op    string
	Value Expr
}

type GroupExpr struct{ Expr Expr }

func (g *GroupExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	g.Expr.emit(w)
	fmt.Fprint(w, ")")
}

type TernaryExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (t *TernaryExpr) emit(w io.Writer) {
	t.Cond.emit(w)
	fmt.Fprint(w, " ? ")
	t.Then.emit(w)
	fmt.Fprint(w, " : ")
	t.Else.emit(w)
}

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprint(w, b.Value) }

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	if _, ok := u.Value.(*BinaryExpr); ok {
		fmt.Fprint(w, "(")
		u.Value.emit(w)
		fmt.Fprint(w, ")")
	} else {
		u.Value.emit(w)
	}
}

type CallExpr struct {
	Func string
	Args []Expr
}

// MethodCallExpr represents target.method(args...)
type MethodCallExpr struct {
	Target Expr
	Name   string
	Args   []Expr
}

func (m *MethodCallExpr) emit(w io.Writer) {
	m.Target.emit(w)
	fmt.Fprint(w, "."+m.Name+"(")
	for i, a := range m.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
}

func (c *CallExpr) emit(w io.Writer) {
	fmt.Fprint(w, c.Func)
	fmt.Fprint(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
}

type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Str.emit(w)
	fmt.Fprint(w, ".substring(")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
}

// IndexExpr represents s[i]. For strings it emits charAt.
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (ix *IndexExpr) emit(w io.Writer) {
	if isStringExpr(ix.Target) {
		ix.Target.emit(w)
		fmt.Fprint(w, ".charAt(")
		ix.Index.emit(w)
		fmt.Fprint(w, ")")
	} else {
		ix.Target.emit(w)
		fmt.Fprint(w, "[")
		ix.Index.emit(w)
		fmt.Fprint(w, "]")
	}
}

// SliceExpr represents s[a:b]. Only strings are currently supported.
type SliceExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (sli *SliceExpr) emit(w io.Writer) {
	if isStringExpr(sli.Value) {
		sli.Value.emit(w)
		fmt.Fprint(w, ".substring(")
		sli.Start.emit(w)
		fmt.Fprint(w, ", ")
		sli.End.emit(w)
		fmt.Fprint(w, ")")
	} else {
		sli.Value.emit(w)
	}
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *VarExpr:
		if t, ok := varTypes[ex.Name]; ok && (t == "string" || t == "String") {
			return true
		}
	case *CallExpr:
		if ex.Func == "String.valueOf" {
			return true
		}
	case *SubstringExpr:
		return true
	case *IndexExpr:
		if isStringExpr(ex.Target) {
			return true
		}
	case *SliceExpr:
		if isStringExpr(ex.Value) {
			return true
		}
	}
	return false
}

// Transpile converts a Mochi AST into a simple Java AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	var prog Program
	varTypes = map[string]string{}
	for _, s := range p.Statements {
		if s.Fun != nil {
			body, err := compileStmts(s.Fun.Body)
			if err != nil {
				return nil, err
			}
			var params []Param
			for _, p := range s.Fun.Params {
				params = append(params, Param{Name: p.Name, Type: typeRefString(p.Type)})
			}
			ret := typeRefString(s.Fun.Return)
			if ret == "" {
				ret = inferReturnType(body)
			}
			prog.Funcs = append(prog.Funcs, &Function{Name: s.Fun.Name, Params: params, Return: ret, Body: body})
			continue
		}
		st, err := compileStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			prog.Stmts = append(prog.Stmts, st)
		}
	}
	_ = env // reserved
	return &prog, nil
}

func compileStmt(s *parser.Statement) (Stmt, error) {
	switch {
	case s.Expr != nil:
		e, err := compileExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case s.Let != nil:
		if s.Let.Value != nil {
			e, err := compileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
			t := typeRefString(s.Let.Type)
			if t == "" {
				t = inferType(e)
			}
			if t != "" {
				varTypes[s.Let.Name] = t
			}
			return &LetStmt{Name: s.Let.Name, Type: t, Expr: e}, nil
		}
		t := typeRefString(s.Let.Type)
		if t != "" {
			varTypes[s.Let.Name] = t
		}
		return &LetStmt{Name: s.Let.Name, Type: t}, nil
	case s.Var != nil:
		if s.Var.Value != nil {
			e, err := compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
			t := typeRefString(s.Var.Type)
			if t == "" {
				t = inferType(e)
			}
			if t != "" {
				varTypes[s.Var.Name] = t
			}
			return &VarStmt{Name: s.Var.Name, Type: t, Expr: e}, nil
		}
		t := typeRefString(s.Var.Type)
		if t != "" {
			varTypes[s.Var.Name] = t
		}
		return &VarStmt{Name: s.Var.Name, Type: t}, nil
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			e, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if _, ok := varTypes[s.Assign.Name]; !ok {
				if t := inferType(e); t != "" {
					varTypes[s.Assign.Name] = t
				}
			}
			return &AssignStmt{Name: s.Assign.Name, Expr: e}, nil
		}
	case s.If != nil:
		cond, err := compileExpr(s.If.Cond)
		if err != nil {
			return nil, err
		}
		var thenStmts []Stmt
		for _, b := range s.If.Then {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				thenStmts = append(thenStmts, st)
			}
		}
		var elseStmts []Stmt
		if s.If.ElseIf != nil {
			st, err := compileStmt(&parser.Statement{If: s.If.ElseIf})
			if err != nil {
				return nil, err
			}
			if st != nil {
				elseStmts = append(elseStmts, st)
			}
		} else {
			for _, b := range s.If.Else {
				st, err := compileStmt(b)
				if err != nil {
					return nil, err
				}
				if st != nil {
					elseStmts = append(elseStmts, st)
				}
			}
		}
		return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
	case s.Return != nil:
		var e Expr
		var err error
		if s.Return.Value != nil {
			e, err = compileExpr(s.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: e}, nil
	case s.While != nil:
		cond, err := compileExpr(s.While.Cond)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.While.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case s.For != nil && s.For.RangeEnd != nil:
		start, err := compileExpr(s.For.Source)
		if err != nil {
			return nil, err
		}
		end, err := compileExpr(s.For.RangeEnd)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.For.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &ForRangeStmt{Name: s.For.Name, Start: start, End: end, Body: body}, nil
	case s.For != nil:
		iter, err := compileExpr(s.For.Source)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.For.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		isMap := false
		switch it := iter.(type) {
		case *MapLit:
			isMap = true
		case *VarExpr:
			if t, ok := varTypes[it.Name]; ok && t == "map" {
				isMap = true
			}
		}
		return &ForEachStmt{Name: s.For.Name, Iterable: iter, Body: body, IsMap: isMap}, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Test == nil && s.Import == nil && s.Type == nil:
		return nil, fmt.Errorf("unsupported statement at %d:%d", s.Pos.Line, s.Pos.Column)
	}
	return nil, nil
}

func compileStmts(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		st, err := compileStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			out = append(out, st)
		}
	}
	return out, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	expr := left
	for _, op := range e.Binary.Right {
		r, err := compilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			expr = &BinaryExpr{Left: expr, Op: op.Op, Right: r}
		default:
			return nil, fmt.Errorf("unsupported binary op: %s", op.Op)
		}
	}
	return expr, nil
}

func compileUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := compilePostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-", "!":
			expr = &UnaryExpr{Op: u.Ops[i], Value: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op: %s", u.Ops[i])
		}
	}
	return expr, nil
}

func compilePostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := compilePrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			if op.Index.Start == nil {
				return nil, fmt.Errorf("unsupported index")
			}
			idx, err := compileExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			if op.Index.Start == nil || op.Index.End == nil {
				return nil, fmt.Errorf("unsupported slice")
			}
			start, err := compileExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			end, err := compileExpr(op.Index.End)
			if err != nil {
				return nil, err
			}
			expr = &SliceExpr{Value: expr, Start: start, End: end}
		case op.Field != nil:
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for j, a := range op.Call.Args {
				ex, err := compileExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			if fe, ok := expr.(*FieldExpr); ok {
				expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
			} else if v, ok := expr.(*VarExpr); ok {
				expr = &CallExpr{Func: v.Name, Args: args}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			switch *op.Cast.Type.Simple {
			case "int":
				expr = &CallExpr{Func: "Integer.parseInt", Args: []Expr{expr}}
			default:
				// ignore other casts
			}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func compilePrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := compileExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		if name == "print" {
			name = "System.out.println"
			if len(args) > 1 {
				expr := args[0]
				for i := 1; i < len(args); i++ {
					expr = &BinaryExpr{Left: expr, Op: "+", Right: args[i]}
				}
				args = []Expr{expr}
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
		if name == "len" && len(args) == 1 {
			return &LenExpr{Value: args[0]}, nil
		}
		if name == "str" && len(args) == 1 {
			return &CallExpr{Func: "String.valueOf", Args: args}, nil
		}
		if name == "substring" && len(args) == 3 {
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Selector != nil:
		expr := Expr(&VarExpr{Name: p.Selector.Root})
		for _, name := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: name}
		}
		return expr, nil
	case p.Group != nil:
		e, err := compileExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: e}, nil
	case p.If != nil:
		return compileIfExpr(p.If)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := compileExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		keys := make([]Expr, len(p.Map.Items))
		vals := make([]Expr, len(p.Map.Items))
		for i, it := range p.Map.Items {
			ke, err := compileExpr(it.Key)
			if err != nil {
				return nil, err
			}
			ve, err := compileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			keys[i] = ke
			vals[i] = ve
		}
		return &MapLit{Keys: keys, Values: vals}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := compileExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = compileIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = compileExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &BoolLit{Value: false}
	}
	return &TernaryExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

// Emit generates formatted Java source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("public class Main {\n")
	// emit global variables first
	for _, st := range prog.Stmts {
		switch st.(type) {
		case *LetStmt, *VarStmt:
			st.emit(&buf, "    ")
		}
	}
	if len(prog.Stmts) > 0 {
		buf.WriteByte('\n')
	}
	for i, fn := range prog.Funcs {
		ret := javaType(fn.Return)
		if ret == "" {
			ret = "void"
		}
		buf.WriteString("    static " + ret + " " + fn.Name + "(")
		for i, p := range fn.Params {
			if i > 0 {
				buf.WriteString(", ")
			}
			typ := javaType(p.Type)
			if typ == "" {
				typ = "int"
			}
			buf.WriteString(typ + " " + p.Name)
		}
		buf.WriteString(") {\n")
		for _, s := range fn.Body {
			s.emit(&buf, "        ")
		}
		buf.WriteString("    }")
		buf.WriteByte('\n')
		if i < len(prog.Funcs)-1 {
			buf.WriteByte('\n')
		}
	}
	buf.WriteString("    public static void main(String[] args) {\n")
	for _, st := range prog.Stmts {
		switch st.(type) {
		case *LetStmt, *VarStmt:
			// already emitted as globals
		default:
			st.emit(&buf, "        ")
		}
	}
	buf.WriteString("    }\n")
	buf.WriteString("}\n")
	return formatJava(buf.Bytes())
}

func formatJava(src []byte) []byte {
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

func typeRefString(tr *parser.TypeRef) string {
	if tr == nil {
		return ""
	}
	if tr.Simple != nil {
		return *tr.Simple
	}
	if tr.Generic != nil {
		return tr.Generic.Name
	}
	return ""
}
