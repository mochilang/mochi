//go:build slow

package javatr

import (
	"bytes"
	"fmt"
	"io"

	"mochi/parser"
	"mochi/types"
)

func javaType(t string) string {
	switch t {
	case "int":
		return "int"
	case "bool":
		return "boolean"
	case "string":
		return "String"
	default:
		return "Object"
	}
}

func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		return "int"
	case *BoolLit:
		return "boolean"
	case *StringLit:
		return "String"
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
	fmt.Fprint(w, indent+"]}\n")
	if len(s.Else) > 0 {
		fmt.Fprint(w, indent+"else {\n")
		for _, st := range s.Else {
			st.emit(w, indent+"    ")
		}
		fmt.Fprint(w, indent+"}\n")
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
        if typ != "" {
		fmt.Fprint(w, javaType(typ)+" "+s.Name)
	} else {
		if indent == "    " {
			fmt.Fprint(w, "int "+s.Name)
		} else {
			fmt.Fprint(w, "var "+s.Name)
		}
	}
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
        if typ != "" {
		fmt.Fprint(w, javaType(typ)+" "+s.Name)
	} else {
		if indent == "    " {
			fmt.Fprint(w, "int "+s.Name)
		} else {
			fmt.Fprint(w, "var "+s.Name)
		}
	}
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

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if isStringExpr(b.Left) && isStringExpr(b.Right) {
		switch b.Op {
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

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *CallExpr:
		if ex.Func == "String.valueOf" {
			return true
		}
	case *SubstringExpr:
		return true
	}
	return false
}

// Transpile converts a Mochi AST into a simple Java AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	var prog Program
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
			return &LetStmt{Name: s.Let.Name, Type: t, Expr: e}, nil
		}
		t := typeRefString(s.Let.Type)
		return &LetStmt{Name: s.Let.Name, Type: t}, nil
	case s.Var != nil:
		if s.Var.Value != nil {
			e, err := compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
			t := typeRefString(s.Var.Type)
			return &VarStmt{Name: s.Var.Name, Type: t, Expr: e}, nil
		}
		t := typeRefString(s.Var.Type)
		return &VarStmt{Name: s.Var.Name, Type: t}, nil
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			e, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
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
	if pf == nil || len(pf.Ops) > 0 {
		return nil, fmt.Errorf("unsupported postfix")
	}
	return compilePrimary(pf.Target)
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
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarExpr{Name: p.Selector.Root}, nil
	case p.Group != nil:
		e, err := compileExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: e}, nil
	case p.If != nil:
		return compileIfExpr(p.If)
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
	for _, fn := range prog.Funcs {
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
		buf.WriteString("    }\n")
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
	header := "// Generated by Mochi transpiler\n"
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return append([]byte(header), src...)
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
