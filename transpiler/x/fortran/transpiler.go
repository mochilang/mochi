//go:build slow

package fortran

import (
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Decl struct {
	Name string
	Type string
	Init string
}

type Param struct {
	Name string
	Type string
}

type Function struct {
	Name   string
	Params []Param
	Ret    string
	Decls  []Decl
	Stmts  []Stmt
}

type Program struct {
	Decls []Decl
	Stmts []Stmt
	Funcs []*Function
}

func writeIndent(w io.Writer, n int) {
	for i := 0; i < n; i++ {
		io.WriteString(w, " ")
	}
}

type Stmt interface{ emit(io.Writer, int) }

type IfStmt struct {
	Cond string
	Then []Stmt
	Else []Stmt
}

type WhileStmt struct {
	Cond string
	Body []Stmt
}

type ForStmt struct {
	Var   string
	Start string
	End   string
	List  []string
	Body  []Stmt
}

type BreakStmt struct{}

type ContinueStmt struct{}

func (f *Function) emit(w io.Writer) {
	writeIndent(w, 2)
	fmt.Fprintf(w, "function %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprint(w, p.Name)
	}
	fmt.Fprintln(w, ") result(res)")
	writeIndent(w, 4)
	fmt.Fprintf(w, "%s :: res\n", f.Ret)
	for _, d := range f.Params {
		writeIndent(w, 4)
		fmt.Fprintf(w, "%s :: %s\n", d.Type, d.Name)
	}
	for _, d := range f.Decls {
		writeIndent(w, 4)
		fmt.Fprintf(w, "%s :: %s", d.Type, d.Name)
		if d.Init != "" {
			fmt.Fprintf(w, " = %s", d.Init)
		}
		fmt.Fprintln(w)
	}
	if len(f.Decls) > 0 {
		fmt.Fprintln(w)
	}
	for _, s := range f.Stmts {
		s.emit(w, 4)
	}
	writeIndent(w, 2)
	fmt.Fprintf(w, "end function %s\n", f.Name)
}

type AssignStmt struct{ Name, Expr string }

type ReturnStmt struct{ Expr string }

type PrintStmt struct {
	Expr string
	Typ  types.Type
}

func (r *ReturnStmt) emit(w io.Writer, ind int) {
	if r.Expr != "" {
		writeIndent(w, ind)
		fmt.Fprintf(w, "res = %s\n", r.Expr)
	}
	writeIndent(w, ind)
	io.WriteString(w, "return\n")
}

func (s *AssignStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	fmt.Fprintf(w, "%s = %s\n", s.Name, s.Expr)
}

func (p *PrintStmt) emit(w io.Writer, ind int) {
	switch p.Typ.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		writeIndent(w, ind)
		fmt.Fprintf(w, "print '(I0)', %s\n", p.Expr)
	case types.FloatType, types.BigRatType:
		writeIndent(w, ind)
		fmt.Fprintf(w, "print '(F0.6)', %s\n", p.Expr)
	case types.BoolType:
		writeIndent(w, ind)
		fmt.Fprintf(w, "if (%s) then\n", p.Expr)
		writeIndent(w, ind+2)
		fmt.Fprintln(w, "print *, 'true'")
		writeIndent(w, ind)
		io.WriteString(w, "else\n")
		writeIndent(w, ind+2)
		fmt.Fprintln(w, "print *, 'false'")
		writeIndent(w, ind)
		io.WriteString(w, "end if\n")
	case types.StringType:
		writeIndent(w, ind)
		fmt.Fprintf(w, "print *, trim(%s)\n", p.Expr)
	default:
		writeIndent(w, ind)
		fmt.Fprintf(w, "print *, %s\n", p.Expr)
	}
}

func (s *IfStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	fmt.Fprintf(w, "if (%s) then\n", s.Cond)
	for _, st := range s.Then {
		st.emit(w, ind+2)
	}
	if len(s.Else) > 0 {
		writeIndent(w, ind)
		io.WriteString(w, "else\n")
		for _, st := range s.Else {
			st.emit(w, ind+2)
		}
	}
	writeIndent(w, ind)
	io.WriteString(w, "end if\n")
}

func (s *WhileStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	fmt.Fprintf(w, "do while (%s)\n", s.Cond)
	for _, st := range s.Body {
		st.emit(w, ind+2)
	}
	writeIndent(w, ind)
	io.WriteString(w, "end do\n")
}

func (b *BreakStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	io.WriteString(w, "exit\n")
}

func (c *ContinueStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	io.WriteString(w, "cycle\n")
}

func (f *ForStmt) emit(w io.Writer, ind int) {
	if len(f.List) > 0 {
		arrName := fmt.Sprintf("%s_arr", f.Var)
		idxName := fmt.Sprintf("i_%s", f.Var)
		writeIndent(w, ind)
		fmt.Fprintf(w, "integer, dimension(%d) :: %s = (/ %s /)\n", len(f.List), arrName, strings.Join(f.List, ", "))
		writeIndent(w, ind)
		fmt.Fprintf(w, "integer :: %s\n", idxName)
		writeIndent(w, ind)
		fmt.Fprintf(w, "do %s = 1, size(%s)\n", idxName, arrName)
		writeIndent(w, ind+2)
		fmt.Fprintf(w, "%s = %s(%s)\n", f.Var, arrName, idxName)
		for _, st := range f.Body {
			st.emit(w, ind+2)
		}
		writeIndent(w, ind)
		io.WriteString(w, "end do\n")
		return
	}
	writeIndent(w, ind)
	fmt.Fprintf(w, "do %s = %s, %s\n", f.Var, f.Start, f.End)
	for _, st := range f.Body {
		st.emit(w, ind+2)
	}
	writeIndent(w, ind)
	io.WriteString(w, "end do\n")
}

func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString("program main\n")
	buf.WriteString("  implicit none\n")
	for _, d := range p.Decls {
		writeIndent(&buf, 2)
		fmt.Fprintf(&buf, "%s :: %s", d.Type, d.Name)
		if d.Init != "" {
			fmt.Fprintf(&buf, " = %s", d.Init)
		}
		buf.WriteByte('\n')
	}
	if len(p.Decls) > 0 {
		buf.WriteByte('\n')
	}
	for _, s := range p.Stmts {
		s.emit(&buf, 2)
	}
	if len(p.Funcs) > 0 {
		buf.WriteString("contains\n")
		for _, f := range p.Funcs {
			f.emit(&buf)
		}
	}
	buf.WriteString("end program main\n")
	return buf.Bytes()
}

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	fp := &Program{}
	for _, st := range prog.Statements {
		stmt, err := compileStmt(fp, st, env)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			fp.Stmts = append(fp.Stmts, stmt)
		}
	}
	return fp, nil
}

func compileStmt(p *Program, st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Let != nil:
		expr := ""
		var err error
		if st.Let.Value != nil {
			if ie := extractIfExpr(st.Let.Value); ie != nil {
				typ := types.ExprType(st.Let.Value, env)
				ft, err := mapTypeName(typ)
				if err != nil {
					return nil, err
				}
				p.Decls = append(p.Decls, Decl{Name: st.Let.Name, Type: ft})
				stmt, err := compileIfExprAssign(st.Let.Name, ie, env)
				if err != nil {
					return nil, err
				}
				return stmt, nil
			}
			expr, err = toExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
		}
		if st.Let.Type != nil || st.Let.Value != nil {
			var typ types.Type = types.AnyType{}
			if st.Let.Type != nil {
				typ = types.ResolveTypeRef(st.Let.Type, env)
			} else {
				typ = types.ExprType(st.Let.Value, env)
			}
			ft, err := mapTypeName(typ)
			if err != nil {
				return nil, err
			}
			init := expr
			if init == "" {
				init = defaultValue(typ)
			}
			p.Decls = append(p.Decls, Decl{Name: st.Let.Name, Type: ft, Init: init})
			env.Types()[st.Let.Name] = typ
			return nil, nil
		}
		return nil, fmt.Errorf("unsupported let statement")
	case st.Var != nil:
		expr := ""
		var err error
		if st.Var.Value != nil {
			expr, err = toExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
		}
		var typ types.Type = types.AnyType{}
		if st.Var.Type != nil {
			typ = types.ResolveTypeRef(st.Var.Type, env)
		} else if st.Var.Value != nil {
			typ = types.ExprType(st.Var.Value, env)
		}
		ft, err := mapTypeName(typ)
		if err != nil {
			return nil, err
		}
		init := expr
		if init == "" {
			init = defaultValue(typ)
		}
		p.Decls = append(p.Decls, Decl{Name: st.Var.Name, Type: ft, Init: init})
		env.Types()[st.Var.Name] = typ
		return nil, nil
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("unsupported assignment")
		}
		expr, err := toExpr(st.Assign.Value, env)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: expr}, nil
	case st.Return != nil:
		expr := ""
		var err error
		if st.Return.Value != nil {
			expr, err = toExpr(st.Return.Value, env)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: expr}, nil
	case st.Fun != nil:
		fn, err := compileFunc(st.Fun, env)
		if err != nil {
			return nil, err
		}
		p.Funcs = append(p.Funcs, fn)
		return nil, nil
	case st.Expr != nil:
		arg, err := extractPrintArg(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		expr, err := toExpr(arg, env)
		if err != nil {
			return nil, err
		}
		typ := types.TypeOfExpr(arg, env)
		return &PrintStmt{Expr: expr, Typ: typ}, nil
	case st.If != nil:
		if st.If.ElseIf != nil {
			return nil, fmt.Errorf("elseif not supported")
		}
		cond, err := toExpr(st.If.Cond, env)
		if err != nil {
			return nil, err
		}
		thenStmts, err := compileStmtList(p, st.If.Then, env)
		if err != nil {
			return nil, err
		}
		var elseStmts []Stmt
		if len(st.If.Else) > 0 {
			elseStmts, err = compileStmtList(p, st.If.Else, env)
			if err != nil {
				return nil, err
			}
		}
		return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
	case st.While != nil:
		cond, err := toExpr(st.While.Cond, env)
		if err != nil {
			return nil, err
		}
		body, err := compileStmtList(p, st.While.Body, env)
		if err != nil {
			return nil, err
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case st.For != nil:
		stmt, err := compileForStmt(p, st.For, env)
		if err != nil {
			return nil, err
		}
		return stmt, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func compileStmtList(p *Program, list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		stmt, err := compileStmt(p, s, env)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			out = append(out, stmt)
		}
	}
	return out, nil
}

func compileForStmt(p *Program, fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	var loopType types.Type = types.IntType{}
	if fs.Source != nil {
		t := types.ExprType(fs.Source, env)
		if lt, ok := t.(types.ListType); ok {
			loopType = lt.Elem
		} else {
			loopType = t
		}
	}
	if ft, err := mapTypeName(loopType); err == nil {
		p.Decls = append(p.Decls, Decl{Name: fs.Name, Type: ft})
	}
	if fs.RangeEnd != nil {
		start, err := toExpr(fs.Source, env)
		if err != nil {
			return nil, err
		}
		end, err := toExpr(fs.RangeEnd, env)
		if err != nil {
			return nil, err
		}
		body, err := compileStmtList(p, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, Start: start, End: end, Body: body}, nil
	}
	if arr, ok := extractConstList(fs.Source, env); ok {
		body, err := compileStmtList(p, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, List: arr, Body: body}, nil
	}
	return nil, fmt.Errorf("unsupported for-loop")
}

func compileFunc(fs *parser.FunStmt, env *types.Env) (*Function, error) {
	fn := &Function{Name: fs.Name}
	if fs.Return != nil {
		t := types.ResolveTypeRef(fs.Return, env)
		rt, err := mapTypeName(t)
		if err != nil {
			return nil, err
		}
		fn.Ret = rt
	} else {
		fn.Ret = "integer"
	}
	funcEnv := types.NewEnv(env)
	for _, p := range fs.Params {
		pt := types.ResolveTypeRef(p.Type, env)
		ft, err := mapTypeName(pt)
		if err != nil {
			return nil, err
		}
		funcEnv.Types()[p.Name] = pt
		fn.Params = append(fn.Params, Param{Name: p.Name, Type: ft})
	}
	body, err := compileFuncStmtList(fn, fs.Body, funcEnv)
	if err != nil {
		return nil, err
	}
	fn.Stmts = body
	return fn, nil
}

func compileFuncStmtList(fn *Function, list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		s, err := compileFuncStmt(fn, st, env)
		if err != nil {
			return nil, err
		}
		if s != nil {
			out = append(out, s)
		}
	}
	return out, nil
}

func compileForFuncStmt(fn *Function, fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	var loopType types.Type = types.IntType{}
	if fs.Source != nil {
		t := types.ExprType(fs.Source, env)
		if lt, ok := t.(types.ListType); ok {
			loopType = lt.Elem
		} else {
			loopType = t
		}
	}
	if ft, err := mapTypeName(loopType); err == nil {
		fn.Decls = append(fn.Decls, Decl{Name: fs.Name, Type: ft})
	}
	if fs.RangeEnd != nil {
		start, err := toExpr(fs.Source, env)
		if err != nil {
			return nil, err
		}
		end, err := toExpr(fs.RangeEnd, env)
		if err != nil {
			return nil, err
		}
		body, err := compileFuncStmtList(fn, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, Start: start, End: end, Body: body}, nil
	}
	if arr, ok := extractConstList(fs.Source, env); ok {
		body, err := compileFuncStmtList(fn, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, List: arr, Body: body}, nil
	}
	return nil, fmt.Errorf("unsupported for-loop")
}

func compileFuncStmt(fn *Function, st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Let != nil:
		expr := ""
		var err error
		if st.Let.Value != nil {
			if ie := extractIfExpr(st.Let.Value); ie != nil {
				typ := types.ExprType(st.Let.Value, env)
				ft, err := mapTypeName(typ)
				if err != nil {
					return nil, err
				}
				fn.Decls = append(fn.Decls, Decl{Name: st.Let.Name, Type: ft})
				stmt, err := compileIfExprAssign(st.Let.Name, ie, env)
				if err != nil {
					return nil, err
				}
				return stmt, nil
			}
			expr, err = toExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
		}
		if st.Let.Type != nil || st.Let.Value != nil {
			var typ types.Type = types.AnyType{}
			if st.Let.Type != nil {
				typ = types.ResolveTypeRef(st.Let.Type, env)
			} else {
				typ = types.ExprType(st.Let.Value, env)
			}
			ft, err := mapTypeName(typ)
			if err != nil {
				return nil, err
			}
			init := expr
			if init == "" {
				init = defaultValue(typ)
			}
			fn.Decls = append(fn.Decls, Decl{Name: st.Let.Name, Type: ft, Init: init})
			env.Types()[st.Let.Name] = typ
			return nil, nil
		}
		return nil, fmt.Errorf("unsupported let statement")
	case st.Var != nil:
		expr := ""
		var err error
		if st.Var.Value != nil {
			expr, err = toExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
		}
		var typ types.Type = types.AnyType{}
		if st.Var.Type != nil {
			typ = types.ResolveTypeRef(st.Var.Type, env)
		} else if st.Var.Value != nil {
			typ = types.ExprType(st.Var.Value, env)
		}
		ft, err := mapTypeName(typ)
		if err != nil {
			return nil, err
		}
		init := expr
		if init == "" {
			init = defaultValue(typ)
		}
		fn.Decls = append(fn.Decls, Decl{Name: st.Var.Name, Type: ft, Init: init})
		env.Types()[st.Var.Name] = typ
		return nil, nil
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("unsupported assignment")
		}
		expr, err := toExpr(st.Assign.Value, env)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: expr}, nil
	case st.Return != nil:
		expr := ""
		var err error
		if st.Return.Value != nil {
			expr, err = toExpr(st.Return.Value, env)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: expr}, nil
	case st.Expr != nil:
		arg, err := extractPrintArg(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		expr, err := toExpr(arg, env)
		if err != nil {
			return nil, err
		}
		typ := types.TypeOfExpr(arg, env)
		return &PrintStmt{Expr: expr, Typ: typ}, nil
	case st.If != nil:
		if st.If.ElseIf != nil {
			return nil, fmt.Errorf("elseif not supported")
		}
		cond, err := toExpr(st.If.Cond, env)
		if err != nil {
			return nil, err
		}
		thenStmts, err := compileFuncStmtList(fn, st.If.Then, env)
		if err != nil {
			return nil, err
		}
		var elseStmts []Stmt
		if len(st.If.Else) > 0 {
			elseStmts, err = compileFuncStmtList(fn, st.If.Else, env)
			if err != nil {
				return nil, err
			}
		}
		return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
	case st.While != nil:
		cond, err := toExpr(st.While.Cond, env)
		if err != nil {
			return nil, err
		}
		body, err := compileFuncStmtList(fn, st.While.Body, env)
		if err != nil {
			return nil, err
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case st.For != nil:
		stmt, err := compileForFuncStmt(fn, st.For, env)
		if err != nil {
			return nil, err
		}
		return stmt, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func extractIfExpr(e *parser.Expr) *parser.IfExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	if e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return nil
	}
	return e.Binary.Left.Value.Target.If
}

func compileIfExprAssign(name string, ie *parser.IfExpr, env *types.Env) (Stmt, error) {
	cond, err := toExpr(ie.Cond, env)
	if err != nil {
		return nil, err
	}
	thenVal, err := toExpr(ie.Then, env)
	if err != nil {
		return nil, err
	}
	thenStmts := []Stmt{&AssignStmt{Name: name, Expr: thenVal}}
	var elseStmts []Stmt
	if ie.ElseIf != nil {
		st, err := compileIfExprAssign(name, ie.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{st}
	} else if ie.Else != nil {
		elseVal, err := toExpr(ie.Else, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{&AssignStmt{Name: name, Expr: elseVal}}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func extractPrintArg(e *parser.Expr) (*parser.Expr, error) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Call == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	call := u.Value.Target.Call
	if call.Func != "print" || len(call.Args) != 1 {
		return nil, fmt.Errorf("unsupported expression")
	}
	return call.Args[0], nil
}

func extractConstList(e *parser.Expr, env *types.Env) ([]string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.List == nil {
		return nil, false
	}
	var out []string
	for _, el := range u.Value.Target.List.Elems {
		v, err := toExpr(el, env)
		if err != nil {
			return nil, false
		}
		out = append(out, v)
	}
	return out, true
}

func toExpr(e *parser.Expr, env *types.Env) (string, error) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return "", fmt.Errorf("unsupported expression")
	}
	return toBinaryExpr(e.Binary, env)
}

func toBinaryExpr(b *parser.BinaryExpr, env *types.Env) (string, error) {
	operands := []string{}
	ops := []string{}

	first, err := toUnary(b.Left, env)
	if err != nil {
		return "", err
	}
	operands = append(operands, first)
	leftType := types.TypeOfPostfixBasic(b.Left, env)

	for _, part := range b.Right {
		rhs, err := toPostfix(part.Right, env)
		if err != nil {
			return "", err
		}
		rightType := types.TypeOfPostfixBasic(&parser.Unary{Value: part.Right}, env)
		opStr, err := mapOp(part.Op, leftType, rightType)
		if err != nil {
			return "", err
		}
		operands = append(operands, rhs)
		ops = append(ops, opStr)
		switch part.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			leftType = types.BoolType{}
		case "+":
			if types.IsStringType(leftType) && types.IsStringType(rightType) {
				leftType = types.StringType{}
			}
		default:
			leftType = rightType
		}
	}

	levels := [][]string{
		{"*", "/", "mod"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "/=", "in"},
		{".and."},
		{".or."},
	}

	apply := func(a, op, b string) string {
		if op == "mod" {
			return fmt.Sprintf("mod(%s, %s)", a, b)
		}
		return fmt.Sprintf("%s %s %s", a, op, b)
	}

	contains := func(list []string, s string) bool {
		for _, v := range list {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if contains(lvl, ops[i]) {
				res := apply(operands[i], ops[i], operands[i+1])
				operands[i] = res
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("expression reduction failed")
	}
	return operands[0], nil
}

func mapOp(op string, left, right types.Type) (string, error) {
	switch op {
	case "+", "-", "*", "/":
		if op == "+" && types.IsStringType(left) && types.IsStringType(right) {
			return "//", nil
		}
		return op, nil
	case "%":
		return "mod", nil
	case "==":
		return "==", nil
	case "!=":
		return "/=", nil
	case "<", "<=", ">", ">=":
		return op, nil
	case "&&":
		return ".and.", nil
	case "||":
		return ".or.", nil
	default:
		return "", fmt.Errorf("unsupported op %s", op)
	}
}

func toUnary(u *parser.Unary, env *types.Env) (string, error) {
	val, err := toPostfix(u.Value, env)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = "-" + val
		case "!":
			val = ".not." + val
		default:
			return "", fmt.Errorf("unsupported unary %s", u.Ops[i])
		}
	}
	return val, nil
}

func toPostfix(pf *parser.PostfixExpr, env *types.Env) (string, error) {
	if len(pf.Ops) > 0 {
		return "", fmt.Errorf("postfix operations unsupported")
	}
	return toPrimary(pf.Target, env)
}

func toPrimary(p *parser.Primary, env *types.Env) (string, error) {
	switch {
	case p.Lit != nil:
		l := p.Lit
		if l.Int != nil {
			return strconv.Itoa(int(*l.Int)), nil
		}
		if l.Bool != nil {
			if bool(*l.Bool) {
				return ".true.", nil
			}
			return ".false.", nil
		}
		if l.Str != nil {
			s := strings.ReplaceAll(*l.Str, "\"", "\"\"")
			return fmt.Sprintf("\"%s\"", s), nil
		}
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "_" + strings.Join(p.Selector.Tail, "_")
		}
		return name, nil
	case p.Call != nil:
		if p.Call.Func == "len" && len(p.Call.Args) == 1 {
			argExpr, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return "", err
			}
			typ := types.ExprType(p.Call.Args[0], env)
			if types.IsStringType(typ) {
				return fmt.Sprintf("len_trim(%s)", argExpr), nil
			}
			if types.IsListType(typ) {
				return fmt.Sprintf("size(%s)", argExpr), nil
			}
			return "", fmt.Errorf("unsupported len argument type")
		}
		var args []string
		for _, a := range p.Call.Args {
			ex, err := toExpr(a, env)
			if err != nil {
				return "", err
			}
			args = append(args, ex)
		}
		return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
	case p.If != nil:
		expr, err := toIfExpr(p.If, env)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.Group != nil:
		expr, err := toExpr(p.Group, env)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
}

func toIfExpr(ie *parser.IfExpr, env *types.Env) (string, error) {
	cond, err := toExpr(ie.Cond, env)
	if err != nil {
		return "", err
	}
	thenExpr, err := toExpr(ie.Then, env)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = toIfExpr(ie.ElseIf, env)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = toExpr(ie.Else, env)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = ""
	}
	return fmt.Sprintf("merge(%s, %s, %s)", thenExpr, elseExpr, cond), nil
}

func mapTypeName(t types.Type) (string, error) {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return "integer", nil
	case types.FloatType, types.BigRatType:
		return "real", nil
	case types.StringType:
		return "character(len=100)", nil
	case types.BoolType:
		return "logical", nil
	default:
		return "", fmt.Errorf("unsupported type %s", t.String())
	}
}

func defaultValue(t types.Type) string {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return "0"
	case types.FloatType, types.BigRatType:
		return "0.0"
	case types.BoolType:
		return ".false."
	case types.StringType:
		return "\"\""
	default:
		return ""
	}
}
