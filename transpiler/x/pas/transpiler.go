//go:build slow

package pas

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"mochi/parser"
	"mochi/types"
)

var currProg *Program

// Program is a minimal Pascal AST consisting of a sequence of statements.
// VarDecl represents a simple variable declaration.
type VarDecl struct {
	Name string
	Type string
	Init Expr
}

// Program is a minimal Pascal AST consisting of a sequence of statements
// plus optional variable declarations.
type Program struct {
	Funs         []FunDecl
	Vars         []VarDecl
	Stmts        []Stmt
	UseSysUtils  bool
	NeedAvg      bool
	NeedMin      bool
	NeedMax      bool
	NeedContains bool
}

// Stmt represents a Pascal statement.
type Stmt interface{ emit(io.Writer) }

// ReturnStmt assigns a value to Result inside a function.
type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "exit(")
	if r.Expr != nil {
		r.Expr.emit(w)
	}
	io.WriteString(w, ");")
}

// IfStmt represents a simple if-then-else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(out io.Writer) {
	io.WriteString(out, "if ")
	if i.Cond != nil {
		i.Cond.emit(out)
	}
	io.WriteString(out, " then begin\n")
	for _, s := range i.Then {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end")
	if len(i.Else) > 0 {
		io.WriteString(out, " else begin\n")
		for _, s := range i.Else {
			io.WriteString(out, "  ")
			s.emit(out)
			io.WriteString(out, "\n")
		}
		io.WriteString(out, "end")
	}
	io.WriteString(out, ";")
}

// FunDecl represents a simple function declaration returning an integer.
type FunDecl struct {
	Name       string
	Params     []string
	ReturnType string
	Body       []Stmt
}

func (f *FunDecl) emit(out io.Writer) {
	rt := f.ReturnType
	if rt == "" {
		rt = "integer"
	}
	fmt.Fprintf(out, "function %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(out, "; ")
		}
		fmt.Fprintf(out, "%s: integer", p)
	}
	fmt.Fprintf(out, "): %s;\nbegin\n", rt)
	for _, s := range f.Body {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end;\n")
}

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (w *WhileStmt) emit(out io.Writer) {
	io.WriteString(out, "while ")
	if w.Cond != nil {
		w.Cond.emit(out)
	}
	io.WriteString(out, " do begin\n")
	for _, s := range w.Body {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end;")
}

// ForRangeStmt represents a numeric for-loop like `for i in 0..10 {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(out io.Writer) {
	fmt.Fprintf(out, "for %s := ", f.Name)
	if f.Start != nil {
		f.Start.emit(out)
	}
	io.WriteString(out, " to ")
	if f.End != nil {
		io.WriteString(out, "(")
		f.End.emit(out)
		io.WriteString(out, " - 1)")
	}
	io.WriteString(out, " do begin\n")
	for _, s := range f.Body {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end;")
}

// ForEachStmt represents iteration over a list collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

func (f *ForEachStmt) emit(out io.Writer) {
	fmt.Fprintf(out, "for %s in ", f.Name)
	if f.Iterable != nil {
		f.Iterable.emit(out)
	}
	io.WriteString(out, " do begin\n")
	for _, s := range f.Body {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end;")
}

// BreakStmt exits the nearest loop.
type BreakStmt struct{}

func (*BreakStmt) emit(w io.Writer) { io.WriteString(w, "break;") }

// ContinueStmt skips to the next loop iteration.
type ContinueStmt struct{}

func (*ContinueStmt) emit(w io.Writer) { io.WriteString(w, "continue;") }

// PrintStmt prints a string literal using writeln.
// Expr represents a Pascal expression.
type Expr interface{ emit(io.Writer) }
type boolExpr interface{ isBool() bool }

type SelectorExpr struct {
	Root string
	Tail []string
}

func (s *SelectorExpr) emit(w io.Writer) {
	io.WriteString(w, s.Root)
	for _, t := range s.Tail {
		fmt.Fprintf(w, ".%s", t)
	}
}

// BoolLit is a boolean literal.
type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (b *BoolLit) isBool() bool { return true }

// VarRef references a variable by name.
type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { io.WriteString(w, v.Name) }

// CallExpr represents a function call.
type CallExpr struct {
	Name string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "%s(", c.Name)
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

// IntLit is a decimal integer literal.
type IntLit struct{ Value int64 }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

// StringLit is a quoted string literal.
type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) {
	escaped := strings.ReplaceAll(s.Value, "'", "''")
	fmt.Fprintf(w, "'%s'", escaped)
}

// ListLit is a simple list literal using Pascal's open array syntax.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "]")
}

// CastExpr represents a simple cast expression, e.g. string to int.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "StrToInt(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	default:
		c.Expr.emit(w)
	}
}

// UnaryExpr represents a unary operation like negation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) isBool() bool { return u.Op == "not " }

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	switch u.Expr.(type) {
	case *BinaryExpr:
		fmt.Fprint(w, "(")
		u.Expr.emit(w)
		fmt.Fprint(w, ")")
	default:
		u.Expr.emit(w)
	}
}

// BinaryExpr represents a binary arithmetic operation.
type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
	Bool  bool
}

type ContainsExpr struct {
	Collection Expr
	Value      Expr
	Kind       string
}

func (c *ContainsExpr) emit(w io.Writer) {
	if c.Kind == "string" {
		io.WriteString(w, "Pos(")
		c.Value.emit(w)
		io.WriteString(w, ", ")
		c.Collection.emit(w)
		io.WriteString(w, ") <> 0")
	} else { // list
		io.WriteString(w, "contains(")
		c.Collection.emit(w)
		io.WriteString(w, ", ")
		c.Value.emit(w)
		io.WriteString(w, ")")
	}
}

func (c *ContainsExpr) isBool() bool { return true }

type IndexExpr struct {
	Target Expr
	Index  Expr
	String bool
}

func (i *IndexExpr) emit(w io.Writer) {
	i.Target.emit(w)
	io.WriteString(w, "[")
	i.Index.emit(w)
	if i.String {
		io.WriteString(w, "+1")
	}
	io.WriteString(w, "]")
}

type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
	String bool
}

func (s *SliceExpr) emit(w io.Writer) {
	io.WriteString(w, "copy(")
	s.Target.emit(w)
	io.WriteString(w, ", ")
	if s.Start != nil {
		s.Start.emit(w)
		if s.String {
			io.WriteString(w, "+1")
		}
	} else {
		if s.String {
			io.WriteString(w, "1")
		} else {
			io.WriteString(w, "0")
		}
	}
	io.WriteString(w, ", ")
	if s.End != nil && s.Start != nil {
		io.WriteString(w, "(")
		s.End.emit(w)
		io.WriteString(w, " - (")
		s.Start.emit(w)
		io.WriteString(w, ")")
		io.WriteString(w, ")")
	} else if s.End != nil {
		s.End.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "Length(")
		s.Target.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, ")")
}

type IfExpr struct {
	Cond   Expr
	Then   Expr
	ElseIf *IfExpr
	Else   Expr
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "IfThen(")
	i.Cond.emit(w)
	io.WriteString(w, ", ")
	i.Then.emit(w)
	io.WriteString(w, ", ")
	if i.ElseIf != nil {
		i.ElseIf.emit(w)
	} else if i.Else != nil {
		i.Else.emit(w)
	} else {
		io.WriteString(w, "''")
	}
	io.WriteString(w, ")")
}

func (b *BinaryExpr) isBool() bool { return b.Bool }

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "in" {
		io.WriteString(w, "Pos(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ") <> 0")
		return
	}
	if _, ok := b.Left.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		b.Left.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emit(w)
	}
	op := b.Op
	switch op {
	case "%":
		op = "mod"
	case "/":
		op = "div"
	}
	fmt.Fprintf(w, " %s ", op)
	if _, ok := b.Right.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		b.Right.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emit(w)
	}
}

// PrintStmt prints one or more expressions using writeln.
type PrintStmt struct{ Exprs []Expr }

// AssignStmt assigns the result of an expression to a variable.
type AssignStmt struct {
	Name string
	Expr Expr
}

// IndexAssignStmt assigns to a list element by index.
type IndexAssignStmt struct {
	Name  string
	Index Expr
	Expr  Expr
}

func (i *IndexAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s[", i.Name)
	if i.Index != nil {
		i.Index.emit(w)
	}
	io.WriteString(w, "] := ")
	if i.Expr != nil {
		i.Expr.emit(w)
	}
	io.WriteString(w, ";")
}

// DoubleIndexAssignStmt assigns to a nested list element.
type DoubleIndexAssignStmt struct {
	Name   string
	Index1 Expr
	Index2 Expr
	Expr   Expr
}

func (d *DoubleIndexAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s[", d.Name)
	if d.Index1 != nil {
		d.Index1.emit(w)
	}
	io.WriteString(w, "][")
	if d.Index2 != nil {
		d.Index2.emit(w)
	}
	io.WriteString(w, "] := ")
	if d.Expr != nil {
		d.Expr.emit(w)
	}
	io.WriteString(w, ";")
}

func (p *PrintStmt) emit(w io.Writer) {
	io.WriteString(w, "writeln(")
	for i, ex := range p.Exprs {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if be, ok := ex.(boolExpr); ok && be.isBool() {
			io.WriteString(w, "ord(")
			ex.emit(w)
			io.WriteString(w, ")")
		} else {
			ex.emit(w)
		}
	}
	io.WriteString(w, ");")
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s := ", a.Name)
	if a.Expr != nil {
		a.Expr.emit(w)
	}
	io.WriteString(w, ";")
}

// Emit renders Pascal code for the program with a deterministic header.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString("{$mode objfpc}\nprogram Main;\n")
	if p.UseSysUtils {
		buf.WriteString("uses SysUtils;\n")
	}
	if p.NeedContains {
		buf.WriteString("function contains(xs: array of integer; v: integer): boolean;\nvar i: integer;\nbegin\n  for i := 0 to High(xs) do begin\n    if xs[i] = v then begin\n      contains := true; exit;\n    end;\n  end;\n  contains := false;\nend;\n")
	}
	if p.NeedAvg {
		buf.WriteString("function avg(xs: array of integer): real;\nvar i, s: integer;\nbegin\n  if Length(xs) = 0 then begin avg := 0; exit; end;\n  s := 0;\n  for i := 0 to High(xs) do s := s + xs[i];\n  avg := s / Length(xs);\nend;\n")
	}
	if p.NeedMin {
		buf.WriteString("function min(xs: array of integer): integer;\nvar i, m: integer;\nbegin\n  if Length(xs) = 0 then begin min := 0; exit; end;\n  m := xs[0];\n  for i := 1 to High(xs) do if xs[i] < m then m := xs[i];\n  min := m;\nend;\n")
	}
	if p.NeedMax {
		buf.WriteString("function max(xs: array of integer): integer;\nvar i, m: integer;\nbegin\n  if Length(xs) = 0 then begin max := 0; exit; end;\n  m := xs[0];\n  for i := 1 to High(xs) do if xs[i] > m then m := xs[i];\n  max := m;\nend;\n")
	}
	for _, f := range p.Funs {
		f.emit(&buf)
	}
	if len(p.Vars) > 0 {
		buf.WriteString("var\n")
		for _, v := range p.Vars {
			typ := v.Type
			if typ == "" {
				typ = "integer"
			}
			fmt.Fprintf(&buf, "  %s: %s;\n", v.Name, typ)
		}
	}
	buf.WriteString("begin\n")
	for _, v := range p.Vars {
		if v.Init != nil {
			buf.WriteString("  ")
			fmt.Fprintf(&buf, "%s := ", v.Name)
			v.Init.emit(&buf)
			buf.WriteString(";\n")
		}
	}
	for _, s := range p.Stmts {
		buf.WriteString("  ")
		s.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("end.\n")
	return buf.Bytes()
}

// Transpile converts a Mochi AST to our Pascal AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	_ = env
	pr := &Program{}
	currProg = pr
	varTypes := map[string]string{}
	for _, st := range prog.Statements {
		switch {
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(st.Expr.Expr.Binary.Right) == 0 {
				var parts []Expr
				for _, a := range call.Args {
					ex, err := convertExpr(env, a)
					if err != nil {
						return nil, err
					}
					parts = append(parts, ex)
				}
				pr.Stmts = append(pr.Stmts, &PrintStmt{Exprs: parts})
				continue
			}
			return nil, fmt.Errorf("unsupported expression")
		case st.Let != nil:
			vd := VarDecl{Name: st.Let.Name}
			if st.Let.Type != nil && st.Let.Type.Simple != nil {
				if *st.Let.Type.Simple == "int" {
					vd.Type = "integer"
				} else if *st.Let.Type.Simple == "string" {
					vd.Type = "string"
				}
			}
			if st.Let.Value != nil {
				ex, err := convertExpr(env, st.Let.Value)
				if err != nil {
					return nil, err
				}
				vd.Init = ex
				if vd.Type == "" {
					switch t := types.ExprType(st.Let.Value, env).(type) {
					case types.StringType:
						vd.Type = "string"
					case types.ListType:
						if _, ok := t.Elem.(types.StringType); ok {
							vd.Type = "array of string"
						} else {
							vd.Type = "array of integer"
						}
					}
				}
			}
			pr.Vars = append(pr.Vars, vd)
		case st.Var != nil:
			vd := VarDecl{Name: st.Var.Name}
			if st.Var.Type != nil && st.Var.Type.Simple != nil {
				if *st.Var.Type.Simple == "int" {
					vd.Type = "integer"
				} else if *st.Var.Type.Simple == "string" {
					vd.Type = "string"
				}
			}
			if st.Var.Value != nil {
				ex, err := convertExpr(env, st.Var.Value)
				if err != nil {
					return nil, err
				}
				vd.Init = ex
				if vd.Type == "" {
					switch t := types.ExprType(st.Var.Value, env).(type) {
					case types.StringType:
						vd.Type = "string"
					case types.ListType:
						if _, ok := t.Elem.(types.StringType); ok {
							vd.Type = "array of string"
						} else {
							vd.Type = "array of integer"
						}
					}
				}
			}
			pr.Vars = append(pr.Vars, vd)
		case st.Assign != nil:
			ex, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
			}
			if len(st.Assign.Index) == 1 && st.Assign.Index[0].Colon == nil && st.Assign.Index[0].Colon2 == nil && len(st.Assign.Field) == 0 {
				idx, err := convertExpr(env, st.Assign.Index[0].Start)
				if err != nil {
					return nil, err
				}
				pr.Stmts = append(pr.Stmts, &IndexAssignStmt{Name: st.Assign.Name, Index: idx, Expr: ex})
				break
			}
			if len(st.Assign.Index) == 2 &&
				st.Assign.Index[0].Colon == nil && st.Assign.Index[1].Colon == nil &&
				st.Assign.Index[0].Colon2 == nil && st.Assign.Index[1].Colon2 == nil &&
				len(st.Assign.Field) == 0 {
				idx1, err := convertExpr(env, st.Assign.Index[0].Start)
				if err != nil {
					return nil, err
				}
				idx2, err := convertExpr(env, st.Assign.Index[1].Start)
				if err != nil {
					return nil, err
				}
				pr.Stmts = append(pr.Stmts, &DoubleIndexAssignStmt{Name: st.Assign.Name, Index1: idx1, Index2: idx2, Expr: ex})
				break
			}
			if _, ok := varTypes[st.Assign.Name]; !ok {
				if t := inferType(ex); t != "" {
					varTypes[st.Assign.Name] = t
				}
			}
			pr.Stmts = append(pr.Stmts, &AssignStmt{Name: st.Assign.Name, Expr: ex})
		case st.For != nil:
			start, err := convertExpr(env, st.For.Source)
			if err != nil {
				return nil, err
			}
			body, err := convertBody(env, st.For.Body, varTypes)
			if err != nil {
				return nil, err
			}
			if st.For.RangeEnd != nil {
				end, err := convertExpr(env, st.For.RangeEnd)
				if err != nil {
					return nil, err
				}
				pr.Stmts = append(pr.Stmts, &ForRangeStmt{Name: st.For.Name, Start: start, End: end, Body: body})
				if _, ok := varTypes[st.For.Name]; !ok {
					varTypes[st.For.Name] = "integer"
				}
			} else {
				pr.Stmts = append(pr.Stmts, &ForEachStmt{Name: st.For.Name, Iterable: start, Body: body})
				if _, ok := varTypes[st.For.Name]; !ok {
					t := types.ExprType(st.For.Source, env)
					if lt, ok := t.(types.ListType); ok {
						if _, ok := lt.Elem.(types.StringType); ok {
							varTypes[st.For.Name] = "string"
						} else {
							varTypes[st.For.Name] = "integer"
						}
					} else {
						varTypes[st.For.Name] = "integer"
					}
				}
			}
		case st.While != nil:
			cond, err := convertExpr(env, st.While.Cond)
			if err != nil {
				return nil, err
			}
			body, err := convertBody(env, st.While.Body, varTypes)
			if err != nil {
				return nil, err
			}
			pr.Stmts = append(pr.Stmts, &WhileStmt{Cond: cond, Body: body})
		case st.If != nil:
			cond, err := convertExpr(env, st.If.Cond)
			if err != nil {
				return nil, err
			}
			thenBody, err := convertBody(env, st.If.Then, varTypes)
			if err != nil {
				return nil, err
			}
			elseBody, err := convertBody(env, st.If.Else, varTypes)
			if err != nil {
				return nil, err
			}
			pr.Stmts = append(pr.Stmts, &IfStmt{Cond: cond, Then: thenBody, Else: elseBody})
		case st.Fun != nil:
			fnBody, err := convertBody(env, st.Fun.Body, varTypes)
			if err != nil {
				return nil, err
			}
			var params []string
			for _, p := range st.Fun.Params {
				params = append(params, p.Name)
			}
			rt := ""
			if st.Fun.Return != nil && st.Fun.Return.Simple != nil {
				if *st.Fun.Return.Simple == "bool" {
					rt = "boolean"
				} else if *st.Fun.Return.Simple == "int" {
					rt = "integer"
				}
			}
			pr.Funs = append(pr.Funs, FunDecl{Name: st.Fun.Name, Params: params, ReturnType: rt, Body: fnBody})
		case st.Return != nil:
			ex, err := convertExpr(env, st.Return.Value)
			if err != nil {
				return nil, err
			}
			pr.Stmts = append(pr.Stmts, &ReturnStmt{Expr: ex})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	for name, typ := range varTypes {
		exists := false
		for _, v := range pr.Vars {
			if v.Name == name {
				exists = true
				break
			}
		}
		if !exists {
			pr.Vars = append(pr.Vars, VarDecl{Name: name, Type: typ})
		}
	}
	markSysUtils(pr)
	currProg = nil
	return pr, nil
}

func convertBody(env *types.Env, body []*parser.Statement, varTypes map[string]string) ([]Stmt, error) {
	var out []Stmt
	for _, st := range body {
		switch {
		case st.Assign != nil:
			ex, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
			}
			if len(st.Assign.Index) == 1 && st.Assign.Index[0].Colon == nil && st.Assign.Index[0].Colon2 == nil && len(st.Assign.Field) == 0 {
				idx, err := convertExpr(env, st.Assign.Index[0].Start)
				if err != nil {
					return nil, err
				}
				out = append(out, &IndexAssignStmt{Name: st.Assign.Name, Index: idx, Expr: ex})
				break
			}
			if len(st.Assign.Index) == 2 &&
				st.Assign.Index[0].Colon == nil && st.Assign.Index[1].Colon == nil &&
				st.Assign.Index[0].Colon2 == nil && st.Assign.Index[1].Colon2 == nil &&
				len(st.Assign.Field) == 0 {
				idx1, err := convertExpr(env, st.Assign.Index[0].Start)
				if err != nil {
					return nil, err
				}
				idx2, err := convertExpr(env, st.Assign.Index[1].Start)
				if err != nil {
					return nil, err
				}
				out = append(out, &DoubleIndexAssignStmt{Name: st.Assign.Name, Index1: idx1, Index2: idx2, Expr: ex})
				break
			}
			if _, ok := varTypes[st.Assign.Name]; !ok {
				if t := inferType(ex); t != "" {
					varTypes[st.Assign.Name] = t
				}
			}
			out = append(out, &AssignStmt{Name: st.Assign.Name, Expr: ex})
		case st.For != nil:
			start, err := convertExpr(env, st.For.Source)
			if err != nil {
				return nil, err
			}
			body, err := convertBody(env, st.For.Body, varTypes)
			if err != nil {
				return nil, err
			}
			if st.For.RangeEnd != nil {
				end, err := convertExpr(env, st.For.RangeEnd)
				if err != nil {
					return nil, err
				}
				out = append(out, &ForRangeStmt{Name: st.For.Name, Start: start, End: end, Body: body})
				if _, ok := varTypes[st.For.Name]; !ok {
					varTypes[st.For.Name] = "integer"
				}
			} else {
				out = append(out, &ForEachStmt{Name: st.For.Name, Iterable: start, Body: body})
				if _, ok := varTypes[st.For.Name]; !ok {
					t := types.ExprType(st.For.Source, env)
					if lt, ok := t.(types.ListType); ok {
						if _, ok := lt.Elem.(types.StringType); ok {
							varTypes[st.For.Name] = "string"
						} else {
							varTypes[st.For.Name] = "integer"
						}
					} else {
						varTypes[st.For.Name] = "integer"
					}
				}
			}
		case st.Break != nil:
			out = append(out, &BreakStmt{})
		case st.Continue != nil:
			out = append(out, &ContinueStmt{})
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(st.Expr.Expr.Binary.Right) == 0 {
				var parts []Expr
				for _, a := range call.Args {
					ex, err := convertExpr(env, a)
					if err != nil {
						return nil, err
					}
					parts = append(parts, ex)
				}
				out = append(out, &PrintStmt{Exprs: parts})
				continue
			}
			return nil, fmt.Errorf("unsupported expression")
		case st.If != nil:
			cond, err := convertExpr(env, st.If.Cond)
			if err != nil {
				return nil, err
			}
			thenBody, err := convertBody(env, st.If.Then, varTypes)
			if err != nil {
				return nil, err
			}
			elseBody, err := convertBody(env, st.If.Else, varTypes)
			if err != nil {
				return nil, err
			}
			out = append(out, &IfStmt{Cond: cond, Then: thenBody, Else: elseBody})
		case st.Return != nil:
			ex, err := convertExpr(env, st.Return.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &ReturnStmt{Expr: ex})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
}

func convertExpr(env *types.Env, e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expr")
	}
	left, err := convertUnary(env, e.Binary.Left)
	if err != nil {
		return nil, err
	}
	if len(e.Binary.Right) == 0 {
		return left, nil
	}
	if len(e.Binary.Right) == 1 && e.Binary.Right[0].Op == "in" {
		right, err := convertPostfix(env, e.Binary.Right[0].Right)
		if err != nil {
			return nil, err
		}
		tmp := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: e.Binary.Right[0].Right}}}
		t := types.ExprType(tmp, env)
		if _, ok := t.(types.ListType); ok {
			currProg.NeedContains = true
			return &ContainsExpr{Collection: right, Value: left, Kind: "list"}, nil
		}
	}

	prec := func(op string) int {
		switch op {
		case "*", "/", "%":
			return 3
		case "+", "-":
			return 2
		case "==", "!=", "<", "<=", ">", ">=", "in":
			return 1
		case "&&", "||":
			return 0
		}
		return -1
	}

	var ops []string
	var exprs []Expr
	exprs = append(exprs, left)

	build := func() error {
		if len(ops) == 0 || len(exprs) < 2 {
			return nil
		}
		op := ops[len(ops)-1]
		ops = ops[:len(ops)-1]
		right := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		left := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		var be *BinaryExpr
		switch op {
		case "+", "-", "*", "/", "%":
			be = &BinaryExpr{Op: op, Left: left, Right: right}
		case "==":
			be = &BinaryExpr{Op: "=", Left: left, Right: right, Bool: true}
		case "!=":
			be = &BinaryExpr{Op: "<>", Left: left, Right: right, Bool: true}
		case "<", "<=", ">", ">=":
			be = &BinaryExpr{Op: op, Left: left, Right: right, Bool: true}
		case "&&":
			be = &BinaryExpr{Op: "and", Left: left, Right: right, Bool: true}
		case "||":
			be = &BinaryExpr{Op: "or", Left: left, Right: right, Bool: true}
		case "in":
			rt := inferType(right)
			if strings.HasPrefix(rt, "array") {
				currProg.NeedContains = true
				be = nil
				exprs = append(exprs, &ContainsExpr{Collection: right, Value: left, Kind: "list"})
				return nil
			}
			be = &BinaryExpr{Op: "in", Left: left, Right: right, Bool: true}
		default:
			return fmt.Errorf("unsupported op")
		}
		exprs = append(exprs, be)
		return nil
	}

	for _, op := range e.Binary.Right {
		right, err := convertPostfix(env, op.Right)
		if err != nil {
			return nil, err
		}
		for len(ops) > 0 && prec(op.Op) <= prec(ops[len(ops)-1]) {
			if err := build(); err != nil {
				return nil, err
			}
		}
		exprs = append(exprs, right)
		ops = append(ops, op.Op)
	}
	for len(ops) > 0 {
		if err := build(); err != nil {
			return nil, err
		}
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("invalid expression")
	}
	return exprs[0], nil
}

func mapLitFromExpr(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return nil
	}
	pf := u.Value
	if pf == nil || len(pf.Ops) > 0 {
		return nil
	}
	if pf.Target != nil {
		return pf.Target.Map
	}
	return nil
}

func convertUnary(env *types.Env, u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(env, u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			expr = &UnaryExpr{Op: "-", Expr: expr}
		case "!":
			expr = &UnaryExpr{Op: "not ", Expr: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertPostfix(env *types.Env, pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(env, pf.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Call != nil:
			switch t := expr.(type) {
			case *VarRef:
				var args []Expr
				for _, a := range op.Call.Args {
					ex, err := convertExpr(env, a)
					if err != nil {
						return nil, err
					}
					args = append(args, ex)
				}
				expr = &CallExpr{Name: t.Name, Args: args}
			case *SelectorExpr:
				if len(t.Tail) == 1 && t.Tail[0] == "contains" {
					if len(op.Call.Args) != 1 {
						return nil, fmt.Errorf("contains expects 1 arg")
					}
					arg, err := convertExpr(env, op.Call.Args[0])
					if err != nil {
						return nil, err
					}
					currProg.NeedContains = true
					expr = &ContainsExpr{Collection: &VarRef{Name: t.Root}, Value: arg, Kind: "list"}
				} else {
					return nil, fmt.Errorf("unsupported call target")
				}
			default:
				return nil, fmt.Errorf("unsupported call target")
			}
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertExpr(env, call.Args[0])
			if err != nil {
				return nil, err
			}
			currProg.NeedContains = true
			expr = &ContainsExpr{Collection: expr, Value: arg, Kind: "list"}
			i++
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(env, op.Index.Start)
			if err != nil {
				return nil, err
			}
			tmp := *pf
			tmp.Ops = tmp.Ops[:i]
			t := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &tmp}}}, env)
			_, isStr := t.(types.StringType)
			expr = &IndexExpr{Target: expr, Index: idx, String: isStr}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			var start Expr
			if op.Index.Start != nil {
				s, err := convertExpr(env, op.Index.Start)
				if err != nil {
					return nil, err
				}
				start = s
			}
			var end Expr
			if op.Index.End != nil {
				e, err := convertExpr(env, op.Index.End)
				if err != nil {
					return nil, err
				}
				end = e
			}
			tmp := *pf
			tmp.Ops = tmp.Ops[:i]
			t := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &tmp}}}, env)
			_, isStr := t.(types.StringType)
			expr = &SliceExpr{Target: expr, Start: start, End: end, String: isStr}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			target := *op.Cast.Type.Simple
			if target == "int" {
				expr = &CastExpr{Expr: expr, Type: target}
			} else {
				return nil, fmt.Errorf("unsupported cast")
			}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(env *types.Env, p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Call != nil:
		var args []Expr
		for _, a := range p.Call.Args {
			ex, err := convertExpr(env, a)
			if err != nil {
				return nil, err
			}
			args = append(args, ex)
		}
		name := p.Call.Func
		if name == "len" && len(p.Call.Args) == 1 {
			if ml := mapLitFromExpr(p.Call.Args[0]); ml != nil {
				return &IntLit{Value: int64(len(ml.Items))}, nil
			}
			name = "Length"
		} else if name == "substring" && len(args) == 3 {
			return &SliceExpr{Target: args[0], Start: args[1], End: args[2], String: true}, nil
		} else if name == "str" && len(args) == 1 {
			name = "IntToStr"
		} else if name == "sum" && len(args) == 1 {
			if l, ok := args[0].(*ListLit); ok {
				var sum Expr
				for i, el := range l.Elems {
					if i == 0 {
						sum = el
					} else {
						sum = &BinaryExpr{Op: "+", Left: sum, Right: el}
					}
				}
				if sum == nil {
					sum = &IntLit{Value: 0}
				}
				return sum, nil
			}
		} else if name == "count" && len(args) == 1 {
			return &CallExpr{Name: "Length", Args: args}, nil
		} else if name == "avg" && len(args) == 1 {
			currProg.NeedAvg = true
			return &CallExpr{Name: "avg", Args: args}, nil
		} else if name == "min" && len(args) == 1 {
			currProg.NeedMin = true
			return &CallExpr{Name: "min", Args: args}, nil
		} else if name == "max" && len(args) == 1 {
			currProg.NeedMax = true
			return &CallExpr{Name: "max", Args: args}, nil
		} else if name == "append" && len(args) == 2 {
			return &CallExpr{Name: "concat", Args: []Expr{args[0], &ListLit{Elems: []Expr{args[1]}}}}, nil
		}
		return &CallExpr{Name: name, Args: args}, nil
	case p.List != nil:
		var elems []Expr
		for _, el := range p.List.Elems {
			ex, err := convertExpr(env, el)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarRef{Name: p.Selector.Root}, nil
	case p.Selector != nil:
		return &SelectorExpr{Root: p.Selector.Root, Tail: p.Selector.Tail}, nil
	case p.If != nil:
		return convertIfExpr(env, p.If)
	case p.Group != nil:
		return convertExpr(env, p.Group)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: int64(*l.Int)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

func convertIfExpr(env *types.Env, ie *parser.IfExpr) (*IfExpr, error) {
	cond, err := convertExpr(env, ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(env, ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	var elseIf *IfExpr
	if ie.ElseIf != nil {
		ei, err := convertIfExpr(env, ie.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = ei
	} else if ie.Else != nil {
		e, err := convertExpr(env, ie.Else)
		if err != nil {
			return nil, err
		}
		elseExpr = e
	}
	return &IfExpr{Cond: cond, Then: thenExpr, ElseIf: elseIf, Else: elseExpr}, nil
}

func inferType(e Expr) string {
	switch v := e.(type) {
	case *IntLit:
		return "integer"
	case *StringLit:
		return "string"
	case *BoolLit:
		return "boolean"
	case *BinaryExpr:
		if v.Bool {
			return "boolean"
		}
		lt := inferType(v.Left)
		rt := inferType(v.Right)
		if lt == rt {
			return lt
		}
		if lt != "" {
			return lt
		}
		return rt
	case *CallExpr:
		switch v.Name {
		case "Length", "Pos":
			return "integer"
		case "IntToStr":
			return "string"
		default:
			return ""
		}
	case *IfExpr:
		thenT := inferType(v.Then)
		elseT := ""
		if v.ElseIf != nil {
			elseT = inferType(v.ElseIf)
		} else if v.Else != nil {
			elseT = inferType(v.Else)
		}
		if thenT == elseT {
			return thenT
		}
		if thenT != "" {
			return thenT
		}
		return elseT
	case *ListLit:
		if len(v.Elems) == 0 {
			return ""
		}
		t := inferType(v.Elems[0])
		for _, el := range v.Elems[1:] {
			if inferType(el) != t {
				return ""
			}
		}
		if t != "" {
			return "array of " + t
		}
		return ""
	case *ContainsExpr:
		return "boolean"
	case *IndexExpr:
		if v.String {
			return "string"
		}
		return "integer"
	case *SliceExpr:
		if v.String {
			return "string"
		}
		return "array of integer"
	case *CastExpr:
		if v.Type == "int" {
			return "integer"
		}
		return inferType(v.Expr)
	case *UnaryExpr:
		return inferType(v.Expr)
	default:
		return ""
	}
}

func usesSysUtilsExpr(e Expr) bool {
	switch v := e.(type) {
	case *CallExpr:
		if v.Name == "IntToStr" || v.Name == "StrToInt" {
			return true
		}
		for _, a := range v.Args {
			if usesSysUtilsExpr(a) {
				return true
			}
		}
	case *IfExpr:
		return true
	case *BinaryExpr:
		if usesSysUtilsExpr(v.Left) || usesSysUtilsExpr(v.Right) {
			return true
		}
	case *UnaryExpr:
		return usesSysUtilsExpr(v.Expr)
	case *ContainsExpr:
		return usesSysUtilsExpr(v.Collection) || usesSysUtilsExpr(v.Value)
	case *IndexExpr:
		return usesSysUtilsExpr(v.Target) || usesSysUtilsExpr(v.Index)
	case *SliceExpr:
		return usesSysUtilsExpr(v.Target) ||
			(v.Start != nil && usesSysUtilsExpr(v.Start)) ||
			(v.End != nil && usesSysUtilsExpr(v.End))
	case *CastExpr:
		if v.Type == "int" {
			return true
		}
		return usesSysUtilsExpr(v.Expr)
	}
	return false
}

func usesSysUtilsStmt(s Stmt) bool {
	switch v := s.(type) {
	case *PrintStmt:
		for _, ex := range v.Exprs {
			if usesSysUtilsExpr(ex) {
				return true
			}
		}
		return false
	case *AssignStmt:
		return usesSysUtilsExpr(v.Expr)
	case *IndexAssignStmt:
		return usesSysUtilsExpr(v.Index) || usesSysUtilsExpr(v.Expr)
	case *DoubleIndexAssignStmt:
		return usesSysUtilsExpr(v.Index1) || usesSysUtilsExpr(v.Index2) || usesSysUtilsExpr(v.Expr)
	case *ReturnStmt:
		return usesSysUtilsExpr(v.Expr)
	case *IfStmt:
		if usesSysUtilsExpr(v.Cond) {
			return true
		}
		for _, st := range v.Then {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
		for _, st := range v.Else {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
	case *WhileStmt:
		if usesSysUtilsExpr(v.Cond) {
			return true
		}
		for _, st := range v.Body {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
	case *ForRangeStmt:
		if usesSysUtilsExpr(v.Start) || usesSysUtilsExpr(v.End) {
			return true
		}
		for _, st := range v.Body {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
	case *ForEachStmt:
		if usesSysUtilsExpr(v.Iterable) {
			return true
		}
		for _, st := range v.Body {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
	}
	return false
}

func markSysUtils(p *Program) {
	for _, v := range p.Vars {
		if v.Init != nil && usesSysUtilsExpr(v.Init) {
			p.UseSysUtils = true
			return
		}
	}
	for _, st := range p.Stmts {
		if usesSysUtilsStmt(st) {
			p.UseSysUtils = true
			return
		}
	}
	for _, f := range p.Funs {
		for _, st := range f.Body {
			if usesSysUtilsStmt(st) {
				p.UseSysUtils = true
				return
			}
		}
	}
}
