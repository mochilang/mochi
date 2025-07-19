//go:build slow

package tstranspiler

import (
	"bytes"
	"fmt"
	"io"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"

	meta "mochi/transpiler/meta"
)

// Simple TypeScript AST nodes used by the transpiler.

type Program struct {
	Stmts []Stmt
}

type Stmt interface {
	emit(io.Writer)
}

// BreakStmt represents a break statement inside loops.
type BreakStmt struct{}

// ContinueStmt represents a continue statement inside loops.
type ContinueStmt struct{}

// ReturnStmt represents returning from a function.
type ReturnStmt struct {
	Value Expr
}

// FuncDecl represents a function definition.
type FuncDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

// IfStmt represents a conditional statement with an optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

// WhileStmt represents a while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForRangeStmt represents a numeric range for-loop like
// `for i in 0..10 {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

// ForInStmt represents iteration over an iterable expression.
type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

type Expr interface {
	emit(io.Writer)
}

type ExprStmt struct {
	Expr Expr
}

// VarDecl represents a variable declaration like `let x = expr`.
type VarDecl struct {
	Name string
	Expr Expr
}

// AssignStmt represents an assignment like `x = expr`.
type AssignStmt struct {
	Name string
	Expr Expr
}

type CallExpr struct {
	Func string
	Args []Expr
}

type StringLit struct {
	Value string
}

// NumberLit is a numeric literal.
type NumberLit struct {
	Value string
}

// BoolLit is a boolean literal.
type BoolLit struct {
	Value bool
}

// NameRef refers to a variable.
type NameRef struct {
	Name string
}

// BinaryExpr represents a binary operation such as 1 + 2.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

// UnaryExpr represents a prefix unary operation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

// ListLit represents a list/array literal.
type ListLit struct {
	Elems []Expr
}

// LenExpr represents the builtin len() call.
type LenExpr struct {
	Value Expr
}

// SubstringExpr represents substring(s, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

// MapLit represents a map/object literal.
type MapLit struct {
	Entries []MapEntry
}

// MapEntry is a key/value pair inside a MapLit.
type MapEntry struct {
	Key   Expr
	Value Expr
}

// IndexExpr represents `target[index]` access.
type IndexExpr struct {
	Target Expr
	Index  Expr
}

// SliceExpr represents a[start:end] slicing.
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

// MethodCallExpr represents target.method(args...).
type MethodCallExpr struct {
	Target Expr
	Method string
	Args   []Expr
}

// FunExpr represents an anonymous function expression.
type FunExpr struct {
	Params []string
	Body   []Stmt
	Expr   Expr
}

// InvokeExpr represents calling a function expression.
type InvokeExpr struct {
	Callee Expr
	Args   []Expr
}

// IndexAssignStmt assigns to an indexed expression like x[i] = v.
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *ExprStmt) emit(w io.Writer) {
	if s == nil {
		return
	}
	s.Expr.emit(w)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Func)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte('(')
	} else {
		io.WriteString(w, "(")
	}
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(')')
	} else {
		io.WriteString(w, ")")
	}
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (n *NumberLit) emit(w io.Writer) { io.WriteString(w, n.Value) }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (n *NameRef) emit(w io.Writer) { io.WriteString(w, n.Name) }

func (b *BinaryExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}

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

func (l *LenExpr) emit(w io.Writer) {
	io.WriteString(w, "__len(")
	if l.Value != nil {
		l.Value.emit(w)
	}
	io.WriteString(w, ")")
}

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	if s.Str != nil {
		s.Str.emit(w)
	}
	io.WriteString(w, ").substring(")
	if s.Start != nil {
		s.Start.emit(w)
	}
	io.WriteString(w, ", ")
	if s.End != nil {
		s.End.emit(w)
	}
	io.WriteString(w, ")")
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "{")
	for i, e := range m.Entries {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, "[")
		e.Key.emit(w)
		io.WriteString(w, "]: ")
		e.Value.emit(w)
	}
	io.WriteString(w, "}")
}

func (i *IndexExpr) emit(w io.Writer) {
	i.Target.emit(w)
	io.WriteString(w, "[")
	if i.Index != nil {
		i.Index.emit(w)
	}
	io.WriteString(w, "]")
}

func (s *SliceExpr) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, ".slice(")
	if s.Start != nil {
		s.Start.emit(w)
	}
	if s.End != nil {
		io.WriteString(w, ", ")
		s.End.emit(w)
	}
	io.WriteString(w, ")")
}

func (m *MethodCallExpr) emit(w io.Writer) {
	m.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, m.Method)
	io.WriteString(w, "(")
	for i, a := range m.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

func (f *FunExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ") => ")
	if f.Expr != nil {
		f.Expr.emit(w)
	} else {
		io.WriteString(w, "{\n")
		for _, st := range f.Body {
			st.emit(w)
			io.WriteString(w, "\n")
		}
		io.WriteString(w, "}")
	}
}

func (i *InvokeExpr) emit(w io.Writer) {
	i.Callee.emit(w)
	io.WriteString(w, "(")
	for j, a := range i.Args {
		if j > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

func (s *IndexAssignStmt) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, " = ")
	s.Value.emit(w)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (v *VarDecl) emit(w io.Writer) {
	io.WriteString(w, "let ")
	io.WriteString(w, v.Name)
	if v.Expr != nil {
		io.WriteString(w, " = ")
		v.Expr.emit(w)
	}
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, a.Name)
	io.WriteString(w, " = ")
	a.Expr.emit(w)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range i.Then {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
	if len(i.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, st := range i.Else {
			st.emit(w)
			io.WriteString(w, "\n")
		}
		io.WriteString(w, "}")
	}
}

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "while (")
	if wst.Cond != nil {
		wst.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range wst.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

func (f *ForRangeStmt) emit(w io.Writer) {
	io.WriteString(w, "for (let ")
	io.WriteString(w, f.Name)
	io.WriteString(w, " = ")
	if f.Start != nil {
		f.Start.emit(w)
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Name)
	io.WriteString(w, " < ")
	if f.End != nil {
		f.End.emit(w)
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Name)
	io.WriteString(w, "++) {\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

func (f *ForInStmt) emit(w io.Writer) {
	io.WriteString(w, "for (const ")
	io.WriteString(w, f.Name)
	io.WriteString(w, " of ")
	if f.Iterable != nil {
		f.Iterable.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

func (b *BreakStmt) emit(w io.Writer) { io.WriteString(w, "break;") }

func (c *ContinueStmt) emit(w io.Writer) { io.WriteString(w, "continue;") }

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "return")
	if r.Value != nil {
		io.WriteString(w, " ")
		r.Value.emit(w)
	}
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (f *FuncDecl) emit(w io.Writer) {
	io.WriteString(w, "function ")
	io.WriteString(w, f.Name)
	io.WriteString(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ") {\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

// Emit converts the AST back into TypeScript source code.
func Emit(p *Program) []byte {
	var b bytes.Buffer
	b.Write(meta.Header("//"))
	b.WriteString("function __len(v:any):number {\n")
	b.WriteString("  if (Array.isArray(v) || typeof v==='string') return v.length;\n")
	b.WriteString("  return Object.keys(v||{}).length;\n")
	b.WriteString("}\n")
	b.WriteString("function __print(...args:any[]) {\n")
	b.WriteString("  const out:string[]=[];\n")
	b.WriteString("  for (const a of args) {\n")
	b.WriteString("    if (typeof a==='boolean') out.push(a? '1':'0');\n")
	b.WriteString("    else if (Array.isArray(a)) out.push(a.map(x=>typeof x==='boolean'? (x?'1':'0'):String(x)).join(' '));\n")
	b.WriteString("    else out.push(String(a));\n")
	b.WriteString("  }\n")
	b.WriteString("  console.log(out.join(' '));\n")
	b.WriteString("}\n")
	for i, s := range p.Stmts {
		if i > 0 {
			b.WriteByte('\n')
		}
		s.emit(&b)
		b.WriteByte('\n')
	}
	return b.Bytes()
}

// Transpile converts a Mochi program into a TypeScript AST. Only a very
// small subset of the language is supported: programs consisting of a single
// call to the builtin `print` with a string literal argument.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env // env not used by this minimal implementation
	tsProg := &Program{}

	for _, st := range prog.Statements {
		stmt, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		tsProg.Stmts = append(tsProg.Stmts, stmt)
	}
	return tsProg, nil
}

func convertStmt(s *parser.Statement) (Stmt, error) {
	switch {
	case s.Let != nil:
		var e Expr
		var err error
		if s.Let.Value != nil {
			e, err = convertExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if s.Let.Type != nil {
			e = zeroValue(s.Let.Type)
		}
		return &VarDecl{Name: s.Let.Name, Expr: e}, nil
	case s.Var != nil:
		var e Expr
		var err error
		if s.Var.Value != nil {
			e, err = convertExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if s.Var.Type != nil {
			e = zeroValue(s.Var.Type)
		}
		return &VarDecl{Name: s.Var.Name, Expr: e}, nil
	case s.Assign != nil:
		val, err := convertExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		if len(s.Assign.Index) > 0 {
			target := Expr(&NameRef{Name: s.Assign.Name})
			target, err = applyIndexOps(target, s.Assign.Index)
			if err != nil {
				return nil, err
			}
			return &IndexAssignStmt{Target: target, Value: val}, nil
		}
		return &AssignStmt{Name: s.Assign.Name, Expr: val}, nil
	case s.Expr != nil:
		e, err := convertExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case s.Return != nil:
		var e Expr
		if s.Return.Value != nil {
			var err error
			e, err = convertExpr(s.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: e}, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Fun != nil:
		body, err := convertStmtList(s.Fun.Body)
		if err != nil {
			return nil, err
		}
		var params []string
		for _, p := range s.Fun.Params {
			params = append(params, p.Name)
		}
		return &FuncDecl{Name: s.Fun.Name, Params: params, Body: body}, nil
	case s.If != nil:
		return convertIfStmt(s.If)
	case s.While != nil:
		return convertWhileStmt(s.While)
	case s.For != nil:
		return convertForStmt(s.For)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertIfStmt(i *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmtList(i.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := convertIfStmt(i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		elseStmts, err = convertStmtList(i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(w *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(w.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(w.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(f *parser.ForStmt) (Stmt, error) {
	if f.RangeEnd != nil {
		start, err := convertExpr(f.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(f.RangeEnd)
		if err != nil {
			return nil, err
		}
		body, err := convertStmtList(f.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: f.Name, Start: start, End: end, Body: body}, nil
	}
	iterable, err := convertExpr(f.Source)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(f.Body)
	if err != nil {
		return nil, err
	}
	return &ForInStmt{Name: f.Name, Iterable: iterable, Body: body}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		out = append(out, st)
	}
	return out, nil
}

func applyIndexOps(base Expr, ops []*parser.IndexOp) (Expr, error) {
	var err error
	for _, op := range ops {
		if op.Colon != nil {
			if op.Colon2 != nil || op.Step != nil {
				return nil, fmt.Errorf("slice step not supported")
			}
			var start, end Expr
			if op.Start != nil {
				start, err = convertExpr(op.Start)
				if err != nil {
					return nil, err
				}
			}
			if op.End != nil {
				end, err = convertExpr(op.End)
				if err != nil {
					return nil, err
				}
			}
			base = &SliceExpr{Target: base, Start: start, End: end}
		} else {
			if op.Colon2 != nil || op.End != nil || op.Step != nil {
				return nil, fmt.Errorf("slice not supported")
			}
			if op.Start == nil {
				return nil, fmt.Errorf("nil index")
			}
			var idx Expr
			idx, err = convertExpr(op.Start)
			if err != nil {
				return nil, err
			}
			base = &IndexExpr{Target: base, Index: idx}
		}
	}
	return base, nil
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	operands := []Expr{}
	ops := []string{}

	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	for _, r := range b.Right {
		o, err := convertPostfix(r.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, o)
		ops = append(ops, r.Op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
	}

	apply := func(i int) {
		expr := &BinaryExpr{Left: operands[i], Op: ops[i], Right: operands[i+1]}
		operands[i] = expr
		operands = append(operands[:i+1], operands[i+2:]...)
		ops = append(ops[:i], ops[i+1:]...)
	}

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range lvl {
				if ops[i] == t {
					apply(i)
					matched = true
					break
				}
			}
			if !matched {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("expression reduction failed")
	}
	return operands[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		expr = &UnaryExpr{Op: op, Expr: expr}
	}
	return expr, nil
}

func convertPostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(p.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil {
				if op.Index.Colon2 != nil || op.Index.Step != nil {
					return nil, fmt.Errorf("slice step not supported")
				}
				var start, end Expr
				if op.Index.Start != nil {
					start, err = convertExpr(op.Index.Start)
					if err != nil {
						return nil, err
					}
				}
				if op.Index.End != nil {
					end, err = convertExpr(op.Index.End)
					if err != nil {
						return nil, err
					}
				}
				expr = &SliceExpr{Target: expr, Start: start, End: end}
			} else {
				if op.Index.Colon2 != nil || op.Index.End != nil || op.Index.Step != nil {
					return nil, fmt.Errorf("postfix slice not supported")
				}
				if op.Index.Start == nil {
					return nil, fmt.Errorf("nil index")
				}
				idx, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				expr = &IndexExpr{Target: expr, Index: idx}
			}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ae, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = ae
			}
			if idx, ok := expr.(*IndexExpr); ok {
				if lit, ok2 := idx.Index.(*StringLit); ok2 && lit.Value == "contains" && len(args) == 1 {
					expr = &MethodCallExpr{Target: idx.Target, Method: "includes", Args: args}
				} else {
					expr = &InvokeExpr{Callee: expr, Args: args}
				}
			} else {
				expr = &InvokeExpr{Callee: expr, Args: args}
			}
		case op.Field != nil:
			expr = &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}}
		case op.Cast != nil:
			// ignore casts
		default:
			return nil, fmt.Errorf("postfix op not supported")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &NameRef{Name: p.Selector.Root}, nil
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ae, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ae
		}
		switch p.Call.Func {
		case "print":
			return &CallExpr{Func: "__print", Args: args}, nil
		case "len":
			if len(args) != 1 {
				return nil, fmt.Errorf("len expects one argument")
			}
			return &LenExpr{Value: args[0]}, nil
		case "str":
			if len(args) != 1 {
				return nil, fmt.Errorf("str expects one argument")
			}
			return &CallExpr{Func: "String", Args: args}, nil
		case "substring":
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects three arguments")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		default:
			return &CallExpr{Func: p.Call.Func, Args: args}, nil
		}
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		entries := make([]MapEntry, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			entries[i] = MapEntry{Key: k, Value: v}
		}
		return &MapLit{Entries: entries}, nil
	case p.FunExpr != nil:
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
		}
		if p.FunExpr.ExprBody != nil {
			expr, err := convertExpr(p.FunExpr.ExprBody)
			if err != nil {
				return nil, err
			}
			return &FunExpr{Params: params, Expr: expr}, nil
		}
		body, err := convertStmtList(p.FunExpr.BlockBody)
		if err != nil {
			return nil, err
		}
		return &FunExpr{Params: params, Body: body}, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &NumberLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		return &NumberLit{Value: fmt.Sprintf("%g", *l.Float)}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

// zeroValue returns a default expression for the given type reference. Only a
// few primitive types are recognized; other types result in no initializer.
func zeroValue(t *parser.TypeRef) Expr {
	if t == nil || t.Simple == nil {
		return nil
	}
	switch *t.Simple {
	case "int", "float":
		return &NumberLit{Value: "0"}
	case "bool":
		return &BoolLit{Value: false}
	case "string":
		return &StringLit{Value: ""}
	default:
		return nil
	}
}

// print converts the given TypeScript AST into a generic ast.Node tree and
// writes it to w. It is useful for debugging and tests.
func print(p *Program, w io.Writer) {
	if p == nil {
		return
	}
	node := progToNode(p)
	io.WriteString(w, node.String())
}

func progToNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtToNode(st))
	}
	return n
}

func stmtToNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{exprToNode(st.Expr)}}
	case *VarDecl:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprToNode(st.Expr)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprToNode(st.Expr)}}
	case *IndexAssignStmt:
		return &ast.Node{Kind: "idx-assign", Children: []*ast.Node{exprToNode(st.Target), exprToNode(st.Value)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprToNode(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, c := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, c := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtToNode(c))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprToNode(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, c := range st.Body {
			body.Children = append(body.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "for-range", Value: st.Name, Children: []*ast.Node{exprToNode(st.Start), exprToNode(st.End)}}
		body := &ast.Node{Kind: "body"}
		for _, c := range st.Body {
			body.Children = append(body.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForInStmt:
		n := &ast.Node{Kind: "for-in", Value: st.Name, Children: []*ast.Node{exprToNode(st.Iterable)}}
		body := &ast.Node{Kind: "body"}
		for _, c := range st.Body {
			body.Children = append(body.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, body)
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *ReturnStmt:
		child := &ast.Node{Kind: "return"}
		if st.Value != nil {
			child.Children = []*ast.Node{exprToNode(st.Value)}
		}
		return child
	case *FuncDecl:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for _, p := range st.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtToNode(b))
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprToNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprToNode(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *NumberLit:
		return &ast.Node{Kind: "number", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *NameRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprToNode(ex.Left), exprToNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprToNode(ex.Expr)}}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, exprToNode(e))
		}
		return n
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprToNode(ex.Value)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprToNode(ex.Str), exprToNode(ex.Start), exprToNode(ex.End)}}
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, e := range ex.Entries {
			n.Children = append(n.Children, &ast.Node{Kind: "entry", Children: []*ast.Node{exprToNode(e.Key), exprToNode(e.Value)}})
		}
		return n
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprToNode(ex.Target), exprToNode(ex.Index)}}
	case *SliceExpr:
		return &ast.Node{Kind: "slice", Children: []*ast.Node{exprToNode(ex.Target), exprToNode(ex.Start), exprToNode(ex.End)}}
	case *FunExpr:
		n := &ast.Node{Kind: "funexpr"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		if ex.Expr != nil {
			n.Children = append(n.Children, exprToNode(ex.Expr))
		} else {
			body := &ast.Node{Kind: "body"}
			for _, st := range ex.Body {
				body.Children = append(body.Children, stmtToNode(st))
			}
			n.Children = append(n.Children, body)
		}
		return n
	case *InvokeExpr:
		n := &ast.Node{Kind: "invoke"}
		n.Children = append(n.Children, exprToNode(ex.Callee))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprToNode(a))
		}
		return n
	case *MethodCallExpr:
		n := &ast.Node{Kind: "method", Value: ex.Method}
		n.Children = append(n.Children, exprToNode(ex.Target))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprToNode(a))
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
