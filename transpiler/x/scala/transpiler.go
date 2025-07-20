//go:build slow

package scalat

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"mochi/ast"
	"mochi/parser"
	meta "mochi/transpiler/meta"
	"mochi/types"
)

// Program represents a simple Scala program consisting of statements in main.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type Param struct {
	Name string
	Type string
}

type FunStmt struct {
	Name   string
	Params []Param
	Return string
	Body   []Stmt
}

type ReturnStmt struct{ Value Expr }

// WhileStmt represents `while cond { ... }` loops.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForRangeStmt represents `for i in a..b { ... }` loops.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

// ForEachStmt represents `for x in list { ... }` loops.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { fmt.Fprint(w, "break") }

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { fmt.Fprint(w, "continue") }

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

type FunExpr struct {
	Params []Param
	Expr   Expr
}

type LetStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "val %s", s.Name)
	if s.Type != "" {
		fmt.Fprintf(w, ": %s", s.Type)
	}
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	}
}

type VarStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "var %s", s.Name)
	if s.Type != "" {
		fmt.Fprintf(w, ": %s", s.Type)
	}
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	}
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", s.Name)
	s.Value.emit(w)
}

func (f *FunStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "def %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
		} else {
			fmt.Fprint(w, p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if f.Return != "" {
		fmt.Fprintf(w, ": %s", f.Return)
	}
	fmt.Fprint(w, " = {\n")
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
}

func (r *ReturnStmt) emit(w io.Writer) {
	if r.Value != nil {
		r.Value.emit(w)
	}
}

func (ws *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "while (")
	ws.Cond.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range ws.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for (%s <- ", fr.Name)
	fr.Start.emit(w)
	fmt.Fprint(w, " until ")
	fr.End.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range fr.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
}

func (fe *ForEachStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for (%s <- ", fe.Name)
	fe.Iterable.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range fe.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if (")
	i.Cond.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range i.Then {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
	if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, st := range i.Else {
			fmt.Fprint(w, "    ")
			st.emit(w)
			fmt.Fprint(w, "\n")
		}
		fmt.Fprint(w, "  }")
	}
}

func (ie *IfExpr) emit(w io.Writer) {
	fmt.Fprint(w, "if (")
	ie.Cond.emit(w)
	fmt.Fprint(w, ") ")
	ie.Then.emit(w)
	fmt.Fprint(w, " else ")
	ie.Else.emit(w)
}

func (f *FunExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	fmt.Fprint(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
		} else {
			fmt.Fprintf(w, "%s: Any", p.Name)
		}
	}
	fmt.Fprint(w, ") => ")
	if f.Expr != nil {
		f.Expr.emit(w)
	}
	fmt.Fprint(w, ")")
}

// CallExpr represents calling a function expression with arguments.
type CallExpr struct {
	Fn   Expr
	Args []Expr
}

// LenExpr represents len(x) which becomes x.length in Scala.
type LenExpr struct{ Value Expr }

func (l *LenExpr) emit(w io.Writer) {
	l.Value.emit(w)
	fmt.Fprint(w, ".length")
}

func (c *CallExpr) emit(w io.Writer) {
	c.Fn.emit(w)
	fmt.Fprint(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprintf(w, "%t", b.Value) }

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) { fmt.Fprint(w, n.Name) }

// ListLit represents a simple list literal using Scala's List.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	fmt.Fprint(w, "List(")
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, ")")
}

// AppendExpr represents append(list, elem) as `list :+ elem`.
type AppendExpr struct {
	List Expr
	Elem Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	a.List.emit(w)
	fmt.Fprint(w, " :+ ")
	a.Elem.emit(w)
}

// IndexExpr represents x[i] which becomes x(i) in Scala.
type IndexExpr struct {
	Value Expr
	Index Expr
}

func (idx *IndexExpr) emit(w io.Writer) {
	idx.Value.emit(w)
	fmt.Fprint(w, "(")
	idx.Index.emit(w)
	fmt.Fprint(w, ")")
}

// SliceExpr represents x[a:b] which becomes x.slice(a, b).
type SliceExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SliceExpr) emit(w io.Writer) {
	s.Value.emit(w)
	fmt.Fprint(w, ".slice(")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
}

// CastExpr represents value as type conversions like `"123" as int`.
type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(w io.Writer) {
	c.Value.emit(w)
	switch c.Type {
	case "int":
		fmt.Fprint(w, ".toInt")
	case "float":
		fmt.Fprint(w, ".toDouble")
	case "string":
		fmt.Fprint(w, ".toString")
	}
}

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Receiver Expr
	Name     string
}

func (f *FieldExpr) emit(w io.Writer) {
	f.Receiver.emit(w)
	fmt.Fprintf(w, ".%s", f.Name)
}

// SubstringExpr represents substring(s, i, j) which becomes s.substring(i, j).
type SubstringExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Value.emit(w)
	fmt.Fprint(w, ".substring(")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
}

// Emit generates formatted Scala source for the given program.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	buf.WriteString("object Main {\n")

	for _, st := range p.Stmts {
		if fn, ok := st.(*FunStmt); ok {
			buf.WriteString("  ")
			fn.emit(&buf)
			buf.WriteByte('\n')
			buf.WriteByte('\n')
		}
	}

	buf.WriteString("  def main(args: Array[String]): Unit = {\n")
	for _, st := range p.Stmts {
		if _, ok := st.(*FunStmt); ok {
			continue
		}
		buf.WriteString("    ")
		st.emit(&buf)
		buf.WriteByte('\n')
	}
	buf.WriteString("  }\n")
	buf.WriteString("}\n")
	return buf.Bytes()
}

// Transpile converts a Mochi AST into our simple Scala AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	sc := &Program{}
	for _, st := range prog.Statements {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		sc.Stmts = append(sc.Stmts, s)
	}
	return sc, nil
}

func convertStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Expr != nil:
		e, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		}
		typ := toScalaType(st.Let.Type)
		if typ == "" {
			typ = inferTypeWithEnv(e, env)
		}
		return &LetStmt{Name: st.Let.Name, Type: typ, Value: e}, nil
	case st.Var != nil:
		var e Expr
		var err error
		if st.Var.Value != nil {
			e, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		}
		typ := toScalaType(st.Var.Type)
		if typ == "" {
			typ = inferTypeWithEnv(e, env)
		}
		return &VarStmt{Name: st.Var.Name, Type: typ, Value: e}, nil
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("unsupported assign")
		}
		e, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Value: e}, nil
	case st.Fun != nil:
		return convertFunStmt(st.Fun, env)
	case st.Return != nil:
		return convertReturnStmt(st.Return)
	case st.While != nil:
		return convertWhileStmt(st.While, env)
	case st.For != nil:
		return convertForStmt(st.For, env)
	case st.If != nil:
		return convertIfStmt(st.If, env)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	operands := []Expr{}
	operators := []string{}

	left, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, left)

	for _, part := range b.Right {
		operators = append(operators, part.Op)
		right, err := convertPostfix(part.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
	}

	apply := func(i int) {
		op := operators[i]
		left := operands[i]
		right := operands[i+1]
		var ex Expr
		if op == "in" {
			ex = &CallExpr{Fn: &FieldExpr{Receiver: right, Name: "contains"}, Args: []Expr{left}}
		} else {
			ex = &BinaryExpr{Left: left, Op: op, Right: right}
		}
		operands[i] = ex
		operands = append(operands[:i+1], operands[i+2:]...)
		operators = append(operators[:i], operators[i+1:]...)
	}

	for _, level := range [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	} {
		for i := 0; i < len(operators); {
			if contains(level, operators[i]) {
				apply(i)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("unsupported binary expression")
	}
	return operands[0], nil
}

func contains(list []string, op string) bool {
	for _, s := range list {
		if s == op {
			return true
		}
	}
	return false
}

func convertUnary(u *parser.Unary) (Expr, error) {
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
		default:
			return nil, fmt.Errorf("unsupported unary")
		}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Field != nil:
			if i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
				call := pf.Ops[i+1].Call
				args := make([]Expr, len(call.Args))
				for j, a := range call.Args {
					ex, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args[j] = ex
				}
				expr = &CallExpr{Fn: &FieldExpr{Receiver: expr, Name: op.Field.Name}, Args: args}
				i++
			} else {
				expr = &FieldExpr{Receiver: expr, Name: op.Field.Name}
			}
		case op.Index != nil:
			idx := op.Index
			if idx.Colon != nil || idx.Colon2 != nil {
				if idx.Start == nil || idx.End == nil {
					return nil, fmt.Errorf("unsupported slice")
				}
				start, err := convertExpr(idx.Start)
				if err != nil {
					return nil, err
				}
				end, err := convertExpr(idx.End)
				if err != nil {
					return nil, err
				}
				expr = &SliceExpr{Value: expr, Start: start, End: end}
			} else {
				start, err := convertExpr(idx.Start)
				if err != nil {
					return nil, err
				}
				expr = &IndexExpr{Value: expr, Index: start}
			}
		case op.Call != nil:
			call := op.Call
			args := make([]Expr, len(call.Args))
			for j, a := range call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			expr = &CallExpr{Fn: expr, Args: args}
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				expr = &CastExpr{Value: expr, Type: *op.Cast.Type.Simple}
			} else {
				return nil, fmt.Errorf("unsupported cast")
			}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		return convertCall(p.Call)
	case p.Selector != nil:
		expr := Expr(&Name{Name: p.Selector.Root})
		for _, f := range p.Selector.Tail {
			expr = &FieldExpr{Receiver: expr, Name: f}
		}
		return expr, nil
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
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.FunExpr != nil:
		return convertFunExpr(p.FunExpr)
	case p.Group != nil:
		return convertExpr(p.Group)
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertCall(c *parser.CallExpr) (Expr, error) {
	args := make([]Expr, len(c.Args))
	for i, a := range c.Args {
		ex, err := convertExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = ex
	}
	name := c.Func
	if name == "len" && len(args) == 1 {
		return &LenExpr{Value: args[0]}, nil
	}
	if name == "print" {
		name = "println"
	}
	if name == "str" && len(args) == 1 {
		name = "String.valueOf"
	}
	if name == "append" && len(args) == 2 {
		return &AppendExpr{List: args[0], Elem: args[1]}, nil
	}
	if name == "substring" && len(args) == 3 {
		return &SubstringExpr{Value: args[0], Start: args[1], End: args[2]}, nil
	}
	return &CallExpr{Fn: &Name{Name: name}, Args: args}, nil
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	if l.Str != nil {
		return &StringLit{Value: *l.Str}, nil
	}
	if l.Int != nil {
		return &IntLit{Value: int(*l.Int)}, nil
	}
	if l.Bool != nil {
		return &BoolLit{Value: bool(*l.Bool)}, nil
	}
	return nil, fmt.Errorf("unsupported literal")
}

func convertFunExpr(fe *parser.FunExpr) (Expr, error) {
	f := &FunExpr{}
	for _, p := range fe.Params {
		f.Params = append(f.Params, Param{Name: p.Name, Type: toScalaType(p.Type)})
	}
	if fe.ExprBody != nil {
		expr, err := convertExpr(fe.ExprBody)
		if err != nil {
			return nil, err
		}
		f.Expr = expr
	} else {
		return nil, fmt.Errorf("unsupported fun expr")
	}
	return f, nil
}

func convertIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = convertIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	}
	if elseExpr == nil {
		elseExpr = &IntLit{Value: 0}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertFunStmt(fs *parser.FunStmt, env *types.Env) (Stmt, error) {
	fn := &FunStmt{Name: fs.Name}
	for _, p := range fs.Params {
		fn.Params = append(fn.Params, Param{Name: p.Name, Type: toScalaType(p.Type)})
	}
	fn.Return = toScalaType(fs.Return)
	for _, st := range fs.Body {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		fn.Body = append(fn.Body, s)
	}
	return fn, nil
}

func convertReturnStmt(rs *parser.ReturnStmt) (Stmt, error) {
	var expr Expr
	var err error
	if rs.Value != nil {
		expr, err = convertExpr(rs.Value)
		if err != nil {
			return nil, err
		}
	}
	return &ReturnStmt{Value: expr}, nil
}

func convertWhileStmt(ws *parser.WhileStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, st := range ws.Body {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := convertExpr(fs.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, st := range fs.Body {
			s, err := convertStmt(st, env)
			if err != nil {
				return nil, err
			}
			body = append(body, s)
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(fs.Source)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, st := range fs.Body {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &ForEachStmt{Name: fs.Name, Iterable: iter, Body: body}, nil
}

func convertIfStmt(is *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(is.Cond)
	if err != nil {
		return nil, err
	}
	var thenStmts []Stmt
	for _, st := range is.Then {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		thenStmts = append(thenStmts, s)
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		s, err := convertIfStmt(is.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(is.Else) > 0 {
		for _, st := range is.Else {
			s, err := convertStmt(st, env)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, s)
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func toScalaType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "string":
			return "String"
		case "bool":
			return "Boolean"
		}
		return "Any"
	}
	if t.Fun != nil {
		parts := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			typ := toScalaType(p)
			if typ == "" {
				typ = "Any"
			}
			parts[i] = typ
		}
		ret := toScalaType(t.Fun.Return)
		if ret == "" {
			ret = "Unit"
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	}
	return "Any"
}

func toScalaTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Boolean"
	case types.ListType:
		return fmt.Sprintf("List[%s]", toScalaTypeFromType(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("Map[%s,%s]", toScalaTypeFromType(tt.Key), toScalaTypeFromType(tt.Value))
	}
	return "Any"
}

// inferType attempts a best-effort static type deduction for the expression.
func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		return "Int"
	case *StringLit:
		return "String"
	case *BoolLit:
		return "Boolean"
	case *ListLit:
		if len(ex.Elems) == 0 {
			return "List[Any]"
		}
		t := inferType(ex.Elems[0])
		if t == "" {
			return "List[Any]"
		}
		for _, e := range ex.Elems[1:] {
			if inferType(e) != t {
				return "List[Any]"
			}
		}
		return fmt.Sprintf("List[%s]", t)
	case *LenExpr:
		return "Int"
	case *AppendExpr:
		if t := inferType(ex.List); strings.HasPrefix(t, "List[") {
			return t
		}
		return "List[Any]"
	case *IndexExpr:
		if t := inferType(ex.Value); strings.HasPrefix(t, "List[") {
			inner := strings.TrimSuffix(strings.TrimPrefix(t, "List["), "]")
			if inner != "" {
				return inner
			}
		}
		return "Any"
	case *BinaryExpr:
		switch ex.Op {
		case "+", "-", "*", "/", "%":
			return "Int"
		case "==", "!=", ">", "<", ">=", "<=":
			return "Boolean"
		}
	case *CastExpr:
		return toScalaType(&parser.TypeRef{Simple: &ex.Type})
	case *FunExpr:
		parts := make([]string, len(ex.Params))
		for i, p := range ex.Params {
			if p.Type != "" {
				parts[i] = p.Type
			} else {
				parts[i] = "Any"
			}
		}
		ret := inferType(ex.Expr)
		if ret == "" {
			ret = "Any"
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	case *IfExpr:
		t1 := inferType(ex.Then)
		t2 := inferType(ex.Else)
		if t1 == t2 {
			return t1
		}
	default:
		_ = ex
	}
	return ""
}

func inferTypeWithEnv(e Expr, env *types.Env) string {
	if t := inferType(e); t != "" {
		return t
	}
	if n, ok := e.(*Name); ok && env != nil {
		if typ, err := env.GetVar(n.Name); err == nil {
			return toScalaTypeFromType(typ)
		}
	}
	return ""
}

func header() string {
	return string(meta.Header("//"))
}

// Print converts the Scala AST to ast.Node and prints it.
func Print(p *Program) {
	toNode(p).Print("")
}

func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtNode(st))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr_stmt", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *ReturnStmt:
		return &ast.Node{Kind: "return", Children: []*ast.Node{exprNode(st.Value)}}
	case *WhileStmt:
		n := &ast.Node{Kind: "while"}
		n.Children = append(n.Children, exprNode(st.Cond))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "forrange", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Start), exprNode(st.End))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForEachStmt:
		n := &ast.Node{Kind: "foreach", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Iterable))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *FunStmt:
		n := &ast.Node{Kind: "fun", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, params, body)
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if"}
		n.Children = append(n.Children, exprNode(st.Cond))
		thenNode := &ast.Node{Kind: "then"}
		for _, s2 := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, s2 := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call"}
		n.Children = append(n.Children, exprNode(ex.Fn))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprint(ex.Value)}
	case *BoolLit:
		return &ast.Node{Kind: "bool", Value: fmt.Sprint(ex.Value)}
	case *Name:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Value)}}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, exprNode(e))
		}
		return n
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Value), exprNode(ex.Start), exprNode(ex.End)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{exprNode(ex.Value)}}
	case *IfExpr:
		n := &ast.Node{Kind: "ifexpr"}
		n.Children = append(n.Children, exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else))
		return n
	case *FunExpr:
		n := &ast.Node{Kind: "funexpr"}
		params := &ast.Node{Kind: "params"}
		for _, p := range ex.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, params, exprNode(ex.Expr))
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
