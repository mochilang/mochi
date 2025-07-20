//go:build slow

package rb

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// --- Ruby AST ---

type Program struct {
	Stmts []Stmt
}

// emitter maintains the current indentation level while emitting Ruby code.
type emitter struct {
	w      io.Writer
	indent int
}

func (e *emitter) writeIndent() {
	for i := 0; i < e.indent; i++ {
		io.WriteString(e.w, "  ")
	}
}

func (e *emitter) nl() {
	io.WriteString(e.w, "\n")
}

// Stmt is an AST node that can emit Ruby code using an emitter.
type Stmt interface{ emit(*emitter) }

// ReturnStmt represents a return statement.
type ReturnStmt struct {
	Value Expr
}

func (r *ReturnStmt) emit(e *emitter) {
	io.WriteString(e.w, "return")
	if r.Value != nil {
		io.WriteString(e.w, " ")
		r.Value.emit(e)
	}
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(e *emitter) { io.WriteString(e.w, "break") }

// ContinueStmt represents a continue/next statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(e *emitter) { io.WriteString(e.w, "next") }

// FuncStmt represents a function definition.
type FuncStmt struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FuncStmt) emit(e *emitter) {
	io.WriteString(e.w, "def ")
	io.WriteString(e.w, f.Name)
	io.WriteString(e.w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		io.WriteString(e.w, p)
	}
	io.WriteString(e.w, ")")
	e.nl()
	e.indent++
	for _, st := range f.Body {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
}

// VarStmt represents a mutable variable declaration.
type VarStmt struct {
	Name  string
	Value Expr
}

func (s *VarStmt) emit(e *emitter) {
	io.WriteString(e.w, s.Name)
	io.WriteString(e.w, " = ")
	s.Value.emit(e)
}

// AssignStmt represents an assignment statement.
type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(e *emitter) {
	io.WriteString(e.w, s.Name)
	io.WriteString(e.w, " = ")
	s.Value.emit(e)
}

// IndexAssignStmt represents assignment to an indexed element.
type IndexAssignStmt struct {
	Name  string
	Index Expr
	Value Expr
}

func (s *IndexAssignStmt) emit(e *emitter) {
	io.WriteString(e.w, s.Name)
	io.WriteString(e.w, "[")
	s.Index.emit(e)
	io.WriteString(e.w, "] = ")
	s.Value.emit(e)
}

type Expr interface{ emit(*emitter) }

// ExprStmt represents a statement consisting of a single expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(e *emitter) { s.Expr.emit(e) }

// IfStmt represents a conditional statement with optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(e *emitter) {
	io.WriteString(e.w, "if ")
	s.Cond.emit(e)
	e.nl()
	e.indent++
	for _, st := range s.Then {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	if len(s.Else) > 0 {
		e.writeIndent()
		io.WriteString(e.w, "else")
		e.nl()
		e.indent++
		for _, st := range s.Else {
			e.writeIndent()
			st.emit(e)
			e.nl()
		}
		e.indent--
	}
	e.writeIndent()
	io.WriteString(e.w, "end")
}

// LetStmt represents a variable binding.
type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(e *emitter) {
	io.WriteString(e.w, s.Name)
	io.WriteString(e.w, " = ")
	s.Value.emit(e)
}

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(e *emitter) {
	io.WriteString(e.w, c.Func)
	io.WriteString(e.w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		a.emit(e)
	}
	io.WriteString(e.w, ")")
}

type StringLit struct{ Value string }

func (s *StringLit) emit(e *emitter) { fmt.Fprintf(e.w, "%q", s.Value) }

type IntLit struct{ Value int }

func (i *IntLit) emit(e *emitter) { fmt.Fprintf(e.w, "%d", i.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(e *emitter) {
	if b.Value {
		io.WriteString(e.w, "true")
	} else {
		io.WriteString(e.w, "false")
	}
}

type Ident struct{ Name string }

func (id *Ident) emit(e *emitter) { io.WriteString(e.w, id.Name) }

// WhileStmt represents a while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(e *emitter) {
	io.WriteString(e.w, "while ")
	wst.Cond.emit(e)
	e.nl()
	e.indent++
	for _, st := range wst.Body {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
}

// ForRangeStmt iterates from Start to End (exclusive).
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(e *emitter) {
	io.WriteString(e.w, "for ")
	io.WriteString(e.w, f.Name)
	io.WriteString(e.w, " in (")
	f.Start.emit(e)
	io.WriteString(e.w, "...")
	f.End.emit(e)
	io.WriteString(e.w, ")")
	e.nl()
	e.indent++
	for _, st := range f.Body {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
}

// ForInStmt iterates over an iterable expression.
type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

func (f *ForInStmt) emit(e *emitter) {
	io.WriteString(e.w, "for ")
	io.WriteString(e.w, f.Name)
	io.WriteString(e.w, " in ")
	f.Iterable.emit(e)
	e.nl()
	e.indent++
	for _, st := range f.Body {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(e *emitter) {
	io.WriteString(e.w, "[")
	for i, el := range l.Elems {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		el.emit(e)
	}
	io.WriteString(e.w, "]")
}

// MapLit represents a Ruby hash literal.
type MapLit struct{ Items []MapItem }

// MapItem is a key/value pair inside a map literal.
type MapItem struct {
	Key   Expr
	Value Expr
}

func (m *MapLit) emit(e *emitter) {
	io.WriteString(e.w, "{")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		it.Key.emit(e)
		io.WriteString(e.w, " => ")
		it.Value.emit(e)
	}
	io.WriteString(e.w, "}")
}

// IndexExpr represents indexing into a collection.
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (ix *IndexExpr) emit(e *emitter) {
	ix.Target.emit(e)
	io.WriteString(e.w, "[")
	ix.Index.emit(e)
	io.WriteString(e.w, "]")
}

// CastExpr represents a type conversion.
type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(e *emitter) {
	c.Value.emit(e)
	switch c.Type {
	case "int":
		io.WriteString(e.w, ".to_i")
	case "float":
		io.WriteString(e.w, ".to_f")
	case "string":
		io.WriteString(e.w, ".to_s")
	}
}

type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (b *BinaryExpr) emit(e *emitter) {
	if b.Op == "/" {
		io.WriteString(e.w, "(")
		b.Left.emit(e)
		io.WriteString(e.w, ".to_f / ")
		b.Right.emit(e)
		io.WriteString(e.w, ")")
		return
	}
	b.Left.emit(e)
	io.WriteString(e.w, " "+b.Op+" ")
	b.Right.emit(e)
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(e *emitter) {
	io.WriteString(e.w, u.Op)
	u.Expr.emit(e)
}

type LenExpr struct{ Value Expr }

func (l *LenExpr) emit(e *emitter) {
	l.Value.emit(e)
	io.WriteString(e.w, ".length")
}

type SumExpr struct{ Value Expr }

func (s *SumExpr) emit(e *emitter) {
	s.Value.emit(e)
	io.WriteString(e.w, ".sum")
}

type AvgExpr struct{ Value Expr }

func (a *AvgExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	a.Value.emit(e)
	io.WriteString(e.w, ".sum.to_f / ")
	a.Value.emit(e)
	io.WriteString(e.w, ".length)")
}

type AppendExpr struct {
	List Expr
	Elem Expr
}

// ValuesExpr returns the list of values of a map.
type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(e *emitter) {
	v.Map.emit(e)
	io.WriteString(e.w, ".values")
}

// CondExpr represents a conditional expression (ternary operator).
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	c.Cond.emit(e)
	io.WriteString(e.w, " ? ")
	c.Then.emit(e)
	io.WriteString(e.w, " : ")
	c.Else.emit(e)
	io.WriteString(e.w, ")")
}

// GroupExpr preserves explicit parentheses from the source.
type GroupExpr struct{ Expr Expr }

func (g *GroupExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	g.Expr.emit(e)
	io.WriteString(e.w, ")")
}

// JoinExpr represents calling join(" ") on a list value.
type JoinExpr struct{ List Expr }

func (j *JoinExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	j.List.emit(e)
	io.WriteString(e.w, ")")
	io.WriteString(e.w, ".join(' ')")
}

// FormatList renders a list as "[a b]" for printing.
type FormatList struct{ List Expr }

func (f *FormatList) emit(e *emitter) {
	io.WriteString(e.w, "\"[\" + (")
	f.List.emit(e)
	io.WriteString(e.w, ").join(', ') + \"]\"")
}

// MethodCallExpr represents calling a method on a target expression.
type MethodCallExpr struct {
	Target Expr
	Method string
	Args   []Expr
}

func (m *MethodCallExpr) emit(e *emitter) {
	m.Target.emit(e)
	io.WriteString(e.w, ".")
	io.WriteString(e.w, m.Method)
	io.WriteString(e.w, "(")
	for i, a := range m.Args {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		a.emit(e)
	}
	io.WriteString(e.w, ")")
}

func (a *AppendExpr) emit(e *emitter) {
	a.List.emit(e)
	io.WriteString(e.w, " + [")
	a.Elem.emit(e)
	io.WriteString(e.w, "]")
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func gitTime() time.Time {
	root := repoRoot()
	if root == "" {
		return time.Now()
	}
	out, err := exec.Command("git", "-C", root, "log", "-1", "--format=%cI").Output()
	if err != nil {
		return time.Now()
	}
	t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(out)))
	if err != nil {
		return time.Now()
	}
	return t
}

func header() string {
	t := gitTime()
	return fmt.Sprintf("# Generated by Mochi transpiler v%s on %s\n", version(), t.Format("2006-01-02 15:04 -0700"))
}

func zeroValueExpr(t types.Type) Expr {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return &IntLit{Value: 0}
	case types.FloatType:
		return &IntLit{Value: 0}
	case types.StringType:
		return &StringLit{Value: ""}
	case types.BoolType:
		return &BoolLit{Value: false}
	case types.ListType:
		return &ListLit{}
	case types.MapType, types.StructType:
		return &MapLit{}
	default:
		return &Ident{Name: "nil"}
	}
}

// global environment used for type inference during conversion
var currentEnv *types.Env

// Emit writes Ruby code for program p to w.
func Emit(w io.Writer, p *Program) error {
	e := &emitter{w: w}
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	for _, s := range p.Stmts {
		e.writeIndent()
		s.emit(e)
		e.nl()
	}
	return nil
}

// Transpile converts a Mochi program into a Ruby AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	currentEnv = env
	rbProg := &Program{}
	for _, st := range prog.Statements {
		conv, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if conv != nil {
			rbProg.Stmts = append(rbProg.Stmts, conv)
		}
	}
	return rbProg, nil
}

func convertStmt(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Expr != nil:
		e, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Type != nil:
		// type declarations are ignored in Ruby output
		return nil, nil
	case st.Let != nil:
		var v Expr
		var err error
		if st.Let.Value != nil {
			v, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil && currentEnv != nil {
			v = zeroValueExpr(types.ResolveTypeRef(st.Let.Type, currentEnv))
		} else {
			v = &Ident{Name: "nil"}
		}
		return &LetStmt{Name: st.Let.Name, Value: v}, nil
	case st.Var != nil:
		var v Expr
		var err error
		if st.Var.Value != nil {
			v, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && currentEnv != nil {
			v = zeroValueExpr(types.ResolveTypeRef(st.Var.Type, currentEnv))
		} else {
			v = &Ident{Name: "nil"}
		}
		return &VarStmt{Name: st.Var.Name, Value: v}, nil
	case st.Assign != nil:
		v, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		if len(st.Assign.Index) == 1 && len(st.Assign.Field) == 0 {
			idx, err := convertExpr(st.Assign.Index[0].Start)
			if err != nil {
				return nil, err
			}
			return &IndexAssignStmt{Name: st.Assign.Name, Index: idx, Value: v}, nil
		}
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 1 {
			idx := &StringLit{Value: st.Assign.Field[0].Name}
			return &IndexAssignStmt{Name: st.Assign.Name, Index: idx, Value: v}, nil
		}
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			return &AssignStmt{Name: st.Assign.Name, Value: v}, nil
		}
		return nil, fmt.Errorf("unsupported assignment")
	case st.If != nil:
		return convertIf(st.If)
	case st.While != nil:
		return convertWhile(st.While)
	case st.For != nil:
		return convertFor(st.For)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Return != nil:
		var v Expr
		if st.Return.Value != nil {
			var err error
			v, err = convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: v}, nil
	case st.Fun != nil:
		body := make([]Stmt, len(st.Fun.Body))
		for i, s := range st.Fun.Body {
			st2, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			body[i] = st2
		}
		var params []string
		for _, p := range st.Fun.Params {
			params = append(params, p.Name)
		}
		return &FuncStmt{Name: st.Fun.Name, Params: params, Body: body}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertIf(ifst *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(ifst.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts := make([]Stmt, len(ifst.Then))
	for i, s := range ifst.Then {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		thenStmts[i] = st
	}
	var elseStmts []Stmt
	if ifst.ElseIf != nil {
		st, err := convertIf(ifst.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{st}
	} else if len(ifst.Else) > 0 {
		elseStmts = make([]Stmt, len(ifst.Else))
		for i, s := range ifst.Else {
			st, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			elseStmts[i] = st
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
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
	} else {
		elseExpr = &BoolLit{Value: false}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertWhile(ws *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	body := make([]Stmt, len(ws.Body))
	for i, s := range ws.Body {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		body[i] = st
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertFor(f *parser.ForStmt) (Stmt, error) {
	body := make([]Stmt, len(f.Body))
	for i, s := range f.Body {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		body[i] = st
	}
	if f.RangeEnd != nil {
		start, err := convertExpr(f.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(f.RangeEnd)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: f.Name, Start: start, End: end, Body: body}, nil
	}
	iterable, err := convertExpr(f.Source)
	if err != nil {
		return nil, err
	}
	return &ForInStmt{Name: f.Name, Iterable: iterable, Body: body}, nil
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := convertUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	expr := left
	for _, op := range e.Binary.Right {
		if op.All {
			return nil, fmt.Errorf("unsupported binary op")
		}
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		if op.Op == "in" {
			typ := types.TypeOfPostfix(op.Right, currentEnv)
			if _, ok := typ.(types.MapType); ok {
				expr = &MethodCallExpr{Target: right, Method: "key?", Args: []Expr{expr}}
			} else {
				expr = &MethodCallExpr{Target: right, Method: "include?", Args: []Expr{expr}}
			}
			continue
		}
		expr = &BinaryExpr{Op: op.Op, Left: expr, Right: right}
	}
	return expr, nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	ex, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			ex = &UnaryExpr{Op: op, Expr: ex}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return ex, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Field != nil:
			idx := &StringLit{Value: op.Field.Name}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				expr = &CastExpr{Value: expr, Type: *op.Cast.Type.Simple}
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
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		switch name {
		case "print":
			return convertPrintCall(args, p.Call.Args)
		case "len", "count":
			if len(args) != 1 {
				return nil, fmt.Errorf("len/count takes one arg")
			}
			return &LenExpr{Value: args[0]}, nil
		case "sum":
			if len(args) != 1 {
				return nil, fmt.Errorf("sum takes one arg")
			}
			return &SumExpr{Value: args[0]}, nil
		case "avg":
			if len(args) != 1 {
				return nil, fmt.Errorf("avg takes one arg")
			}
			return &AvgExpr{Value: args[0]}, nil
		case "values":
			if len(args) != 1 {
				return nil, fmt.Errorf("values takes one arg")
			}
			return &ValuesExpr{Map: args[0]}, nil
		case "append":
			if len(args) != 2 {
				return nil, fmt.Errorf("append takes two args")
			}
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		default:
			return &CallExpr{Func: name, Args: args}, nil
		}
	case p.Lit != nil:
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
		if p.Lit.Int != nil {
			return &IntLit{Value: int(*p.Lit.Int)}, nil
		}
		if p.Lit.Bool != nil {
			return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
		}
		return nil, fmt.Errorf("unsupported literal")
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
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Group != nil:
		ex, err := convertExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: ex}, nil
	case p.Selector != nil:
		expr := Expr(&Ident{Name: p.Selector.Root})
		for _, t := range p.Selector.Tail {
			expr = &IndexExpr{Target: expr, Index: &StringLit{Value: t}}
		}
		return expr, nil
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertPrintCall(args []Expr, orig []*parser.Expr) (Expr, error) {
	if len(args) == 1 {
		ex := args[0]
		t := types.ExprType(orig[0], currentEnv)
		switch t.(type) {
		case types.ListType:
			if isValuesCall(orig[0]) {
				ex = &JoinExpr{List: ex}
			} else {
				ex = &FormatList{List: ex}
			}
		case types.BoolType:
			if !isStringComparison(orig[0]) && !isMembershipExpr(orig[0]) {
				ex = &CondExpr{Cond: ex, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
			}
		}
		return &CallExpr{Func: "puts", Args: []Expr{ex}}, nil
	}
	conv := make([]Expr, len(args))
	for i, a := range args {
		ex := a
		switch types.ExprType(orig[i], currentEnv).(type) {
		case types.ListType:
			if isValuesCall(orig[i]) {
				ex = &JoinExpr{List: ex}
			} else {
				ex = &FormatList{List: ex}
			}
		case types.BoolType:
			if !isStringComparison(orig[i]) && !isMembershipExpr(orig[i]) {
				ex = &CondExpr{Cond: ex, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
			}
		}
		conv[i] = ex
	}
	list := &ListLit{Elems: conv}
	return &CallExpr{Func: "puts", Args: []Expr{&JoinExpr{List: list}}}, nil
}

func isStringComparison(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 1 {
		return false
	}
	op := e.Binary.Right[0].Op
	switch op {
	case "==", "!=", "<", "<=", ">", ">=":
		lt := types.TypeOfUnary(e.Binary.Left, currentEnv)
		rt := types.TypeOfPostfix(e.Binary.Right[0].Right, currentEnv)
		if _, ok := lt.(types.StringType); ok {
			if _, ok2 := rt.(types.StringType); ok2 {
				return true
			}
		}
	}
	return false
}

func isValuesCall(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil {
		return false
	}
	if u.Value.Target.Call != nil && u.Value.Target.Call.Func == "values" {
		return true
	}
	return false
}

func isMembershipExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 1 {
		return false
	}
	return e.Binary.Right[0].Op == "in"
}

func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr_stmt", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *VarStmt:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *IndexAssignStmt:
		return &ast.Node{Kind: "index_assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Index), exprNode(st.Value)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(st.Cond)}}
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
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprNode(st.Cond)}}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "for_range", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Start), exprNode(st.End))
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *ForInStmt:
		n := &ast.Node{Kind: "for_in", Value: st.Name, Children: []*ast.Node{exprNode(st.Iterable)}}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *ReturnStmt:
		n := &ast.Node{Kind: "return"}
		if st.Value != nil {
			n.Children = append(n.Children, exprNode(st.Value))
		}
		return n
	case *FuncStmt:
		n := &ast.Node{Kind: "func", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, params)
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *Ident:
		return &ast.Node{Kind: "ident", Value: ex.Name}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprintf("%d", ex.Value)}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, exprNode(e))
		}
		return n
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, it := range ex.Items {
			n.Children = append(n.Children, &ast.Node{Kind: "entry", Children: []*ast.Node{exprNode(it.Key), exprNode(it.Value)}})
		}
		return n
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Expr)}}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Value)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprNode(ex.Value)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprNode(ex.Value)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Elem)}}
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{exprNode(ex.Map)}}
	case *CondExpr:
		return &ast.Node{Kind: "cond", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	case *JoinExpr:
		return &ast.Node{Kind: "join", Children: []*ast.Node{exprNode(ex.List)}}
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.Expr)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{exprNode(ex.Value)}}
	case *MethodCallExpr:
		n := &ast.Node{Kind: "method", Value: ex.Method}
		n.Children = append(n.Children, exprNode(ex.Target))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes a Lisp-like representation of the AST to stdout.
func Print(p *Program) { toNode(p).Print("") }
