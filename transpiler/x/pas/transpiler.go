//go:build slow

package pas

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

func quote(s string) string {
	s = strings.ReplaceAll(s, "'", "''")
	return "'" + s + "'"
}

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
	Funs  []FunDecl
	Vars  []VarDecl
	Stmts []Stmt
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

// PrintStmt prints a string literal using writeln.
// Expr represents a Pascal expression.
type Expr interface{ emit(io.Writer) }
type boolExpr interface{ isBool() bool }

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

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%s", quote(s.Value)) }

// UnaryExpr represents a unary operation like negation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

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

func (b *BinaryExpr) isBool() bool { return b.Bool }

func (b *BinaryExpr) emit(w io.Writer) {
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

// PrintStmt prints the result of an expression using writeln.
type PrintStmt struct{ Expr Expr }

// AssignStmt assigns the result of an expression to a variable.
type AssignStmt struct {
	Name string
	Expr Expr
}

func (p *PrintStmt) emit(w io.Writer) {
	if be, ok := p.Expr.(boolExpr); ok && be.isBool() {
		io.WriteString(w, "writeln(ord(")
		p.Expr.emit(w)
		io.WriteString(w, "));")
		return
	}
	io.WriteString(w, "writeln(")
	if p.Expr != nil {
		p.Expr.emit(w)
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
	loc := time.FixedZone("GMT+7", 7*3600)
	now := time.Now().In(loc)
	fmt.Fprintf(&buf, "// Generated by Mochi transpiler v%s on %s\n", version(), now.Format("2006-01-02 15:04:05 MST"))
	buf.WriteString("{$mode objfpc}\nprogram Main;\n")
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
	for _, st := range prog.Statements {
		switch {
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(call.Args) == 1 && len(st.Expr.Expr.Binary.Right) == 0 {
				ex, err := convertExpr(call.Args[0])
				if err != nil {
					return nil, err
				}
				pr.Stmts = append(pr.Stmts, &PrintStmt{Expr: ex})
				continue
			}
			return nil, fmt.Errorf("unsupported expression")
		case st.Let != nil:
			vd := VarDecl{Name: st.Let.Name}
			if st.Let.Type != nil && st.Let.Type.Simple != nil {
				if *st.Let.Type.Simple == "int" {
					vd.Type = "integer"
				}
			}
			if st.Let.Value != nil {
				ex, err := convertExpr(st.Let.Value)
				if err != nil {
					return nil, err
				}
				vd.Init = ex
			}
			pr.Vars = append(pr.Vars, vd)
		case st.Var != nil:
			vd := VarDecl{Name: st.Var.Name}
			if st.Var.Type != nil && st.Var.Type.Simple != nil {
				if *st.Var.Type.Simple == "int" {
					vd.Type = "integer"
				}
			}
			if st.Var.Value != nil {
				ex, err := convertExpr(st.Var.Value)
				if err != nil {
					return nil, err
				}
				vd.Init = ex
			}
			pr.Vars = append(pr.Vars, vd)
		case st.Assign != nil:
			ex, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			pr.Stmts = append(pr.Stmts, &AssignStmt{Name: st.Assign.Name, Expr: ex})
		case st.While != nil:
			cond, err := convertExpr(st.While.Cond)
			if err != nil {
				return nil, err
			}
			body, err := convertBody(st.While.Body)
			if err != nil {
				return nil, err
			}
			pr.Stmts = append(pr.Stmts, &WhileStmt{Cond: cond, Body: body})
		case st.If != nil:
			cond, err := convertExpr(st.If.Cond)
			if err != nil {
				return nil, err
			}
			thenBody, err := convertBody(st.If.Then)
			if err != nil {
				return nil, err
			}
			elseBody, err := convertBody(st.If.Else)
			if err != nil {
				return nil, err
			}
			pr.Stmts = append(pr.Stmts, &IfStmt{Cond: cond, Then: thenBody, Else: elseBody})
		case st.Fun != nil:
			fnBody, err := convertBody(st.Fun.Body)
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
			ex, err := convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
			pr.Stmts = append(pr.Stmts, &ReturnStmt{Expr: ex})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return pr, nil
}

func convertBody(body []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, st := range body {
		switch {
		case st.Assign != nil:
			ex, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &AssignStmt{Name: st.Assign.Name, Expr: ex})
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(call.Args) == 1 && len(st.Expr.Expr.Binary.Right) == 0 {
				ex, err := convertExpr(call.Args[0])
				if err != nil {
					return nil, err
				}
				out = append(out, &PrintStmt{Expr: ex})
				continue
			}
			return nil, fmt.Errorf("unsupported expression")
		case st.If != nil:
			cond, err := convertExpr(st.If.Cond)
			if err != nil {
				return nil, err
			}
			thenBody, err := convertBody(st.If.Then)
			if err != nil {
				return nil, err
			}
			elseBody, err := convertBody(st.If.Else)
			if err != nil {
				return nil, err
			}
			out = append(out, &IfStmt{Cond: cond, Then: thenBody, Else: elseBody})
		case st.Return != nil:
			ex, err := convertExpr(st.Return.Value)
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

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expr")
	}
	left, err := convertUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	if len(e.Binary.Right) == 0 {
		return left, nil
	}
	expr := left
	for _, op := range e.Binary.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		var be *BinaryExpr
		switch op.Op {
		case "+", "-", "*", "/", "%":
			be = &BinaryExpr{Op: op.Op, Left: expr, Right: right}
		case "==":
			be = &BinaryExpr{Op: "=", Left: expr, Right: right, Bool: true}
		case "!=":
			be = &BinaryExpr{Op: "<>", Left: expr, Right: right, Bool: true}
		case "<", "<=", ">", ">=":
			be = &BinaryExpr{Op: op.Op, Left: expr, Right: right, Bool: true}
		case "&&":
			be = &BinaryExpr{Op: "and", Left: expr, Right: right, Bool: true}
		case "||":
			be = &BinaryExpr{Op: "or", Left: expr, Right: right, Bool: true}
		default:
			return nil, fmt.Errorf("unsupported op")
		}
		expr = be
	}
	return expr, nil
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
		if u.Ops[i] != "-" {
			return nil, fmt.Errorf("unsupported unary op")
		}
		expr = &UnaryExpr{Op: "-", Expr: expr}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		if op.Call != nil {
			vr, ok := expr.(*VarRef)
			if !ok {
				return nil, fmt.Errorf("unsupported call target")
			}
			var args []Expr
			for _, a := range op.Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ex)
			}
			expr = &CallExpr{Name: vr.Name, Args: args}
			continue
		}
		return nil, fmt.Errorf("unsupported postfix")
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Call != nil:
		var args []Expr
		for _, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ex)
		}
		return &CallExpr{Name: p.Call.Func, Args: args}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarRef{Name: p.Selector.Root}, nil
	case p.Group != nil:
		return convertExpr(p.Group)
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

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("go.mod not found")
}

func version() string {
	root, err := repoRoot()
	if err != nil {
		return "unknown"
	}
	b, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "unknown"
	}
	return strings.TrimSpace(string(b))
}
