package javatr

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// --- Simple Java AST ---

type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type LetStmt struct {
	Name string
	Expr Expr
}

func (s *LetStmt) emit(w io.Writer) {
	fmt.Fprint(w, "var "+s.Name+" = ")
	s.Expr.emit(w)
}

type VarStmt struct {
	Name string
	Expr Expr
}

func (s *VarStmt) emit(w io.Writer) {
	fmt.Fprint(w, "var "+s.Name)
	if s.Expr != nil {
		fmt.Fprint(w, " = ")
		s.Expr.emit(w)
	}
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	fmt.Fprint(w, s.Name+" = ")
	s.Expr.emit(w)
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "while (")
	if wst.Cond != nil {
		wst.Cond.emit(w)
	}
	fmt.Fprint(w, ") {\n")
	for _, s := range wst.Body {
		fmt.Fprint(w, "\t")
		s.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}")
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
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

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

// Transpile converts a Mochi AST into a simple Java AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	var prog Program
	for _, s := range p.Statements {
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
	case s.Let != nil && s.Let.Value != nil:
		e, err := compileExpr(s.Let.Value)
		if err != nil {
			return nil, err
		}
		return &LetStmt{Name: s.Let.Name, Expr: e}, nil
	case s.Var != nil:
		e, err := compileExpr(s.Var.Value)
		if err != nil {
			return nil, err
		}
		return &VarStmt{Name: s.Var.Name, Expr: e}, nil
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			e, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			return &AssignStmt{Name: s.Assign.Name, Expr: e}, nil
		}
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
	case s.Test == nil && s.Import == nil && s.Type == nil:
		return nil, fmt.Errorf("unsupported statement at %d:%d", s.Pos.Line, s.Pos.Column)
	}
	return nil, nil
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
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=":
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
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarExpr{Name: p.Selector.Root}, nil
	case p.Group != nil:
		return compileExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported primary")
}

// Emit generates formatted Java source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("public class Main {\n")
	buf.WriteString("\tpublic static void main(String[] args) {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("\t\t")
		s.emit(&buf)
		if _, ok := s.(*WhileStmt); ok {
			buf.WriteString("\n")
		} else {
			buf.WriteString(";\n")
		}
	}
	buf.WriteString("\t}\n")
	buf.WriteString("}\n")
	return formatJava(buf.Bytes())
}

func formatJava(src []byte) []byte {
	ver := readVersion()
	ts := time.Now().Format("2006-01-02 15:04:05 MST")
	header := fmt.Sprintf("// Generated by Mochi transpiler v%s on %s\n", ver, ts)
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return append([]byte(header), src...)
}

var version string

func readVersion() string {
	if version != "" {
		return version
	}
	_, file, _, _ := runtime.Caller(0)
	dir := filepath.Dir(file)
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			v, err := os.ReadFile(filepath.Join(dir, "VERSION"))
			if err != nil {
				version = "unknown"
			} else {
				version = strings.TrimSpace(string(v))
			}
			return version
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	version = "unknown"
	return version
}

// Print converts the custom AST into ast.Node form and prints it.
func Print(p *Program) {
	toNodeProg(p).Print("")
}

func toNodeProg(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, toNodeStmt(s))
	}
	return n
}

func toNodeStmt(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{toNodeExpr(st.Expr)}}
	case *LetStmt:
		n := &ast.Node{Kind: "let", Value: st.Name}
		n.Children = append(n.Children, toNodeExpr(st.Expr))
		return n
	case *VarStmt:
		n := &ast.Node{Kind: "var", Value: st.Name}
		if st.Expr != nil {
			n.Children = append(n.Children, toNodeExpr(st.Expr))
		}
		return n
	case *AssignStmt:
		n := &ast.Node{Kind: "assign", Value: st.Name}
		n.Children = append(n.Children, toNodeExpr(st.Expr))
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while"}
		if st.Cond != nil {
			n.Children = append(n.Children, toNodeExpr(st.Cond))
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, toNodeStmt(b))
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func toNodeExpr(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, toNodeExpr(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *VarExpr:
		return &ast.Node{Kind: "var", Value: ex.Name}
	case *UnaryExpr:
		n := &ast.Node{Kind: "unary", Value: ex.Op}
		n.Children = append(n.Children, toNodeExpr(ex.Value))
		return n
	case *BinaryExpr:
		n := &ast.Node{Kind: "bin", Value: ex.Op}
		n.Children = append(n.Children, toNodeExpr(ex.Left), toNodeExpr(ex.Right))
		return n
	case *LenExpr:
		n := &ast.Node{Kind: "len"}
		n.Children = append(n.Children, toNodeExpr(ex.Value))
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
