//go:build slow

package kt

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

// Program represents a simple Kotlin program consisting of statements executed in main.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement that evaluates an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

// CallExpr represents a function call.
type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Func)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

// StringLit represents a quoted string literal.
type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type IntLit struct{ Value int64 }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { io.WriteString(w, v.Name) }

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " "+b.Op+" ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) { // 'let' is immutable
	io.WriteString(w, "val "+s.Name+" = ")
	s.Value.emit(w)
}

type VarStmt struct {
	Name  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) {
	io.WriteString(w, "var "+s.Name+" = ")
	s.Value.emit(w)
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, s.Name+" = ")
	s.Value.emit(w)
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "listOf(")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, ")")
}

type CountExpr struct{ Value Expr }

func (c *CountExpr) emit(w io.Writer) {
	c.Value.emit(w)
	io.WriteString(w, ".size")
}

type SumExpr struct{ Value Expr }

func (s *SumExpr) emit(w io.Writer) {
	s.Value.emit(w)
	io.WriteString(w, ".sum()")
}

type AvgExpr struct{ Value Expr }

func (a *AvgExpr) emit(w io.Writer) {
	a.Value.emit(w)
	io.WriteString(w, ".average()")
}

type LenExpr struct {
	Value    Expr
	IsString bool
}

func (l *LenExpr) emit(w io.Writer) {
	l.Value.emit(w)
	if l.IsString {
		io.WriteString(w, ".length")
	} else {
		io.WriteString(w, ".size")
	}
}

type StrExpr struct{ Value Expr }

func (s *StrExpr) emit(w io.Writer) {
	s.Value.emit(w)
	io.WriteString(w, ".toString()")
}

type NotExpr struct{ Value Expr }

func (n *NotExpr) emit(w io.Writer) {
	io.WriteString(w, "!")
	n.Value.emit(w)
}

type AppendExpr struct {
	List Expr
	Elem Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	a.List.emit(w)
	io.WriteString(w, " + listOf(")
	a.Elem.emit(w)
	io.WriteString(w, ")")
}

type MinExpr struct{ Value Expr }

func (m *MinExpr) emit(w io.Writer) {
	m.Value.emit(w)
	io.WriteString(w, ".min()")
}

type MaxExpr struct{ Value Expr }

func (m *MaxExpr) emit(w io.Writer) {
	m.Value.emit(w)
	io.WriteString(w, ".max()")
}

type SubstringExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Value.emit(w)
	io.WriteString(w, ".substring(")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	s.End.emit(w)
	io.WriteString(w, ")")
}

// Transpile converts a Mochi program to a simple Kotlin AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	p := &Program{}
	for _, st := range prog.Statements {
		switch {
		case st.Expr != nil:
			e, err := convertExpr(env, st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &ExprStmt{Expr: e})
		case st.Let != nil:
			var val Expr
			if st.Let.Value != nil {
				var err error
				val, err = convertExpr(env, st.Let.Value)
				if err != nil {
					return nil, err
				}
			} else {
				val = &IntLit{Value: 0}
			}
			p.Stmts = append(p.Stmts, &LetStmt{Name: st.Let.Name, Value: val})
		case st.Var != nil:
			var val Expr
			if st.Var.Value != nil {
				var err error
				val, err = convertExpr(env, st.Var.Value)
				if err != nil {
					return nil, err
				}
			} else {
				val = &IntLit{Value: 0}
			}
			p.Stmts = append(p.Stmts, &VarStmt{Name: st.Var.Name, Value: val})
		case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
			e, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &AssignStmt{Name: st.Assign.Name, Value: e})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return p, nil
}

func convertExpr(env *types.Env, e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	first, err := convertUnary(env, e.Binary.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := []string{}
	for _, part := range e.Binary.Right {
		r, err := convertPostfix(env, part.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, r)
		ops = append(ops, part.Op)
	}
	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!="}, {"&&"}, {"||"}}
	contains := func(list []string, s string) bool {
		for _, v := range list {
			if v == s {
				return true
			}
		}
		return false
	}
	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				operands[i] = &BinaryExpr{Left: l, Op: ops[i], Right: r}
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(operands) != 1 {
		return nil, fmt.Errorf("invalid expression")
	}
	return operands[0], nil
}

func convertUnary(env *types.Env, u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	ex, err := convertPostfix(env, u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			ex = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: ex}
		case "!":
			ex = &NotExpr{Value: ex}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return ex, nil
}

func convertPostfix(env *types.Env, p *parser.PostfixExpr) (Expr, error) {
	if p == nil || len(p.Ops) > 0 {
		return nil, fmt.Errorf("unsupported postfix")
	}
	return convertPrimary(env, p.Target)
}

func convertPrimary(env *types.Env, p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		switch p.Call.Func {
		case "count", "sum", "avg", "len", "str":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("%s expects 1 arg", p.Call.Func)
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			switch p.Call.Func {
			case "count":
				return &CountExpr{Value: arg}, nil
			case "sum":
				return &SumExpr{Value: arg}, nil
			case "avg":
				return &AvgExpr{Value: arg}, nil
			case "len":
				isStr := types.IsStringExpr(p.Call.Args[0], env)
				return &LenExpr{Value: arg, IsString: isStr}, nil
			case "str":
				return &StrExpr{Value: arg}, nil
			}
			return nil, fmt.Errorf("unsupported builtin")
		case "append":
			if len(p.Call.Args) != 2 {
				return nil, fmt.Errorf("append expects 2 args")
			}
			list, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			elem, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &AppendExpr{List: list, Elem: elem}, nil
		case "min":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("min expects 1 arg")
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MinExpr{Value: arg}, nil
		case "max":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("max expects 1 arg")
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MaxExpr{Value: arg}, nil
		case "substring":
			if len(p.Call.Args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			str, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			start, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(env, p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			return &SubstringExpr{Value: str, Start: start, End: end}, nil
		default:
			args := make([]Expr, len(p.Call.Args))
			for i, a := range p.Call.Args {
				ex, err := convertExpr(env, a)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			name := p.Call.Func
			if name == "print" {
				name = "println"
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int64(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarRef{Name: p.Selector.Root}, nil
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(env, e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Group != nil:
		return convertExpr(env, p.Group)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

// Emit returns formatted Kotlin source code for prog.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	ver := readVersion()
	ts := time.Now().Format("2006-01-02 15:04:05 MST")
	fmt.Fprintf(&buf, "// Mochi %s - generated %s\n", ver, ts)
	buf.WriteString("fun main() {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteByte('\n')
	}
	buf.WriteString("}\n")
	return buf.Bytes()
}

func readVersion() string {
	_, file, _, _ := runtime.Caller(0)
	dir := filepath.Dir(file)
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			v, err := os.ReadFile(filepath.Join(dir, "VERSION"))
			if err != nil {
				return "unknown"
			}
			return strings.TrimSpace(string(v))
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "unknown"
}

// Print writes a Lisp-like representation of the Kotlin AST to stdout.
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
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
	case *VarStmt:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
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
	case *CountExpr:
		return &ast.Node{Kind: "count", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *StrExpr:
		return &ast.Node{Kind: "str", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *NotExpr:
		return &ast.Node{Kind: "not", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *VarRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *BoolLit:
		return &ast.Node{Kind: "bool", Value: ex.Value}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, el := range ex.Elems {
			n.Children = append(n.Children, toNodeExpr(el))
		}
		return n
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{toNodeExpr(ex.List), toNodeExpr(ex.Elem)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{toNodeExpr(ex.Value), toNodeExpr(ex.Start), toNodeExpr(ex.End)}}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
