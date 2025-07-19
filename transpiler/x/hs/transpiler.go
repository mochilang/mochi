//go:build slow

package hs

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Program represents a minimal Haskell program AST.
type Program struct {
	Funcs []Func
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type Func struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *Func) emit(w io.Writer) {
	io.WriteString(w, f.Name)
	for _, p := range f.Params {
		io.WriteString(w, " ")
		io.WriteString(w, p)
	}
	io.WriteString(w, " = ")
	if len(f.Body) == 1 {
		if r, ok := f.Body[0].(*ReturnStmt); ok {
			r.Expr.emit(w)
			return
		}
	}
	io.WriteString(w, "do\n")
	for _, st := range f.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
}

type PrintStmt struct {
	Expr   Expr
	String bool
}
type LetStmt struct {
	Name string
	Expr Expr
}

// AssignStmt updates the value of an existing variable.
type AssignStmt struct {
	Name string
	Expr Expr
}

// IfStmt executes a list of statements based on a condition.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}
type ReturnStmt struct {
	Expr Expr
}

func (p *PrintStmt) emit(w io.Writer) {
	if p.String {
		io.WriteString(w, "putStrLn (")
		p.Expr.emit(w)
		io.WriteString(w, ")")
		return
	}

	if isBoolExpr(p.Expr) {
		io.WriteString(w, "print (fromEnum (")
		p.Expr.emit(w)
		io.WriteString(w, "))")
		return
	}
	if isListExpr(p.Expr) && !isStringExpr(p.Expr) {
		io.WriteString(w, "putStrLn (unwords (map show (")
		p.Expr.emit(w)
		io.WriteString(w, ")))")
		return
	}

	if isStringExpr(p.Expr) {
		io.WriteString(w, "putStrLn (")
		if _, ok := p.Expr.(*IndexExpr); ok {
			io.WriteString(w, "[")
			p.Expr.emit(w)
			io.WriteString(w, "]")
		} else {
			p.Expr.emit(w)
		}
		io.WriteString(w, ")")
		return
	}

	io.WriteString(w, "print (")
	p.Expr.emit(w)
	io.WriteString(w, ")")
}

func (l *LetStmt) emit(w io.Writer) {
	io.WriteString(w, l.Name)
	io.WriteString(w, " = ")
	l.Expr.emit(w)
}

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, a.Name)
	io.WriteString(w, " = ")
	a.Expr.emit(w)
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then\n")
	for _, st := range i.Then {
		io.WriteString(w, "        ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if len(i.Else) > 0 {
		io.WriteString(w, "    else\n")
		for _, st := range i.Else {
			io.WriteString(w, "        ")
			st.emit(w)
			io.WriteString(w, "\n")
		}
	}
}
func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "return (")
	r.Expr.emit(w)
	io.WriteString(w, ")")
}

type IntLit struct{ Value string }
type StringLit struct{ Value string }
type BoolLit struct{ Value bool }
type NameRef struct{ Name string }
type LenExpr struct{ Arg Expr }
type BinaryExpr struct {
	Left Expr
	Ops  []BinaryOp
}

type BinaryOp struct {
	Op    string
	All   bool
	Right Expr
}
type UnaryExpr struct {
	Op   string
	Expr Expr
}
type GroupExpr struct{ Expr Expr }
type ListLit struct{ Elems []Expr }
type CallExpr struct {
	Fun  Expr
	Args []Expr
}
type LambdaExpr struct {
	Params []string
	Body   Expr
}
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}
type IndexExpr struct {
	Target Expr
	Index  Expr
}
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

func (i *IntLit) emit(w io.Writer)    { io.WriteString(w, i.Value) }
func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }
func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "True")
	} else {
		io.WriteString(w, "False")
	}
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
func (n *NameRef) emit(w io.Writer) { io.WriteString(w, n.Name) }
func (l *LenExpr) emit(w io.Writer) {
	io.WriteString(w, "length ")
	l.Arg.emit(w)
}
func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *BinaryExpr:
		if len(ex.Ops) > 0 && ex.Ops[0].Op == "+" {
			if isStringExpr(ex.Left) || isStringExpr(ex.Ops[0].Right) {
				return true
			}
		}
	case *IfExpr:
		return isStringExpr(ex.Then) && isStringExpr(ex.Else)
	case *SliceExpr:
		return isStringExpr(ex.Target)
	case *IndexExpr:
		return isStringExpr(ex.Target)
	}
	return false
}

func isBoolExpr(e Expr) bool {
	switch ex := e.(type) {
	case *BoolLit:
		return true
	case *UnaryExpr:
		if ex.Op == "!" {
			return true
		}
		return isBoolExpr(ex.Expr)
	case *BinaryExpr:
		if len(ex.Ops) > 0 {
			op := ex.Ops[0].Op
			switch op {
			case "==", "!=", "<", ">", "<=", ">=", "&&", "||":
				return true
			}
		}
	}
	return false
}

func isListExpr(e Expr) bool {
	switch ex := e.(type) {
	case *ListLit, *SliceExpr, *IndexExpr:
		return true
	case *BinaryExpr:
		if len(ex.Ops) > 0 {
			switch ex.Ops[0].Op {
			case "union", "except", "intersect":
				return true
			}
		}
	}
	return false
}

func (b *BinaryExpr) emit(w io.Writer) {
	left := b.Left
	left.emit(w)
	for _, op := range b.Ops {
		io.WriteString(w, " ")
		switch op.Op {
		case "+":
			if isStringExpr(left) || isStringExpr(op.Right) {
				io.WriteString(w, "++")
			} else {
				io.WriteString(w, "+")
			}
		case "%":
			io.WriteString(w, "`mod`")
		case "in":
			if isStringExpr(left) || isStringExpr(op.Right) {
				io.WriteString(w, "`isInfixOf`")
			} else {
				io.WriteString(w, "`elem`")
			}
		case "union":
			if op.All {
				io.WriteString(w, "++")
			} else {
				io.WriteString(w, "`union`")
			}
		case "except":
			io.WriteString(w, "\\\\")
		case "intersect":
			io.WriteString(w, "`intersect`")
		default:
			io.WriteString(w, op.Op)
		}
		io.WriteString(w, " ")
		op.Right.emit(w)
		left = op.Right
	}
}
func (u *UnaryExpr) emit(w io.Writer) {
	if u.Op == "!" {
		io.WriteString(w, "not ")
		if _, ok := u.Expr.(*BinaryExpr); ok {
			io.WriteString(w, "(")
			u.Expr.emit(w)
			io.WriteString(w, ")")
			return
		}
		u.Expr.emit(w)
		return
	}
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}
func (g *GroupExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	g.Expr.emit(w)
	io.WriteString(w, ")")
}
func (c *CallExpr) emit(w io.Writer) {
	c.Fun.emit(w)
	for _, a := range c.Args {
		io.WriteString(w, " ")
		a.emit(w)
	}
}
func (l *LambdaExpr) emit(w io.Writer) {
	io.WriteString(w, "\\")
	for i, p := range l.Params {
		if i > 0 {
			io.WriteString(w, " ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, " -> ")
	l.Body.emit(w)
}
func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then ")
	i.Then.emit(w)
	io.WriteString(w, " else ")
	i.Else.emit(w)
}
func (i *IndexExpr) emit(w io.Writer) {
	i.Target.emit(w)
	io.WriteString(w, " !! ")
	i.Index.emit(w)
}
func (s *SliceExpr) emit(w io.Writer) {
	io.WriteString(w, "take (")
	diff := &BinaryExpr{Left: s.End, Ops: []BinaryOp{{Op: "-", Right: s.Start}}}
	diff.emit(w)
	io.WriteString(w, ") (drop ")
	s.Start.emit(w)
	io.WriteString(w, " ")
	s.Target.emit(w)
	io.WriteString(w, ")")
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
	b, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(b))
}

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	t := time.Now().In(loc)
	return fmt.Sprintf("-- Generated by Mochi transpiler v%s on %s\nimport Data.List (isInfixOf, union, intersect, (\\\\))\n", version(), t.Format("2006-01-02 15:04:05 MST"))
}

// Emit generates formatted Haskell code.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	for _, f := range p.Funcs {
		f.emit(&buf)
		buf.WriteByte('\n')
		buf.WriteByte('\n')
	}
	for _, s := range p.Stmts {
		if l, ok := s.(*LetStmt); ok {
			l.emit(&buf)
			buf.WriteByte('\n')
			buf.WriteByte('\n')
		}
	}
	buf.WriteString("main :: IO ()\n")
	buf.WriteString("main = do\n")
	for _, s := range p.Stmts {
		if _, ok := s.(*LetStmt); ok {
			continue
		}
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteByte('\n')
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into a simple Haskell AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env
	h := &Program{}
	vars := map[string]Expr{}
	for _, st := range prog.Statements {
		switch {
		case st.Let != nil:
			ex, err := convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			vars[st.Let.Name] = ex
		case st.Var != nil:
			ex, err := convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			vars[st.Var.Name] = ex
		case st.Assign != nil:
			ex, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			vars[st.Assign.Name] = ex
		case st.Fun != nil:
			fn, err := convertFunStmt(st.Fun)
			if err != nil {
				return nil, err
			}
			h.Funcs = append(h.Funcs, *fn)
		case st.If != nil:
			s, err := convertIfStmt(st.If)
			if err != nil {
				return nil, err
			}
			h.Stmts = append(h.Stmts, s)
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(call.Args) == 1 {
				arg, err := convertExpr(call.Args[0])
				if err != nil {
					return nil, err
				}
				str := isStringExpr(arg)
				if !str {
					if lit, ok := arg.(*NameRef); ok {
						if v, ok := vars[lit.Name]; ok {
							str = isStringExpr(v)
						}
					}
				}
				h.Stmts = append(h.Stmts, &PrintStmt{Expr: arg, String: str})
				continue
			}
			return nil, fmt.Errorf("unsupported expression")
		default:
			if st.Test == nil && st.Import == nil && st.Type == nil {
				return nil, fmt.Errorf("unsupported statement")
			}
		}
	}
	// deterministically emit variables in alphabetical order
	names := make([]string, 0, len(vars))
	for n := range vars {
		names = append(names, n)
	}
	sort.Strings(names)
	for i := len(names) - 1; i >= 0; i-- {
		n := names[i]
		h.Stmts = append([]Stmt{&LetStmt{Name: n, Expr: vars[n]}}, h.Stmts...)
	}
	return h, nil
}

// Print writes a lisp-like representation of the AST to stdout.
func Print(p *Program) { toNode(p).Print("") }

func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *PrintStmt:
		return &ast.Node{Kind: "print", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(st.Cond)}}
		for _, c := range st.Then {
			n.Children = append(n.Children, stmtNode(c))
		}
		for _, c := range st.Else {
			n.Children = append(n.Children, stmtNode(c))
		}
		return n
	default:
		return &ast.Node{Kind: "stmt"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "True"}
		}
		return &ast.Node{Kind: "bool", Value: "False"}
	case *NameRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Arg)}}
	case *BinaryExpr:
		n := &ast.Node{Kind: "bin"}
		n.Children = append(n.Children, exprNode(ex.Left))
		for _, op := range ex.Ops {
			n.Children = append(n.Children, &ast.Node{Kind: "op", Value: op.Op})
			n.Children = append(n.Children, exprNode(op.Right))
		}
		return n
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Expr)}}
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.Expr)}}
	case *CallExpr:
		n := &ast.Node{Kind: "call", Children: []*ast.Node{exprNode(ex.Fun)}}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *LambdaExpr:
		n := &ast.Node{Kind: "lambda"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, exprNode(ex.Body))
		return n
	case *IfExpr:
		return &ast.Node{Kind: "ifexpr", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	default:
		return &ast.Node{Kind: "expr"}
	}
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		// treat missing expression as zero
		return &IntLit{Value: "0"}, nil
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	left, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	be := &BinaryExpr{Left: left}
	for _, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		be.Ops = append(be.Ops, BinaryOp{Op: op.Op, All: op.All, Right: right})
	}
	if len(be.Ops) == 0 {
		return left, nil
	}
	return be, nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = &UnaryExpr{Op: u.Ops[i], Expr: expr}
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
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		if op.Call != nil {
			var args []Expr
			for _, a := range op.Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ex)
			}
			expr = &CallExpr{Fun: expr, Args: args}
			continue
		}
		if op.Index != nil {
			if op.Index.Colon == nil {
				idx, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				expr = &IndexExpr{Target: expr, Index: idx}
				continue
			}
			if op.Index.Colon2 != nil || op.Index.Step != nil || op.Index.End == nil {
				return nil, fmt.Errorf("unsupported slice")
			}
			var start Expr = &IntLit{Value: "0"}
			if op.Index.Start != nil {
				s, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				start = s
			}
			end, err := convertExpr(op.Index.End)
			if err != nil {
				return nil, err
			}
			expr = &SliceExpr{Target: expr, Start: start, End: end}
			continue
		}
		if op.Field != nil && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
			if op.Field.Name == "contains" {
				call := pf.Ops[i+1].Call
				if len(call.Args) != 1 {
					return nil, fmt.Errorf("contains expects 1 arg")
				}
				arg, err := convertExpr(call.Args[0])
				if err != nil {
					return nil, err
				}
				expr = &CallExpr{Fun: &NameRef{Name: "isInfixOf"}, Args: []Expr{arg, expr}}
				i++
				continue
			}
		}
		return nil, fmt.Errorf("postfix op not supported")
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &NameRef{Name: p.Selector.Root}, nil
	case p.If != nil:
		cond, err := convertExpr(p.If.Cond)
		if err != nil {
			return nil, err
		}
		thn, err := convertExpr(p.If.Then)
		if err != nil {
			return nil, err
		}
		var elseExpr *parser.Expr
		if p.If.ElseIf != nil {
			// chain else-if as nested if expression
			e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{If: p.If.ElseIf}}}}}
			elseExpr = e
		} else {
			elseExpr = p.If.Else
		}
		els, err := convertExpr(elseExpr)
		if err != nil {
			return nil, err
		}
		return &IfExpr{Cond: cond, Then: thn, Else: els}, nil
	case p.Group != nil:
		e, err := convertExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: e}, nil
	case p.List != nil:
		var elems []Expr
		for _, e := range p.List.Elems {
			ce, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ce)
		}
		return &ListLit{Elems: elems}, nil
	case p.FunExpr != nil && p.FunExpr.ExprBody != nil:
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
		}
		body, err := convertExpr(p.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Body: body}, nil
	case p.Call != nil:
		if p.Call.Func == "len" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &LenExpr{Arg: arg}, nil
		}
		if p.Call.Func == "count" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &LenExpr{Arg: arg}, nil
		}
		if p.Call.Func == "append" && len(p.Call.Args) == 2 {
			lst, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			it, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &BinaryExpr{Left: lst, Ops: []BinaryOp{{Op: "union", All: true, Right: &ListLit{Elems: []Expr{it}}}}}, nil
		}
		if p.Call.Func == "substring" && len(p.Call.Args) == 3 {
			str, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			start, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			return &SliceExpr{Target: str, Start: start, End: end}, nil
		}
		if p.Call.Func == "str" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Fun: &NameRef{Name: "show"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "min" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Fun: &NameRef{Name: "minimum"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "max" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Fun: &NameRef{Name: "maximum"}, Args: []Expr{arg}}, nil
		}
		fun := &NameRef{Name: p.Call.Func}
		var args []Expr
		for _, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ex)
		}
		return &CallExpr{Fun: fun, Args: args}, nil
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

func convertIfStmt(s *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmtList(s.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if s.ElseIf != nil {
		es, err := convertIfStmt(s.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{es}
	} else if len(s.Else) > 0 {
		elseStmts, err = convertStmtList(s.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertFunStmt(f *parser.FunStmt) (*Func, error) {
	stmts, err := convertStmtList(f.Body)
	if err != nil {
		return nil, err
	}
	var params []string
	for _, p := range f.Params {
		params = append(params, p.Name)
	}
	return &Func{Name: f.Name, Params: params, Body: stmts}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		switch {
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(call.Args) == 1 {
				arg, err := convertExpr(call.Args[0])
				if err != nil {
					return nil, err
				}
				str := isStringExpr(arg)
				out = append(out, &PrintStmt{Expr: arg, String: str})
				continue
			}
			return nil, fmt.Errorf("unsupported expression")
		case st.Return != nil:
			ex, err := convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &ReturnStmt{Expr: ex})
		case st.If != nil:
			s, err := convertIfStmt(st.If)
			if err != nil {
				return nil, err
			}
			out = append(out, s)
		default:
			return nil, fmt.Errorf("unsupported statement in block")
		}
	}
	return out, nil
}
