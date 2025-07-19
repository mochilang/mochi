//go:build slow

package ctrans

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

// --- Simple C AST ---

type Program struct {
	Functions []*Function
}

type Function struct {
	Name   string
	Params []string
	Body   []Stmt
}

type Stmt interface {
	emit(io.Writer)
}

type CallStmt struct {
	Func string
	Args []Expr
	Type string
}

type ReturnStmt struct {
	Expr Expr
}

type DeclStmt struct {
	Name  string
	Value Expr
	Type  string
}

type AssignStmt struct {
	Name  string
	Value Expr
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

type ForStmt struct {
	Var   string
	Start Expr
	End   Expr
	List  []Expr
	Body  []Stmt
}

func (c *CallStmt) emit(w io.Writer) {
	if c.Func == "print" && len(c.Args) == 1 {
		if c.Type == "string" {
			io.WriteString(w, "\tprintf(\"%s\\n\", ")
			c.Args[0].emitExpr(w)
			io.WriteString(w, ");\n")
		} else {
			switch arg := c.Args[0].(type) {
			case *StringLit:
				fmt.Fprintf(w, "\tprintf(\"%s\\n\");\n", escape(arg.Value))
			default:
				io.WriteString(w, "\tprintf(\"%d\\n\", ")
				arg.emitExpr(w)
				io.WriteString(w, ");\n")
			}
		}
		return
	}
}

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "\treturn")
	if r.Expr != nil {
		io.WriteString(w, " ")
		r.Expr.emitExpr(w)
	} else {
		io.WriteString(w, " 0")
	}
	io.WriteString(w, ";\n")
}

func (d *DeclStmt) emit(w io.Writer) {
	typ := d.Type
	if typ == "" {
		typ = "int"
	}
	io.WriteString(w, "\t")
	io.WriteString(w, typ)
	io.WriteString(w, " ")
	io.WriteString(w, d.Name)
	if d.Value != nil {
		io.WriteString(w, " = ")
		d.Value.emitExpr(w)
	}
	io.WriteString(w, ";\n")
}

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, "\t")
	io.WriteString(w, a.Name)
	io.WriteString(w, " = ")
	if a.Value != nil {
		a.Value.emitExpr(w)
	}
	io.WriteString(w, ";\n")
}

func (ws *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "\twhile (")
	if ws.Cond != nil {
		ws.Cond.emitExpr(w)
	}
	io.WriteString(w, ") {\n")
	for _, s := range ws.Body {
		s.emit(w)
	}
	io.WriteString(w, "\t}\n")
}

func (f *ForStmt) emit(w io.Writer) {
	if len(f.List) > 0 {
		io.WriteString(w, "\t{\n")
		io.WriteString(w, "\t\tint __arr[] = {")
		for i, e := range f.List {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			e.emitExpr(w)
		}
		io.WriteString(w, "};\n")
		io.WriteString(w, "\t\tfor (int __i = 0; __i < (int)(sizeof(__arr)/sizeof(__arr[0])); __i++) {\n")
		fmt.Fprintf(w, "\t\t\tint %s = __arr[__i];\n", f.Var)
		for _, s := range f.Body {
			s.emit(w)
		}
		io.WriteString(w, "\t\t}\n")
		io.WriteString(w, "\t}\n")
		return
	}
	io.WriteString(w, "\tfor (int ")
	io.WriteString(w, f.Var)
	io.WriteString(w, " = ")
	if f.Start != nil {
		f.Start.emitExpr(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Var)
	io.WriteString(w, " < ")
	if f.End != nil {
		f.End.emitExpr(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Var)
	io.WriteString(w, "++ ) {\n")
	for _, s := range f.Body {
		s.emit(w)
	}
	io.WriteString(w, "\t}\n")
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "\tif (")
	if i.Cond != nil {
		i.Cond.emitExpr(w)
	}
	io.WriteString(w, ") {\n")
	for _, s := range i.Then {
		s.emit(w)
	}
	io.WriteString(w, "\t}")
	if len(i.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, s := range i.Else {
			s.emit(w)
		}
		io.WriteString(w, "\t}")
	}
	io.WriteString(w, "\n")
}

type Expr interface{ emitExpr(io.Writer) }

type StringLit struct{ Value string }

func (s *StringLit) emitExpr(w io.Writer) {
	fmt.Fprintf(w, "\"%s\"", escape(s.Value))
}

type IntLit struct{ Value int }

func (i *IntLit) emitExpr(w io.Writer) {
	fmt.Fprintf(w, "%d", i.Value)
}

type VarRef struct{ Name string }

func (v *VarRef) emitExpr(w io.Writer) {
	io.WriteString(w, v.Name)
}

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emitExpr(w io.Writer) {
	io.WriteString(w, c.Func)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emitExpr(w)
	}
	io.WriteString(w, ")")
}

type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emitExpr(w io.Writer) {
	io.WriteString(w, "(")
	if c.Cond != nil {
		c.Cond.emitExpr(w)
	}
	io.WriteString(w, " ? ")
	if c.Then != nil {
		c.Then.emitExpr(w)
	}
	io.WriteString(w, " : ")
	if c.Else != nil {
		c.Else.emitExpr(w)
	}
	io.WriteString(w, ")")
}

func (b *BinaryExpr) emitExpr(w io.Writer) {
	if (exprIsString(b.Left) || exprIsString(b.Right)) &&
		(b.Op == "==" || b.Op == "!=" || b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=") {
		io.WriteString(w, "strcmp(")
		b.Left.emitExpr(w)
		io.WriteString(w, ", ")
		b.Right.emitExpr(w)
		io.WriteString(w, ") ")
		switch b.Op {
		case "==":
			io.WriteString(w, "== 0")
		case "!=":
			io.WriteString(w, "!= 0")
		case "<":
			io.WriteString(w, "< 0")
		case "<=":
			io.WriteString(w, "<= 0")
		case ">":
			io.WriteString(w, "> 0")
		case ">=":
			io.WriteString(w, ">= 0")
		}
		return
	}
	if _, ok := b.Left.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		b.Left.emitExpr(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emitExpr(w)
	}
	fmt.Fprintf(w, " %s ", b.Op)
	if _, ok := b.Right.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		b.Right.emitExpr(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emitExpr(w)
	}
}

func escape(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, "\"", "\\\"")
	return s
}

// repoRoot attempts to locate the repository root containing go.mod.
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

// mochiVersion reads the VERSION file from the repository root.
func mochiVersion() string {
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

// Emit generates C source from AST.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	loc := time.FixedZone("GMT+7", 7*60*60)
	now := time.Now().In(loc)
	fmt.Fprintf(&buf, "// Generated by Mochi %s on %s\n", mochiVersion(), now.Format("2006-01-02 15:04:05 MST"))
	buf.WriteString("#include <stdio.h>\n")
	buf.WriteString("#include <string.h>\n\n")
	for _, f := range p.Functions {
		buf.WriteString("int ")
		buf.WriteString(f.Name)
		buf.WriteString("(")
		for i, p := range f.Params {
			if i > 0 {
				buf.WriteString(", ")
			}
			buf.WriteString("int ")
			buf.WriteString(p)
		}
		buf.WriteString(") {\n")
		for _, s := range f.Body {
			s.emit(&buf)
		}
		buf.WriteString("\treturn 0;\n")
		buf.WriteString("}\n")
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into a C AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	p := &Program{}
	mainFn := &Function{Name: "main"}
	for _, st := range prog.Statements {
		if st.Fun != nil {
			body, err := compileStmts(env, st.Fun.Body)
			if err != nil {
				return nil, err
			}
			var params []string
			for _, pa := range st.Fun.Params {
				params = append(params, pa.Name)
			}
			p.Functions = append(p.Functions, &Function{Name: st.Fun.Name, Params: params, Body: body})
			continue
		}
		stmt, err := compileStmt(env, st)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			mainFn.Body = append(mainFn.Body, stmt)
		}
	}
	p.Functions = append(p.Functions, mainFn)
	return p, nil
}

func compileStmts(env *types.Env, list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		stmt, err := compileStmt(env, s)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			out = append(out, stmt)
		}
	}
	return out, nil
}

func compileStmt(env *types.Env, s *parser.Statement) (Stmt, error) {
	switch {
	case s.Expr != nil:
		call := s.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			if arg != nil {
				typ := ""
				if exprIsString(arg) {
					typ = "string"
				} else if v, ok := arg.(*VarRef); ok {
					if t, err := env.GetVar(v.Name); err == nil {
						if _, ok := t.(types.StringType); ok {
							typ = "string"
						}
					}
				}
				return &CallStmt{Func: "print", Args: []Expr{arg}, Type: typ}, nil
			}
		}
	case s.Let != nil:
		t, _ := env.GetVar(s.Let.Name)
		declType := "int"
		if _, ok := t.(types.StringType); ok {
			declType = "const char*"
		}
		return &DeclStmt{Name: s.Let.Name, Value: convertExpr(s.Let.Value), Type: declType}, nil
	case s.Var != nil:
		t, _ := env.GetVar(s.Var.Name)
		declType := "int"
		if _, ok := t.(types.StringType); ok {
			declType = "const char*"
		}
		return &DeclStmt{Name: s.Var.Name, Value: convertExpr(s.Var.Value), Type: declType}, nil
	case s.Assign != nil:
		return &AssignStmt{Name: s.Assign.Name, Value: convertExpr(s.Assign.Value)}, nil
	case s.Return != nil:
		return &ReturnStmt{Expr: convertExpr(s.Return.Value)}, nil
	case s.While != nil:
		cond := convertExpr(s.While.Cond)
		body, err := compileStmts(env, s.While.Body)
		if err != nil {
			return nil, err
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case s.For != nil:
		body, err := compileStmts(env, s.For.Body)
		if err != nil {
			return nil, err
		}
		if s.For.RangeEnd != nil {
			start := convertExpr(s.For.Source)
			end := convertExpr(s.For.RangeEnd)
			return &ForStmt{Var: s.For.Name, Start: start, End: end, Body: body}, nil
		}
		list, ok := convertListExpr(s.For.Source)
		if ok {
			return &ForStmt{Var: s.For.Name, List: list, Body: body}, nil
		}
		return nil, fmt.Errorf("unsupported for-loop")
	case s.If != nil:
		return compileIfStmt(env, s.If)
	}
	return nil, nil
}

func compileIfStmt(env *types.Env, n *parser.IfStmt) (Stmt, error) {
	cond := convertExpr(n.Cond)
	thenBody, err := compileStmts(env, n.Then)
	if err != nil {
		return nil, err
	}
	var elseBody []Stmt
	if n.ElseIf != nil {
		s, err := compileIfStmt(env, n.ElseIf)
		if err != nil {
			return nil, err
		}
		elseBody = []Stmt{s}
	} else if len(n.Else) > 0 {
		elseBody, err = compileStmts(env, n.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenBody, Else: elseBody}, nil
}

func convertIfExpr(n *parser.IfExpr) Expr {
	if n == nil {
		return nil
	}
	cond := convertExpr(n.Cond)
	if cond == nil {
		return nil
	}
	thenExpr := convertExpr(n.Then)
	if thenExpr == nil {
		return nil
	}
	var elseExpr Expr
	if n.ElseIf != nil {
		elseExpr = convertIfExpr(n.ElseIf)
	} else if n.Else != nil {
		elseExpr = convertExpr(n.Else)
	}
	if elseExpr == nil {
		return nil
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}
}

func convertExpr(e *parser.Expr) Expr {
	if e == nil || e.Binary == nil {
		return nil
	}
	// Convert left operand
	left := convertUnary(e.Binary.Left)
	if left == nil {
		return nil
	}

	operands := []Expr{left}
	var operators []string

	// Convert remaining operators and operands
	for _, part := range e.Binary.Right {
		rhs := convertUnary(&parser.Unary{Value: part.Right})
		if rhs == nil {
			return nil
		}
		operators = append(operators, part.Op)
		operands = append(operands, rhs)
	}

	// Operator precedence levels (highest to lowest) to match interpreter
	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	contains := func(list []string, op string) bool {
		for _, v := range list {
			if v == op {
				return true
			}
		}
		return false
	}

	// Build expression tree respecting precedence
	for _, level := range levels {
		for i := 0; i < len(operators); {
			if contains(level, operators[i]) {
				bin := &BinaryExpr{Op: operators[i], Left: operands[i], Right: operands[i+1]}
				operands[i] = bin
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil
	}
	// Constant fold simple string concatenation
	if bin, ok := operands[0].(*BinaryExpr); ok && bin.Op == "+" {
		if l, ok := bin.Left.(*StringLit); ok {
			if r, ok2 := bin.Right.(*StringLit); ok2 {
				return &StringLit{Value: l.Value + r.Value}
			}
		}
	}
	return operands[0]
}

func convertUnary(u *parser.Unary) Expr {
	if u == nil || u.Value == nil {
		return nil
	}
	if g := u.Value.Target.Group; g != nil {
		return convertExpr(g)
	}
	if call := u.Value.Target.Call; call != nil && len(u.Ops) == 0 {
		if call.Func == "len" && len(call.Args) == 1 {
			if list, ok := convertListExpr(call.Args[0]); ok {
				return &IntLit{Value: len(list)}
			}
			arg := call.Args[0]
			if arg != nil && arg.Binary != nil && arg.Binary.Left != nil && arg.Binary.Left.Value != nil {
				t := arg.Binary.Left.Value.Target
				if t != nil && t.Lit != nil && t.Lit.Str != nil {
					return &IntLit{Value: len(*t.Lit.Str)}
				}
			}
		}
		var args []Expr
		for _, a := range call.Args {
			ex := convertExpr(a)
			if ex == nil {
				return nil
			}
			args = append(args, ex)
		}
		return &CallExpr{Func: call.Func, Args: args}
	}
	if ifexpr := u.Value.Target.If; ifexpr != nil && len(u.Ops) == 0 {
		return convertIfExpr(ifexpr)
	}
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 && len(u.Ops) == 0 {
		return &VarRef{Name: sel.Root}
	}
	lit := u.Value.Target.Lit
	if lit == nil {
		return nil
	}
	if lit.Str != nil && len(u.Ops) == 0 {
		return &StringLit{Value: *lit.Str}
	}
	if lit.Int != nil {
		v := int(*lit.Int)
		for _, op := range u.Ops {
			if op == "-" {
				v = -v
			}
		}
		return &IntLit{Value: v}
	}
	if lit.Bool != nil && len(u.Ops) == 0 {
		if bool(*lit.Bool) {
			return &IntLit{Value: 1}
		}
		return &IntLit{Value: 0}
	}
	return nil
}

func convertListExpr(e *parser.Expr) ([]Expr, bool) {
	if e == nil || e.Binary == nil {
		return nil, false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil || u.Value.Target.List == nil {
		return nil, false
	}
	list := u.Value.Target.List
	var out []Expr
	for _, item := range list.Elems {
		ex := convertExpr(item)
		if ex == nil {
			return nil, false
		}
		out = append(out, ex)
	}
	return out, true
}

func exprIsString(e Expr) bool {
	switch v := e.(type) {
	case *StringLit:
		return true
	case *CondExpr:
		return exprIsString(v.Then) && exprIsString(v.Else)
	default:
		return false
	}
}
