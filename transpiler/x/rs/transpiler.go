//go:build slow

package rs

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

var usesHashMap bool
var usesPrintVec bool
var mapVars map[string]bool

// Program represents a Rust program consisting of a list of statements.
type Program struct {
	Stmts        []Stmt
	UsesHashMap  bool
	UsesPrintVec bool
}

type Stmt interface{ emit(io.Writer) }

// Expr represents an expression node.
type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement consisting of a single expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

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

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type NumberLit struct{ Value string }

func (n *NumberLit) emit(w io.Writer) { io.WriteString(w, n.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "vec![")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "]")
}

type MapEntry struct {
	Key   Expr
	Value Expr
}

type MapLit struct{ Items []MapEntry }

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "HashMap::from([")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, "(")
		it.Key.emit(w)
		io.WriteString(w, ", ")
		it.Value.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, "])")
}

// IndexExpr represents `target[index]` access.
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (i *IndexExpr) emit(w io.Writer) {
	switch t := i.Target.(type) {
	case *NameRef:
		if mapVars[t.Name] {
			t.emit(w)
			io.WriteString(w, "[&")
			i.Index.emit(w)
			io.WriteString(w, "]")
			return
		}
	case *MapLit:
		i.Target.emit(w)
		io.WriteString(w, "[&")
		i.Index.emit(w)
		io.WriteString(w, "]")
		return
	}
	i.Target.emit(w)
	io.WriteString(w, "[")
	i.Index.emit(w)
	io.WriteString(w, "]")
}

// LenExpr represents a call to the `len` builtin.
type LenExpr struct{ Arg Expr }

func (l *LenExpr) emit(w io.Writer) {
	l.Arg.emit(w)
	io.WriteString(w, ".len()")
}

// SumExpr represents a call to the `sum` builtin.
type SumExpr struct{ Arg Expr }

func (s *SumExpr) emit(w io.Writer) {
	s.Arg.emit(w)
	io.WriteString(w, ".iter().sum::<i64>()")
}

// StrExpr represents a call to the `str` builtin.
type StrExpr struct{ Arg Expr }

func (s *StrExpr) emit(w io.Writer) {
	s.Arg.emit(w)
	io.WriteString(w, ".to_string()")
}

// ValuesExpr represents a call to the `values` builtin on a map.
type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let mut tmp = ")
	v.Map.emit(w)
	io.WriteString(w, ".values().cloned().collect::<Vec<_>>(); tmp.sort(); tmp }")
}

// AppendExpr represents a call to the `append` builtin on a list.
type AppendExpr struct {
	List Expr
	Elem Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let mut tmp = ")
	a.List.emit(w)
	io.WriteString(w, ".clone(); tmp.push(")
	a.Elem.emit(w)
	io.WriteString(w, "); tmp }")
}

// AvgExpr represents a call to the `avg` builtin.
type AvgExpr struct{ List Expr }

func (a *AvgExpr) emit(w io.Writer) {
	io.WriteString(w, "format!(\"{:.1}\", { let tmp = ")
	a.List.emit(w)
	io.WriteString(w, "; tmp.iter().map(|x| *x as f64).sum::<f64>() / (tmp.len() as f64) })")
}

// MinExpr represents a call to the `min` builtin.
type MinExpr struct{ List Expr }

func (m *MinExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let tmp = ")
	m.List.emit(w)
	io.WriteString(w, ".clone(); *tmp.iter().min().unwrap_or(&0) }")
}

// MaxExpr represents a call to the `max` builtin.
type MaxExpr struct{ List Expr }

func (m *MaxExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let tmp = ")
	m.List.emit(w)
	io.WriteString(w, ".clone(); *tmp.iter().max().unwrap_or(&0) }")
}

type NameRef struct{ Name string }

func (n *NameRef) emit(w io.Writer) { io.WriteString(w, n.Name) }

type VarDecl struct {
	Name    string
	Expr    Expr
	Type    string
	Mutable bool
}

func (v *VarDecl) emit(w io.Writer) {
	io.WriteString(w, "let ")
	if v.Mutable {
		io.WriteString(w, "mut ")
	}
	io.WriteString(w, v.Name)
	if v.Type != "" {
		io.WriteString(w, ": ")
		io.WriteString(w, v.Type)
	}
	if v.Expr != nil {
		io.WriteString(w, " = ")
		v.Expr.emit(w)
	} else if v.Type != "" {
		io.WriteString(w, " = ")
		io.WriteString(w, defaultValueForType(v.Type))
	}
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, a.Name)
	io.WriteString(w, " = ")
	a.Expr.emit(w)
}

// IndexAssignStmt assigns to an indexed expression like x[i] = v.
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *IndexAssignStmt) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, " = ")
	s.Value.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		if _, ok := b.Left.(*StringLit); ok {
			io.WriteString(w, "format!(\"{}{}\", ")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
		if _, ok := b.Right.(*StringLit); ok {
			io.WriteString(w, "format!(\"{}{}\", ")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}

type IfExpr struct {
	Cond   Expr
	Then   Expr
	ElseIf *IfExpr
	Else   Expr
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " { ")
	i.Then.emit(w)
	io.WriteString(w, " }")
	if i.ElseIf != nil {
		io.WriteString(w, " else ")
		i.ElseIf.emit(w)
	} else if i.Else != nil {
		io.WriteString(w, " else { ")
		i.Else.emit(w)
		io.WriteString(w, " }")
	}
}

type IfStmt struct {
	Cond   Expr
	Then   []Stmt
	ElseIf *IfStmt
	Else   []Stmt
}

func (i *IfStmt) emit(w io.Writer) {}

// WhileStmt represents a basic while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {}

// --- Transpiler ---

// Transpile converts a Mochi AST to a simplified Rust AST. Only a very small
// subset of Mochi is supported which is sufficient for tests.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	usesHashMap = false
	usesPrintVec = false
	mapVars = make(map[string]bool)
	prog := &Program{}
	for _, st := range p.Statements {
		s, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			prog.Stmts = append(prog.Stmts, s)
		}
	}
	_ = env // reserved for future use
	prog.UsesHashMap = usesHashMap
	prog.UsesPrintVec = usesPrintVec
	return prog, nil
}

func compileStmt(stmt *parser.Statement) (Stmt, error) {
	switch {
	case stmt.Expr != nil:
		e, err := compileExpr(stmt.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case stmt.Let != nil:
		var e Expr
		var err error
		if stmt.Let.Value != nil {
			e, err = compileExpr(stmt.Let.Value)
			if err != nil {
				return nil, err
			}
			if _, ok := e.(*MapLit); ok {
				mapVars[stmt.Let.Name] = true
			}
		}
		typ := ""
		if stmt.Let.Type != nil && stmt.Let.Type.Simple != nil {
			typ = rustType(*stmt.Let.Type.Simple)
		}
		return &VarDecl{Name: stmt.Let.Name, Expr: e, Type: typ}, nil
	case stmt.Var != nil:
		var e Expr
		var err error
		if stmt.Var.Value != nil {
			e, err = compileExpr(stmt.Var.Value)
			if err != nil {
				return nil, err
			}
			if _, ok := e.(*MapLit); ok {
				mapVars[stmt.Var.Name] = true
			}
		}
		typ := ""
		if stmt.Var.Type != nil && stmt.Var.Type.Simple != nil {
			typ = rustType(*stmt.Var.Type.Simple)
		}
		return &VarDecl{Name: stmt.Var.Name, Expr: e, Type: typ, Mutable: true}, nil
	case stmt.Assign != nil:
		val, err := compileExpr(stmt.Assign.Value)
		if err != nil {
			return nil, err
		}
		if len(stmt.Assign.Index) > 0 {
			target := Expr(&NameRef{Name: stmt.Assign.Name})
			target, err = applyIndexOps(target, stmt.Assign.Index)
			if err != nil {
				return nil, err
			}
			return &IndexAssignStmt{Target: target, Value: val}, nil
		}
		if _, ok := val.(*MapLit); ok {
			mapVars[stmt.Assign.Name] = true
		}
		return &AssignStmt{Name: stmt.Assign.Name, Expr: val}, nil
	case stmt.If != nil:
		return compileIfStmt(stmt.If)
	case stmt.While != nil:
		return compileWhileStmt(stmt.While)
	case stmt.Test == nil && stmt.Import == nil && stmt.Type == nil:
		return nil, fmt.Errorf("unsupported statement at %d:%d", stmt.Pos.Line, stmt.Pos.Column)
	}
	return nil, nil
}

func compileIfStmt(n *parser.IfStmt) (Stmt, error) {
	cond, err := compileExpr(n.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts := make([]Stmt, 0, len(n.Then))
	for _, st := range n.Then {
		cs, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if cs != nil {
			thenStmts = append(thenStmts, cs)
		}
	}
	var elseStmts []Stmt
	if len(n.Else) > 0 {
		elseStmts = make([]Stmt, 0, len(n.Else))
		for _, st := range n.Else {
			cs, err := compileStmt(st)
			if err != nil {
				return nil, err
			}
			if cs != nil {
				elseStmts = append(elseStmts, cs)
			}
		}
	}
	var elseIf *IfStmt
	if n.ElseIf != nil {
		s, err := compileIfStmt(n.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = s.(*IfStmt)
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts, ElseIf: elseIf}, nil
}

func compileWhileStmt(n *parser.WhileStmt) (Stmt, error) {
	cond, err := compileExpr(n.Cond)
	if err != nil {
		return nil, err
	}
	body := make([]Stmt, 0, len(n.Body))
	for _, st := range n.Body {
		cs, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if cs != nil {
			body = append(body, cs)
		}
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func applyIndexOps(base Expr, ops []*parser.IndexOp) (Expr, error) {
	var err error
	for _, op := range ops {
		if op.Colon != nil || op.Colon2 != nil || op.End != nil || op.Step != nil {
			return nil, fmt.Errorf("slice not supported")
		}
		if op.Start == nil {
			return nil, fmt.Errorf("nil index")
		}
		var idx Expr
		idx, err = compileExpr(op.Start)
		if err != nil {
			return nil, err
		}
		base = &IndexExpr{Target: base, Index: idx}
	}
	return base, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	first, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := make([]string, len(e.Binary.Right))
	for i, op := range e.Binary.Right {
		right, err := compilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
		ops[i] = op.Op
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				left := operands[i]
				right := operands[i+1]
				expr := &BinaryExpr{Left: left, Op: ops[i], Right: right}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	return operands[0], nil
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
		op := u.Ops[i]
		expr = &UnaryExpr{Op: op, Expr: expr}
	}
	return expr, nil
}

func compilePostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := compilePrimary(p.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.End != nil || op.Index.Step != nil {
				return nil, fmt.Errorf("slice not supported")
			}
			if op.Index.Start == nil {
				return nil, fmt.Errorf("nil index")
			}
			idx, err := compileExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Cast != nil:
			// ignore casts
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
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
			if len(args) == 1 {
				switch args[0].(type) {
				case *AppendExpr:
					usesPrintVec = true
					return &CallExpr{Func: "mochi_print_vec", Args: args}, nil
				}
				if _, ok := args[0].(*StringLit); !ok {
					fmtStr := "{}"
					switch args[0].(type) {
					case *MapLit, *ValuesExpr:
						fmtStr = "{:?}"
					}
					args = append([]Expr{&StringLit{Value: fmtStr}}, args...)
				}
			} else {
				fmtStr := "{}"
				for i := 1; i < len(args); i++ {
					fmtStr += " {}"
				}
				args = append([]Expr{&StringLit{Value: fmtStr}}, args...)
			}
			name = "println!"
			return &CallExpr{Func: name, Args: args}, nil
		}
		if name == "len" && len(args) == 1 {
			return &LenExpr{Arg: args[0]}, nil
		}
		if name == "sum" && len(args) == 1 {
			return &SumExpr{Arg: args[0]}, nil
		}
		if name == "str" && len(args) == 1 {
			return &StrExpr{Arg: args[0]}, nil
		}
		if name == "values" && len(args) == 1 {
			return &ValuesExpr{Map: args[0]}, nil
		}
		if name == "append" && len(args) == 2 {
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		}
		if name == "avg" && len(args) == 1 {
			return &AvgExpr{List: args[0]}, nil
		}
		if name == "count" && len(args) == 1 {
			return &LenExpr{Arg: args[0]}, nil
		}
		if name == "min" && len(args) == 1 {
			return &MinExpr{List: args[0]}, nil
		}
		if name == "max" && len(args) == 1 {
			return &MaxExpr{List: args[0]}, nil
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil:
		return compileLiteral(p.Lit)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := compileExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		entries := make([]MapEntry, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := compileExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := compileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			entries[i] = MapEntry{Key: k, Value: v}
		}
		usesHashMap = true
		return &MapLit{Items: entries}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &NameRef{Name: p.Selector.Root}, nil
	case p.If != nil:
		return compileIfExpr(p.If)
	case p.Group != nil:
		return compileExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileIfExpr(n *parser.IfExpr) (Expr, error) {
	cond, err := compileExpr(n.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(n.Then)
	if err != nil {
		return nil, err
	}
	var elseIf *IfExpr
	if n.ElseIf != nil {
		ei, err := compileIfExpr(n.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = ei.(*IfExpr)
	}
	var elseExpr Expr
	if n.Else != nil {
		e, err := compileExpr(n.Else)
		if err != nil {
			return nil, err
		}
		elseExpr = e
	}
	return &IfExpr{Cond: cond, Then: thenExpr, ElseIf: elseIf, Else: elseExpr}, nil
}

func compileLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Int != nil:
		return &NumberLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		return &NumberLit{Value: fmt.Sprintf("%g", *l.Float)}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

func rustType(t string) string {
	switch t {
	case "int":
		return "i64"
	case "float":
		return "f64"
	case "bool":
		return "bool"
	case "string":
		return "String"
	}
	return "i64"
}

func defaultValueForType(t string) string {
	switch t {
	case "i64":
		return "0"
	case "f64":
		return "0.0"
	case "bool":
		return "false"
	case "String":
		return "String::new()"
	}
	return "Default::default()"
}

func writeStmt(buf *bytes.Buffer, s Stmt, indent int) {
	for i := 0; i < indent; i++ {
		buf.WriteString("    ")
	}
	switch st := s.(type) {
	case *IfStmt:
		writeIfStmt(buf, st, indent)
	case *WhileStmt:
		writeWhileStmt(buf, st, indent)
	default:
		st.emit(buf)
		buf.WriteString(";")
	}
	buf.WriteByte('\n')
}

func writeIfStmt(buf *bytes.Buffer, s *IfStmt, indent int) {
	buf.WriteString("if ")
	s.Cond.emit(buf)
	buf.WriteString(" {\n")
	for _, st := range s.Then {
		writeStmt(buf, st, indent+1)
	}
	for i := 0; i < indent; i++ {
		buf.WriteString("    ")
	}
	buf.WriteString("}")
	if s.ElseIf != nil {
		buf.WriteString(" else ")
		writeIfStmt(buf, s.ElseIf, indent)
	} else if len(s.Else) > 0 {
		buf.WriteString(" else {\n")
		for _, st := range s.Else {
			writeStmt(buf, st, indent+1)
		}
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		buf.WriteString("}")
	}
}

func writeWhileStmt(buf *bytes.Buffer, s *WhileStmt, indent int) {
	buf.WriteString("while ")
	if s.Cond != nil {
		s.Cond.emit(buf)
	} else {
		buf.WriteString("true")
	}
	buf.WriteString(" {\n")
	for _, st := range s.Body {
		writeStmt(buf, st, indent+1)
	}
	for i := 0; i < indent; i++ {
		buf.WriteString("    ")
	}
	buf.WriteString("}")
}

// Emit generates formatted Rust source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	if prog.UsesHashMap {
		buf.WriteString("use std::collections::HashMap;\n")
	}
	if prog.UsesPrintVec {
		buf.WriteString("fn mochi_print_vec(v: Vec<i64>) {\n")
		buf.WriteString("    for (i, n) in v.iter().enumerate() {\n")
		buf.WriteString("        if i > 0 { print!(\" \"); }\n")
		buf.WriteString("        print!(\"{}\", n);\n")
		buf.WriteString("    }\n    println!();\n}\n")
	}
	buf.WriteString("fn main() {\n")
	for _, s := range prog.Stmts {
		writeStmt(&buf, s, 1)
	}
	buf.WriteString("}\n")
	if b := buf.Bytes(); len(b) > 0 && b[len(b)-1] != '\n' {
		b = append(b, '\n')
	}
	return buf.Bytes()
}

func repoRoot() string {
	dir, _ := os.Getwd()
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
	if b, err := os.ReadFile(filepath.Join(root, "VERSION")); err == nil {
		return strings.TrimSpace(string(b))
	}
	return "dev"
}

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	ts := time.Now().In(loc).Format("2006-01-02 15:04:05 MST")
	return fmt.Sprintf("// Generated by Mochi transpiler v%s on %s\n", version(), ts)
}

// Print converts prog to ast.Node form and prints it.
func Print(prog *Program) {
	toNode(prog).Print("")
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
	case *VarDecl:
		n := &ast.Node{Kind: "let", Value: st.Name}
		if st.Expr != nil {
			n.Children = []*ast.Node{exprNode(st.Expr)}
		}
		return n
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *IndexAssignStmt:
		return &ast.Node{Kind: "idx-assign", Children: []*ast.Node{exprNode(st.Target), exprNode(st.Value)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if"}
		n.Children = append(n.Children, exprNode(st.Cond))
		thenNode := &ast.Node{Kind: "then"}
		for _, s2 := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, thenNode)
		if st.ElseIf != nil {
			n.Children = append(n.Children, stmtNode(st.ElseIf))
		} else if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, s2 := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while"}
		n.Children = append(n.Children, exprNode(st.Cond))
		bodyNode := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			bodyNode.Children = append(bodyNode.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, bodyNode)
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
	case *NameRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *NumberLit:
		return &ast.Node{Kind: "number", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, el := range ex.Elems {
			n.Children = append(n.Children, exprNode(el))
		}
		return n
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, it := range ex.Items {
			pair := &ast.Node{Kind: "item"}
			pair.Children = append(pair.Children, exprNode(it.Key))
			pair.Children = append(pair.Children, exprNode(it.Value))
			n.Children = append(n.Children, pair)
		}
		return n
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Arg)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprNode(ex.Arg)}}
	case *StrExpr:
		return &ast.Node{Kind: "str", Children: []*ast.Node{exprNode(ex.Arg)}}
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{exprNode(ex.Map)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Elem)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprNode(ex.List)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{exprNode(ex.List)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{exprNode(ex.List)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Expr)}}
	case *IfExpr:
		n := &ast.Node{Kind: "if_expr"}
		n.Children = append(n.Children, exprNode(ex.Cond))
		n.Children = append(n.Children, exprNode(ex.Then))
		if ex.ElseIf != nil {
			n.Children = append(n.Children, exprNode(ex.ElseIf))
		} else if ex.Else != nil {
			n.Children = append(n.Children, exprNode(ex.Else))
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
