//go:build slow

package gotranspiler

import (
	"bytes"
	"fmt"
	"go/format"
	"io"

	"mochi/ast"
	"mochi/parser"
	meta "mochi/transpiler/meta"
	"mochi/types"
)

// Program represents a Go program consisting of a sequence of statements.
type Program struct {
	Stmts        []Stmt
	UseAvg       bool
	UseSum       bool
	UseMin       bool
	UseMax       bool
	UseContains  bool
	UseUnion     bool
	UseUnionAll  bool
	UseExcept    bool
	UseIntersect bool
	UseSubstring bool
	UsePrint     bool
}

var (
	usesAvg       bool
	usesSum       bool
	usesMin       bool
	usesMax       bool
	usesContains  bool
	usesUnion     bool
	usesUnionAll  bool
	usesExcept    bool
	usesIntersect bool
	usesSubstring bool
	usesPrint     bool
)

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type VarDecl struct {
	Name  string
	Type  string
	Value Expr
}

func (v *VarDecl) emit(w io.Writer) {
	switch {
	case v.Value != nil && v.Type != "":
		fmt.Fprintf(w, "var %s %s = ", v.Name, v.Type)
		v.Value.emit(w)
	case v.Value != nil:
		fmt.Fprintf(w, "%s := ", v.Name)
		v.Value.emit(w)
	case v.Type != "":
		fmt.Fprintf(w, "var %s %s", v.Name, v.Type)
	default:
		fmt.Fprintf(w, "var %s", v.Name)
	}
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", a.Name)
	a.Value.emit(w)
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

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	fmt.Fprint(w, "[]int{")
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "}")
}

type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { fmt.Fprint(w, v.Name) }

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
	fmt.Fprint(w, ")")
}

// Transpile converts a Mochi program to a minimal Go AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	usesAvg = false
	usesSum = false
	usesMin = false
	usesMax = false
	usesContains = false
	usesUnion = false
	usesUnionAll = false
	usesExcept = false
	usesIntersect = false
	usesSubstring = false
	usesPrint = false
	gp := &Program{}
	for _, stmt := range p.Statements {
		switch {
		case stmt.Expr != nil:
			e, err := compileExpr(stmt.Expr.Expr)
			if err != nil {
				return nil, err
			}
			gp.Stmts = append(gp.Stmts, &ExprStmt{Expr: e})
		case stmt.Let != nil:
			if stmt.Let.Value != nil {
				e, err := compileExpr(stmt.Let.Value)
				if err != nil {
					return nil, err
				}
				gp.Stmts = append(gp.Stmts, &VarDecl{Name: stmt.Let.Name, Type: toGoType(stmt.Let.Type), Value: e})
			} else {
				gp.Stmts = append(gp.Stmts, &VarDecl{Name: stmt.Let.Name, Type: toGoType(stmt.Let.Type)})
			}
		case stmt.Var != nil:
			if stmt.Var.Value != nil {
				e, err := compileExpr(stmt.Var.Value)
				if err != nil {
					return nil, err
				}
				gp.Stmts = append(gp.Stmts, &VarDecl{Name: stmt.Var.Name, Type: toGoType(stmt.Var.Type), Value: e})
			} else {
				gp.Stmts = append(gp.Stmts, &VarDecl{Name: stmt.Var.Name, Type: toGoType(stmt.Var.Type)})
			}
		case stmt.Assign != nil && len(stmt.Assign.Index) == 0 && len(stmt.Assign.Field) == 0:
			e, err := compileExpr(stmt.Assign.Value)
			if err != nil {
				return nil, err
			}
			gp.Stmts = append(gp.Stmts, &AssignStmt{Name: stmt.Assign.Name, Value: e})
		default:
			if stmt.Test == nil && stmt.Import == nil && stmt.Type == nil {
				return nil, fmt.Errorf("unsupported statement at %d:%d", stmt.Pos.Line, stmt.Pos.Column)
			}
		}
	}
	_ = env // reserved for future use
	gp.UseAvg = usesAvg
	gp.UseSum = usesSum
	gp.UseMin = usesMin
	gp.UseMax = usesMax
	gp.UseContains = usesContains
	gp.UseUnion = usesUnion
	gp.UseUnionAll = usesUnionAll
	gp.UseExcept = usesExcept
	gp.UseIntersect = usesIntersect
	gp.UseSubstring = usesSubstring
	gp.UsePrint = usesPrint
	return gp, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return compileBinary(e.Binary)
}

func compileBinary(b *parser.BinaryExpr) (Expr, error) {
	first, err := compileUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := make([]*parser.BinaryOp, len(b.Right))
	for i, op := range b.Right {
		expr, err := compilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		ops[i] = op
		operands = append(operands, expr)
	}

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
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			opName := ops[i].Op
			if opName == "union" && ops[i].All {
				opName = "union_all"
			}
			if contains(level, opName) {
				left := operands[i]
				right := operands[i+1]
				var newExpr Expr
				switch opName {
				case "in":
					usesContains = true
					newExpr = &CallExpr{Func: "contains", Args: []Expr{right, left}}
				case "union":
					usesUnion = true
					newExpr = &CallExpr{Func: "union", Args: []Expr{left, right}}
				case "union_all":
					usesUnionAll = true
					newExpr = &CallExpr{Func: "unionAll", Args: []Expr{left, right}}
				case "except":
					usesExcept = true
					newExpr = &CallExpr{Func: "except", Args: []Expr{left, right}}
				case "intersect":
					usesIntersect = true
					newExpr = &CallExpr{Func: "intersect", Args: []Expr{left, right}}
				default:
					newExpr = &BinaryExpr{Left: left, Op: ops[i].Op, Right: right}
				}
				operands[i] = newExpr
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
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := compilePostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
		} else {
			return nil, fmt.Errorf("unsupported unary op")
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
		switch name {
		case "print":
			usesPrint = true
			name = "mochiPrint"
		case "avg":
			usesAvg = true
		case "count":
			name = "len"
		case "str":
			name = "fmt.Sprint"
		case "sum":
			usesSum = true
		case "min":
			usesMin = true
		case "max":
			usesMax = true
		case "substring":
			usesSubstring = true
			return &CallExpr{Func: "substring", Args: args}, nil
		}
		return &CallExpr{Func: name, Args: args}, nil
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
	case p.Lit != nil:
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
		if p.Lit.Int != nil {
			return &IntLit{Value: int(*p.Lit.Int)}, nil
		}
		return nil, fmt.Errorf("unsupported literal")
	case p.Group != nil:
		return compileExpr(p.Group)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarRef{Name: p.Selector.Root}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

func toGoType(t *parser.TypeRef) string {
	if t == nil || t.Simple == nil {
		return ""
	}
	switch *t.Simple {
	case "int":
		return "int"
	case "string":
		return "string"
	case "bool":
		return "bool"
	default:
		return "interface{}"
	}
}

// Emit formats the Go AST back into source code.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("//go:build ignore\n\n")
	buf.Write(meta.Header("//"))
	buf.WriteString("package main\n\n")
	buf.WriteString("import (\n    \"fmt\"\n")
	if prog.UsePrint {
		buf.WriteString("    \"math\"\n")
	}
	buf.WriteString(")\n\n")
	if prog.UseAvg {
		buf.WriteString("func avg(nums []int) float64 {\n")
		buf.WriteString("    sum := 0\n")
		buf.WriteString("    for _, n := range nums { sum += n }\n")
		buf.WriteString("    return float64(sum) / float64(len(nums))\n")
		buf.WriteString("}\n\n")
	}
	if prog.UseSum {
		buf.WriteString("func sum(nums []int) int {\n    s := 0\n    for _, n := range nums { s += n }\n    return s\n}\n\n")
	}
	if prog.UseMin {
		buf.WriteString("func min(nums []int) int {\n    if len(nums)==0 { return 0 }\n    m := nums[0]\n    for _, n := range nums[1:] { if n < m { m = n } }\n    return m\n}\n\n")
	}
	if prog.UseMax {
		buf.WriteString("func max(nums []int) int {\n    if len(nums)==0 { return 0 }\n    m := nums[0]\n    for _, n := range nums[1:] { if n > m { m = n } }\n    return m\n}\n\n")
	}
	if prog.UseContains {
		buf.WriteString("func contains(s []int, v int) bool {\n    for _, n := range s { if n == v { return true } }\n    return false\n}\n\n")
	}
	if prog.UseUnion {
		buf.WriteString("func union(a, b []int) []int {\n    m := map[int]bool{}\n    res := []int{}\n    for _, n := range a { if !m[n] { m[n]=true; res=append(res,n) } }\n    for _, n := range b { if !m[n] { m[n]=true; res=append(res,n) } }\n    return res\n}\n\n")
	}
	if prog.UseUnionAll {
		buf.WriteString("func unionAll(a, b []int) []int {\n    res := make([]int, len(a))\n    copy(res, a)\n    res = append(res, b...)\n    return res\n}\n\n")
	}
	if prog.UseExcept {
		buf.WriteString("func except(a, b []int) []int {\n    m := map[int]bool{}\n    for _, n := range b { m[n]=true }\n    res := []int{}\n    for _, n := range a { if !m[n] { res=append(res,n) } }\n    return res\n}\n\n")
	}
	if prog.UseIntersect {
		buf.WriteString("func intersect(a, b []int) []int {\n    m := map[int]bool{}\n    for _, n := range a { m[n]=true }\n    res := []int{}\n    for _, n := range b { if m[n] { res=append(res,n) } }\n    return res\n}\n\n")
	}
	if prog.UseSubstring {
		buf.WriteString("func substring(s string, i, j int) string {\n    r := []rune(s)\n    return string(r[i:j])\n}\n\n")
	}
	if prog.UsePrint {
		buf.WriteString("func mochiPrint(v any) {\n    switch x := v.(type) {\n    case bool:\n        if x { fmt.Println(1) } else { fmt.Println(\"nil\") }\n    case float64:\n        if math.Trunc(x) == x { fmt.Printf(\"%.1f\\n\", x) } else { fmt.Printf(\"%v\\n\", x) }\n    case []int:\n        for i, n := range x { if i > 0 { fmt.Print(\" \") }; fmt.Print(n) }\n        fmt.Println()\n    default:\n        fmt.Println(v)\n    }\n}\n\n")
	}
	buf.WriteString("func main() {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("}\n")
	out, err := format.Source(buf.Bytes())
	if err == nil {
		return out
	}
	return buf.Bytes()
}

// print converts prog to an ast.Node and prints it.
func print(prog *Program) {
	node := toNodeProg(prog)
	fmt.Print(node.String())
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
	case *VarDecl:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
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
		return &ast.Node{Kind: "int"}
	case *VarRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, toNodeExpr(e))
		}
		return n
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
