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
	UseStrings   bool
	UseIndex     bool
	UseSlice     bool
	UseBoolInt   bool
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
	usesStrings   bool
	usesUnion     bool
	usesUnionAll  bool
	usesExcept    bool
	usesIntersect bool
	usesSubstring bool
	usesPrint     bool
	usesIndex     bool
	usesSlice     bool
	usesBoolInt   bool
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

// IfStmt represents a simple if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if ")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	fmt.Fprint(w, " {\n")
	for _, s := range i.Then {
		fmt.Fprint(w, "    ")
		s.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
	if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, s := range i.Else {
			fmt.Fprint(w, "    ")
			s.emit(w)
			fmt.Fprint(w, "\n")
		}
		fmt.Fprint(w, "}")
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
	usesStrings = false
	usesUnion = false
	usesUnionAll = false
	usesExcept = false
	usesIntersect = false
	usesSubstring = false
	usesPrint = false
	usesIndex = false
	usesSlice = false
	usesBoolInt = false
	gp := &Program{}
	for _, stmt := range p.Statements {
		s, err := compileStmt(stmt, env)
		if err != nil {
			return nil, err
		}
		if s != nil {
			gp.Stmts = append(gp.Stmts, s)
		}
	}
	_ = env // reserved for future use
	gp.UseAvg = usesAvg
	gp.UseSum = usesSum
	gp.UseMin = usesMin
	gp.UseMax = usesMax
	gp.UseContains = usesContains
	gp.UseStrings = usesStrings || usesContains
	gp.UseIndex = usesIndex
	gp.UseSlice = usesSlice
	gp.UseBoolInt = usesBoolInt
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

func compileStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Expr != nil:
		e, err := compileExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		if st.Let.Value != nil {
			e, err := compileExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			return &VarDecl{Name: st.Let.Name, Type: toGoType(st.Let.Type), Value: e}, nil
		}
		return &VarDecl{Name: st.Let.Name, Type: toGoType(st.Let.Type)}, nil
	case st.Var != nil:
		if st.Var.Value != nil {
			e, err := compileExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			return &VarDecl{Name: st.Var.Name, Type: toGoType(st.Var.Type), Value: e}, nil
		}
		return &VarDecl{Name: st.Var.Name, Type: toGoType(st.Var.Type)}, nil
	case st.Assign != nil:
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			e, err := compileExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			return &AssignStmt{Name: st.Assign.Name, Value: e}, nil
		}
		return nil, fmt.Errorf("unsupported statement at %d:%d", st.Pos.Line, st.Pos.Column)
	case st.If != nil:
		return compileIfStmt(st.If, env)
	default:
		if st.Test == nil && st.Import == nil && st.Type == nil {
			return nil, fmt.Errorf("unsupported statement at %d:%d", st.Pos.Line, st.Pos.Column)
		}
	}
	return nil, nil
}

func compileStmts(list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		st, err := compileStmt(s, env)
		if err != nil {
			return nil, err
		}
		if st != nil {
			out = append(out, st)
		}
	}
	return out, nil
}

func compileIfStmt(is *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := compileExpr(is.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := compileStmts(is.Then, env)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		elseStmt, err := compileIfStmt(is.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{elseStmt}
	} else if len(is.Else) > 0 {
		elseStmts, err = compileStmts(is.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
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
					usesStrings = true
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
					if ops[i].Op == "==" || ops[i].Op == "!=" {
						usesBoolInt = true
						newExpr = &CallExpr{Func: "boolInt", Args: []Expr{newExpr}}
					}
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
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := compilePrimary(pf.Target)
	if err != nil {
		// allow selector with tail handled here
		if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) > 0 {
			expr = &VarRef{Name: pf.Target.Selector.Root}
		} else {
			return nil, err
		}
	}
	// handle selector tail as method call
	tail := []string{}
	if pf.Target != nil && pf.Target.Selector != nil {
		tail = pf.Target.Selector.Tail
	}
	// if tail has one element and first op is CallOp => method call
	if len(tail) == 1 && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		method := tail[0]
		args := make([]Expr, len(pf.Ops[0].Call.Args))
		for i, a := range pf.Ops[0].Call.Args {
			ex, err := compileExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		switch method {
		case "contains":
			usesContains = true
			usesStrings = true
			return &CallExpr{Func: "contains", Args: append([]Expr{expr}, args...)}, nil
		default:
			return nil, fmt.Errorf("unsupported method %s", method)
		}
	}
	for _, op := range pf.Ops {
		if op.Index != nil {
			idx := op.Index
			if idx.Colon == nil && idx.Colon2 == nil {
				if idx.Start == nil {
					return nil, fmt.Errorf("unsupported index")
				}
				iex, err := compileExpr(idx.Start)
				if err != nil {
					return nil, err
				}
				usesIndex = true
				expr = &CallExpr{Func: "index", Args: []Expr{expr, iex}}
			} else {
				var start, end Expr
				if idx.Start != nil {
					start, err = compileExpr(idx.Start)
					if err != nil {
						return nil, err
					}
				}
				if idx.End != nil {
					end, err = compileExpr(idx.End)
					if err != nil {
						return nil, err
					}
				}
				usesSlice = true
				expr = &CallExpr{Func: "slice", Args: []Expr{expr, start, end}}
			}
		} else if op.Call != nil {
			return nil, fmt.Errorf("unsupported call")
		} else if op.Field != nil || op.Cast != nil {
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
	if prog.UseStrings {
		buf.WriteString("    \"strings\"\n")
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
		buf.WriteString("func contains(s any, v any) any {\n    switch xs := s.(type) {\n    case []int:\n        n, ok := v.(int)\n        if !ok { return false }\n        for _, m := range xs { if m == n { return true } }\n        return false\n    case string:\n        str, ok := v.(string)\n        if !ok { return 0 }\n        if strings.Contains(xs, str) { return 1 }\n        return 0\n    }\n    return nil\n}\n\n")
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
	if prog.UseIndex {
		buf.WriteString("func index(v any, i int) any {\n    switch x := v.(type) {\n    case []int:\n        return x[i]\n    case string:\n        r := []rune(x)\n        return string(r[i])\n    }\n    return nil\n}\n\n")
	}
	if prog.UseSlice {
		buf.WriteString("func slice(v any, i, j int) any {\n    switch x := v.(type) {\n    case []int:\n        return x[i:j]\n    case string:\n        r := []rune(x)\n        return string(r[i:j])\n    }\n    return nil\n}\n\n")
	}
	if prog.UseBoolInt {
		buf.WriteString("func boolInt(b bool) int {\n    if b { return 1 }\n    return 0\n}\n\n")
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
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{toNodeExpr(st.Cond)}}
		then := &ast.Node{Kind: "then"}
		for _, t := range st.Then {
			then.Children = append(then.Children, toNodeStmt(t))
		}
		n.Children = append(n.Children, then)
		if len(st.Else) > 0 {
			els := &ast.Node{Kind: "else"}
			for _, e := range st.Else {
				els.Children = append(els.Children, toNodeStmt(e))
			}
			n.Children = append(n.Children, els)
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
