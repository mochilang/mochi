//go:build slow

package cstranspiler

import (
	"bytes"
	"fmt"
	"io"

	"mochi/parser"
	"mochi/types"
)

// --- C# AST ---

type Program struct {
	Funcs []*Function
	Stmts []Stmt
}

var stringVars map[string]bool
var mapVars map[string]bool
var varTypes map[string]string
var usesDict bool

type Stmt interface{ emit(io.Writer) }

// LetStmt represents a variable declaration.
type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "var %s = ", s.Name)
	s.Value.emit(w)
}

// VarStmt represents a mutable variable declaration.
type VarStmt struct {
	Name  string
	Value Expr // optional
}

func (s *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "var %s", s.Name)
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	}
}

// AssignStmt represents simple assignment to a variable.
type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", s.Name)
	s.Value.emit(w)
}

// AssignIndexStmt represents assignment to an indexed element like xs[i] = v.
type AssignIndexStmt struct {
	Target Expr
	Index  Expr
	Value  Expr
}

func (s *AssignIndexStmt) emit(w io.Writer) {
	s.Target.emit(w)
	fmt.Fprint(w, "[")
	s.Index.emit(w)
	fmt.Fprint(w, "] = ")
	s.Value.emit(w)
}

// ReturnStmt represents a return statement within a function.
type ReturnStmt struct {
	Value Expr // optional
}

func (r *ReturnStmt) emit(w io.Writer) {
	fmt.Fprint(w, "return")
	if r.Value != nil {
		fmt.Fprint(w, " ")
		r.Value.emit(w)
	}
}

type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { fmt.Fprint(w, "break") }

type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { fmt.Fprint(w, "continue") }

// ForRangeStmt represents a numeric range for-loop like `for i in 0..10 {}`.
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for (var %s = ", f.Var)
	if f.Start != nil {
		f.Start.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "; ")
	fmt.Fprintf(w, "%s < ", f.Var)
	if f.End != nil {
		f.End.emit(w)
	}
	fmt.Fprint(w, "; ")
	fmt.Fprintf(w, "%s++) {\n", f.Var)
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}")
}

// ForInStmt represents iteration over elements of an iterable.
type ForInStmt struct {
	Var      string
	Iterable Expr
	Body     []Stmt
}

func (f *ForInStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "foreach (var %s in ", f.Var)
	if isMapExpr(f.Iterable) {
		f.Iterable.emit(w)
		fmt.Fprint(w, ".Keys")
	} else {
		f.Iterable.emit(w)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}")
}

// Function represents a simple function declaration.
type Function struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *Function) emit(w io.Writer) {
	fmt.Fprintf(w, "static int %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "int %s", p)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}")
}

// WhileStmt represents a while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "while (")
	if ws.Cond != nil {
		ws.Cond.emit(w)
		fmt.Fprint(w, " != 0")
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range ws.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}")
}

// IfStmt represents a conditional statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
		fmt.Fprint(w, " != 0")
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range i.Then {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}")
	if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, st := range i.Else {
			fmt.Fprint(w, "    ")
			st.emit(w)
			fmt.Fprint(w, ";\n")
		}
		fmt.Fprint(w, "}")
	}
}

// IfExpr is a ternary conditional expression.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (ie *IfExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	ie.Cond.emit(w)
	fmt.Fprint(w, " != 0 ? ")
	ie.Then.emit(w)
	fmt.Fprint(w, " : ")
	ie.Else.emit(w)
	fmt.Fprint(w, ")")
}

type Expr interface{ emit(io.Writer) }

// VarRef references a variable by name.
type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { fmt.Fprint(w, v.Name) }

// BinaryExpr represents a binary operation like addition or comparison.
type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
	fmt.Fprint(w, ")")
}

// BoolOpExpr represents boolean && and || operations with integer semantics.
type BoolOpExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (b *BoolOpExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	fmt.Fprint(w, "(")
	b.Left.emit(w)
	fmt.Fprint(w, " != 0")
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
	fmt.Fprint(w, " != 0")
	fmt.Fprint(w, ") ? 1 : 0)")
}

// UnaryExpr represents a unary prefix operation.
type UnaryExpr struct {
	Op  string
	Val Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	u.Val.emit(w)
}

type NotExpr struct{ Val Expr }

func (n *NotExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	n.Val.emit(w)
	fmt.Fprint(w, " == 0 ? 1 : 0)")
}

// CmpExpr emits comparison result as 1 or 0.
type CmpExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (c *CmpExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	if (c.Op == "<" || c.Op == "<=" || c.Op == ">" || c.Op == ">=") &&
		(isStringExpr(c.Left) || isStringExpr(c.Right)) {
		fmt.Fprint(w, "string.Compare(")
		c.Left.emit(w)
		fmt.Fprint(w, ", ")
		c.Right.emit(w)
		fmt.Fprint(w, ")")
		switch c.Op {
		case "<":
			fmt.Fprint(w, " < 0")
		case "<=":
			fmt.Fprint(w, " <= 0")
		case ">":
			fmt.Fprint(w, " > 0")
		case ">=":
			fmt.Fprint(w, " >= 0")
		}
		fmt.Fprint(w, " ? 1 : 0)")
		return
	}
	c.Left.emit(w)
	fmt.Fprintf(w, " %s ", c.Op)
	c.Right.emit(w)
	fmt.Fprint(w, " ? 1 : 0)")
}

type InExpr struct {
	Item       Expr
	Collection Expr
}

func (ie *InExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	if isStringExpr(ie.Collection) {
		ie.Collection.emit(w)
		fmt.Fprint(w, ".Contains(")
		ie.Item.emit(w)
		fmt.Fprint(w, ") ? 1 : 0")
	} else if isMapExpr(ie.Collection) {
		ie.Collection.emit(w)
		fmt.Fprint(w, ".ContainsKey(")
		ie.Item.emit(w)
		fmt.Fprint(w, ") ? 1 : 0")
	} else {
		fmt.Fprint(w, "Array.IndexOf(")
		ie.Collection.emit(w)
		fmt.Fprint(w, ", ")
		ie.Item.emit(w)
		fmt.Fprint(w, ") >= 0 ? 1 : 0")
	}
	fmt.Fprint(w, ")")
}

// ExprStmt represents a statement consisting solely of an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

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

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		fmt.Fprint(w, "1")
	} else {
		fmt.Fprint(w, "0")
	}
}

// IndexExpr represents xs[i].
type IndexExpr struct {
	Target Expr
	Index  Expr
}

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (f *FieldExpr) emit(w io.Writer) {
	f.Target.emit(w)
	fmt.Fprintf(w, ".%s", f.Name)
}

// SliceExpr represents xs[a:b] or s[a:b] for lists and strings.
type SliceExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SliceExpr) emit(w io.Writer) {
	if isStringExpr(s.Value) {
		s.Value.emit(w)
		fmt.Fprint(w, ".Substring(")
		s.Start.emit(w)
		fmt.Fprint(w, ", ")
		fmt.Fprint(w, "(")
		s.End.emit(w)
		fmt.Fprint(w, " - ")
		s.Start.emit(w)
		fmt.Fprint(w, "))")
	} else {
		s.Value.emit(w)
		fmt.Fprint(w, ".Skip(")
		s.Start.emit(w)
		fmt.Fprint(w, ").Take(")
		fmt.Fprint(w, "(")
		s.End.emit(w)
		fmt.Fprint(w, " - ")
		s.Start.emit(w)
		fmt.Fprint(w, ")).ToArray()")
	}
}

// ContainsExpr represents s.contains(x)
type ContainsExpr struct {
	Str Expr
	Sub Expr
}

func (c *ContainsExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	c.Str.emit(w)
	fmt.Fprint(w, ".Contains(")
	c.Sub.emit(w)
	fmt.Fprint(w, ") ? 1 : 0)")
}

func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *StrExpr:
		return true
	case *SliceExpr:
		return isStringExpr(ex.Value)
	case *SubstringExpr:
		return true
	case *VarRef:
		return stringVars[ex.Name]
	case *CallExpr:
		if ex.Func == "Convert.ToString" {
			return true
		}
	}
	return false
}

func isMapExpr(e Expr) bool {
	switch ex := e.(type) {
	case *MapLit:
		return true
	case *VarRef:
		return mapVars[ex.Name]
	}
	return false
}

func typeOfExpr(e Expr) string {
	switch ex := e.(type) {
	case *StringLit:
		return "string"
	case *IntLit:
		return "int"
	case *BoolLit:
		return "int"
	case *MapLit:
		k, v := mapTypes(ex)
		return fmt.Sprintf("Dictionary<%s, %s>", k, v)
	case *VarRef:
		if t, ok := varTypes[ex.Name]; ok {
			return t
		}
	}
	return ""
}

func mapTypes(m *MapLit) (string, string) {
	keyType := ""
	valType := ""
	for i, it := range m.Items {
		kt := typeOfExpr(it.Key)
		vt := typeOfExpr(it.Value)
		if i == 0 {
			keyType = kt
			valType = vt
		} else {
			if keyType != kt {
				keyType = ""
			}
			if valType != vt {
				valType = ""
			}
		}
	}
	if keyType == "" {
		keyType = "object"
	}
	if valType == "" {
		valType = "object"
	}
	return keyType, valType
}

func (ix *IndexExpr) emit(w io.Writer) {
	ix.Target.emit(w)
	fmt.Fprint(w, "[")
	ix.Index.emit(w)
	fmt.Fprint(w, "]")
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	fmt.Fprint(w, "new[]{")
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "}")
}

type MapItem struct {
	Key   Expr
	Value Expr
}

type MapLit struct{ Items []MapItem }

func (m *MapLit) emit(w io.Writer) {
	k, v := mapTypes(m)
	if k != "object" || v != "object" {
		usesDict = true
	}
	fmt.Fprintf(w, "new Dictionary<%s, %s>{", k, v)
	for i, it := range m.Items {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprint(w, "{")
		it.Key.emit(w)
		fmt.Fprint(w, ", ")
		it.Value.emit(w)
		fmt.Fprint(w, "}")
	}
	fmt.Fprint(w, "}")
}

type CountExpr struct{ Arg Expr }

func (c *CountExpr) emit(w io.Writer) {
	c.Arg.emit(w)
	if isMapExpr(c.Arg) {
		fmt.Fprint(w, ".Count")
	} else {
		fmt.Fprint(w, ".Length")
	}
}

type AvgExpr struct{ Arg Expr }

func (a *AvgExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	a.Arg.emit(w)
	fmt.Fprint(w, ".Average()).ToString(\"0.0\")")
}

type LenExpr struct{ Arg Expr }

func (l *LenExpr) emit(w io.Writer) {
	l.Arg.emit(w)
	if isMapExpr(l.Arg) {
		fmt.Fprint(w, ".Count")
	} else {
		fmt.Fprint(w, ".Length")
	}
}

type SumExpr struct{ Arg Expr }

func (s *SumExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	s.Arg.emit(w)
	fmt.Fprint(w, ".Sum())")
}

type AppendExpr struct {
	List Expr
	Item Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	a.List.emit(w)
	fmt.Fprint(w, ".Append(")
	a.Item.emit(w)
	fmt.Fprint(w, ").ToList()")
}

type StrExpr struct{ Arg Expr }

func (s *StrExpr) emit(w io.Writer) {
	fmt.Fprint(w, "Convert.ToString(")
	s.Arg.emit(w)
	fmt.Fprint(w, ")")
}

type MinExpr struct{ Arg Expr }

func (m *MinExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	m.Arg.emit(w)
	fmt.Fprint(w, ".Min())")
}

type MaxExpr struct{ Arg Expr }

func (m *MaxExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	m.Arg.emit(w)
	fmt.Fprint(w, ".Max())")
}

type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Str.emit(w)
	fmt.Fprint(w, ".Substring(")
	s.Start.emit(w)
	fmt.Fprint(w, ", (")
	s.End.emit(w)
	fmt.Fprint(w, " - ")
	s.Start.emit(w)
	fmt.Fprint(w, "))")
}

type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) {
	v.Map.emit(w)
	fmt.Fprint(w, ".Values.ToList()")
}

// Transpile converts a Mochi AST to a simple C# AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	prog := &Program{}
	stringVars = make(map[string]bool)
	mapVars = make(map[string]bool)
	varTypes = make(map[string]string)
	usesDict = false
	for _, st := range p.Statements {
		s, err := compileStmt(prog, st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			prog.Stmts = append(prog.Stmts, s)
		}
	}
	_ = env // env reserved for future use
	return prog, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	operands := []Expr{}
	ops := []string{}

	first, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	for _, p := range e.Binary.Right {
		r, err := compilePostfix(p.Right)
		if err != nil {
			return nil, err
		}
		op := p.Op
		if p.All {
			op = op + "_all"
		}
		ops = append(ops, op)
		operands = append(operands, r)
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

	apply := func(left Expr, op string, right Expr) Expr {
		switch op {
		case "==", "!=", "<", "<=", ">", ">=":
			return &CmpExpr{Op: op, Left: left, Right: right}
		case "&&", "||":
			return &BoolOpExpr{Op: op, Left: left, Right: right}
		case "in":
			return &InExpr{Item: left, Collection: right}
		default:
			return &BinaryExpr{Op: op, Left: left, Right: right}
		}
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range level {
				if ops[i] == t {
					expr := apply(operands[i], ops[i], operands[i+1])
					operands[i] = expr
					operands = append(operands[:i+1], operands[i+2:]...)
					ops = append(ops[:i], ops[i+1:]...)
					matched = true
					break
				}
			}
			if !matched {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("expression reduction failed")
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
		if u.Ops[i] == "!" {
			expr = &NotExpr{Val: expr}
		} else {
			expr = &UnaryExpr{Op: u.Ops[i], Val: expr}
		}
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
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := compileExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			if op.Index.Start == nil || op.Index.End == nil {
				return nil, fmt.Errorf("unsupported slice")
			}
			start, err := compileExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			end, err := compileExpr(op.Index.End)
			if err != nil {
				return nil, err
			}
			expr = &SliceExpr{Value: expr, Start: start, End: end}
		case op.Field != nil:
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
		case op.Call != nil:
			if fe, ok := expr.(*FieldExpr); ok && fe.Name == "contains" {
				if len(op.Call.Args) != 1 {
					return nil, fmt.Errorf("unsupported method call")
				}
				arg, err := compileExpr(op.Call.Args[0])
				if err != nil {
					return nil, err
				}
				expr = &ContainsExpr{Str: fe.Target, Sub: arg}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			switch *op.Cast.Type.Simple {
			case "int":
				expr = &CallExpr{Func: "Convert.ToInt32", Args: []Expr{expr}}
			default:
				return nil, fmt.Errorf("unsupported cast")
			}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func compileStmt(prog *Program, s *parser.Statement) (Stmt, error) {
	switch {
	case s.Expr != nil:
		e, err := compileExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case s.Let != nil:
		var val Expr
		var err error
		if s.Let.Value != nil {
			val, err = compileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if s.Let.Type != nil && s.Let.Type.Simple != nil {
			switch *s.Let.Type.Simple {
			case "int":
				val = &IntLit{Value: 0}
			case "string":
				val = &StringLit{Value: ""}
				stringVars[s.Let.Name] = true
			case "bool":
				val = &BoolLit{Value: false}
			default:
				return nil, fmt.Errorf("unsupported let type")
			}
		} else {
			return nil, fmt.Errorf("unsupported let")
		}
		if isStringExpr(val) {
			stringVars[s.Let.Name] = true
		}
		if isMapExpr(val) {
			mapVars[s.Let.Name] = true
		}
		if t := typeOfExpr(val); t != "" {
			varTypes[s.Let.Name] = t
		}
		return &LetStmt{Name: s.Let.Name, Value: val}, nil
	case s.Var != nil:
		var val Expr
		var err error
		if s.Var.Value != nil {
			val, err = compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if s.Var.Type != nil && s.Var.Type.Simple != nil {
			switch *s.Var.Type.Simple {
			case "int":
				val = &IntLit{Value: 0}
			case "string":
				val = &StringLit{Value: ""}
				stringVars[s.Var.Name] = true
			case "bool":
				val = &BoolLit{Value: false}
			default:
				return nil, fmt.Errorf("unsupported var type")
			}
		}
		if isStringExpr(val) {
			stringVars[s.Var.Name] = true
		}
		if isMapExpr(val) {
			mapVars[s.Var.Name] = true
		}
		if t := typeOfExpr(val); t != "" {
			varTypes[s.Var.Name] = t
		}
		return &VarStmt{Name: s.Var.Name, Value: val}, nil
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if isStringExpr(val) {
				stringVars[s.Assign.Name] = true
			}
			if isMapExpr(val) {
				mapVars[s.Assign.Name] = true
			}
			if t := typeOfExpr(val); t != "" {
				varTypes[s.Assign.Name] = t
			}
			return &AssignStmt{Name: s.Assign.Name, Value: val}, nil
		}
		if len(s.Assign.Index) > 0 && len(s.Assign.Field) == 0 {
			var target Expr = &VarRef{Name: s.Assign.Name}
			for i := 0; i < len(s.Assign.Index)-1; i++ {
				idx, err := compileExpr(s.Assign.Index[i].Start)
				if err != nil {
					return nil, err
				}
				target = &IndexExpr{Target: target, Index: idx}
			}
			idx, err := compileExpr(s.Assign.Index[len(s.Assign.Index)-1].Start)
			if err != nil {
				return nil, err
			}
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			return &AssignIndexStmt{Target: target, Index: idx, Value: val}, nil
		}
		return nil, fmt.Errorf("unsupported assignment")
	case s.Fun != nil:
		params := make([]string, len(s.Fun.Params))
		for i, p := range s.Fun.Params {
			params[i] = p.Name
		}
		var body []Stmt
		for _, b := range s.Fun.Body {
			st, err := compileStmt(prog, b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		prog.Funcs = append(prog.Funcs, &Function{Name: s.Fun.Name, Params: params, Body: body})
		return nil, nil
	case s.Return != nil:
		var val Expr
		if s.Return.Value != nil {
			var err error
			val, err = compileExpr(s.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: val}, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.For != nil:
		if s.For.RangeEnd != nil {
			start, err := compileExpr(s.For.Source)
			if err != nil {
				return nil, err
			}
			end, err := compileExpr(s.For.RangeEnd)
			if err != nil {
				return nil, err
			}
			var body []Stmt
			for _, b := range s.For.Body {
				st, err := compileStmt(prog, b)
				if err != nil {
					return nil, err
				}
				if st != nil {
					body = append(body, st)
				}
			}
			return &ForRangeStmt{Var: s.For.Name, Start: start, End: end, Body: body}, nil
		}
		iterable, err := compileExpr(s.For.Source)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.For.Body {
			st, err := compileStmt(prog, b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &ForInStmt{Var: s.For.Name, Iterable: iterable, Body: body}, nil
	case s.While != nil:
		cond, err := compileExpr(s.While.Cond)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.While.Body {
			st, err := compileStmt(prog, b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case s.If != nil:
		return compileIfStmt(prog, s.If)
	case s.Test == nil && s.Import == nil && s.Type == nil:
		return nil, fmt.Errorf("unsupported statement at %d:%d", s.Pos.Line, s.Pos.Column)
	}
	return nil, nil
}

func compileIfStmt(prog *Program, i *parser.IfStmt) (Stmt, error) {
	cond, err := compileExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	var thenStmts []Stmt
	for _, st := range i.Then {
		s, err := compileStmt(prog, st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			thenStmts = append(thenStmts, s)
		}
	}
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := compileIfStmt(prog, i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		for _, st := range i.Else {
			s, err := compileStmt(prog, st)
			if err != nil {
				return nil, err
			}
			if s != nil {
				elseStmts = append(elseStmts, s)
			}
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
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
			name = "Console.WriteLine"
			return &CallExpr{Func: name, Args: args}, nil
		case "append":
			if len(args) == 2 {
				return &AppendExpr{List: args[0], Item: args[1]}, nil
			}
		case "count":
			if len(args) == 1 {
				return &CountExpr{Arg: args[0]}, nil
			}
		case "avg":
			if len(args) == 1 {
				return &AvgExpr{Arg: args[0]}, nil
			}
		case "len":
			if len(args) == 1 {
				return &LenExpr{Arg: args[0]}, nil
			}
		case "sum":
			if len(args) == 1 {
				return &SumExpr{Arg: args[0]}, nil
			}
		case "str":
			if len(args) == 1 {
				return &StrExpr{Arg: args[0]}, nil
			}
		case "min":
			if len(args) == 1 {
				return &MinExpr{Arg: args[0]}, nil
			}
		case "max":
			if len(args) == 1 {
				return &MaxExpr{Arg: args[0]}, nil
			}
		case "substring":
			if len(args) == 3 {
				return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
			}
		case "values":
			if len(args) == 1 {
				usesDict = true
				return &ValuesExpr{Map: args[0]}, nil
			}
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
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
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := compileExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := compileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		usesDict = true
		return &MapLit{Items: items}, nil
	case p.Selector != nil:
		expr := Expr(&VarRef{Name: p.Selector.Root})
		for _, t := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: t}
		}
		return expr, nil
	case p.Group != nil:
		return compileExpr(p.Group)
	case p.If != nil:
		return compileIfExpr(p.If)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileIfExpr(i *parser.IfExpr) (Expr, error) {
	cond, err := compileExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(i.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if i.ElseIf != nil {
		elseExpr, err = compileIfExpr(i.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if i.Else != nil {
		elseExpr, err = compileExpr(i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

// Emit generates formatted C# source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("using System;\n")
	if usesDict {
		buf.WriteString("using System.Collections.Generic;\n")
	}
	if needsLinq(prog) {
		buf.WriteString("using System.Linq;\n")
	}
	buf.WriteString("\n")
	buf.WriteString("class Program {\n")
	for _, fn := range prog.Funcs {
		buf.WriteString("\t")
		fn.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("\tstatic void Main() {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("\t\t")
		s.emit(&buf)
		buf.WriteString(";\n")
	}
	buf.WriteString("\t}\n")
	buf.WriteString("}\n")
	return formatCS(buf.Bytes())
}

// formatCS performs very basic formatting and prepends a standard header.
func formatCS(src []byte) []byte {
	header := "// Code generated by Mochi\n"
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return append([]byte(header), src...)
}

func needsLinq(p *Program) bool {
	var found bool
	for _, s := range p.Stmts {
		found = found || inspectLinq(s)
	}
	return found
}

func inspectLinq(e Expr) bool {
	switch ex := e.(type) {
	case *ExprStmt:
		return inspectLinq(ex.Expr)
	case *AvgExpr:
		return true
	case *SumExpr:
		return true
	case *IfExpr:
		return inspectLinq(ex.Cond) || inspectLinq(ex.Then) || inspectLinq(ex.Else)
	case *IfStmt:
		if inspectLinq(ex.Cond) {
			return true
		}
		for _, s := range ex.Then {
			if inspectLinq(s) {
				return true
			}
		}
		for _, s := range ex.Else {
			if inspectLinq(s) {
				return true
			}
		}
		return false
	case *WhileStmt:
		if inspectLinq(ex.Cond) {
			return true
		}
		for _, s := range ex.Body {
			if inspectLinq(s) {
				return true
			}
		}
		return false
	case *ForRangeStmt:
		if inspectLinq(ex.Start) || inspectLinq(ex.End) {
			return true
		}
		for _, s := range ex.Body {
			if inspectLinq(s) {
				return true
			}
		}
		return false
	case *ForInStmt:
		if inspectLinq(ex.Iterable) {
			return true
		}
		for _, s := range ex.Body {
			if inspectLinq(s) {
				return true
			}
		}
		return false
	case *CallExpr:
		for _, a := range ex.Args {
			if inspectLinq(a) {
				return true
			}
		}
	case *ListLit:
		for _, el := range ex.Elems {
			if inspectLinq(el) {
				return true
			}
		}
	case *CountExpr:
		return inspectLinq(ex.Arg)
	case *LenExpr:
		return inspectLinq(ex.Arg)
	case *AppendExpr:
		return true
	case *MinExpr:
		return true
	case *MaxExpr:
		return true
	case *ValuesExpr:
		return true
	case *SliceExpr:
		return true
	case *ContainsExpr:
		return true
	case *IndexExpr:
		return inspectLinq(ex.Target) || inspectLinq(ex.Index)
	case *UnaryExpr:
		return inspectLinq(ex.Val)
	case *BinaryExpr:
		return inspectLinq(ex.Left) || inspectLinq(ex.Right)
	}
	return false
}
