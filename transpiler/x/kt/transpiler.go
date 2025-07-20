//go:build slow

package kt

import (
	"bytes"
	"fmt"
	"io"

	"mochi/parser"
	"mochi/types"
)

// Program represents a simple Kotlin program consisting of statements executed in main.
// Program contains top level functions and statements executed in `main`.
type Program struct {
	Funcs []*FuncDef
	Stmts []Stmt
}

func indent(w io.Writer, n int) {
	for i := 0; i < n; i++ {
		io.WriteString(w, "    ")
	}
}

type Stmt interface{ emit(io.Writer, int) }

// IndexAssignStmt assigns to a[i] or m[key].
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *IndexAssignStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	s.Target.emit(w)
	io.WriteString(w, " = ")
	s.Value.emit(w)
}

// ReturnStmt is a return statement inside a function.
type ReturnStmt struct{ Value Expr }

func (s *ReturnStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "return")
	if s.Value != nil {
		io.WriteString(w, " ")
		s.Value.emit(w)
	}
}

// FuncDef represents a top level function definition.
type FuncDef struct {
	Name   string
	Params []string
	Ret    string
	Body   []Stmt
}

func (f *FuncDef) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "fun "+f.Name+"(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	ret := f.Ret
	if ret == "" {
		ret = "Any"
	}
	io.WriteString(w, "): "+ret+" {\n")
	for _, s := range f.Body {
		s.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}\n")
}

type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement that evaluates an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	s.Expr.emit(w)
}

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

// IndexExpr represents a[i].
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (ix *IndexExpr) emit(w io.Writer) {
	ix.Target.emit(w)
	io.WriteString(w, "[")
	ix.Index.emit(w)
	io.WriteString(w, "]")
}

// NonNullExpr appends `!!` to force unwrap a nullable value.
type NonNullExpr struct{ Value Expr }

func (n *NonNullExpr) emit(w io.Writer) {
	n.Value.emit(w)
	io.WriteString(w, "!!")
}

// MapLit represents a Kotlin map literal.
type MapLit struct{ Items []MapItem }

type MapItem struct {
	Key   Expr
	Value Expr
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "mutableMapOf(")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		it.Key.emit(w)
		io.WriteString(w, " to ")
		it.Value.emit(w)
	}
	io.WriteString(w, ")")
}

// ContainsExpr represents s.contains(sub).
type ContainsExpr struct {
	Str Expr
	Sub Expr
}

func (c *ContainsExpr) emit(w io.Writer) {
	c.Str.emit(w)
	io.WriteString(w, ".contains(")
	c.Sub.emit(w)
	io.WriteString(w, ")")
}

type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { io.WriteString(w, v.Name) }

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Receiver Expr
	Name     string
}

func (f *FieldExpr) emit(w io.Writer) {
	f.Receiver.emit(w)
	io.WriteString(w, "."+f.Name)
}

// CastExpr represents value as type conversions like "\"123\" as int".
type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(w io.Writer) {
	c.Value.emit(w)
	switch c.Type {
	case "int":
		io.WriteString(w, ".toInt()")
	case "float":
		io.WriteString(w, ".toDouble()")
	case "string":
		io.WriteString(w, ".toString()")
	}
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	b.Left.emit(w)
	io.WriteString(w, " "+b.Op+" ")
	b.Right.emit(w)
}

type LetStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer, indentLevel int) { // 'let' is immutable
	indent(w, indentLevel)
	io.WriteString(w, "val "+s.Name)
	if s.Type != "" {
		io.WriteString(w, ": "+s.Type)
	}
	io.WriteString(w, " = ")
	s.Value.emit(w)
}

type VarStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "var "+s.Name)
	if s.Type != "" {
		io.WriteString(w, ": "+s.Type)
	}
	io.WriteString(w, " = ")
	s.Value.emit(w)
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, s.Name+" = ")
	s.Value.emit(w)
}

// IfStmt represents a simple if/else conditional.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range i.Then {
		st.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}")
	if len(i.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, st := range i.Else {
			st.emit(w, indentLevel+1)
			io.WriteString(w, "\n")
		}
		indent(w, indentLevel)
		io.WriteString(w, "}")
	}
}

// WhileStmt represents a basic while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "while (")
	if ws.Cond != nil {
		ws.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range ws.Body {
		st.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}")
}

// ForRangeStmt represents a numeric for-loop like `for i in a..b {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "for ("+fr.Name+" in ")
	if fr.Start != nil {
		fr.Start.emit(w)
	}
	io.WriteString(w, " until ")
	if fr.End != nil {
		fr.End.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range fr.Body {
		st.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}")
}

// ForEachStmt iterates over a collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

func (fe *ForEachStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "for ("+fe.Name+" in ")
	if fe.Iterable != nil {
		fe.Iterable.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range fe.Body {
		st.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}")
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "break")
}

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "continue")
}

// IfExpr is a conditional expression using Kotlin's `if`.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (ie *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if (")
	ie.Cond.emit(w)
	io.WriteString(w, ") ")
	ie.Then.emit(w)
	if ie.Else != nil {
		io.WriteString(w, " else ")
		ie.Else.emit(w)
	}
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "mutableListOf(")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, ")")
}

// SliceExpr represents s[i:j] for strings and lists.
type SliceExpr struct {
	Value    Expr
	Start    Expr
	End      Expr
	IsString bool
}

func (s *SliceExpr) emit(w io.Writer) {
	s.Value.emit(w)
	if s.IsString {
		io.WriteString(w, ".substring(")
	} else {
		io.WriteString(w, ".subList(")
	}
	if s.Start != nil {
		s.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, ", ")
	if s.End != nil {
		s.End.emit(w)
	} else {
		s.Value.emit(w)
		if s.IsString {
			io.WriteString(w, ".length")
		} else {
			io.WriteString(w, ".size")
		}
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
	io.WriteString(w, " + ")
	a.Elem.emit(w)
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

type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) {
	v.Map.emit(w)
	io.WriteString(w, ".values")
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

func kotlinType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "bool":
			return "Boolean"
		case "string":
			return "String"
		}
		return "Any"
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			elem := kotlinType(t.Generic.Args[0])
			if elem == "" {
				elem = "Any"
			}
			return "MutableList<" + elem + ">"
		case "map":
			k := kotlinType(t.Generic.Args[0])
			v := kotlinType(t.Generic.Args[1])
			if k == "" {
				k = "Any"
			}
			if v == "" {
				v = "Any"
			}
			return "MutableMap<" + k + ", " + v + ">"
		}
	}
	return "Any"
}

func kotlinTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, *types.IntType, types.Int64Type, *types.Int64Type:
		return "Int"
	case types.BoolType, *types.BoolType:
		return "Boolean"
	case types.StringType, *types.StringType:
		return "String"
	case types.FloatType, *types.FloatType, types.BigRatType, *types.BigRatType:
		return "Double"
	case types.ListType:
		elem := kotlinTypeFromType(tt.Elem)
		if elem == "" {
			elem = "Any"
		}
		return "MutableList<" + elem + ">"
	case types.MapType:
		k := kotlinTypeFromType(tt.Key)
		v := kotlinTypeFromType(tt.Value)
		if k == "" {
			k = "Any"
		}
		if v == "" {
			v = "Any"
		}
		return "MutableMap<" + k + ", " + v + ">"
	}
	return ""
}

func guessType(e Expr) string {
	switch v := e.(type) {
	case *IntLit:
		return "Int"
	case *BoolLit:
		return "Boolean"
	case *StringLit:
		return "String"
	case *ListLit:
		if len(v.Elems) > 0 {
			elem := guessType(v.Elems[0])
			if elem == "" {
				elem = "Any"
			}
			return "MutableList<" + elem + ">"
		}
		return "MutableList<Any>"
	case *MapLit:
		if len(v.Items) > 0 {
			k := guessType(v.Items[0].Key)
			if k == "" {
				k = "Any"
			}
			val := guessType(v.Items[0].Value)
			if val == "" {
				val = "Any"
			}
			return "MutableMap<" + k + ", " + val + ">"
		}
		return "MutableMap<Any, Any>"
	}
	return ""
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
			typ := kotlinType(st.Let.Type)
			if typ == "" {
				if t0, err := env.GetVar(st.Let.Name); err == nil {
					typ = kotlinTypeFromType(t0)
				} else {
					typ = guessType(val)
				}
			}
			p.Stmts = append(p.Stmts, &LetStmt{Name: st.Let.Name, Type: typ, Value: val})
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
			typ := kotlinType(st.Var.Type)
			if typ == "" {
				if t0, err := env.GetVar(st.Var.Name); err == nil {
					typ = kotlinTypeFromType(t0)
				} else {
					typ = guessType(val)
				}
			}
			p.Stmts = append(p.Stmts, &VarStmt{Name: st.Var.Name, Type: typ, Value: val})
		case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
			e, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &AssignStmt{Name: st.Assign.Name, Value: e})
		case st.Assign != nil && len(st.Assign.Index) > 0 && len(st.Assign.Field) == 0:
			target, err := buildIndexTarget(env, st.Assign.Name, st.Assign.Index)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &IndexAssignStmt{Target: target, Value: v})
		case st.Return != nil:
			var val Expr
			if st.Return.Value != nil {
				var err error
				val, err = convertExpr(env, st.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			p.Stmts = append(p.Stmts, &ReturnStmt{Value: val})
		case st.Fun != nil:
			body, err := convertStmts(env, st.Fun.Body)
			if err != nil {
				return nil, err
			}
			var params []string
			for _, p0 := range st.Fun.Params {
				typ := kotlinType(p0.Type)
				if typ == "" {
					typ = "Any"
				}
				params = append(params, fmt.Sprintf("%s: %s", p0.Name, typ))
			}
			ret := kotlinType(st.Fun.Return)
			if ret == "" {
				if t, ok := env.Types()[st.Fun.Name]; ok {
					if ft, ok := t.(types.FuncType); ok {
						ret = kotlinTypeFromType(ft.Return)
					}
				}
			}
			p.Funcs = append(p.Funcs, &FuncDef{Name: st.Fun.Name, Params: params, Ret: ret, Body: body})
		case st.If != nil:
			stmt, err := convertIfStmt(env, st.If)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
		case st.While != nil:
			stmt, err := convertWhileStmt(env, st.While)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
		case st.For != nil:
			stmt, err := convertForStmt(env, st.For)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
		case st.Break != nil:
			p.Stmts = append(p.Stmts, &BreakStmt{})
		case st.Continue != nil:
			p.Stmts = append(p.Stmts, &ContinueStmt{})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return p, nil
}

func convertStmts(env *types.Env, list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		switch {
		case s.Expr != nil:
			e, err := convertExpr(env, s.Expr.Expr)
			if err != nil {
				return nil, err
			}
			out = append(out, &ExprStmt{Expr: e})
		case s.Let != nil:
			v, err := convertExpr(env, s.Let.Value)
			if err != nil {
				return nil, err
			}
			typ := kotlinType(s.Let.Type)
			if typ == "" {
				if t0, err := env.GetVar(s.Let.Name); err == nil {
					typ = kotlinTypeFromType(t0)
				}
			}
			out = append(out, &LetStmt{Name: s.Let.Name, Type: typ, Value: v})
		case s.Var != nil:
			v, err := convertExpr(env, s.Var.Value)
			if err != nil {
				return nil, err
			}
			typ := kotlinType(s.Var.Type)
			if typ == "" {
				if t0, err := env.GetVar(s.Var.Name); err == nil {
					typ = kotlinTypeFromType(t0)
				}
			}
			out = append(out, &VarStmt{Name: s.Var.Name, Type: typ, Value: v})
		case s.Assign != nil && len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0:
			v, err := convertExpr(env, s.Assign.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &AssignStmt{Name: s.Assign.Name, Value: v})
		case s.Assign != nil && len(s.Assign.Index) > 0 && len(s.Assign.Field) == 0:
			target, err := buildIndexTarget(env, s.Assign.Name, s.Assign.Index)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(env, s.Assign.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &IndexAssignStmt{Target: target, Value: v})
		case s.Return != nil:
			var v Expr
			if s.Return.Value != nil {
				var err error
				v, err = convertExpr(env, s.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			out = append(out, &ReturnStmt{Value: v})
		case s.If != nil:
			st, err := convertIfStmt(env, s.If)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.While != nil:
			st, err := convertWhileStmt(env, s.While)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.For != nil:
			st, err := convertForStmt(env, s.For)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.Break != nil:
			out = append(out, &BreakStmt{})
		case s.Continue != nil:
			out = append(out, &ContinueStmt{})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
}

func convertIfStmt(env *types.Env, is *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(env, is.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmts(env, is.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		stmt, err := convertIfStmt(env, is.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{stmt}
	} else if len(is.Else) > 0 {
		elseStmts, err = convertStmts(env, is.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(env *types.Env, ws *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(env, ws.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmts(env, ws.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(env *types.Env, fs *parser.ForStmt) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := convertExpr(env, fs.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(env, fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		body, err := convertStmts(env, fs.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(env, fs.Source)
	if err != nil {
		return nil, err
	}
	if types.IsMapExpr(fs.Source, env) {
		iter = &FieldExpr{Receiver: iter, Name: "keys"}
	}
	body, err := convertStmts(env, fs.Body)
	if err != nil {
		return nil, err
	}
	return &ForEachStmt{Name: fs.Name, Iterable: iter, Body: body}, nil
}

func buildIndexTarget(env *types.Env, name string, idx []*parser.IndexOp) (Expr, error) {
	var target Expr = &VarRef{Name: name}
	for j, op := range idx {
		if op.Colon != nil || op.Colon2 != nil {
			return nil, fmt.Errorf("slice assign unsupported")
		}
		idxExpr, err := convertExpr(env, op.Start)
		if err != nil {
			return nil, err
		}
		target = &IndexExpr{Target: target, Index: idxExpr}
		if j < len(idx)-1 {
			target = &NonNullExpr{Value: target}
		}
	}
	return target, nil
}

func convertIfExpr(env *types.Env, ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(env, ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(env, ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = convertIfExpr(env, ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(env, ie.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
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
	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}}
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
	if p == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 1 && p.Target.Selector.Tail[0] == "contains" && len(p.Ops) == 1 && p.Ops[0].Call != nil {
		if len(p.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("contains expects 1 arg")
		}
		base, err := convertPrimary(env, &parser.Primary{Selector: &parser.SelectorExpr{Root: p.Target.Selector.Root}})
		if err != nil {
			return nil, err
		}
		arg, err := convertExpr(env, p.Ops[0].Call.Args[0])
		if err != nil {
			return nil, err
		}
		return &ContainsExpr{Str: base, Sub: arg}, nil
	}
	expr, err := convertPrimary(env, p.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(env, op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
			if i+1 < len(p.Ops) {
				expr = &NonNullExpr{Value: expr}
			}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			var startExpr Expr
			if op.Index.Start != nil {
				startExpr, err = convertExpr(env, op.Index.Start)
				if err != nil {
					return nil, err
				}
			}
			var endExpr Expr
			if op.Index.End != nil {
				endExpr, err = convertExpr(env, op.Index.End)
				if err != nil {
					return nil, err
				}
			}
			isStr := false
			switch v := expr.(type) {
			case *VarRef:
				if typ, err := env.GetVar(v.Name); err == nil {
					if _, ok := typ.(types.StringType); ok {
						isStr = true
					}
				}
			case *StringLit:
				isStr = true
			}
			if endExpr == nil {
				endExpr = &LenExpr{Value: expr, IsString: isStr}
			}
			expr = &SliceExpr{Value: expr, Start: startExpr, End: endExpr, IsString: isStr}
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil:
			call := p.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertExpr(env, call.Args[0])
			if err != nil {
				return nil, err
			}
			expr = &ContainsExpr{Str: expr, Sub: arg}
			i++ // skip call op
		case op.Field != nil:
			expr = &FieldExpr{Receiver: expr, Name: op.Field.Name}
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				expr = &CastExpr{Value: expr, Type: *op.Cast.Type.Simple}
			} else {
				return nil, fmt.Errorf("unsupported cast")
			}
		case op.Call != nil:
			return nil, fmt.Errorf("unsupported call")
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
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
		case "values":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("values expects 1 arg")
			}
			m, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &ValuesExpr{Map: m}, nil
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
				if len(args) == 1 {
					name = "println"
					return &CallExpr{Func: name, Args: args}, nil
				}
				// concatenate arguments with spaces
				expr := args[0]
				for _, a := range args[1:] {
					expr = &BinaryExpr{Left: &BinaryExpr{Left: expr, Op: "+", Right: &StringLit{Value: " "}}, Op: "+", Right: a}
				}
				return &CallExpr{Func: "println", Args: []Expr{expr}}, nil
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
	case p.If != nil:
		return convertIfExpr(env, p.If)
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
	case p.Map != nil:
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := convertExpr(env, it.Key)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(env, it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.Group != nil:
		return convertExpr(env, p.Group)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

// Emit returns formatted Kotlin source code for prog.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	for _, f := range prog.Funcs {
		f.emit(&buf, 0)
		buf.WriteString("\n")
	}
	buf.WriteString("fun main() {\n")
	for _, s := range prog.Stmts {
		s.emit(&buf, 1)
		buf.WriteString("\n")
	}
	buf.WriteString("}\n")
	return buf.Bytes()
}
