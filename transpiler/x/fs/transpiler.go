//go:build slow

package fstrans

import (
	"bytes"
	"fmt"
	"io"
	"os/exec"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

// Program represents a simple sequence of statements.
type StructField struct {
	Name string
	Type string
	Mut  bool
}

type StructDef struct {
	Name   string
	Fields []StructField
}

type Program struct {
	Structs []StructDef
	Stmts   []Stmt
}

// varTypes holds the inferred type for each variable defined during
// transpilation. It is reset for every call to Transpile.
var (
	varTypes     map[string]string
	structDefs   []StructDef
	structCount  int
	transpileEnv *types.Env
)

func copyMap(src map[string]string) map[string]string {
	dst := make(map[string]string, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

func fsType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "float"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "string"
	case types.ListType:
		return fsType(tt.Elem) + " list"
	case types.MapType:
		return fmt.Sprintf("Map<%s, %s>", fsType(tt.Key), fsType(tt.Value))
	case types.OptionType:
		return fsType(tt.Elem) + " option"
	case types.StructType:
		if tt.Name != "" {
			return tt.Name
		}
	}
	return "obj"
}

func fsTypeFromString(s string) string {
	switch s {
	case "int", "int64":
		return "int"
	case "float":
		return "float"
	case "bool":
		return "bool"
	case "string":
		return "string"
	default:
		if strings.HasSuffix(s, " list") || strings.HasPrefix(s, "Map<") || s == "obj" {
			return "obj"
		}
		return s
	}
}

func inferStructFromMapVars(ml *parser.MapLiteral) ([]StructField, bool) {
	fields := make([]StructField, len(ml.Items))
	for i, it := range ml.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			return nil, false
		}
		if it.Value == nil || it.Value.Binary == nil || len(it.Value.Binary.Right) != 0 {
			return nil, false
		}
		tgt := it.Value.Binary.Left.Value.Target
		if tgt.Selector == nil || len(tgt.Selector.Tail) != 0 {
			return nil, false
		}
		vname := tgt.Selector.Root
		vtype, ok := varTypes[vname]
		if !ok {
			return nil, false
		}
		fields[i] = StructField{Name: key, Type: fsTypeFromString(vtype), Mut: true}
	}
	return fields, true
}

func addStructDef(name string, st types.StructType) {
	def := StructDef{Name: name}
	for _, f := range st.Order {
		def.Fields = append(def.Fields, StructField{Name: f, Type: fsType(st.Fields[f]), Mut: true})
	}
	structDefs = append(structDefs, def)
}

func simpleListType(l *ListLit) string {
	if l == nil || len(l.Elems) == 0 {
		return ""
	}
	first := inferType(l.Elems[0])
	if first == "" {
		return ""
	}
	for _, e := range l.Elems[1:] {
		if inferType(e) != first {
			return ""
		}
	}
	return first + " list"
}

func inferLiteralType(e *parser.Expr) string {
	if transpileEnv == nil || e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return ""
	}
	if ll := u.Value.Target.List; ll != nil {
		if st, ok := types.InferStructFromList(ll, transpileEnv); ok {
			structCount++
			name := fmt.Sprintf("Anon%d", structCount)
			addStructDef(name, st)
			return name + " list"
		}
		if len(ll.Elems) > 0 && ll.Elems[0].Binary != nil && ll.Elems[0].Binary.Left.Value.Target.Map != nil {
			if fields, ok := inferStructFromMapVars(ll.Elems[0].Binary.Left.Value.Target.Map); ok {
				structCount++
				name := fmt.Sprintf("Anon%d", structCount)
				structDefs = append(structDefs, StructDef{Name: name, Fields: fields})
				return name + " list"
			}
		}
	}
	return ""
}

func inferQueryType(q *parser.QueryExpr) string {
	if transpileEnv == nil || q == nil {
		return ""
	}
	if stSel := q.Select; stSel != nil {
		if stSel.Binary != nil && len(stSel.Binary.Right) == 0 {
			if ml := stSel.Binary.Left.Value.Target.Map; ml != nil {
				if st, ok := types.InferStructFromMapEnv(ml, transpileEnv); ok {
					structCount++
					name := fmt.Sprintf("Anon%d", structCount)
					addStructDef(name, st)
					return name + " list"
				}
				if fields, ok := inferStructFromMapVars(ml); ok {
					structCount++
					name := fmt.Sprintf("Anon%d", structCount)
					structDefs = append(structDefs, StructDef{Name: name, Fields: fields})
					return name + " list"
				}
			}
		}
	}
	return "list"
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

// LambdaExpr represents an inline function expression.
type LambdaExpr struct {
	Params []string
	Expr   Expr
	Body   []Stmt
}

func (l *LambdaExpr) emit(w io.Writer) {
	io.WriteString(w, "fun")
	if len(l.Params) == 0 {
		io.WriteString(w, " ()")
	}
	for _, p := range l.Params {
		io.WriteString(w, " ")
		io.WriteString(w, p)
	}
	io.WriteString(w, " -> ")
	if l.Expr != nil {
		if needsParen(l.Expr) {
			io.WriteString(w, "(")
			l.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			l.Expr.emit(w)
		}
		return
	}
	if len(l.Body) == 0 {
		io.WriteString(w, "()")
		return
	}
	w.Write([]byte{'\n'})
	for i, st := range l.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		if i < len(l.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
}

type FunDef struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FunDef) emit(w io.Writer) {
	io.WriteString(w, "let rec ")
	io.WriteString(w, f.Name)
	if len(f.Params) == 0 {
		io.WriteString(w, " ()")
	}
	for _, p := range f.Params {
		io.WriteString(w, " ")
		io.WriteString(w, p)
	}
	io.WriteString(w, " =\n")
	for i, st := range f.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		if i < len(f.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
}

type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	if r.Expr != nil {
		r.Expr.emit(w)
	}
}

type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { io.WriteString(w, "break") }

type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { io.WriteString(w, "continue") }

// ListLit represents an F# list literal.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		e.emit(w)
		if i < len(l.Elems)-1 {
			io.WriteString(w, "; ")
		}
	}
	io.WriteString(w, "]")
}

// MapLit represents an F# map literal.
type MapLit struct{ Items [][2]Expr }

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "Map.ofList [")
	for i, kv := range m.Items {
		io.WriteString(w, "(")
		kv[0].emit(w)
		io.WriteString(w, ", ")
		kv[1].emit(w)
		io.WriteString(w, ")")
		if i < len(m.Items)-1 {
			io.WriteString(w, "; ")
		}
	}
	io.WriteString(w, "]")
}

// AppendExpr represents append(list, elem).
type AppendExpr struct {
	List Expr
	Elem Expr
}

type queryFrom struct {
	Var string
	Src Expr
}

type queryJoin struct {
	Var string
	Src Expr
	On  Expr
}

type QueryExpr struct {
	Var    string
	Src    Expr
	Froms  []queryFrom
	Joins  []queryJoin
	Where  Expr
	Sort   Expr
	Skip   Expr
	Take   Expr
	Select Expr
}

// GroupQueryExpr models `group by` queries.
type GroupQueryExpr struct {
	Var      string
	Src      Expr
	Froms    []queryFrom
	Joins    []queryJoin
	Where    Expr
	Key      Expr
	GroupVar string
	Select   Expr
}

type StructLit struct {
	Name   string
	Fields []StructFieldExpr
}

type StructFieldExpr struct {
	Name  string
	Value Expr
}

func (s *StructLit) emit(w io.Writer) {
	io.WriteString(w, "{ ")
	for i, f := range s.Fields {
		io.WriteString(w, f.Name)
		io.WriteString(w, " = ")
		f.Value.emit(w)
		if i < len(s.Fields)-1 {
			io.WriteString(w, "; ")
		}
	}
	io.WriteString(w, " }")
}

func (q *QueryExpr) emit(w io.Writer) {
	io.WriteString(w, "[ for ")
	io.WriteString(w, q.Var)
	io.WriteString(w, " in ")
	src := q.Src
	if q.Sort != nil {
		src = &CallExpr{Func: "List.sortBy", Args: []Expr{&LambdaExpr{Params: []string{q.Var}, Expr: q.Sort}, src}}
	}
	if q.Skip != nil {
		src = &CallExpr{Func: "List.skip", Args: []Expr{q.Skip, src}}
	}
	if q.Take != nil {
		src = &CallExpr{Func: "List.take", Args: []Expr{q.Take, src}}
	}
	src.emit(w)
	io.WriteString(w, " do")
	for _, j := range q.Joins {
		io.WriteString(w, " for ")
		io.WriteString(w, j.Var)
		io.WriteString(w, " in ")
		j.Src.emit(w)
		io.WriteString(w, " do")
		if j.On != nil {
			io.WriteString(w, " if ")
			j.On.emit(w)
			io.WriteString(w, " then")
		}
	}
	for _, f := range q.Froms {
		io.WriteString(w, " for ")
		io.WriteString(w, f.Var)
		io.WriteString(w, " in ")
		f.Src.emit(w)
		io.WriteString(w, " do")
	}
	if q.Where != nil {
		io.WriteString(w, " if ")
		q.Where.emit(w)
		io.WriteString(w, " then")
	}
	io.WriteString(w, " yield ")
	q.Select.emit(w)
	io.WriteString(w, " ]")
}

func (g *GroupQueryExpr) emit(w io.Writer) {
	io.WriteString(w, "[ for (key, items) in List.groupBy (fun ")
	if len(g.Joins) == 0 && len(g.Froms) == 0 && g.Where == nil {
		io.WriteString(w, g.Var)
	} else {
		io.WriteString(w, "{ ")
		names := []string{g.Var}
		for _, j := range g.Joins {
			names = append(names, j.Var)
		}
		for _, f := range g.Froms {
			names = append(names, f.Var)
		}
		for i, n := range names {
			if i > 0 {
				io.WriteString(w, "; ")
			}
			io.WriteString(w, n)
			io.WriteString(w, " = ")
			io.WriteString(w, n)
		}
		io.WriteString(w, " }")
	}
	io.WriteString(w, " -> ")
	g.Key.emit(w)
	io.WriteString(w, ") ")
	if len(g.Joins) == 0 && len(g.Froms) == 0 && g.Where == nil {
		g.Src.emit(w)
	} else {
		io.WriteString(w, "[ for ")
		io.WriteString(w, g.Var)
		io.WriteString(w, " in ")
		g.Src.emit(w)
		io.WriteString(w, " do")
		for _, j := range g.Joins {
			io.WriteString(w, " for ")
			io.WriteString(w, j.Var)
			io.WriteString(w, " in ")
			j.Src.emit(w)
			io.WriteString(w, " do")
			if j.On != nil {
				io.WriteString(w, " if ")
				j.On.emit(w)
				io.WriteString(w, " then")
			}
		}
		for _, f := range g.Froms {
			io.WriteString(w, " for ")
			io.WriteString(w, f.Var)
			io.WriteString(w, " in ")
			f.Src.emit(w)
			io.WriteString(w, " do")
		}
		if g.Where != nil {
			io.WriteString(w, " if ")
			g.Where.emit(w)
			io.WriteString(w, " then")
		}
		io.WriteString(w, " yield {| ")
		names := []string{g.Var}
		for _, j := range g.Joins {
			names = append(names, j.Var)
		}
		for _, f := range g.Froms {
			names = append(names, f.Var)
		}
		for i, n := range names {
			if i > 0 {
				io.WriteString(w, "; ")
			}
			io.WriteString(w, n)
			io.WriteString(w, " = ")
			io.WriteString(w, n)
		}
		io.WriteString(w, " |} ]")
	}
	io.WriteString(w, " do\n    let ")
	io.WriteString(w, g.GroupVar)
	io.WriteString(w, " = {| key = key; items = items |}\n    yield ")
	g.Select.emit(w)
	io.WriteString(w, " ]")
}

func (a *AppendExpr) emit(w io.Writer) {
	a.List.emit(w)
	io.WriteString(w, " @ [")
	a.Elem.emit(w)
	io.WriteString(w, "]")
}

// SubstringExpr represents substring(str, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Str.emit(w)
	io.WriteString(w, ".Substring(")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
	io.WriteString(w, ")")
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then\n")
	for idx, st := range i.Then {
		st.emit(w)
		if idx < len(i.Then)-1 {
			w.Write([]byte{'\n'})
		}
	}
	if len(i.Else) > 0 {
		io.WriteString(w, "\nelse\n")
		for idx, st := range i.Else {
			st.emit(w)
			if idx < len(i.Else)-1 {
				w.Write([]byte{'\n'})
			}
		}
	}
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type AssignStmt struct {
	Name string
	Expr Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, s.Name)
	io.WriteString(w, " <- ")
	s.Expr.emit(w)
}

type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *IndexAssignStmt) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, " <- ")
	s.Value.emit(w)
}

type StructUpdateExpr struct {
	Target Expr
	Field  string
	Value  Expr
}

func (s *StructUpdateExpr) emit(w io.Writer) {
	io.WriteString(w, "{ ")
	s.Target.emit(w)
	io.WriteString(w, " with ")
	io.WriteString(w, s.Field)
	io.WriteString(w, " = ")
	s.Value.emit(w)
	io.WriteString(w, " }")
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "while ")
	wst.Cond.emit(w)
	io.WriteString(w, " do\n")
	for i, st := range wst.Body {
		st.emit(w)
		if i < len(wst.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
}

type ForStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fst *ForStmt) emit(w io.Writer) {
	io.WriteString(w, "for ")
	io.WriteString(w, fst.Name)
	io.WriteString(w, " in ")
	if fst.End != nil {
		fst.Start.emit(w)
		io.WriteString(w, " .. (")
		(&BinaryExpr{Left: fst.End, Op: "-", Right: &IntLit{Value: 1}}).emit(w)
		io.WriteString(w, ")")
	} else {
		fst.Start.emit(w)
	}
	io.WriteString(w, " do\n")
	for i, st := range fst.Body {
		st.emit(w)
		if i < len(fst.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
}

type LetStmt struct {
	Name    string
	Mutable bool
	Type    string
	Expr    Expr
}

func (s *LetStmt) emit(w io.Writer) {
	io.WriteString(w, "let ")
	if s.Mutable {
		io.WriteString(w, "mutable ")
	}
	io.WriteString(w, s.Name)
	if s.Type != "" {
		io.WriteString(w, ": ")
		io.WriteString(w, s.Type)
	}
	io.WriteString(w, " = ")
	if s.Expr != nil {
		s.Expr.emit(w)
	} else {
		io.WriteString(w, "0")
	}
}

type CallExpr struct {
	Func string
	Args []Expr
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	if u.Op != "-" {
		io.WriteString(w, " ")
	}
	if needsParen(u.Expr) {
		io.WriteString(w, "(")
		u.Expr.emit(w)
		io.WriteString(w, ")")
	} else {
		u.Expr.emit(w)
	}
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "in" {
		rtyp := inferType(b.Right)
		if rtyp == "string" {
			if needsParen(b.Right) {
				io.WriteString(w, "(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
			io.WriteString(w, ".Contains(")
			if needsParen(b.Left) {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			io.WriteString(w, ")")
		} else if rtyp == "list" {
			io.WriteString(w, "List.contains ")
			if needsParen(b.Left) {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			io.WriteString(w, " ")
			if needsParen(b.Right) {
				io.WriteString(w, "(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
		} else if rtyp == "map" {
			io.WriteString(w, "Map.containsKey ")
			if needsParen(b.Left) {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			io.WriteString(w, " ")
			if needsParen(b.Right) {
				io.WriteString(w, "(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
		} else {
			io.WriteString(w, "Seq.contains ")
			if needsParen(b.Left) {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			io.WriteString(w, " ")
			if needsParen(b.Right) {
				io.WriteString(w, "(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
		}
		return
	}
	if needsParen(b.Left) {
		io.WriteString(w, "(")
		b.Left.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emit(w)
	}
	io.WriteString(w, " ")
	io.WriteString(w, mapOp(b.Op))
	io.WriteString(w, " ")
	if needsParen(b.Right) {
		io.WriteString(w, "(")
		b.Right.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emit(w)
	}
}

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

type IdentExpr struct{ Name string }

func (i *IdentExpr) emit(w io.Writer) { io.WriteString(w, i.Name) }

type UnitLit struct{}

func (u *UnitLit) emit(w io.Writer) { io.WriteString(w, "()") }

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then ")
	if needsParen(i.Then) {
		io.WriteString(w, "(")
		i.Then.emit(w)
		io.WriteString(w, ")")
	} else {
		i.Then.emit(w)
	}
	io.WriteString(w, " else ")
	if needsParen(i.Else) {
		io.WriteString(w, "(")
		i.Else.emit(w)
		io.WriteString(w, ")")
	} else {
		i.Else.emit(w)
	}
}

func mapOp(op string) string {
	switch op {
	case "==":
		return "="
	case "!=":
		return "<>"
	default:
		return op
	}
}

func mapMethod(name string) string {
	switch name {
	case "contains":
		return "Contains"
	default:
		return name
	}
}

func precedence(op string) int {
	switch op {
	case "||":
		return 1
	case "&&":
		return 2
	case "==", "!=", "<", "<=", ">", ">=":
		return 3
	case "in":
		return 3
	case "+", "-":
		return 4
	case "*", "/", "%":
		return 5
	default:
		return 0
	}
}

func needsParen(e Expr) bool {
	switch e.(type) {
	case *BinaryExpr, *UnaryExpr, *IfExpr, *AppendExpr, *SubstringExpr, *CallExpr, *IndexExpr, *LambdaExpr, *FieldExpr, *MethodCallExpr, *SliceExpr, *CastExpr, *MapLit:
		return true
	default:
		return false
	}
}

func inferType(e Expr) string {
	switch v := e.(type) {
	case *IntLit:
		return "int"
	case *StringLit:
		return "string"
	case *BoolLit:
		return "bool"
	case *ListLit:
		return "list"
	case *MapLit:
		return "map"
	case *StructLit:
		return "struct"
	case *QueryExpr, *GroupQueryExpr:
		return "list"
	case *IdentExpr:
		if t, ok := varTypes[v.Name]; ok {
			if strings.HasSuffix(t, " list") {
				return "list"
			}
			if strings.HasPrefix(t, "Map<") {
				return "map"
			}
			return t
		}
		return ""
	case *UnaryExpr:
		if v.Op == "not" {
			return "bool"
		}
		return inferType(v.Expr)
	case *BinaryExpr:
		switch v.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return "bool"
		case "+", "-", "*", "/", "%":
			lt := inferType(v.Left)
			rt := inferType(v.Right)
			if lt == rt {
				return lt
			}
		}
	case *AppendExpr:
		return "list"
	case *SubstringExpr:
		return "string"
	case *SliceExpr:
		return inferType(v.Target)
	case *CallExpr:
		switch v.Func {
		case "string":
			return "string"
		case "Seq.length", "List.length", "String.length":
			return "int"
		case "Seq.sum", "List.sum":
			return "int"
		case "Seq.averageBy float", "List.averageBy float":
			return "float"
		}
	case *MethodCallExpr:
		switch v.Name {
		case "contains", "Contains":
			return "bool"
		}
	case *IfExpr:
		t := inferType(v.Then)
		e2 := inferType(v.Else)
		if t == e2 {
			return t
		}
	}
	return ""
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Func)
	if len(c.Args) == 0 {
		io.WriteString(w, "()")
		return
	}
	for _, a := range c.Args {
		io.WriteString(w, " ")
		if needsParen(a) {
			io.WriteString(w, "(")
			a.emit(w)
			io.WriteString(w, ")")
		} else {
			a.emit(w)
		}
	}
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (i *IndexExpr) emit(w io.Writer) {
	i.Target.emit(w)
	io.WriteString(w, ".[")
	i.Index.emit(w)
	io.WriteString(w, "]")
}

// FieldExpr represents a field selection like obj.field.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (f *FieldExpr) emit(w io.Writer) {
	f.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, f.Name)
}

// MethodCallExpr represents a method invocation target.method(args).
type MethodCallExpr struct {
	Target Expr
	Name   string
	Args   []Expr
}

func (m *MethodCallExpr) emit(w io.Writer) {
	m.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, mapMethod(m.Name))
	io.WriteString(w, "(")
	for i, a := range m.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

// SliceExpr represents slicing start:end on strings.
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

func (s *SliceExpr) emit(w io.Writer) {
	if inferType(s.Target) == "list" {
		s.Target.emit(w)
		io.WriteString(w, ".[")
		s.Start.emit(w)
		io.WriteString(w, "..(")
		(&BinaryExpr{Left: s.End, Op: "-", Right: &IntLit{Value: 1}}).emit(w)
		io.WriteString(w, ")]")
		return
	}
	s.Target.emit(w)
	io.WriteString(w, ".Substring(")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
	io.WriteString(w, ")")
}

// CastExpr represents expr as type.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "int ")
		if needsParen(c.Expr) {
			io.WriteString(w, "(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
	default:
		c.Expr.emit(w)
	}
}

// Emit generates formatted F# code from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	for _, st := range prog.Structs {
		fmt.Fprintf(&buf, "type %s = {\n", st.Name)
		for _, f := range st.Fields {
			if f.Mut {
				fmt.Fprintf(&buf, "    mutable %s: %s\n", f.Name, f.Type)
			} else {
				fmt.Fprintf(&buf, "    %s: %s\n", f.Name, f.Type)
			}
		}
		buf.WriteString("}\n")
	}
	for i, st := range prog.Stmts {
		st.emit(&buf)
		if i < len(prog.Stmts)-1 {
			buf.WriteByte('\n')
		}
	}
	if b := buf.Bytes(); len(b) > 0 && b[len(b)-1] != '\n' {
		buf.WriteByte('\n')
	}
	return buf.Bytes()
}

func header() string {
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	return fmt.Sprintf("// Generated %s\nopen System\n\n", ts.Format("2006-01-02 15:04 MST"))
}

// Transpile converts a Mochi program to a simple F# AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	transpileEnv = env
	varTypes = map[string]string{}
	structDefs = nil
	structCount = 0
	p := &Program{}
	for _, st := range prog.Statements {
		conv, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if conv != nil {
			p.Stmts = append(p.Stmts, optimizeFun(conv))
		}
	}
	p.Structs = structDefs
	transpileEnv = nil
	return p, nil
}

func optimizeFun(st Stmt) Stmt {
	fd, ok := st.(*FunDef)
	if !ok {
		return st
	}
	fd.Body = optimizeBody(fd.Body)
	return fd
}

func optimizeBody(body []Stmt) []Stmt {
	if len(body) == 2 {
		if ifs, ok := body[0].(*IfStmt); ok && len(ifs.Then) == 1 {
			if r1, ok := ifs.Then[0].(*ReturnStmt); ok {
				if r2, ok := body[1].(*ReturnStmt); ok {
					expr := &IfExpr{Cond: ifs.Cond, Then: r1.Expr, Else: r2.Expr}
					return []Stmt{&ReturnStmt{Expr: expr}}
				}
			}
		}
	}
	return body
}

func convertStmt(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Expr != nil:
		e, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		}
		declared := ""
		if st.Let.Type != nil && st.Let.Type.Simple != nil {
			declared = *st.Let.Type.Simple
		} else if t := inferLiteralType(st.Let.Value); t != "" {
			declared = t
		} else if st.Let.Value != nil && st.Let.Value.Binary != nil && len(st.Let.Value.Binary.Right) == 0 && st.Let.Value.Binary.Left.Value.Target.Query != nil {
			declared = inferQueryType(st.Let.Value.Binary.Left.Value.Target.Query)
		} else {
			declared = inferType(e)
		}
		if sl, ok := e.(*StructLit); ok && sl.Name != "" {
			declared = sl.Name
		}
		if declared == "list" {
			if ll, ok := e.(*ListLit); ok {
				if t := simpleListType(ll); t != "" {
					declared = t
				}
			}
		}
		varTypes[st.Let.Name] = declared
		typ := declared
		if typ == "list" || typ == "map" {
			typ = ""
		}
		return &LetStmt{Name: st.Let.Name, Expr: e, Type: typ}, nil
	case st.Var != nil:
		var e Expr
		var err error
		if st.Var.Value != nil {
			e, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		}
		declared := ""
		if st.Var.Type != nil && st.Var.Type.Simple != nil {
			declared = *st.Var.Type.Simple
		} else if t := inferLiteralType(st.Var.Value); t != "" {
			declared = t
		} else if st.Var.Value != nil && st.Var.Value.Binary != nil && len(st.Var.Value.Binary.Right) == 0 && st.Var.Value.Binary.Left.Value.Target.Query != nil {
			declared = inferQueryType(st.Var.Value.Binary.Left.Value.Target.Query)
		} else {
			declared = inferType(e)
		}
		if sl, ok := e.(*StructLit); ok && sl.Name != "" {
			declared = sl.Name
		}
		if declared == "list" {
			if ll, ok := e.(*ListLit); ok {
				if t := simpleListType(ll); t != "" {
					declared = t
				}
			}
		}
		varTypes[st.Var.Name] = declared
		typ := declared
		if typ == "list" || typ == "map" {
			typ = ""
		}
		return &LetStmt{Name: st.Var.Name, Expr: e, Type: typ, Mutable: true}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		e, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		varTypes[st.Assign.Name] = inferType(e)
		return &AssignStmt{Name: st.Assign.Name, Expr: e}, nil
	case st.Assign != nil && (len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0):
		val, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		target := Expr(&IdentExpr{Name: st.Assign.Name})
		if len(st.Assign.Index) > 0 {
			target, err = applyIndexOps(target, st.Assign.Index)
			if err != nil {
				return nil, err
			}
			if t := varTypes[st.Assign.Name]; t == "map" && len(st.Assign.Index) == 1 {
				upd := &CallExpr{Func: "Map.add", Args: []Expr{target.(*IndexExpr).Index, val, &IdentExpr{Name: st.Assign.Name}}}
				return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
			}
			if t := varTypes[st.Assign.Name]; t == "list" {
				indices := make([]Expr, len(st.Assign.Index))
				for i, ix := range st.Assign.Index {
					idx, err := convertExpr(ix.Start)
					if err != nil {
						return nil, err
					}
					indices[i] = idx
				}
				list := &IdentExpr{Name: st.Assign.Name}
				upd := buildListUpdate(list, indices, val)
				return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
			}
			if t := varTypes[st.Assign.Name]; t == "map" && len(st.Assign.Index) > 1 {
				indices := make([]Expr, len(st.Assign.Index))
				for i, ix := range st.Assign.Index {
					idx, err := convertExpr(ix.Start)
					if err != nil {
						return nil, err
					}
					indices[i] = idx
				}
				upd := buildMapUpdate(&IdentExpr{Name: st.Assign.Name}, indices, val)
				return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
			}
			return &IndexAssignStmt{Target: target, Value: val}, nil
		}
		if len(st.Assign.Field) == 1 {
			upd := &StructUpdateExpr{Target: target, Field: st.Assign.Field[0].Name, Value: val}
			return &ExprStmt{Expr: upd}, nil
		}
		return nil, fmt.Errorf("field assignment not supported")
	case st.Return != nil:
		var e Expr
		if st.Return.Value != nil {
			var err error
			e, err = convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: e}, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Fun != nil:
		save := varTypes
		varTypes = copyMap(varTypes)
		for _, p := range st.Fun.Params {
			if p.Type != nil && p.Type.Simple != nil {
				varTypes[p.Name] = *p.Type.Simple
			}
		}
		body := make([]Stmt, len(st.Fun.Body))
		for i, s := range st.Fun.Body {
			cs, err := convertStmt(s)
			if err != nil {
				varTypes = save
				return nil, err
			}
			body[i] = cs
		}
		params := make([]string, len(st.Fun.Params))
		for i, p := range st.Fun.Params {
			params[i] = p.Name
		}
		varTypes = save
		return &FunDef{Name: st.Fun.Name, Params: params, Body: body}, nil
	case st.While != nil:
		cond, err := convertExpr(st.While.Cond)
		if err != nil {
			return nil, err
		}
		body := make([]Stmt, len(st.While.Body))
		for i, s := range st.While.Body {
			cs, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			body[i] = cs
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case st.For != nil:
		start, err := convertExpr(st.For.Source)
		if err != nil {
			return nil, err
		}
		var end Expr
		if st.For.RangeEnd != nil {
			end, err = convertExpr(st.For.RangeEnd)
			if err != nil {
				return nil, err
			}
		}
		body := make([]Stmt, len(st.For.Body))
		for i, s := range st.For.Body {
			cs, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			body[i] = cs
		}
		return &ForStmt{Name: st.For.Name, Start: start, End: end, Body: body}, nil
	case st.Type != nil:
		if err := convertTypeDecl(st.Type); err != nil {
			return nil, err
		}
		return nil, nil
	case st.If != nil:
		return convertIfStmt(st.If)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := convertUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	exprs := []Expr{left}
	ops := []string{}
	for _, op := range e.Binary.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		for len(ops) > 0 && precedence(ops[len(ops)-1]) >= precedence(op.Op) {
			r := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			l := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			o := ops[len(ops)-1]
			ops = ops[:len(ops)-1]
			exprs = append(exprs, &BinaryExpr{Left: l, Op: o, Right: r})
		}
		ops = append(ops, op.Op)
		exprs = append(exprs, right)
	}
	for len(ops) > 0 {
		r := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		l := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		o := ops[len(ops)-1]
		ops = ops[:len(ops)-1]
		exprs = append(exprs, &BinaryExpr{Left: l, Op: o, Right: r})
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("expr reduce error")
	}
	return exprs[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			expr = &UnaryExpr{Op: "-", Expr: expr}
		case "!":
			expr = &UnaryExpr{Op: "not", Expr: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil && op.Index.End == nil && op.Index.Step == nil && op.Index.Start != nil:
			idx, err := convertExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Index != nil && op.Index.Colon != nil && op.Index.End != nil && op.Index.Step == nil && op.Index.Colon2 == nil:
			start, err := convertExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(op.Index.End)
			if err != nil {
				return nil, err
			}
			expr = &SliceExpr{Target: expr, Start: start, End: end}
		case op.Field != nil:
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ae, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = ae
			}
			if fe, ok := expr.(*FieldExpr); ok {
				expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
			} else {
				if id, ok := expr.(*IdentExpr); ok {
					expr = &CallExpr{Func: id.Name, Args: args}
				} else {
					return nil, fmt.Errorf("unsupported postfix")
				}
			}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			expr = &CastExpr{Expr: expr, Type: *op.Cast.Type.Simple}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		switch p.Call.Func {
		case "print":
			if len(args) == 1 {
				switch inferType(args[0]) {
				case "bool":
					return &CallExpr{Func: "printfn \"%b\"", Args: []Expr{args[0]}}, nil
				case "int":
					return &CallExpr{Func: "printfn \"%d\"", Args: []Expr{args[0]}}, nil
				case "float":
					return &CallExpr{Func: "printfn \"%.1f\"", Args: []Expr{args[0]}}, nil
				case "list":
					mapped := &CallExpr{Func: "List.map string", Args: []Expr{args[0]}}
					concat := &CallExpr{Func: "String.concat", Args: []Expr{&StringLit{Value: ", "}, mapped}}
					wrapped := &BinaryExpr{Left: &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: concat}, Op: "+", Right: &StringLit{Value: "]"}}
					return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{wrapped}}, nil
				default:
					arg := &CallExpr{Func: "string", Args: []Expr{args[0]}}
					return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{arg}}, nil
				}
			}
			elems := make([]Expr, len(args))
			for i, a := range args {
				switch inferType(a) {
				case "bool":
					elems[i] = &CallExpr{Func: "sprintf \"%b\"", Args: []Expr{a}}
				case "int":
					elems[i] = &CallExpr{Func: "sprintf \"%d\"", Args: []Expr{a}}
				case "float":
					elems[i] = &CallExpr{Func: "sprintf \"%.1f\"", Args: []Expr{a}}
				case "list":
					mapped := &CallExpr{Func: "List.map string", Args: []Expr{a}}
					elems[i] = &BinaryExpr{Left: &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: &CallExpr{Func: "String.concat", Args: []Expr{&StringLit{Value: ", "}, mapped}}}, Op: "+", Right: &StringLit{Value: "]"}}
				default:
					elems[i] = &CallExpr{Func: "string", Args: []Expr{a}}
				}
			}
			list := &ListLit{Elems: elems}
			concat := &CallExpr{Func: "String.concat", Args: []Expr{&StringLit{Value: " "}, list}}
			return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{concat}}, nil
		case "count", "len":
			fn := "Seq.length"
			if len(args) == 1 {
				switch inferType(args[0]) {
				case "list":
					fn = "List.length"
				case "string":
					fn = "String.length"
				case "map":
					fn = "Seq.length"
				case "group":
					if id, ok := args[0].(*IdentExpr); ok {
						field := &FieldExpr{Target: id, Name: "items"}
						return &CallExpr{Func: "List.length", Args: []Expr{field}}, nil
					}
				}
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "str":
			return &CallExpr{Func: "string", Args: args}, nil
		case "sum":
			fn := "Seq.sum"
			if len(args) == 1 && inferType(args[0]) == "list" {
				fn = "List.sum"
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "avg":
			fn := "Seq.averageBy float"
			if len(args) == 1 && inferType(args[0]) == "list" {
				fn = "List.averageBy float"
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "exists":
			if len(args) != 1 {
				return nil, fmt.Errorf("exists expects 1 arg")
			}
			fn := "Seq.isEmpty"
			if inferType(args[0]) == "list" {
				fn = "List.isEmpty"
			}
			return &UnaryExpr{Op: "not", Expr: &CallExpr{Func: fn, Args: args}}, nil
		case "append":
			if len(args) != 2 {
				return nil, fmt.Errorf("append expects 2 args")
			}
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		case "substring":
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		case "values":
			if len(args) != 1 {
				return nil, fmt.Errorf("values expects 1 arg")
			}
			if inferType(args[0]) == "map" {
				inner := &CallExpr{Func: "Map.toList", Args: []Expr{args[0]}}
				return &CallExpr{Func: "List.map snd", Args: []Expr{inner}}, nil
			}
			return &CallExpr{Func: "Seq.map snd", Args: args}, nil
		default:
			return &CallExpr{Func: p.Call.Func, Args: args}, nil
		}
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.List != nil:
		if st, ok := types.InferStructFromList(p.List, transpileEnv); ok {
			structCount++
			name := fmt.Sprintf("Anon%d", structCount)
			addStructDef(name, st)
			elems := make([]Expr, len(p.List.Elems))
			for i, el := range p.List.Elems {
				ml := el.Binary.Left.Value.Target.Map
				fields := make([]StructFieldExpr, len(ml.Items))
				for j, it := range ml.Items {
					val, err := convertExpr(it.Value)
					if err != nil {
						return nil, err
					}
					key := ""
					tgt := it.Key.Binary.Left.Value.Target
					if tgt.Selector != nil && len(tgt.Selector.Tail) == 0 {
						key = tgt.Selector.Root
					} else if tgt.Lit != nil && tgt.Lit.Str != nil {
						key = *tgt.Lit.Str
					}
					fields[j] = StructFieldExpr{Name: key, Value: val}
				}
				elems[i] = &StructLit{Fields: fields}
			}
			return &ListLit{Elems: elems}, nil
		}
		if len(p.List.Elems) > 0 && p.List.Elems[0].Binary != nil && p.List.Elems[0].Binary.Left.Value.Target.Map != nil {
			if fields, ok := inferStructFromMapVars(p.List.Elems[0].Binary.Left.Value.Target.Map); ok {
				structCount++
				name := fmt.Sprintf("Anon%d", structCount)
				structDefs = append(structDefs, StructDef{Name: name, Fields: fields})
				elems := make([]Expr, len(p.List.Elems))
				for i, el := range p.List.Elems {
					ml := el.Binary.Left.Value.Target.Map
					vals := make([]StructFieldExpr, len(ml.Items))
					for j, it := range ml.Items {
						v, err := convertExpr(it.Value)
						if err != nil {
							return nil, err
						}
						key, _ := types.SimpleStringKey(it.Key)
						vals[j] = StructFieldExpr{Name: key, Value: v}
					}
					elems[i] = &StructLit{Name: name, Fields: vals}
				}
				return &ListLit{Elems: elems}, nil
			}
		}
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Struct != nil:
		fields := make([]StructFieldExpr, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			fields[i] = StructFieldExpr{Name: f.Name, Value: v}
		}
		return &StructLit{Name: p.Struct.Name, Fields: fields}, nil
	case p.Map != nil:
		if st, ok := types.InferStructFromMapEnv(p.Map, transpileEnv); ok {
			structCount++
			name := fmt.Sprintf("Anon%d", structCount)
			addStructDef(name, st)
			fields := make([]StructFieldExpr, len(p.Map.Items))
			for i, it := range p.Map.Items {
				val, err := convertExpr(it.Value)
				if err != nil {
					return nil, err
				}
				key, _ := types.SimpleStringKey(it.Key)
				fields[i] = StructFieldExpr{Name: key, Value: val}
			}
			return &StructLit{Name: name, Fields: fields}, nil
		}
		if fields, ok := inferStructFromMapVars(p.Map); ok {
			structCount++
			name := fmt.Sprintf("Anon%d", structCount)
			structDefs = append(structDefs, StructDef{Name: name, Fields: fields})
			vals := make([]StructFieldExpr, len(p.Map.Items))
			for i, it := range p.Map.Items {
				val, err := convertExpr(it.Value)
				if err != nil {
					return nil, err
				}
				key, _ := types.SimpleStringKey(it.Key)
				vals[i] = StructFieldExpr{Name: key, Value: val}
			}
			return &StructLit{Name: name, Fields: vals}, nil
		}
		items := make([][2]Expr, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = [2]Expr{k, v}
		}
		return &MapLit{Items: items}, nil
	case p.FunExpr != nil:
		save := varTypes
		varTypes = copyMap(varTypes)
		params := make([]string, len(p.FunExpr.Params))
		for i, par := range p.FunExpr.Params {
			params[i] = par.Name
			if par.Type != nil && par.Type.Simple != nil {
				varTypes[par.Name] = *par.Type.Simple
			}
		}
		if p.FunExpr.ExprBody != nil {
			body, err := convertExpr(p.FunExpr.ExprBody)
			varTypes = save
			if err != nil {
				return nil, err
			}
			return &LambdaExpr{Params: params, Expr: body}, nil
		}
		stmts := make([]Stmt, len(p.FunExpr.BlockBody))
		for i, s := range p.FunExpr.BlockBody {
			cs, err := convertStmt(s)
			if err != nil {
				varTypes = save
				return nil, err
			}
			stmts[i] = cs
		}
		varTypes = save
		return &LambdaExpr{Params: params, Body: stmts}, nil
	case p.Selector != nil:
		expr := Expr(&IdentExpr{Name: p.Selector.Root})
		for _, name := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: name}
		}
		return expr, nil
	case p.Query != nil:
		return convertQueryExpr(p.Query)
	case p.Group != nil:
		return convertExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func convertIfExpr(in *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(in.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(in.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if in.ElseIf != nil {
		elseExpr, err = convertIfExpr(in.ElseIf)
	} else if in.Else != nil {
		elseExpr, err = convertExpr(in.Else)
	} else {
		elseExpr = &UnitLit{}
	}
	if err != nil {
		return nil, err
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertIfStmt(in *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(in.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts := make([]Stmt, len(in.Then))
	for i, s := range in.Then {
		cs, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		thenStmts[i] = cs
	}
	var elseStmts []Stmt
	if in.ElseIf != nil {
		es, err := convertIfStmt(in.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{es}
	} else if len(in.Else) > 0 {
		elseStmts = make([]Stmt, len(in.Else))
		for i, s := range in.Else {
			cs, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			elseStmts[i] = cs
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func applyIndexOps(base Expr, ops []*parser.IndexOp) (Expr, error) {
	for _, op := range ops {
		if op.Colon != nil || op.Colon2 != nil || op.End != nil || op.Step != nil {
			return nil, fmt.Errorf("slice assignment not supported")
		}
		if op.Start == nil {
			return nil, fmt.Errorf("nil index")
		}
		idx, err := convertExpr(op.Start)
		if err != nil {
			return nil, err
		}
		base = &IndexExpr{Target: base, Index: idx}
	}
	return base, nil
}

func buildListUpdate(list Expr, indexes []Expr, val Expr) Expr {
	idx := indexes[0]
	if len(indexes) == 1 {
		lam := &LambdaExpr{Params: []string{"i", "x"}, Expr: &IfExpr{Cond: &BinaryExpr{Left: &IdentExpr{Name: "i"}, Op: "=", Right: idx}, Then: val, Else: &IdentExpr{Name: "x"}}}
		return &CallExpr{Func: "List.mapi", Args: []Expr{lam, list}}
	}
	inner := buildListUpdate(&IndexExpr{Target: list, Index: idx}, indexes[1:], val)
	lam := &LambdaExpr{Params: []string{"i", "x"}, Expr: &IfExpr{Cond: &BinaryExpr{Left: &IdentExpr{Name: "i"}, Op: "=", Right: idx}, Then: inner, Else: &IdentExpr{Name: "x"}}}
	return &CallExpr{Func: "List.mapi", Args: []Expr{lam, list}}
}

func buildMapUpdate(m Expr, keys []Expr, val Expr) Expr {
	key := keys[0]
	if len(keys) == 1 {
		return &CallExpr{Func: "Map.add", Args: []Expr{key, val, m}}
	}
	inner := buildMapUpdate(&IndexExpr{Target: m, Index: key}, keys[1:], val)
	return &CallExpr{Func: "Map.add", Args: []Expr{key, inner, m}}
}

func convertTypeDecl(td *parser.TypeDecl) error {
	if len(td.Variants) > 0 {
		return fmt.Errorf("union types not supported")
	}
	st := types.StructType{Name: td.Name, Fields: map[string]types.Type{}}
	for _, m := range td.Members {
		if m.Field == nil {
			continue
		}
		ft := types.ResolveTypeRef(m.Field.Type, transpileEnv)
		st.Fields[m.Field.Name] = ft
		st.Order = append(st.Order, m.Field.Name)
	}
	addStructDef(td.Name, st)
	return nil
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	if id, ok := src.(*IdentExpr); ok {
		if varTypes[id.Name] == "group" {
			src = &FieldExpr{Target: id, Name: "items"}
		}
	}
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		e, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		froms[i] = queryFrom{Var: f.Var, Src: e}
	}
	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		src, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		var on Expr
		if j.On != nil {
			on, err = convertExpr(j.On)
			if err != nil {
				return nil, err
			}
		}
		joins[i] = queryJoin{Var: j.Var, Src: src, On: on}
	}
	var where Expr
	if q.Where != nil {
		where, err = convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
	}
	var sort, skip, take Expr
	if q.Sort != nil {
		sort, err = convertExpr(q.Sort)
		if err != nil {
			return nil, err
		}
	}
	if q.Skip != nil {
		skip, err = convertExpr(q.Skip)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		take, err = convertExpr(q.Take)
		if err != nil {
			return nil, err
		}
	}
	if q.Group != nil && len(q.Group.Exprs) == 1 {
		key, err := convertExpr(q.Group.Exprs[0])
		if err != nil {
			return nil, err
		}
		save := copyMap(varTypes)
		varTypes[q.Group.Name] = "group"
		sel, err := convertExpr(q.Select)
		varTypes = save
		if err != nil {
			return nil, err
		}
		return &GroupQueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Key: key, GroupVar: q.Group.Name, Select: sel}, nil
	}
	sel, err := convertExpr(q.Select)
	if err != nil {
		return nil, err
	}
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Sort: sort, Skip: skip, Take: take, Select: sel}, nil
}
