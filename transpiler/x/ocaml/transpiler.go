//go:build slow

package ocaml

import (
	"bytes"
	"fmt"
	"io"
	"os/exec"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
	"mochi/types"
)

// Program represents an OCaml program. All statements are emitted inside
// a `let () =` block.
type Program struct {
	Stmts []Stmt
}

func (p *Program) UsesStrModule() bool {
	var exprUses func(Expr) bool
	exprUses = func(e Expr) bool {
		switch x := e.(type) {
		case *StringContainsBuiltin:
			return true
		case *BinaryExpr:
			return exprUses(x.Left) || exprUses(x.Right)
		case *UnaryMinus:
			return exprUses(x.Expr)
		case *IfExpr:
			return exprUses(x.Cond) || exprUses(x.Then) || exprUses(x.Else)
		case *ListLit:
			for _, el := range x.Elems {
				if exprUses(el) {
					return true
				}
			}
			return false
		case *StrBuiltin:
			return exprUses(x.Expr)
		case *LenBuiltin:
			return exprUses(x.Arg)
		case *SubstringBuiltin:
			return exprUses(x.Str) || exprUses(x.Start) || exprUses(x.End)
		case *SumBuiltin:
			return exprUses(x.List)
		case *FuncCall:
			for _, a := range x.Args {
				if exprUses(a) {
					return true
				}
			}
			return false
		default:
			return false
		}
	}
	var stmtUses func(Stmt) bool
	stmtUses = func(s Stmt) bool {
		switch st := s.(type) {
		case *LetStmt:
			return exprUses(st.Expr)
		case *VarStmt:
			return exprUses(st.Expr)
		case *AssignStmt:
			return exprUses(st.Expr)
		case *PrintStmt:
			for _, e := range st.Exprs {
				if exprUses(e) {
					return true
				}
			}
			return false
		case *IfStmt:
			if exprUses(st.Cond) {
				return true
			}
			for _, t := range st.Then {
				if stmtUses(t) {
					return true
				}
			}
			for _, e := range st.Else {
				if stmtUses(e) {
					return true
				}
			}
			return false
		case *WhileStmt:
			if exprUses(st.Cond) {
				return true
			}
			for _, b := range st.Body {
				if stmtUses(b) {
					return true
				}
			}
			return false
		case *ForRangeStmt:
			if exprUses(st.Start) || exprUses(st.End) {
				return true
			}
			for _, b := range st.Body {
				if stmtUses(b) {
					return true
				}
			}
			return false
		case *ForEachStmt:
			if exprUses(st.Iterable) {
				return true
			}
			for _, b := range st.Body {
				if stmtUses(b) {
					return true
				}
			}
			return false
		case *FunStmt:
			for _, b := range st.Body {
				if stmtUses(b) {
					return true
				}
			}
			if st.Ret != nil {
				return exprUses(st.Ret)
			}
			return false
		}
		return false
	}
	for _, s := range p.Stmts {
		if stmtUses(s) {
			return true
		}
	}
	return false
}

func (p *Program) UsesControl() bool {
	var uses bool
	var check func(Stmt)
	check = func(s Stmt) {
		switch st := s.(type) {
		case *BreakStmt, *ContinueStmt:
			uses = true
		case *WhileStmt:
			for _, b := range st.Body {
				check(b)
			}
		case *ForRangeStmt:
			for _, b := range st.Body {
				check(b)
			}
		case *ForEachStmt:
			for _, b := range st.Body {
				check(b)
			}
		case *IfStmt:
			for _, t := range st.Then {
				check(t)
			}
			for _, e := range st.Else {
				check(e)
			}
		case *FunStmt:
			for _, b := range st.Body {
				check(b)
			}
		}
	}
	for _, s := range p.Stmts {
		check(s)
	}
	return uses
}

type VarInfo struct {
	typ   string
	ref   bool
	group bool
}

func mapFieldType(typ, field string) (string, bool) {
	if strings.HasPrefix(typ, "map{") && strings.HasSuffix(typ, "}") {
		inner := strings.TrimSuffix(strings.TrimPrefix(typ, "map{"), "}")
		parts := strings.Split(inner, ",")
		for _, p := range parts {
			kv := strings.SplitN(p, ":", 2)
			if len(kv) == 2 && kv[0] == field {
				return kv[1], true
			}
		}
	}
	if strings.HasPrefix(typ, "map-") {
		elems := strings.Split(typ, "-")
		if len(elems) >= 3 {
			return elems[2], true
		}
	}
	return "", false
}

// Stmt can emit itself as OCaml code.
type Stmt interface{ emit(io.Writer) }

// LetStmt represents a simple variable binding.
type LetStmt struct {
	Name string
	Expr Expr
}

func (l *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  let %s = ", l.Name)
	l.Expr.emit(w)
	io.WriteString(w, " in\n")
}

// VarStmt represents a mutable variable binding using OCaml references.
type VarStmt struct {
	Name string
	Expr Expr
}

func (v *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  let %s = ref ", v.Name)
	v.Expr.emit(w)
	io.WriteString(w, " in\n")
}

// AssignStmt represents an update to a mutable variable.
type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  %s := ", a.Name)
	a.Expr.emit(w)
	io.WriteString(w, ";\n")
}

// PrintStmt represents a call to print_endline with one or more values.
type PrintStmt struct{ Exprs []Expr }

func (p *PrintStmt) emit(w io.Writer) {
	io.WriteString(w, "  print_endline (String.concat \" \" (List.filter (fun s -> s <> \"\") [")
	for i, e := range p.Exprs {
		if i > 0 {
			io.WriteString(w, "; ")
		}
		e.emitPrint(w)
	}
	io.WriteString(w, "]));\n")
}

// IfStmt represents a basic if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "  if ")
	i.Cond.emit(w)
	io.WriteString(w, " then (\n")
	for _, st := range i.Then {
		st.emit(w)
	}
	io.WriteString(w, "  )")
	if len(i.Else) > 0 {
		io.WriteString(w, " else (\n")
		for _, st := range i.Else {
			st.emit(w)
		}
		io.WriteString(w, "  )")
	}
	io.WriteString(w, ";\n")
}

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "  (try while ")
	ws.Cond.emit(w)
	io.WriteString(w, " do\n    try\n")
	for _, st := range ws.Body {
		st.emit(w)
	}
	io.WriteString(w, "    with Continue -> ()\n  done with Break -> ())\n")
}

// ForRangeStmt represents a numeric for-loop like `for i in a..b {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	io.WriteString(w, "  (try for ")
	io.WriteString(w, fr.Name)
	io.WriteString(w, " = ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, " to (")
	fr.End.emit(w)
	io.WriteString(w, " - 1) do\n    try\n")
	for _, st := range fr.Body {
		st.emit(w)
	}
	io.WriteString(w, "    with Continue -> ()\n  done with Break -> ())\n")
}

// ForEachStmt represents iteration over a collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	Typ      string
}

func (fe *ForEachStmt) emit(w io.Writer) {
	if strings.HasPrefix(fe.Typ, "map") {
		io.WriteString(w, "  (try List.iter (fun (")
		io.WriteString(w, fe.Name)
		io.WriteString(w, ", _) ->\n    try\n")
		for _, st := range fe.Body {
			st.emit(w)
		}
		io.WriteString(w, "    with Continue -> ()) ")
		fe.Iterable.emit(w)
		io.WriteString(w, " with Break -> ())\n")
		return
	}
	io.WriteString(w, "  (try List.iter (fun ")
	io.WriteString(w, fe.Name)
	io.WriteString(w, " ->\n    try\n")
	for _, st := range fe.Body {
		st.emit(w)
	}
	io.WriteString(w, "    with Continue -> ()) ")
	fe.Iterable.emit(w)
	io.WriteString(w, " with Break -> ())\n")
}

// BreakStmt exits the nearest loop.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) {
	io.WriteString(w, "  raise Break;\n")
}

// ContinueStmt skips to the next loop iteration.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) {
	io.WriteString(w, "  raise Continue;\n")
}

// FunStmt represents a simple function declaration with no parameters.
type FunStmt struct {
	Name   string
	Params []string
	Body   []Stmt
	Ret    Expr
}

func (f *FunStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "let rec %s", f.Name)
	if len(f.Params) == 0 {
		io.WriteString(w, " ()")
	} else {
		for _, p := range f.Params {
			fmt.Fprintf(w, " %s", p)
		}
	}
	io.WriteString(w, " =\n")
	for _, st := range f.Body {
		st.emit(w)
	}
	io.WriteString(w, "  ")
	if f.Ret != nil {
		f.Ret.emit(w)
	} else {
		io.WriteString(w, "()")
	}
	io.WriteString(w, "\n\n")
}

// Expr is any OCaml expression that can emit itself.
type Expr interface {
	emit(io.Writer)
	emitPrint(io.Writer)
}

// StrBuiltin represents a call to the builtin str() function.
type StrBuiltin struct{ Expr Expr }

func (s *StrBuiltin) emit(w io.Writer) {
	io.WriteString(w, "string_of_int ")
	s.Expr.emit(w)
}

func (s *StrBuiltin) emitPrint(w io.Writer) { s.emit(w) }

// LenBuiltin represents a call to len() builtin for strings.
type LenBuiltin struct {
	Arg Expr
	Typ string
}

func (l *LenBuiltin) emit(w io.Writer) {
	switch l.Typ {
	case "string":
		io.WriteString(w, "String.length ")
	case "list":
		io.WriteString(w, "List.length ")
	case "map":
		io.WriteString(w, "List.length ")
	default:
		if strings.HasPrefix(l.Typ, "map-") {
			io.WriteString(w, "List.length ")
		} else {
			io.WriteString(w, "List.length ")
		}
	}
	l.Arg.emit(w)
}

func (l *LenBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int (")
	l.emit(w)
	io.WriteString(w, ")")
}

// SubstringBuiltin represents substring(str, start, end).
type SubstringBuiltin struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringBuiltin) emit(w io.Writer) {
	io.WriteString(w, "String.sub ")
	s.Str.emit(w)
	io.WriteString(w, " ")
	s.Start.emit(w)
	io.WriteString(w, " (")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ")")
}

func (s *SubstringBuiltin) emitPrint(w io.Writer) { s.emit(w) }

// SumBuiltin represents sum(list).
type SumBuiltin struct{ List Expr }

func (s *SumBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(List.fold_left (fun acc x -> acc + x) 0 ")
	s.List.emit(w)
	io.WriteString(w, ")")
}

func (s *SumBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int ")
	s.emit(w)
}

// AvgBuiltin represents avg(list).
type AvgBuiltin struct{ List Expr }

func (a *AvgBuiltin) emit(w io.Writer) {
	io.WriteString(w, "((List.fold_left (fun acc x -> acc + x) 0 ")
	a.List.emit(w)
	io.WriteString(w, ") / List.length ")
	a.List.emit(w)
	io.WriteString(w, ")")
}

func (a *AvgBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int ")
	a.emit(w)
}

// CountBuiltin represents count(list).
type CountBuiltin struct{ List Expr }

func (c *CountBuiltin) emit(w io.Writer) {
	io.WriteString(w, "List.length ")
	c.List.emit(w)
}

func (c *CountBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int (List.length ")
	c.List.emit(w)
	io.WriteString(w, ")")
}

// AppendBuiltin represents append(list, value).
type AppendBuiltin struct {
	List  Expr
	Value Expr
}

func (a *AppendBuiltin) emit(w io.Writer) {
	io.WriteString(w, "List.append ")
	a.List.emit(w)
	io.WriteString(w, " [")
	a.Value.emit(w)
	io.WriteString(w, "]")
}

func (a *AppendBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "(\"[\" ^ String.concat \", \" (List.map string_of_int (")
	a.emit(w)
	io.WriteString(w, ")) ^ \"]\")")
}

// MinBuiltin represents min(list).
type MinBuiltin struct{ List Expr }

func (m *MinBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(List.fold_left min max_int ")
	m.List.emit(w)
	io.WriteString(w, ")")
}

func (m *MinBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int ")
	m.emit(w)
}

// MaxBuiltin represents max(list).
type MaxBuiltin struct{ List Expr }

func (m *MaxBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(List.fold_left max min_int ")
	m.List.emit(w)
	io.WriteString(w, ")")
}

func (m *MaxBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int ")
	m.emit(w)
}

// ValuesBuiltin represents values(map) returning list of values.
type ValuesBuiltin struct{ Map Expr }

func (v *ValuesBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(List.map snd ")
	v.Map.emit(w)
	io.WriteString(w, ")")
}

func (v *ValuesBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "String.concat \" \" (List.map string_of_int (")
	v.emit(w)
	io.WriteString(w, "))")
}

// StringContainsBuiltin represents s.contains(sub).
type StringContainsBuiltin struct {
	Str Expr
	Sub Expr
}

func (s *StringContainsBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(let len_s = String.length ")
	s.Str.emit(w)
	io.WriteString(w, " and len_sub = String.length ")
	s.Sub.emit(w)
	io.WriteString(w, " in let rec aux i = if i + len_sub > len_s then false else if String.sub ")
	s.Str.emit(w)
	io.WriteString(w, " i len_sub = ")
	s.Sub.emit(w)
	io.WriteString(w, " then true else aux (i + 1) in aux 0)")
}

func (s *StringContainsBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_bool (if ")
	s.emit(w)
	io.WriteString(w, " then true else false)")
}

// FuncCall represents a call to a user-defined function.
type FuncCall struct {
	Name string
	Args []Expr
	Ret  string
}

func (f *FuncCall) emit(w io.Writer) {
	io.WriteString(w, f.Name)
	if len(f.Args) == 0 {
		io.WriteString(w, " ()")
		return
	}
	for _, a := range f.Args {
		io.WriteString(w, " ")
		a.emit(w)
	}
}

func (f *FuncCall) emitPrint(w io.Writer) {
	switch f.Ret {
	case "int":
		io.WriteString(w, "string_of_int (")
		f.emit(w)
		io.WriteString(w, ")")
	case "bool":
		io.WriteString(w, "string_of_bool (")
		f.emit(w)
		io.WriteString(w, ")")
	default:
		f.emit(w)
	}
}

// FuncExpr represents an anonymous function expression.
type FuncExpr struct {
	Params []string
	Body   []Stmt
	Ret    Expr
}

func (f *FuncExpr) emit(w io.Writer) {
	io.WriteString(w, "(fun")
	for _, p := range f.Params {
		io.WriteString(w, " ")
		io.WriteString(w, p)
	}
	io.WriteString(w, " ->\n")
	for _, st := range f.Body {
		st.emit(w)
	}
	io.WriteString(w, "  ")
	if f.Ret != nil {
		f.Ret.emit(w)
	} else {
		io.WriteString(w, "()")
	}
	io.WriteString(w, ")")
}

func (f *FuncExpr) emitPrint(w io.Writer) { f.emit(w) }

// ListLit represents a list literal of integers.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, "; ")
		}
		e.emit(w)
	}
	io.WriteString(w, "]")
}

func (l *ListLit) emitPrint(w io.Writer) {
	io.WriteString(w, "(\"[\" ^ String.concat \", \" (List.map string_of_int (")
	l.emit(w)
	io.WriteString(w, ")) ^ \"]\")")
}

// MapEntry represents a key/value pair in a map literal.
type MapEntry struct {
	Key   Expr
	Value Expr
}

// MapLit represents a simple map literal implemented as a list of
// key/value tuples. Only string keys and int values are currently supported.
type MapLit struct{ Items []MapEntry }

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, "; ")
		}
		io.WriteString(w, "(")
		it.Key.emit(w)
		io.WriteString(w, ", ")
		it.Value.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, "]")
}

func (m *MapLit) emitPrint(w io.Writer) { m.emit(w) }

// MapIndexExpr represents map[key] access.
type MapIndexExpr struct {
	Map Expr
	Key Expr
	Typ string
}

func (mi *MapIndexExpr) emit(w io.Writer) {
	io.WriteString(w, "(List.assoc ")
	mi.Key.emit(w)
	io.WriteString(w, " ")
	mi.Map.emit(w)
	io.WriteString(w, ")")
}

func (mi *MapIndexExpr) emitPrint(w io.Writer) {
	switch mi.Typ {
	case "int":
		io.WriteString(w, "string_of_int (List.assoc ")
		mi.Key.emit(w)
		io.WriteString(w, " ")
		mi.Map.emit(w)
		io.WriteString(w, "))")
	default:
		mi.emit(w)
	}
}

// IndexExpr represents list[index] or string[index].
type IndexExpr struct {
	Col   Expr
	Index Expr
	Typ   string
}

// SliceExpr represents col[start:end] for strings.
type SliceExpr struct {
	Col   Expr
	Start Expr
	End   Expr
	Typ   string
}

// ListUpdateExpr updates a list at a specific index and returns the new list.
type ListUpdateExpr struct {
	List  Expr
	Index Expr
	Value Expr
}

func (lu *ListUpdateExpr) emit(w io.Writer) {
	io.WriteString(w, "(List.mapi (fun i x -> if i = ")
	lu.Index.emit(w)
	io.WriteString(w, " then ")
	lu.Value.emit(w)
	io.WriteString(w, " else x) (")
	lu.List.emit(w)
	io.WriteString(w, "))")
}

func (lu *ListUpdateExpr) emitPrint(w io.Writer) { lu.emit(w) }

// MapUpdateExpr updates a map by replacing or adding a key/value pair.
type MapUpdateExpr struct {
	Map   Expr
	Key   Expr
	Value Expr
}

// queryLoop is one iteration variable and its source list.
type queryLoop struct {
	Name string
	Src  Expr
}

// QueryExpr represents a basic cross join query without filtering.
type QueryExpr struct {
	Loops  []queryLoop
	Where  Expr
	Sort   Expr
	Desc   bool
	Skip   Expr
	Take   Expr
	Select Expr
}

func emitQueryLoop(w io.Writer, loops []queryLoop, sel Expr, where Expr, idx int) {
	loop := loops[idx]
	if idx == len(loops)-1 {
		if where != nil {
			io.WriteString(w, "(List.filter_map (fun ")
			io.WriteString(w, loop.Name)
			io.WriteString(w, " -> if ")
			where.emit(w)
			io.WriteString(w, " then Some (")
			sel.emit(w)
			io.WriteString(w, ") else None) ")
			loop.Src.emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "(List.map (fun ")
			io.WriteString(w, loop.Name)
			io.WriteString(w, " -> ")
			sel.emit(w)
			io.WriteString(w, ") ")
			loop.Src.emit(w)
			io.WriteString(w, ")")
		}
		return
	}
	io.WriteString(w, "(List.concat (List.map (fun ")
	io.WriteString(w, loop.Name)
	io.WriteString(w, " -> ")
	emitQueryLoop(w, loops, sel, where, idx+1)
	io.WriteString(w, ") ")
	loop.Src.emit(w)
	io.WriteString(w, "))")
}

func (q *QueryExpr) emit(w io.Writer) {
	if q.Sort == nil && q.Skip == nil && q.Take == nil {
		emitQueryLoop(w, q.Loops, q.Select, q.Where, 0)
		return
	}
	io.WriteString(w, "(let __tmp0 = ")
	emitQueryLoop(w, q.Loops, q.Select, q.Where, 0)
	io.WriteString(w, " in\n")
	if q.Sort != nil {
		keyA := strings.ReplaceAll(renderExpr(q.Sort), q.Loops[0].Name, "a")
		keyB := strings.ReplaceAll(renderExpr(q.Sort), q.Loops[0].Name, "b")
		io.WriteString(w, "  let __tmp0 = List.sort (fun a b -> compare ")
		if q.Desc {
			io.WriteString(w, keyB+" "+keyA)
		} else {
			io.WriteString(w, keyA+" "+keyB)
		}
		io.WriteString(w, ") __tmp0 in\n")
	}
	if q.Skip != nil || q.Take != nil {
		io.WriteString(w, "  let rec drop n l = if n <= 0 then l else match l with [] -> [] | _::t -> drop (n-1) t in\n")
		io.WriteString(w, "  let rec take n l = if n <= 0 then [] else match l with [] -> [] | x::xs -> x :: take (n-1) xs in\n")
		if q.Skip != nil {
			io.WriteString(w, "  let __tmp0 = drop (")
			q.Skip.emit(w)
			io.WriteString(w, ") __tmp0 in\n")
		}
		if q.Take != nil {
			io.WriteString(w, "  let __tmp0 = take (")
			q.Take.emit(w)
			io.WriteString(w, ") __tmp0 in\n")
		}
	}
	io.WriteString(w, "  __tmp0)")
}

func (q *QueryExpr) emitPrint(w io.Writer) { q.emit(w) }

// GroupByQueryExpr represents a query with a grouping step.
type GroupByQueryExpr struct {
	Loops  []queryLoop
	Where  Expr
	Key    Expr
	Into   string
	Having Expr
	Select Expr
}

func emitGroupLoop(w io.Writer, loops []queryLoop, key Expr, item string, where Expr, idx int) {
	loop := loops[idx]
	io.WriteString(w, "  List.iter (fun ")
	io.WriteString(w, loop.Name)
	io.WriteString(w, " ->\n")
	if idx == len(loops)-1 {
		if where != nil {
			io.WriteString(w, "    if ")
			where.emit(w)
			io.WriteString(w, " then (\n")
		}
		io.WriteString(w, "    let key = ")
		key.emit(w)
		io.WriteString(w, " in\n")
		io.WriteString(w, "    let cur = try List.assoc key !__groups0 with Not_found -> [] in\n")
		fmt.Fprintf(w, "    __groups0 := (key, %s :: cur) :: List.remove_assoc key !__groups0;\n", item)
		if where != nil {
			io.WriteString(w, "    )\n")
		}
	} else {
		emitGroupLoop(w, loops, key, item, where, idx+1)
	}
	io.WriteString(w, "  ) ")
	loop.Src.emit(w)
	io.WriteString(w, ";\n")
}

func (g *GroupByQueryExpr) emit(w io.Writer) {
	io.WriteString(w, "(let __groups0 = ref [] in\n")
	emitGroupLoop(w, g.Loops, g.Key, g.Loops[0].Name, g.Where, 0)
	io.WriteString(w, "  let __res0 = ref [] in\n")
	io.WriteString(w, "  List.iter (fun (")
	io.WriteString(w, g.Into+"_key, "+g.Into+"_items")
	io.WriteString(w, ") ->\n")
	fmt.Fprintf(w, "    let %s = List.rev %s_items in\n", g.Into, g.Into)
	if g.Having != nil {
		io.WriteString(w, "    if ")
		g.Having.emit(w)
		io.WriteString(w, " then (\n      __res0 := ")
		g.Select.emit(w)
		io.WriteString(w, " :: !__res0)\n    else ()\n")
	} else {
		io.WriteString(w, "    __res0 := ")
		g.Select.emit(w)
		io.WriteString(w, " :: !__res0\n")
	}
	io.WriteString(w, "  ) !__groups0;\n")
	io.WriteString(w, "  List.rev !__res0)")
}

func (g *GroupByQueryExpr) emitPrint(w io.Writer) { g.emit(w) }

// InExpr represents `item in collection`.
type InExpr struct {
	Item Expr
	Coll Expr
	Typ  string
}

func (in *InExpr) emit(w io.Writer) {
	switch in.Typ {
	case "string":
		(&StringContainsBuiltin{Str: in.Coll, Sub: in.Item}).emit(w)
	case "map":
		io.WriteString(w, "(List.mem_assoc ")
		in.Item.emit(w)
		io.WriteString(w, " ")
		in.Coll.emit(w)
		io.WriteString(w, ")")
	default: // list
		if strings.HasPrefix(in.Typ, "map-") {
			io.WriteString(w, "(List.mem_assoc ")
			in.Item.emit(w)
			io.WriteString(w, " ")
			in.Coll.emit(w)
			io.WriteString(w, ")")
			return
		}
		io.WriteString(w, "(List.mem ")
		in.Item.emit(w)
		io.WriteString(w, " ")
		in.Coll.emit(w)
		io.WriteString(w, ")")
	}
}

func (in *InExpr) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_bool (")
	in.emit(w)
	io.WriteString(w, ")")
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func (mu *MapUpdateExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	io.WriteString(w, "(")
	mu.Key.emit(w)
	io.WriteString(w, ", ")
	mu.Value.emit(w)
	io.WriteString(w, ") :: List.remove_assoc ")
	mu.Key.emit(w)
	io.WriteString(w, " ")
	mu.Map.emit(w)
	io.WriteString(w, ")")
}

func (mu *MapUpdateExpr) emitPrint(w io.Writer) { mu.emit(w) }

func (ix *IndexExpr) emit(w io.Writer) {
	switch ix.Typ {
	case "string":
		io.WriteString(w, "String.make 1 (String.get ")
		ix.Col.emit(w)
		io.WriteString(w, " ")
		ix.Index.emit(w)
		io.WriteString(w, ")")
	default:
		io.WriteString(w, "List.nth (")
		ix.Col.emit(w)
		io.WriteString(w, ") ")
		ix.Index.emit(w)
	}
}

func (ix *IndexExpr) emitPrint(w io.Writer) {
	switch ix.Typ {
	case "string":
		ix.emit(w)
	default:
		io.WriteString(w, "string_of_int (")
		ix.emit(w)
		io.WriteString(w, ")")
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	// Only string slices are supported for now
	io.WriteString(w, "String.sub ")
	s.Col.emit(w)
	io.WriteString(w, " ")
	s.Start.emit(w)
	io.WriteString(w, " (")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ")")
}

func (s *SliceExpr) emitPrint(w io.Writer) { s.emit(w) }

// UnaryMinus represents negation of an integer expression.
type UnaryMinus struct{ Expr Expr }

func (u *UnaryMinus) emit(w io.Writer) {
	io.WriteString(w, "(-")
	u.Expr.emit(w)
	io.WriteString(w, ")")
}

func (u *UnaryMinus) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int (-")
	u.Expr.emit(w)
	io.WriteString(w, ")")
}

// CastExpr represents a simple cast expression like string -> int.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "int_of_string ")
		c.Expr.emit(w)
	default:
		c.Expr.emit(w)
	}
}

func (c *CastExpr) emitPrint(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "string_of_int (int_of_string ")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	default:
		c.Expr.emitPrint(w)
	}
}

// IfExpr represents a conditional expression.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
	Typ  string
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then ")
	i.Then.emit(w)
	io.WriteString(w, " else ")
	i.Else.emit(w)
}

func (i *IfExpr) emitPrint(w io.Writer) {
	switch i.Typ {
	case "int":
		io.WriteString(w, "string_of_int ")
		i.emit(w)
	case "bool":
		io.WriteString(w, "string_of_bool (if ")
		i.Cond.emit(w)
		io.WriteString(w, " then ")
		i.Then.emit(w)
		io.WriteString(w, " else ")
		i.Else.emit(w)
		io.WriteString(w, ")")
	default:
		i.emit(w)
	}
}

// MatchExpr represents a basic pattern matching expression.
type MatchExpr struct {
	Target Expr
	Arms   []MatchArm
	Typ    string
}

type MatchArm struct {
	Pattern Expr // nil for wildcard
	Result  Expr
}

func (m *MatchExpr) emit(w io.Writer) {
	io.WriteString(w, "(match ")
	m.Target.emit(w)
	io.WriteString(w, " with")
	for _, a := range m.Arms {
		io.WriteString(w, " | ")
		if a.Pattern != nil {
			a.Pattern.emit(w)
		} else {
			io.WriteString(w, "_")
		}
		io.WriteString(w, " -> ")
		a.Result.emit(w)
	}
	io.WriteString(w, ")")
}

func (m *MatchExpr) emitPrint(w io.Writer) {
	switch m.Typ {
	case "int":
		io.WriteString(w, "string_of_int ")
		m.emit(w)
	case "bool":
		io.WriteString(w, "string_of_bool ")
		m.emit(w)
	default:
		m.emit(w)
	}
}

// Name represents a variable reference.
type Name struct {
	Ident string
	Typ   string
	Ref   bool
}

func (n *Name) emit(w io.Writer) {
	if n.Ref {
		fmt.Fprintf(w, "!%s", n.Ident)
	} else {
		io.WriteString(w, n.Ident)
	}
}
func (n *Name) emitPrint(w io.Writer) {
	ident := n.Ident
	if n.Ref {
		ident = "!" + n.Ident
	}
	switch n.Typ {
	case "int":
		fmt.Fprintf(w, "string_of_int %s", ident)
	case "bool":
		fmt.Fprintf(w, "string_of_bool %s", ident)
	default:
		io.WriteString(w, ident)
	}
}

// IntLit represents an integer literal.
type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer)      { fmt.Fprintf(w, "%d", i.Value) }
func (i *IntLit) emitPrint(w io.Writer) { fmt.Fprintf(w, "string_of_int %d", i.Value) }

// BoolLit represents a boolean literal.
type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprintf(w, "%t", b.Value) }
func (b *BoolLit) emitPrint(w io.Writer) {
	if b.Value {
		io.WriteString(w, "string_of_bool true")
	} else {
		io.WriteString(w, "string_of_bool false")
	}
}

// StringLit represents a string literal.
type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer)      { fmt.Fprintf(w, "%q", s.Value) }
func (s *StringLit) emitPrint(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

// BinaryExpr represents a binary operation.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
	Typ   string
}

func (b *BinaryExpr) emit(w io.Writer) {
	op := b.Op
	if b.Op == "%" {
		op = "mod"
	}
	if b.Op == "+" && b.Typ == "string" {
		op = "^"
	}
	fmt.Fprintf(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", op)
	b.Right.emit(w)
	fmt.Fprintf(w, ")")
}

func (b *BinaryExpr) emitPrint(w io.Writer) {
	switch b.Typ {
	case "bool":
		io.WriteString(w, "string_of_bool ")
		b.emit(w)
	case "string":
		b.emit(w)
	default:
		io.WriteString(w, "string_of_int ")
		b.emit(w)
	}
}

func header() string {
	ts := gitTimestamp()
	version := strings.TrimSpace(meta.Version())
	return fmt.Sprintf("(* Generated by Mochi transpiler v%s on %s *)\n", version, ts)
}

func gitTimestamp() string {
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			return t.Format("2006-01-02 15:04 MST")
		}
	}
	return time.Now().UTC().Format("2006-01-02 15:04 MST")
}

func defaultValueExpr(typ string) Expr {
	switch typ {
	case "int":
		return &IntLit{Value: 0}
	case "bool":
		return &BoolLit{Value: false}
	case "string":
		return &StringLit{Value: ""}
	case "list":
		return &ListLit{Elems: nil}
	case "map":
		return &MapLit{Items: nil}
	case "func":
		return &FuncExpr{}
	}
	return &IntLit{Value: 0}
}

// Emit renders OCaml source code for the program.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	buf.WriteString("\n")
	if p.UsesStrModule() {
		buf.WriteString("open Str\n\n")
	}
	if p.UsesControl() {
		buf.WriteString("exception Break\nexception Continue\n\n")
	}
	for _, s := range p.Stmts {
		if _, ok := s.(*FunStmt); ok {
			s.emit(&buf)
		}
	}
	buf.WriteString("let () =\n")
	for _, s := range p.Stmts {
		if _, ok := s.(*FunStmt); ok {
			continue
		}
		s.emit(&buf)
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into a simple OCaml AST.
func transpileStmt(st *parser.Statement, env *types.Env, vars map[string]VarInfo) (Stmt, error) {
	switch {
	case st.Let != nil:
		var expr Expr
		var typ string
		var err error
		if st.Let.Value != nil {
			expr, typ, err = convertExpr(st.Let.Value, env, vars)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil {
			typ = *st.Let.Type.Simple
			expr = defaultValueExpr(typ)
		} else {
			return nil, fmt.Errorf("let without value not supported")
		}
		vars[st.Let.Name] = VarInfo{typ: typ}
		return &LetStmt{Name: st.Let.Name, Expr: expr}, nil
	case st.Var != nil:
		var expr Expr
		var typ string
		var err error
		if st.Var.Value != nil {
			expr, typ, err = convertExpr(st.Var.Value, env, vars)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil {
			typ = *st.Var.Type.Simple
			expr = defaultValueExpr(typ)
		} else {
			return nil, fmt.Errorf("var without type or value not supported")
		}
		vars[st.Var.Name] = VarInfo{typ: typ, ref: true}
		return &VarStmt{Name: st.Var.Name, Expr: expr}, nil
	case st.Assign != nil:
		info, ok := vars[st.Assign.Name]
		if !ok || !info.ref {
			return nil, fmt.Errorf("assignment to unknown or immutable variable")
		}
		valExpr, _, err := convertExpr(st.Assign.Value, env, vars)
		if err != nil {
			return nil, err
		}
		if len(st.Assign.Index) > 0 {
			if strings.HasPrefix(info.typ, "map") && len(st.Assign.Index) == 1 {
				ix := st.Assign.Index[0]
				if ix.Colon != nil || ix.Colon2 != nil || ix.End != nil || ix.Step != nil {
					return nil, fmt.Errorf("slice assignment not supported")
				}
				key, _, err := convertExpr(ix.Start, env, vars)
				if err != nil {
					return nil, err
				}
				mapExpr := Expr(&Name{Ident: st.Assign.Name, Typ: info.typ, Ref: true})
				upd := &MapUpdateExpr{Map: mapExpr, Key: key, Value: valExpr}
				return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
			}
			if strings.HasPrefix(info.typ, "map") {
				keys := make([]Expr, len(st.Assign.Index))
				for i, ix := range st.Assign.Index {
					if ix.Colon != nil || ix.Colon2 != nil || ix.End != nil || ix.Step != nil {
						return nil, fmt.Errorf("slice assignment not supported")
					}
					k, _, err := convertExpr(ix.Start, env, vars)
					if err != nil {
						return nil, err
					}
					keys[i] = k
				}
				mapExpr := Expr(&Name{Ident: st.Assign.Name, Typ: info.typ, Ref: true})
				upd := buildMapUpdate(mapExpr, keys, valExpr)
				return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
			}
			indices := make([]Expr, len(st.Assign.Index))
			for i, ix := range st.Assign.Index {
				if ix.Colon != nil || ix.Colon2 != nil || ix.End != nil || ix.Step != nil {
					return nil, fmt.Errorf("slice assignment not supported")
				}
				idx, _, err := convertExpr(ix.Start, env, vars)
				if err != nil {
					return nil, err
				}
				indices[i] = idx
			}
			listExpr := Expr(&Name{Ident: st.Assign.Name, Typ: info.typ, Ref: true})
			upd := buildListUpdate(listExpr, indices, valExpr)
			return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: valExpr}, nil
	case st.Expr != nil:
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" {
			args := make([]Expr, len(call.Args))
			for i, a := range call.Args {
				ex, _, err := convertExpr(a, env, vars)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			return &PrintStmt{Exprs: args}, nil
		}
		return nil, fmt.Errorf("unsupported expression")
	case st.If != nil:
		cond, _, err := convertExpr(st.If.Cond, env, vars)
		if err != nil {
			return nil, err
		}
		thenStmts, err := transpileStmts(st.If.Then, env, vars)
		if err != nil {
			return nil, err
		}
		var elseStmts []Stmt
		if st.If.ElseIf != nil {
			elseStmt, err := transpileStmt(&parser.Statement{If: st.If.ElseIf}, env, vars)
			if err != nil {
				return nil, err
			}
			if elseStmt != nil {
				elseStmts = []Stmt{elseStmt}
			}
		} else if len(st.If.Else) > 0 {
			elseStmts, err = transpileStmts(st.If.Else, env, vars)
			if err != nil {
				return nil, err
			}
		}
		return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
	case st.While != nil:
		cond, _, err := convertExpr(st.While.Cond, env, vars)
		if err != nil {
			return nil, err
		}
		body, err := transpileStmts(st.While.Body, env, vars)
		if err != nil {
			return nil, err
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case st.For != nil:
		if st.For.RangeEnd != nil {
			start, _, err := convertExpr(st.For.Source, env, vars)
			if err != nil {
				return nil, err
			}
			endExpr, _, err := convertExpr(st.For.RangeEnd, env, vars)
			if err != nil {
				return nil, err
			}
			vars[st.For.Name] = VarInfo{typ: "int"}
			body, err := transpileStmts(st.For.Body, env, vars)
			if err != nil {
				return nil, err
			}
			delete(vars, st.For.Name)
			return &ForRangeStmt{Name: st.For.Name, Start: start, End: endExpr, Body: body}, nil
		}
		iter, iterTyp, err := convertExpr(st.For.Source, env, vars)
		if err != nil {
			return nil, err
		}
		vtyp := "int"
		if strings.HasPrefix(iterTyp, "list-") {
			vtyp = strings.TrimPrefix(iterTyp, "list-")
		} else if strings.HasPrefix(iterTyp, "list") {
			vtyp = "int"
		}
		if strings.HasPrefix(iterTyp, "map-") {
			parts := strings.Split(iterTyp, "-")
			if len(parts) >= 3 {
				vtyp = parts[1]
			}
		}
		vars[st.For.Name] = VarInfo{typ: vtyp}
		body, err := transpileStmts(st.For.Body, env, vars)
		if err != nil {
			return nil, err
		}
		delete(vars, st.For.Name)
		return &ForEachStmt{Name: st.For.Name, Iterable: iter, Body: body, Typ: iterTyp}, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Fun != nil:
		child := types.NewEnv(env)
		fnVars := map[string]VarInfo{}
		params := make([]string, len(st.Fun.Params))
		for i, p := range st.Fun.Params {
			params[i] = p.Name
			typ := "int"
			if p.Type != nil && p.Type.Simple != nil {
				typ = *p.Type.Simple
			}
			fnVars[p.Name] = VarInfo{typ: typ}
		}
		bodyStmts := st.Fun.Body
		if len(bodyStmts) > 0 && bodyStmts[len(bodyStmts)-1].Return != nil {
			bodyStmts = bodyStmts[:len(bodyStmts)-1]
		}
		body, err := transpileStmts(bodyStmts, child, fnVars)
		if err != nil {
			return nil, err
		}
		var ret Expr
		if len(st.Fun.Body) > 0 {
			last := st.Fun.Body[len(st.Fun.Body)-1]
			if last.Return != nil && last.Return.Value != nil {
				r, _, err := convertExpr(last.Return.Value, child, fnVars)
				if err != nil {
					return nil, err
				}
				ret = r
			}
		}
		env.SetFunc(st.Fun.Name, st.Fun)
		var retTyp types.Type = types.BoolType{}
		if st.Fun.Return != nil {
			if st.Fun.Return.Simple != nil {
				switch *st.Fun.Return.Simple {
				case "int":
					retTyp = types.IntType{}
				case "string":
					retTyp = types.StringType{}
				case "bool":
					retTyp = types.BoolType{}
				}
			} else if st.Fun.Return.Fun != nil {
				retTyp = types.FuncType{Return: types.IntType{}}
			}
		}
		env.SetFuncType(st.Fun.Name, types.FuncType{Return: retTyp})
		return &FunStmt{Name: st.Fun.Name, Params: params, Body: body, Ret: ret}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func transpileStmts(list []*parser.Statement, env *types.Env, vars map[string]VarInfo) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		compiled, err := transpileStmt(st, env, vars)
		if err != nil {
			return nil, err
		}
		if compiled != nil {
			out = append(out, compiled)
		}
	}
	return out, nil
}

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	pr := &Program{}
	vars := map[string]VarInfo{}
	stmts, err := transpileStmts(prog.Statements, env, vars)
	if err != nil {
		return nil, err
	}
	pr.Stmts = stmts
	return pr, nil
}

// Print converts the custom AST into a generic ast.Node and prints it.
func Print(p *Program) {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		switch s := st.(type) {
		case *PrintStmt:
			n.Children = append(n.Children, &ast.Node{Kind: "print"})
		case *LetStmt:
			n.Children = append(n.Children, &ast.Node{Kind: "let", Value: s.Name})
		case *VarStmt:
			n.Children = append(n.Children, &ast.Node{Kind: "var", Value: s.Name})
		case *AssignStmt:
			n.Children = append(n.Children, &ast.Node{Kind: "assign", Value: s.Name})
		default:
			n.Children = append(n.Children, &ast.Node{Kind: "stmt"})
		}
	}
	n.Print("")
}

// --- Conversion helpers ---

func convertExpr(e *parser.Expr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if e == nil {
		return nil, "", fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary, env, vars)
}

func convertBinary(b *parser.BinaryExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if b == nil {
		return nil, "", fmt.Errorf("nil binary")
	}
	// Convert all operands first
	left, typ, err := convertUnary(b.Left, env, vars)
	if err != nil {
		return nil, "", err
	}
	operands := []Expr{left}
	typesList := []string{typ}
	ops := []string{}
	for _, op := range b.Right {
		right, rtyp, err := convertPostfix(op.Right, env, vars)
		if err != nil {
			return nil, "", err
		}
		operands = append(operands, right)
		typesList = append(typesList, rtyp)
		ops = append(ops, op.Op)
	}

	prec := func(op string) int {
		switch op {
		case "*", "/", "%":
			return 4
		case "+", "-":
			return 3
		case "==", "!=", "<", "<=", ">", ">=", "in":
			return 2
		case "&&":
			return 1
		case "||":
			return 0
		default:
			return -1
		}
	}

	// Shunting-yard like evaluation
	var exprStack []Expr
	var typeStack []string
	var opStack []string

	pushResult := func() {
		if len(opStack) == 0 {
			return
		}
		op := opStack[len(opStack)-1]
		opStack = opStack[:len(opStack)-1]
		if len(exprStack) < 2 || len(typeStack) < 2 {
			return
		}
		right := exprStack[len(exprStack)-1]
		exprStack = exprStack[:len(exprStack)-1]
		rtyp := typeStack[len(typeStack)-1]
		typeStack = typeStack[:len(typeStack)-1]
		left := exprStack[len(exprStack)-1]
		exprStack = exprStack[:len(exprStack)-1]
		ltyp := typeStack[len(typeStack)-1]
		typeStack = typeStack[:len(typeStack)-1]

		resTyp := ltyp
		switch op {
		case "+", "-", "*", "/", "%":
			if op == "+" && ltyp == "string" && rtyp == "string" {
				resTyp = "string"
			} else {
				resTyp = "int"
			}
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			resTyp = "bool"
		}

		var expr Expr
		if op == "in" {
			expr = &InExpr{Item: left, Coll: right, Typ: rtyp}
		} else {
			expr = &BinaryExpr{Left: left, Op: op, Right: right, Typ: resTyp}
		}
		exprStack = append(exprStack, expr)
		typeStack = append(typeStack, resTyp)
	}

	exprStack = append(exprStack, operands[0])
	typeStack = append(typeStack, typesList[0])
	for i, op := range ops {
		for len(opStack) > 0 && prec(opStack[len(opStack)-1]) >= prec(op) {
			pushResult()
		}
		opStack = append(opStack, op)
		exprStack = append(exprStack, operands[i+1])
		typeStack = append(typeStack, typesList[i+1])
	}
	for len(opStack) > 0 {
		pushResult()
	}
	if len(exprStack) != 1 {
		return nil, "", fmt.Errorf("binary conversion failed")
	}
	return exprStack[0], typeStack[0], nil
}

func convertUnary(u *parser.Unary, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if u == nil {
		return nil, "", fmt.Errorf("nil unary")
	}
	if len(u.Ops) > 0 {
		if len(u.Ops) == 1 && u.Ops[0] == "-" {
			expr, typ, err := convertPostfix(u.Value, env, vars)
			if err != nil {
				return nil, "", err
			}
			if typ != "int" {
				return nil, "", fmt.Errorf("unary - only for ints")
			}
			return &UnaryMinus{Expr: expr}, "int", nil
		}
		return nil, "", fmt.Errorf("unary ops not supported")
	}
	return convertPostfix(u.Value, env, vars)
}

func convertPostfix(p *parser.PostfixExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if p == nil {
		return nil, "", fmt.Errorf("nil postfix")
	}
	// Handle selector call patterns like s.contains("sub")
	if p.Target != nil && p.Target.Selector != nil &&
		len(p.Target.Selector.Tail) == 1 && p.Target.Selector.Tail[0] == "contains" &&
		len(p.Ops) == 1 && p.Ops[0].Call != nil {
		arg, at, err := convertExpr(p.Ops[0].Call.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		info := vars[p.Target.Selector.Root]
		if info.typ != "string" || at != "string" {
			return nil, "", fmt.Errorf("contains expects string")
		}
		root := &Name{Ident: p.Target.Selector.Root, Typ: info.typ, Ref: info.ref}
		return &StringContainsBuiltin{Str: root, Sub: arg}, "bool", nil
	}
	var expr Expr
	var typ string
	var err error
	if p.Target.Selector != nil {
		expr, typ, err = convertSelector(p.Target.Selector, env, vars)
		if err != nil {
			return nil, "", err
		}
	} else {
		expr, typ, err = convertPrimary(p.Target, env, vars)
		if err != nil {
			return nil, "", err
		}
	}
	for i := 0; i < len(p.Ops); {
		op := p.Ops[i]
		switch {
		case op.Field != nil && op.Field.Name == "contains":
			if i+1 >= len(p.Ops) || p.Ops[i+1].Call == nil {
				return nil, "", fmt.Errorf("contains must be called")
			}
			call := p.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, "", fmt.Errorf("contains expects 1 arg")
			}
			arg, aTyp, err := convertExpr(call.Args[0], env, vars)
			if err != nil {
				return nil, "", err
			}
			if typ != "string" || aTyp != "string" {
				return nil, "", fmt.Errorf("contains expects string")
			}
			expr = &StringContainsBuiltin{Str: expr, Sub: arg}
			typ = "bool"
			i += 2
			continue
		case op.Field != nil:
			if name, ok := expr.(*Name); ok {
				if info, ok := vars[name.Ident]; ok && info.group && op.Field.Name == "key" {
					expr = &Name{Ident: name.Ident + "_key", Typ: "string"}
					typ = "string"
					i++
					continue
				}
			}
			key := &StringLit{Value: op.Field.Name}
			if ft, ok := mapFieldType(typ, op.Field.Name); ok {
				expr = &MapIndexExpr{Map: expr, Key: key, Typ: ft}
				typ = ft
			} else {
				expr = &MapIndexExpr{Map: expr, Key: key, Typ: "int"}
				typ = "int"
			}
			i++
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil && op.Index.End == nil && op.Index.Step == nil:
			idxExpr, _, err := convertExpr(op.Index.Start, env, vars)
			if err != nil {
				return nil, "", err
			}
			if sl, ok := idxExpr.(*StringLit); ok {
				if ft, ok := mapFieldType(typ, sl.Value); ok {
					expr = &MapIndexExpr{Map: expr, Key: idxExpr, Typ: ft}
					typ = ft
				} else {
					expr = &MapIndexExpr{Map: expr, Key: idxExpr, Typ: "int"}
					typ = "int"
				}
			} else if strings.HasPrefix(typ, "map-") {
				parts := strings.Split(typ, "-")
				valTyp := "int"
				if len(parts) >= 3 {
					valTyp = parts[2]
				}
				expr = &MapIndexExpr{Map: expr, Key: idxExpr, Typ: valTyp}
				typ = valTyp
			} else if typ == "map" || strings.HasPrefix(typ, "map{") {
				expr = &MapIndexExpr{Map: expr, Key: idxExpr, Typ: "int"}
				typ = "int"
			} else {
				expr = &IndexExpr{Col: expr, Index: idxExpr, Typ: typ}
				if typ == "string" {
					typ = "string"
				} else {
					typ = "int"
				}
			}
			i++
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			startExpr, _, err := convertExpr(op.Index.Start, env, vars)
			if err != nil {
				return nil, "", err
			}
			endExpr, _, err := convertExpr(op.Index.End, env, vars)
			if err != nil {
				return nil, "", err
			}
			expr = &SliceExpr{Col: expr, Start: startExpr, End: endExpr, Typ: typ}
			i++
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			target := *op.Cast.Type.Simple
			if typ == "string" && target == "int" {
				expr = &CastExpr{Expr: expr, Type: target}
				typ = target
				i++
				continue
			}
			return nil, "", fmt.Errorf("unsupported cast")
		default:
			return nil, "", fmt.Errorf("postfix op not supported")
		}
	}
	return expr, typ, nil
}

func convertSelector(sel *parser.SelectorExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	info := vars[sel.Root]
	expr := Expr(&Name{Ident: sel.Root, Typ: info.typ, Ref: info.ref})
	typ := info.typ
	for _, t := range sel.Tail {
		if info.group && t == "key" {
			expr = &Name{Ident: sel.Root + "_key", Typ: "string"}
			typ = "string"
			continue
		}
		key := &StringLit{Value: t}
		if ft, ok := mapFieldType(typ, t); ok {
			expr = &MapIndexExpr{Map: expr, Key: key, Typ: ft}
			typ = ft
		} else {
			expr = &MapIndexExpr{Map: expr, Key: key, Typ: "int"}
			typ = "int"
		}
	}
	return expr, typ, nil
}

func convertPrimary(p *parser.Primary, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Group != nil:
		return convertExpr(p.Group, env, vars)
	case p.If != nil:
		return convertIf(p.If, env, vars)
	case p.Call != nil:
		return convertCall(p.Call, env, vars)
	case p.Match != nil:
		return convertMatch(p.Match, env, vars)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		elemTyp := ""
		for i, e := range p.List.Elems {
			ex, t, err := convertExpr(e, env, vars)
			if err != nil {
				return nil, "", err
			}
			if i == 0 {
				elemTyp = t
			}
			elems[i] = ex
		}
		listTyp := "list"
		if elemTyp != "" {
			listTyp = "list-" + elemTyp
		}
		return &ListLit{Elems: elems}, listTyp, nil
	case p.Map != nil:
		items := make([]MapEntry, len(p.Map.Items))
		fieldTypes := []string{}
		for i, it := range p.Map.Items {
			k, _, err := convertExpr(it.Key, env, vars)
			if err != nil {
				return nil, "", err
			}
			if n, ok := k.(*Name); ok {
				k = &StringLit{Value: n.Ident}
			}
			v, vt, err := convertExpr(it.Value, env, vars)
			if err != nil {
				return nil, "", err
			}
			if ks, ok := k.(*StringLit); ok {
				fieldTypes = append(fieldTypes, ks.Value+":"+vt)
			} else {
				fieldTypes = nil
			}
			items[i] = MapEntry{Key: k, Value: v}
		}
		typ := "map"
		if fieldTypes != nil {
			typ = "map{" + strings.Join(fieldTypes, ",") + "}"
		}
		return &MapLit{Items: items}, typ, nil
	case p.Query != nil:
		qe, qtyp, err := convertQueryExpr(p.Query, env, vars)
		if err != nil {
			return nil, "", err
		}
		return qe, qtyp, nil
	case p.FunExpr != nil:
		return convertFunExpr(p.FunExpr, env, vars)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			info := vars[p.Selector.Root]
			return &Name{Ident: p.Selector.Root, Typ: info.typ, Ref: info.ref}, info.typ, nil
		}
		return convertSelector(p.Selector, env, vars)
	}
	return nil, "", fmt.Errorf("unsupported expression")
}

func convertLiteral(l *parser.Literal) (Expr, string, error) {
	if l.Int != nil {
		return &IntLit{Value: int(*l.Int)}, "int", nil
	}
	if l.Bool != nil {
		return &BoolLit{Value: bool(*l.Bool)}, "bool", nil
	}
	if l.Str != nil {
		return &StringLit{Value: *l.Str}, "string", nil
	}
	return nil, "", fmt.Errorf("unsupported literal")
}

func convertIf(ifx *parser.IfExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	cond, _, err := convertExpr(ifx.Cond, env, vars)
	if err != nil {
		return nil, "", err
	}
	thenExpr, thenTyp, err := convertExpr(ifx.Then, env, vars)
	if err != nil {
		return nil, "", err
	}
	var elseExpr Expr
	var elseTyp string
	if ifx.ElseIf != nil {
		elseExpr, elseTyp, err = convertIf(ifx.ElseIf, env, vars)
		if err != nil {
			return nil, "", err
		}
	} else if ifx.Else != nil {
		elseExpr, elseTyp, err = convertExpr(ifx.Else, env, vars)
		if err != nil {
			return nil, "", err
		}
	} else {
		return nil, "", fmt.Errorf("if expression missing else")
	}
	if thenTyp != elseTyp {
		return nil, "", fmt.Errorf("if branches have different types")
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr, Typ: thenTyp}, thenTyp, nil
}

func convertMatch(m *parser.MatchExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	target, _, err := convertExpr(m.Target, env, vars)
	if err != nil {
		return nil, "", err
	}
	arms := make([]MatchArm, len(m.Cases))
	var typ string
	for i, c := range m.Cases {
		var pat Expr
		if !isUnderscoreExpr(c.Pattern) {
			p, _, err := convertExpr(c.Pattern, env, vars)
			if err != nil {
				return nil, "", err
			}
			pat = p
		}
		res, rtyp, err := convertExpr(c.Result, env, vars)
		if err != nil {
			return nil, "", err
		}
		if i == 0 {
			typ = rtyp
		}
		arms[i] = MatchArm{Pattern: pat, Result: res}
	}
	if typ == "" {
		typ = "int"
	}
	return &MatchExpr{Target: target, Arms: arms, Typ: typ}, typ, nil
}

func convertFunExpr(fn *parser.FunExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	child := env.Copy()
	fnVars := make(map[string]VarInfo)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := "int"
		if p.Type != nil && p.Type.Simple != nil {
			typ = *p.Type.Simple
		}
		fnVars[p.Name] = VarInfo{typ: typ}
		params[i] = p.Name
	}
	var retExpr Expr
	if fn.ExprBody != nil {
		ex, _, err := convertExpr(fn.ExprBody, child, fnVars)
		if err != nil {
			return nil, "", err
		}
		retExpr = ex
	} else if len(fn.BlockBody) > 0 {
		body, err := transpileStmts(fn.BlockBody, child, fnVars)
		if err != nil {
			return nil, "", err
		}
		var ret Expr
		if last := fn.BlockBody[len(fn.BlockBody)-1]; last.Return != nil && last.Return.Value != nil {
			r, _, err := convertExpr(last.Return.Value, child, fnVars)
			if err != nil {
				return nil, "", err
			}
			ret = r
		}
		return &FuncExpr{Params: params, Body: body, Ret: ret}, "func", nil
	}
	return &FuncExpr{Params: params, Body: nil, Ret: retExpr}, "func", nil
}

func convertQueryExpr(q *parser.QueryExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	loops := []queryLoop{}
	src, srcTyp, err := convertExpr(q.Source, env, vars)
	if err != nil {
		return nil, "", err
	}
	loops = append(loops, queryLoop{Name: q.Var, Src: src})
	qVars := map[string]VarInfo{}
	for k, v := range vars {
		qVars[k] = v
	}
	var whereExpr Expr
	vtyp := "map"
	if strings.HasPrefix(srcTyp, "list-") {
		vtyp = strings.TrimPrefix(srcTyp, "list-")
	}
	qVars[q.Var] = VarInfo{typ: vtyp}
	for _, f := range q.Froms {
		fs, fsTyp, err := convertExpr(f.Src, env, vars)
		if err != nil {
			return nil, "", err
		}
		loops = append(loops, queryLoop{Name: f.Var, Src: fs})
		ftyp := "map"
		if strings.HasPrefix(fsTyp, "list-") {
			ftyp = strings.TrimPrefix(fsTyp, "list-")
		}
		qVars[f.Var] = VarInfo{typ: ftyp}
	}
	for _, j := range q.Joins {
		js, jsTyp, err := convertExpr(j.Src, env, qVars)
		if err != nil {
			return nil, "", err
		}
		loops = append(loops, queryLoop{Name: j.Var, Src: js})
		jtyp := "map"
		if strings.HasPrefix(jsTyp, "list-") {
			jtyp = strings.TrimPrefix(jsTyp, "list-")
		}
		qVars[j.Var] = VarInfo{typ: jtyp}
		onExpr, _, err := convertExpr(j.On, env, qVars)
		if err != nil {
			return nil, "", err
		}
		if whereExpr == nil {
			whereExpr = onExpr
		} else {
			whereExpr = &BinaryExpr{Left: whereExpr, Op: "&&", Right: onExpr, Typ: "bool"}
		}
	}
	if q.Where != nil {
		cond, _, err := convertExpr(q.Where, env, qVars)
		if err != nil {
			return nil, "", err
		}
		if whereExpr == nil {
			whereExpr = cond
		} else {
			whereExpr = &BinaryExpr{Left: whereExpr, Op: "&&", Right: cond, Typ: "bool"}
		}
	}
	if q.Group != nil {
		keyExpr, _, err := convertExpr(q.Group.Exprs[0], env, qVars)
		if err != nil {
			return nil, "", err
		}
		gVars := map[string]VarInfo{}
		for k, v := range qVars {
			gVars[k] = v
		}
		gVars[q.Group.Name] = VarInfo{typ: "list", group: true}
		gVars[q.Group.Name+"Key"] = VarInfo{typ: "string"}
		var havingExpr Expr
		if q.Group.Having != nil {
			var err error
			havingExpr, _, err = convertExpr(q.Group.Having, env, gVars)
			if err != nil {
				return nil, "", err
			}
		}
		sel, selTyp, err := convertExpr(q.Select, env, gVars)
		if err != nil {
			return nil, "", err
		}
		retTyp := "list"
		if selTyp != "" {
			retTyp = "list-" + selTyp
		}
		return &GroupByQueryExpr{Loops: loops, Where: whereExpr, Key: keyExpr, Into: q.Group.Name, Having: havingExpr, Select: sel}, retTyp, nil
	}
	var sortExpr Expr
	var desc bool
	if q.Sort != nil {
		sx, _, err := convertExpr(q.Sort, env, qVars)
		if err != nil {
			return nil, "", err
		}
		if um, ok := sx.(*UnaryMinus); ok {
			sortExpr = um.Expr
			desc = true
		} else {
			sortExpr = sx
		}
	}
	var skipExpr Expr
	if q.Skip != nil {
		var err error
		skipExpr, _, err = convertExpr(q.Skip, env, qVars)
		if err != nil {
			return nil, "", err
		}
	}
	var takeExpr Expr
	if q.Take != nil {
		var err error
		takeExpr, _, err = convertExpr(q.Take, env, qVars)
		if err != nil {
			return nil, "", err
		}
	}
	sel, selTyp, err := convertExpr(q.Select, env, qVars)
	if err != nil {
		return nil, "", err
	}
	retTyp := "list"
	if selTyp != "" {
		retTyp = "list-" + selTyp
	}
	return &QueryExpr{Loops: loops, Where: whereExpr, Sort: sortExpr, Desc: desc, Skip: skipExpr, Take: takeExpr, Select: sel}, retTyp, nil
}

func convertCall(c *parser.CallExpr, env *types.Env, vars map[string]VarInfo) (Expr, string, error) {
	if c.Func == "str" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "int" {
			return nil, "", fmt.Errorf("str only supports int")
		}
		return &StrBuiltin{Expr: arg}, "string", nil
	}
	if c.Func == "len" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		switch {
		case typ == "string" || typ == "list" || typ == "map" || strings.HasPrefix(typ, "map-"):
			return &LenBuiltin{Arg: arg, Typ: typ}, "int", nil
		default:
			return nil, "", fmt.Errorf("len unsupported for %s", typ)
		}
	}
	if c.Func == "substring" && len(c.Args) == 3 {
		strArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "string" {
			return nil, "", fmt.Errorf("substring expects string")
		}
		startArg, typ2, err := convertExpr(c.Args[1], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ2 != "int" {
			return nil, "", fmt.Errorf("substring start expects int")
		}
		endArg, typ3, err := convertExpr(c.Args[2], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ3 != "int" {
			return nil, "", fmt.Errorf("substring end expects int")
		}
		return &SubstringBuiltin{Str: strArg, Start: startArg, End: endArg}, "string", nil
	}
	if c.Func == "append" && len(c.Args) == 2 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("append expects list")
		}
		valArg, _, err := convertExpr(c.Args[1], env, vars)
		if err != nil {
			return nil, "", err
		}
		return &AppendBuiltin{List: listArg, Value: valArg}, "list", nil
	}
	if c.Func == "sum" && len(c.Args) == 1 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("sum expects list")
		}
		return &SumBuiltin{List: listArg}, "int", nil
	}
	if c.Func == "count" && len(c.Args) == 1 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("count expects list")
		}
		return &CountBuiltin{List: listArg}, "int", nil
	}
	if c.Func == "avg" && len(c.Args) == 1 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("avg expects list")
		}
		return &AvgBuiltin{List: listArg}, "int", nil
	}
	if c.Func == "min" && len(c.Args) == 1 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("min expects list")
		}
		return &MinBuiltin{List: listArg}, "int", nil
	}
	if c.Func == "max" && len(c.Args) == 1 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("max expects list")
		}
		return &MaxBuiltin{List: listArg}, "int", nil
	}
	if c.Func == "values" && len(c.Args) == 1 {
		mapArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "map" && !strings.HasPrefix(typ, "map-") {
			return nil, "", fmt.Errorf("values expects map")
		}
		return &ValuesBuiltin{Map: mapArg}, "list", nil
	}
	if fn, ok := env.GetFunc(c.Func); ok {
		ret := "int"
		if fn.Return != nil {
			if fn.Return.Simple != nil {
				ret = *fn.Return.Simple
			} else if fn.Return.Fun != nil {
				ret = "func"
			}
		}
		args := make([]Expr, len(c.Args))
		for i, a := range c.Args {
			ex, _, err := convertExpr(a, env, vars)
			if err != nil {
				return nil, "", err
			}
			args[i] = ex
		}
		return &FuncCall{Name: c.Func, Args: args, Ret: ret}, ret, nil
	}
	if v, ok := vars[c.Func]; ok && v.typ == "func" {
		args := make([]Expr, len(c.Args))
		for i, a := range c.Args {
			ex, _, err := convertExpr(a, env, vars)
			if err != nil {
				return nil, "", err
			}
			args[i] = ex
		}
		return &FuncCall{Name: c.Func, Args: args, Ret: "int"}, "int", nil
	}
	return nil, "", fmt.Errorf("call %s not supported", c.Func)
}

func buildListUpdate(list Expr, indexes []Expr, val Expr) Expr {
	idx := indexes[0]
	if len(indexes) == 1 {
		return &ListUpdateExpr{List: list, Index: idx, Value: val}
	}
	inner := buildListUpdate(&IndexExpr{Col: list, Index: idx, Typ: "list"}, indexes[1:], val)
	return &ListUpdateExpr{List: list, Index: idx, Value: inner}
}

func buildMapUpdate(mp Expr, keys []Expr, val Expr) Expr {
	key := keys[0]
	if len(keys) == 1 {
		return &MapUpdateExpr{Map: mp, Key: key, Value: val}
	}
	inner := buildMapUpdate(&MapIndexExpr{Map: mp, Key: key, Typ: "map"}, keys[1:], val)
	return &MapUpdateExpr{Map: mp, Key: key, Value: inner}
}

func renderExpr(e Expr) string {
	var b bytes.Buffer
	e.emit(&b)
	return b.String()
}
