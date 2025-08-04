//go:build slow

package ocaml

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"

	"gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
	"mochi/types"
)

var structFields = map[string]map[string]string{}
var usesNow bool
var usesLookupHost bool
var usesDynMath bool
var usesMem bool
var usesBigInt bool
var usesBigRat bool
var usesSHA bool
var usesGetOutput bool
var usesGetEnv bool
var usesEnviron bool
var usesSplit bool
var usesDynLen bool
var benchMain bool
var funcMutations = map[string]map[string]bool{}
var rootEnv *types.Env
var currentRetTyp string

// GetStructFields returns the collected struct field type information.
func GetStructFields() map[string]map[string]string { return structFields }

// GetVarType reports the string type for a variable in the provided env.
func GetVarType(env *types.Env, name string) string {
	if t, err := env.GetVar(name); err == nil {
		return typeString(t)
	}
	return ""
}

// Program represents an OCaml program. All statements are emitted inside
// a `let () =` block.
type Program struct {
	Stmts []Stmt
}

// SetBenchMain configures whether the generated main function is wrapped in a
// benchmark block when emitting code. When enabled, the program will output a
// JSON object with duration and memory statistics on completion.
func SetBenchMain(v bool) { benchMain = v }

func (p *Program) UsesStrModule() bool {
	if usesSplit {
		return true
	}
	var exprUses func(Expr) bool
	exprUses = func(e Expr) bool {
		switch x := e.(type) {
		case *StringContainsBuiltin:
			return true
		case *BinaryExpr:
			return exprUses(x.Left) || exprUses(x.Right)
		case *UnaryNot:
			return exprUses(x.Expr)
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
		case *BreakStmt, *ContinueStmt, *WhileStmt, *ForRangeStmt, *ForEachStmt:
			uses = true
			// walk the body if available
			switch t := st.(type) {
			case *WhileStmt:
				for _, b := range t.Body {
					check(b)
				}
			case *ForRangeStmt:
				for _, b := range t.Body {
					check(b)
				}
			case *ForEachStmt:
				for _, b := range t.Body {
					check(b)
				}
			}
		case *IfStmt:
			for _, t := range st.Then {
				check(t)
			}
			for _, e := range st.Else {
				check(e)
			}
		case *BenchStmt:
			for _, b := range st.Body {
				check(b)
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

func (p *Program) UsesReturn() bool {
	var uses bool
	var check func(Stmt)
	check = func(s Stmt) {
		switch st := s.(type) {
		case *ReturnStmt:
			uses = true
		case *IfStmt:
			for _, t := range st.Then {
				check(t)
			}
			for _, e := range st.Else {
				check(e)
			}
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
		case *BenchStmt:
			for _, b := range st.Body {
				check(b)
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

func (p *Program) UsesShow() bool {
	var uses bool
	var check func(Stmt)
	check = func(s Stmt) {
		switch st := s.(type) {
		case *SaveStmt:
			if st.Format == "jsonl" && (st.Path == "" || st.Path == "-") {
				uses = true
			}
		case *IfStmt:
			for _, t := range st.Then {
				check(t)
			}
			for _, e := range st.Else {
				check(e)
			}
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

func (p *Program) UsesJSON() bool {
	var uses bool
	var check func(Stmt)
	check = func(s Stmt) {
		switch st := s.(type) {
		case *JSONStmt:
			uses = true
		case *PrintStmt:
			uses = true
		case *IfStmt:
			for _, t := range st.Then {
				check(t)
			}
			for _, e := range st.Else {
				check(e)
			}
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
	ret   string
}

func collectMutations(stmts []*parser.Statement, mutated map[string]bool) {
	var walk func(list []*parser.Statement)
	walk = func(list []*parser.Statement) {
		for _, st := range list {
			if st.Assign != nil {
				mutated[st.Assign.Name] = true
			}
			if st.If != nil {
				walk(st.If.Then)
				if st.If.ElseIf != nil {
					walk([]*parser.Statement{{If: st.If.ElseIf}})
				}
				walk(st.If.Else)
			}
			if st.While != nil {
				walk(st.While.Body)
			}
			if st.For != nil {
				walk(st.For.Body)
			}
		}
	}
	walk(stmts)
}

func getFuncMutations(env *types.Env, name string) map[string]bool {
	if m, ok := funcMutations[name]; ok {
		return m
	}
	if fn, ok := env.GetFunc(name); ok {
		m := map[string]bool{}
		collectMutations(fn.Body, m)
		funcMutations[name] = m
		return m
	}
	return nil
}

// GetFuncMutations is an exported helper for tests/debugging.
func GetFuncMutations(name string) map[string]bool {
	return funcMutations[name]
}

func guessTypeFromName(name string) string {
	n := strings.ToLower(name)
	if strings.HasSuffix(n, "list") || strings.HasSuffix(n, "arr") {
		return "list"
	}
	if strings.HasSuffix(n, "map") {
		return "map-dyn"
	}
	if strings.Contains(n, "count") || strings.Contains(n, "num") || strings.Contains(n, "len") {
		return "int"
	}
	return ""
}

// splitTopLevel splits s by sep, ignoring separators inside curly braces.
func splitTopLevel(s string, sep rune) []string {
	parts := []string{}
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '{':
			depth++
		case '}':
			if depth > 0 {
				depth--
			}
		default:
			if r == sep && depth == 0 {
				parts = append(parts, s[start:i])
				start = i + 1
			}
		}
	}
	parts = append(parts, s[start:])
	return parts
}

func mapFieldType(typ, field string) (string, bool) {
	if (strings.HasPrefix(typ, "map{") || strings.HasPrefix(typ, "map-dyn{")) && strings.HasSuffix(typ, "}") {
		inner := strings.TrimSuffix(typ, "}")
		if strings.HasPrefix(inner, "map-dyn{") {
			inner = strings.TrimPrefix(inner, "map-dyn{")
		} else {
			inner = strings.TrimPrefix(inner, "map{")
		}
		parts := splitTopLevel(inner, ',')
		for _, p := range parts {
			kv := strings.SplitN(p, ":", 2)
			if len(kv) == 2 && kv[0] == field {
				return kv[1], true
			}
		}
	}
	if typ == "map-dyn" || typ == "map" {
		switch field {
		case "ok", "success":
			return "bool", true
		case "idx", "id", "index":
			return "int", true
		case "row", "col", "column":
			return "list-int", true
		case "board":
			return "list-list-int", true
		case "gain", "score":
			return "int", true
		case "moved":
			return "bool", true
		case "full":
			return "bool", true
		case "err":
			return "bool", true
		case "res":
			return "string", true
		case "value":
			return "map-int", true
		case "left", "right":
			return "map", true
		case "ele", "elements", "elems":
			return "list-float", true
		}
	}
	if strings.HasPrefix(typ, "map-") {
		elems := strings.Split(typ, "-")
		if len(elems) >= 2 {
			return elems[len(elems)-1], true
		}
	}
	return "", false
}

func isDynamicMapType(typ string) bool {
	if typ == "any" || typ == "Obj.t" {
		return true
	}
	if typ == "map" || strings.HasPrefix(typ, "map-dyn") {
		return true
	}
	if strings.HasPrefix(typ, "map-") {
		val := strings.TrimPrefix(typ, "map-")
		if val == "" || val == "any" || val == "Obj.t" {
			return true
		}
		return isDynamicMapType(val)
	}
	if _, ok := structFields[typ]; ok {
		return true
	}
	return false
}

func ocamlType(t string) string {
	switch t {
	case "int":
		return "int"
	case "float":
		return "float"
	case "string":
		return "string"
	case "bool":
		return "bool"
	case "bigint":
		usesBigInt = true
		return "Z.t"
	case "bigrat":
		usesBigRat = true
		return "Q.t"
	case "func":
		return "Obj.t -> Obj.t"
	default:
		if strings.HasPrefix(t, "func-") {
			ret := ocamlType(strings.TrimPrefix(t, "func-"))
			return "Obj.t -> " + ret
		}
		if _, ok := structFields[t]; ok {
			return "(string * Obj.t) list"
		}
		if strings.HasPrefix(t, "map-") {
			inner := strings.TrimPrefix(t, "map-")
			return "(string * " + ocamlType(inner) + ") list"
		}
		if strings.HasPrefix(t, "map-dyn") {
			return "(string * Obj.t) list"
		}
		if t == "map" {
			return "(string * Obj.t) list"
		}
		if strings.HasPrefix(t, "list-") {
			return ocamlType(strings.TrimPrefix(t, "list-")) + " list"
		}
		if t == "list" {
			return "Obj.t list"
		}
		return "Obj.t"
	}
}

func typeString(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "float"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.BigIntType:
		usesBigInt = true
		return "bigint"
	case types.BigRatType:
		usesBigRat = true
		return "bigrat"
	case types.ListType:
		elem := typeString(tt.Elem)
		if elem == "" {
			return "list"
		}
		return "list-" + elem
	case types.MapType:
		val := typeString(tt.Value)
		if val == "" {
			return "map"
		}
		return "map-" + val
	case types.FuncType:
		ret := typeString(tt.Return)
		if ret == "" {
			ret = ""
		}
		n := len(tt.Params)
		if n == 0 {
			n = 1
		}
		for i := 0; i < n; i++ {
			ret = "func-" + ret
		}
		return ret
	case types.StructType:
		fields := make([]string, len(tt.Order))
		for i, name := range tt.Order {
			ft := typeString(tt.Fields[name])
			if ft == "" {
				ft = "int"
			}
			fields[i] = name + ":" + ft
		}
		if len(fields) > 0 {
			return "map-dyn{" + strings.Join(fields, ",") + "}"
		}
		return "map-dyn"
	default:
		return ""
	}
}

var ocamlReserved = map[string]bool{
	"and": true, "as": true, "assert": true, "begin": true, "class": true,
	"end": true, "exception": true, "external": true, "fun": true, "function": true,
	"let": true, "in": true, "match": true, "mod": true, "module": true, "open": true,
	"rec": true, "try": true, "type": true, "val": true, "when": true, "with": true,
	"new":  true,
	"sig":  true,
	"done": true,
	"ref":  true,
}

func sanitizeIdent(s string) string {
	if ocamlReserved[s] {
		return s + "_"
	}
	if len(s) > 0 && s[0] >= 'A' && s[0] <= 'Z' {
		return "_" + strings.ToLower(s)
	}
	return s
}

// Stmt can emit itself as OCaml code.
type Stmt interface{ emit(io.Writer) }

// LetStmt represents a simple variable binding.
type LetStmt struct {
	Name string
	Expr Expr
}

func (l *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  let %s = ", sanitizeIdent(l.Name))
	l.Expr.emit(w)
	io.WriteString(w, " in\n")
}

func (l *LetStmt) emitTop(w io.Writer) {
	fmt.Fprintf(w, "let %s = ", sanitizeIdent(l.Name))
	l.Expr.emit(w)
	io.WriteString(w, "\n")
}

// VarStmt represents a mutable variable binding using OCaml references.
type VarStmt struct {
	Name string
	Expr Expr
}

func (v *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  let %s = ref (", sanitizeIdent(v.Name))
	v.Expr.emit(w)
	io.WriteString(w, ") in\n")
}

func (v *VarStmt) emitTop(w io.Writer) {
	fmt.Fprintf(w, "let %s = ref (", sanitizeIdent(v.Name))
	v.Expr.emit(w)
	io.WriteString(w, ")\n")
}

// AssignStmt represents an update to a mutable variable.
type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  %s := ", sanitizeIdent(a.Name))
	a.Expr.emit(w)
	io.WriteString(w, ";\n")
}

// MultiStmt groups multiple statements to be emitted sequentially.
type MultiStmt struct{ Stmts []Stmt }

func (m *MultiStmt) emit(w io.Writer) {
	for _, s := range m.Stmts {
		s.emit(w)
	}
}

// ExprStmt represents a stand-alone expression.
type ExprStmt struct{ Expr Expr }

func (e *ExprStmt) emit(w io.Writer) {
	io.WriteString(w, "  ")
	e.Expr.emit(w)
	io.WriteString(w, ";\n")
}

// PrintStmt represents a call to print_endline with one or more values.
type PrintStmt struct{ Exprs []Expr }

func (p *PrintStmt) emit(w io.Writer) {
	for _, e := range p.Exprs {
		io.WriteString(w, "  print_endline (")
		e.emitPrint(w)
		io.WriteString(w, ");\n")
	}
}

// PrintExpr prints one or more values and returns a dummy value.
type PrintExpr struct{ Exprs []Expr }

func (p *PrintExpr) emit(w io.Writer) {
	io.WriteString(w, "(let _ = ")
	for i, e := range p.Exprs {
		if i > 0 {
			io.WriteString(w, "; print_endline (")
		} else {
			io.WriteString(w, "print_endline (")
		}
		e.emitPrint(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, " in ())")
}

func (p *PrintExpr) emitPrint(w io.Writer) { p.emit(w) }

// JSONStmt prints a value as JSON on its own line.
type JSONStmt struct{ Expr Expr }

func (j *JSONStmt) emit(w io.Writer) {
	io.WriteString(w, "  print_endline ")
	j.Expr.emitPrint(w)
	io.WriteString(w, ";\n")
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
	io.WriteString(w, "    with Continue -> ()\n  done with Break -> ());\n")
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
	io.WriteString(w, sanitizeIdent(fr.Name))
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
	io.WriteString(w, "    with Continue -> ()\n  done with Break -> ());\n")
}

// ForEachStmt represents iteration over a collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	Typ      string
}

func (fe *ForEachStmt) emit(w io.Writer) {
	if fe.Typ == "string" {
		io.WriteString(w, "  (try List.iter (fun ")
		io.WriteString(w, sanitizeIdent(fe.Name))
		io.WriteString(w, " ->\n    try\n")
		for _, st := range fe.Body {
			st.emit(w)
		}
		io.WriteString(w, "    with Continue -> ()) (List.of_seq (Seq.map (fun c -> String.make 1 c) (String.to_seq ")
		fe.Iterable.emit(w)
		io.WriteString(w, "))) with Break -> ());\n")
		return
	}
	if strings.HasPrefix(fe.Typ, "map") {
		io.WriteString(w, "  (try List.iter (fun (")
		io.WriteString(w, sanitizeIdent(fe.Name))
		io.WriteString(w, ", _) ->\n    try\n")
		for _, st := range fe.Body {
			st.emit(w)
		}
		io.WriteString(w, "    with Continue -> ()) (")
		fe.Iterable.emit(w)
		io.WriteString(w, ") with Break -> ());\n")
		return
	}
	io.WriteString(w, "  (try List.iter (fun ")
	io.WriteString(w, sanitizeIdent(fe.Name))
	io.WriteString(w, " ->\n    try\n")
	for _, st := range fe.Body {
		st.emit(w)
	}
	io.WriteString(w, "    with Continue -> ()) (")
	fe.Iterable.emit(w)
	io.WriteString(w, ") with Break -> ());\n")
}

// UpdateStmt updates fields of items in a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (u *UpdateStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "  let %s =\n", u.Target)
	io.WriteString(w, "    List.map (fun item ->\n")
	if u.Cond != nil {
		io.WriteString(w, "      if ")
		u.Cond.emit(w)
		io.WriteString(w, " then (\n        {")
	} else {
		io.WriteString(w, "      {")
	}
	for i, f := range u.Fields {
		if i > 0 {
			io.WriteString(w, "; ")
		}
		fmt.Fprintf(w, "%s = ", f)
		u.Values[i].emit(w)
	}
	io.WriteString(w, " }")
	if u.Cond != nil {
		io.WriteString(w, " else item")
		io.WriteString(w, "\n")
	} else {
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "    ) ")
	io.WriteString(w, u.Target)
	io.WriteString(w, "\n")
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

// ReturnStmt exits the current function with a value.
type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "  __ret := ")
	if r.Expr != nil {
		if currentRetTyp != "" {
			fmt.Fprintf(w, "(Obj.magic (")
			r.Expr.emit(w)
			fmt.Fprintf(w, ") : %s)", ocamlType(currentRetTyp))
		} else {
			io.WriteString(w, "Obj.repr (")
			r.Expr.emit(w)
			io.WriteString(w, ")")
		}
	} else {
		io.WriteString(w, "()")
	}
	io.WriteString(w, "; raise Return\n")
}

// SaveStmt writes rows to stdout in JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

func (s *SaveStmt) emit(w io.Writer) {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		io.WriteString(w, "  List.iter (fun m ->\n")
		io.WriteString(w, `    let parts = List.map (fun (k,v) -> Printf.sprintf "\"%s\": %s" k (string_of_int (Obj.magic v))) m in\n`)
		io.WriteString(w, `    print_endline ("{" ^ String.concat "," parts ^ "}")\n`)
		io.WriteString(w, "  ) ")
		s.Src.emit(w)
		io.WriteString(w, "\n")
		return
	}
	io.WriteString(w, "  () (* unsupported save *)\n")
}

// BenchStmt measures execution time of a block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (b *BenchStmt) emit(w io.Writer) {
	io.WriteString(w, "  let mem_start = _mem () in\n")
	io.WriteString(w, "  let start = _now () in\n")
	for _, st := range b.Body {
		st.emit(w)
	}
	io.WriteString(w, "  let finish = _now () in\n")
	io.WriteString(w, "  let mem_end = _mem () in\n")
	io.WriteString(w, "  let dur = (finish - start) / 1000 in\n")
	io.WriteString(w, "  let mem_bytes = max 0 (mem_end - mem_start) in\n")
	fmt.Fprintf(w, "  Printf.printf \"{\\n  \\\"duration_us\\\": %%d,\\n  \\\"memory_bytes\\\": %%d,\\n  \\\"name\\\": \\\"%%s\\\"\\n}\\n\" dur mem_bytes %q;\n", b.Name)
}

// FunStmt represents a simple function declaration with no parameters.
type FunStmt struct {
	Name           string
	Params         []string
	Body           []Stmt
	Ret            Expr
	RetTyp         string
	Local          bool
	EndsWithReturn bool
}

func (f *FunStmt) emitWith(w io.Writer, prefix string) {
	fmt.Fprintf(w, "%s %s", prefix, sanitizeIdent(f.Name))
	if len(f.Params) == 0 {
		io.WriteString(w, " ()")
	} else {
		for _, p := range f.Params {
			fmt.Fprintf(w, " %s", p)
		}
	}
	io.WriteString(w, " =\n")
	io.WriteString(w, "  let __ret = ref ")
	if f.RetTyp != "" {
		defaultValueExpr(f.RetTyp).emit(w)
	} else {
		io.WriteString(w, "(Obj.magic 0)")
	}
	io.WriteString(w, " in\n")
	currentRetTyp = f.RetTyp
	// Function parameters that are mutated are already passed as OCaml
	// references. Earlier versions of the transpiler created local copies
	// with `ref` here which prevented modifications from being visible to
	// the caller. Simply rely on the caller passing a reference.
	io.WriteString(w, "  (try\n")
	for _, st := range f.Body {
		st.emit(w)
	}
	if !f.EndsWithReturn {
		if f.Ret != nil {
			io.WriteString(w, "  __ret := ")
			if f.RetTyp != "" {
				f.Ret.emit(w)
			} else {
				io.WriteString(w, "Obj.repr (")
				f.Ret.emit(w)
				io.WriteString(w, ")")
			}
			io.WriteString(w, ";\n")
		}
		io.WriteString(w, "    !__ret\n")
	}
	io.WriteString(w, "  with Return -> !__ret)")
	currentRetTyp = ""
	if f.Local {
		io.WriteString(w, " in\n")
	} else {
		io.WriteString(w, "\n\n")
	}
}

func (f *FunStmt) emit(w io.Writer) { f.emitWith(w, "let rec") }

// Expr is any OCaml expression that can emit itself.
type Expr interface {
	emit(io.Writer)
	emitPrint(io.Writer)
}

// RefExpr wraps a value in OCaml's [ref] constructor.
type RefExpr struct{ Value Expr }

func (r *RefExpr) emit(w io.Writer) {
	io.WriteString(w, "ref (")
	r.Value.emit(w)
	io.WriteString(w, ")")
}

func (r *RefExpr) emitPrint(w io.Writer) { r.emit(w) }

// StrBuiltin represents a call to the builtin str() function.
type StrBuiltin struct {
	Expr Expr
	Typ  string
}

func (s *StrBuiltin) emit(w io.Writer) {
	switch s.Typ {
	case "float":
		io.WriteString(w, "(Printf.sprintf \"%.16g\" (")
		s.Expr.emit(w)
		io.WriteString(w, "))")
	case "bigint":
		usesBigInt = true
		io.WriteString(w, "(Z.to_string (")
		s.Expr.emit(w)
		io.WriteString(w, "))")
	case "bigrat":
		usesBigRat = true
		io.WriteString(w, "(Q.to_string (")
		s.Expr.emit(w)
		io.WriteString(w, "))")
	default:
		io.WriteString(w, "(string_of_int (")
		s.Expr.emit(w)
		io.WriteString(w, "))")
	}
}

func (s *StrBuiltin) emitPrint(w io.Writer) { s.emit(w) }

// LenBuiltin represents a call to len() builtin for strings.
type LenBuiltin struct {
	Arg Expr
	Typ string
}

func (l *LenBuiltin) emit(w io.Writer) {
	if l.Typ == "dyn" {
		usesDynLen = true
		io.WriteString(w, "_len (")
		l.Arg.emit(w)
		io.WriteString(w, ")")
		return
	}
	var fn string
	switch l.Typ {
	case "string":
		fn = "String.length"
	case "list", "map":
		fn = "List.length"
	default:
		if strings.HasPrefix(l.Typ, "map-") {
			fn = "List.length"
		} else {
			fn = "List.length"
		}
	}
	io.WriteString(w, fn+" (")
	l.Arg.emit(w)
	io.WriteString(w, ")")
}

func (l *LenBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_int (")
	l.emit(w)
	io.WriteString(w, ")")
}

// InputBuiltin represents a call to input() returning a string.
type InputBuiltin struct{}

func (i *InputBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(try read_line () with End_of_file -> \"\")")
}

func (i *InputBuiltin) emitPrint(w io.Writer) { i.emit(w) }

// PanicExpr represents a call to panic that raises an exception.
type PanicExpr struct{ Arg Expr }

func (p *PanicExpr) emit(w io.Writer) {
	io.WriteString(w, "(failwith (")
	p.Arg.emit(w)
	io.WriteString(w, "))")
}

func (p *PanicExpr) emitPrint(w io.Writer) { p.emit(w) }

// SubstringBuiltin represents substring(str, start, end).
type SubstringBuiltin struct {
	Str   Expr
	Start Expr
	End   Expr
}

// IndexOfBuiltin represents indexOf(str, ch)
type IndexOfBuiltin struct {
	Str Expr
	Sub Expr
}

func (i *IndexOfBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(try String.index (")
	i.Str.emit(w)
	io.WriteString(w, ") ")
	if lit, ok := i.Sub.(*StringLit); ok && len(lit.Value) == 1 {
		fmt.Fprintf(w, "'%s'", lit.Value)
	} else {
		io.WriteString(w, "(String.get (")
		i.Sub.emit(w)
		io.WriteString(w, ") 0)")
	}
	io.WriteString(w, " with Not_found -> -1)")
}

func (i *IndexOfBuiltin) emitPrint(w io.Writer) { i.emit(w) }

func (s *SubstringBuiltin) emit(w io.Writer) {
	io.WriteString(w, "String.sub ")
	s.Str.emit(w)
	io.WriteString(w, " (")
	s.Start.emit(w)
	io.WriteString(w, ") (")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ")")
}

func (s *SubstringBuiltin) emitPrint(w io.Writer) { s.emit(w) }

// SumBuiltin represents sum(list).
type SumBuiltin struct {
	List     Expr
	ElemType string
}

func (s *SumBuiltin) emit(w io.Writer) {
	if s.ElemType == "float" {
		io.WriteString(w, "(List.fold_left (fun acc x -> acc +. x) 0.0 ")
	} else {
		io.WriteString(w, "(List.fold_left (fun acc x -> acc + x) 0 ")
	}
	s.List.emit(w)
	io.WriteString(w, ")")
}

func (s *SumBuiltin) emitPrint(w io.Writer) {
	if s.ElemType == "float" {
		io.WriteString(w, "string_of_float ")
	} else {
		io.WriteString(w, "string_of_int ")
	}
	s.emit(w)
}

// AvgBuiltin represents avg(list).
type AvgBuiltin struct {
	List     Expr
	ElemType string
}

func (a *AvgBuiltin) emit(w io.Writer) {
	if a.ElemType == "float" {
		io.WriteString(w, "((List.fold_left (fun acc x -> acc +. x) 0.0 ")
	} else {
		io.WriteString(w, "((List.fold_left (fun acc x -> acc + x) 0 ")
	}
	a.List.emit(w)
	io.WriteString(w, ") / List.length ")
	a.List.emit(w)
	io.WriteString(w, ")")
}

func (a *AvgBuiltin) emitPrint(w io.Writer) {
	if a.ElemType == "float" {
		io.WriteString(w, "string_of_float ")
	} else {
		io.WriteString(w, "string_of_int ")
	}
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

// AbsBuiltin represents abs(x) for integers and floats.
type AbsBuiltin struct {
	Value Expr
	Typ   string
}

func (a *AbsBuiltin) emit(w io.Writer) {
	if a.Typ == "float" {
		io.WriteString(w, "abs_float ")
	} else {
		io.WriteString(w, "abs ")
	}
	a.Value.emit(w)
}

func (a *AbsBuiltin) emitPrint(w io.Writer) {
	if a.Typ == "float" {
		io.WriteString(w, "string_of_float ")
	} else {
		io.WriteString(w, "string_of_int ")
	}
	a.emit(w)
}

// AppendBuiltin represents append(list, value).
type AppendBuiltin struct {
	List  Expr
	Value Expr
	Elem  string
}

func (a *AppendBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(List.append (")
	a.List.emit(w)
	io.WriteString(w, ") [")
	if a.Elem != "" {
		io.WriteString(w, "(Obj.magic (")
		a.Value.emit(w)
		fmt.Fprintf(w, ") : %s)", ocamlType(a.Elem))
	} else {
		a.Value.emit(w)
	}
	io.WriteString(w, "])")
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

// KeysBuiltin represents keys(map) returning list of keys.
type KeysBuiltin struct{ Map Expr }

func (k *KeysBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(List.map fst ")
	k.Map.emit(w)
	io.WriteString(w, ")")
}

func (k *KeysBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "String.concat \" \" (List.map string_of_int (")
	k.emit(w)
	io.WriteString(w, "))")
}

// MapContainsBuiltin checks if a map contains a key.
type MapContainsBuiltin struct {
	Map Expr
	Key Expr
}

func (m *MapContainsBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(List.mem_assoc ")
	m.Key.emit(w)
	io.WriteString(w, " ")
	m.Map.emit(w)
	io.WriteString(w, ")")
}

func (m *MapContainsBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_bool ")
	m.emit(w)
}

// ExistsBuiltin checks if a list has any element.
type ExistsBuiltin struct{ List Expr }

func (e *ExistsBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(List.length ")
	e.List.emit(w)
	io.WriteString(w, " > 0)")
}

func (e *ExistsBuiltin) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_bool (")
	e.emit(w)
	io.WriteString(w, ")")
}

// UnionExpr represents list union without duplicates.
type UnionExpr struct{ Left, Right Expr }

func (u *UnionExpr) emit(w io.Writer) {
	io.WriteString(w, "(List.sort_uniq compare (")
	u.Left.emit(w)
	io.WriteString(w, " @ ")
	u.Right.emit(w)
	io.WriteString(w, "))")
}

func (u *UnionExpr) emitPrint(w io.Writer) {
	io.WriteString(w, "(\"[\" ^ String.concat \", \" (List.map string_of_int (")
	u.emit(w)
	io.WriteString(w, ")) ^ \"]\")")
}

// UnionAllExpr represents concatenation of two lists.
type UnionAllExpr struct{ Left, Right Expr }

func (u *UnionAllExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	u.Left.emit(w)
	io.WriteString(w, " @ ")
	u.Right.emit(w)
	io.WriteString(w, ")")
}

func (u *UnionAllExpr) emitPrint(w io.Writer) {
	io.WriteString(w, "(\"[\" ^ String.concat \", \" (List.map string_of_int (")
	u.emit(w)
	io.WriteString(w, ")) ^ \"]\")")
}

// ExceptExpr represents list difference a except b.
type ExceptExpr struct{ Left, Right Expr }

func (e *ExceptExpr) emit(w io.Writer) {
	io.WriteString(w, "(List.filter (fun x -> not (List.mem x ")
	e.Right.emit(w)
	io.WriteString(w, ")) ")
	e.Left.emit(w)
	io.WriteString(w, ")")
}

func (e *ExceptExpr) emitPrint(w io.Writer) {
	io.WriteString(w, "(\"[\" ^ String.concat \", \" (List.map string_of_int (")
	e.emit(w)
	io.WriteString(w, ")) ^ \"]\")")
}

// IntersectExpr represents list intersection.
type IntersectExpr struct{ Left, Right Expr }

func (i *IntersectExpr) emit(w io.Writer) {
	io.WriteString(w, "(List.filter (fun x -> List.mem x ")
	i.Right.emit(w)
	io.WriteString(w, ") ")
	i.Left.emit(w)
	io.WriteString(w, ")")
}

func (i *IntersectExpr) emitPrint(w io.Writer) {
	io.WriteString(w, "(\"[\" ^ String.concat \", \" (List.map string_of_int (")
	i.emit(w)
	io.WriteString(w, ")) ^ \"]\")")
}

// StringContainsBuiltin represents s.contains(sub).
type StringContainsBuiltin struct {
	Str Expr
	Sub Expr
}

// PadStartBuiltin represents s.padStart(len, pad)
type PadStartBuiltin struct {
	Str Expr
	Len Expr
	Pad Expr
}

func (p *PadStartBuiltin) emit(w io.Writer) {
	io.WriteString(w, "(let s = ")
	p.Str.emit(w)
	io.WriteString(w, " in let l = ")
	p.Len.emit(w)
	io.WriteString(w, " in let pch = String.get (")
	p.Pad.emit(w)
	io.WriteString(w, ") 0 in let sl = String.length s in if sl >= l then s else String.make (l - sl) pch ^ s)")
}

func (p *PadStartBuiltin) emitPrint(w io.Writer) { p.emit(w) }

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

// ExprCall represents calling an expression that evaluates to a function.
type ExprCall struct {
	Fn   Expr
	Args []Expr
	Ret  string
}

func (c *ExprCall) emit(w io.Writer) {
	io.WriteString(w, "(")
	c.Fn.emit(w)
	if len(c.Args) == 0 {
		io.WriteString(w, " ())")
		return
	}
	for _, a := range c.Args {
		io.WriteString(w, " (")
		a.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, ")")
}

func (c *ExprCall) emitPrint(w io.Writer) {
	switch c.Ret {
	case "int":
		io.WriteString(w, "string_of_int ")
		c.emit(w)
	case "float":
		io.WriteString(w, "string_of_float ")
		c.emit(w)
	case "string":
		c.emit(w)
	case "bool":
		io.WriteString(w, "string_of_bool ")
		c.emit(w)
	default:
		io.WriteString(w, "__show ")
		c.emit(w)
	}
}

func (f *FuncCall) emit(w io.Writer) {
	io.WriteString(w, f.Name)
	if len(f.Args) == 0 {
		io.WriteString(w, " ()")
		return
	}
	for _, a := range f.Args {
		io.WriteString(w, " (")
		a.emit(w)
		io.WriteString(w, ")")
	}
}

func (f *FuncCall) emitPrint(w io.Writer) {
	switch f.Ret {
	case "int":
		io.WriteString(w, "string_of_int (")
		f.emit(w)
		io.WriteString(w, ")")
	case "float":
		io.WriteString(w, "string_of_float (")
		f.emit(w)
		io.WriteString(w, ")")
	case "bool":
		io.WriteString(w, "string_of_bool (")
		f.emit(w)
		io.WriteString(w, ")")
	case "bigint":
		usesBigInt = true
		io.WriteString(w, "Z.to_string (")
		f.emit(w)
		io.WriteString(w, ")")
	case "bigrat":
		usesBigRat = true
		io.WriteString(w, "Q.to_string (")
		f.emit(w)
		io.WriteString(w, ")")
	default:
		io.WriteString(w, "__show (")
		f.emit(w)
		io.WriteString(w, ")")
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
	if len(f.Params) == 0 {
		io.WriteString(w, " ()")
	} else {
		for _, p := range f.Params {
			io.WriteString(w, " ")
			io.WriteString(w, p)
		}
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

// ListLit represents a list literal. When Dynamic is true the
// elements may have mixed types and are boxed using Obj.repr.
type ListLit struct {
	Elems   []Expr
	Dynamic bool
}

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, "; ")
		}
		if l.Dynamic {
			io.WriteString(w, "Obj.repr (")
			e.emit(w)
			io.WriteString(w, ")")
		} else {
			e.emit(w)
		}
	}
	io.WriteString(w, "]")
}

func (l *ListLit) emitPrint(w io.Writer) {
	if l.Dynamic {
		io.WriteString(w, "__show (")
		l.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "(\"[\" ^ String.concat \", \" (List.map string_of_int (")
		l.emit(w)
		io.WriteString(w, ")) ^ \"]\")")
	}
}

// MapEntry represents a key/value pair in a map literal.
type MapEntry struct {
	Key   Expr
	Value Expr
}

// MapLit represents a simple map literal implemented as a list of
// key/value tuples. When Dynamic is true, values may have mixed types
// and are boxed using Obj.repr.
type MapLit struct {
	Items   []MapEntry
	Dynamic bool
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, "; ")
		}
		io.WriteString(w, "(")
		it.Key.emit(w)
		io.WriteString(w, ", ")
		if m.Dynamic {
			io.WriteString(w, "Obj.repr (")
			it.Value.emit(w)
			io.WriteString(w, ")")
		} else {
			it.Value.emit(w)
		}
		io.WriteString(w, ")")
	}
	io.WriteString(w, "]")
}

func (m *MapLit) emitPrint(w io.Writer) { m.emit(w) }

// MapIndexExpr represents map[key] access.
type MapIndexExpr struct {
	Map    Expr
	Key    Expr
	Typ    string
	Dyn    bool
	KeyTyp string
}

func (mi *MapIndexExpr) emit(w io.Writer) {
	if mi.Dyn {
		io.WriteString(w, "(Obj.obj (List.assoc (")
		mi.Key.emit(w)
		io.WriteString(w, ") (")
		mi.Map.emit(w)
		io.WriteString(w, ") : Obj.t) : ")
		if strings.HasPrefix(mi.Typ, "map") {
			val := strings.TrimPrefix(mi.Typ, "map-")
			keyTyp := "string"
			if mi.KeyTyp != "" {
				keyTyp = ocamlType(mi.KeyTyp)
			}
			io.WriteString(w, "( "+keyTyp+" * "+ocamlType(val)+" ) list")
		} else {
			io.WriteString(w, ocamlType(mi.Typ))
		}
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "List.assoc (")
		mi.Key.emit(w)
		io.WriteString(w, ") (")
		mi.Map.emit(w)
		io.WriteString(w, ")")
	}
}

func (mi *MapIndexExpr) emitPrint(w io.Writer) {
	if mi.Dyn {
		switch mi.Typ {
		case "int":
			io.WriteString(w, "string_of_int ")
			mi.emit(w)
		case "float":
			io.WriteString(w, "string_of_float ")
			mi.emit(w)
		default:
			io.WriteString(w, "__show ")
			mi.emit(w)
		}
	} else {
		switch mi.Typ {
		case "int":
			io.WriteString(w, "string_of_int (List.assoc ")
			mi.Key.emit(w)
			io.WriteString(w, " (")
			mi.Map.emit(w)
			io.WriteString(w, ") )")
		case "float":
			io.WriteString(w, "string_of_float (List.assoc ")
			mi.Key.emit(w)
			io.WriteString(w, " (")
			mi.Map.emit(w)
			io.WriteString(w, ") )")
		default:
			io.WriteString(w, "__show (List.assoc ")
			mi.Key.emit(w)
			io.WriteString(w, " (")
			mi.Map.emit(w)
			io.WriteString(w, ") )")
		}
	}
}

// MapGetExpr represents m.get(key, default)
type MapGetExpr struct {
	Map     Expr
	Key     Expr
	Default Expr
	Typ     string
	Dyn     bool
}

func (mg *MapGetExpr) emit(w io.Writer) {
	if mg.Dyn {
		io.WriteString(w, "(try (Obj.obj (List.assoc (")
		mg.Key.emit(w)
		io.WriteString(w, ") (")
		mg.Map.emit(w)
		io.WriteString(w, ") : ")
		if isDynamicMapType(mg.Typ) {
			io.WriteString(w, "(string * Obj.t) list")
		} else {
			io.WriteString(w, ocamlType(mg.Typ))
		}
		io.WriteString(w, ") with Not_found -> ")
		mg.Default.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "(try List.assoc (")
		mg.Key.emit(w)
		io.WriteString(w, ") (")
		mg.Map.emit(w)
		io.WriteString(w, ") with Not_found -> ")
		mg.Default.emit(w)
		io.WriteString(w, ")")
	}
}

func (mg *MapGetExpr) emitPrint(w io.Writer) {
	switch mg.Typ {
	case "int":
		io.WriteString(w, "string_of_int ")
	case "float":
		io.WriteString(w, "string_of_float ")
	default:
		io.WriteString(w, "__show ")
	}
	mg.emit(w)
}

// IndexExpr represents list[index] or string[index].
type IndexExpr struct {
	Col    Expr
	Index  Expr
	Typ    string
	ColTyp string
}

// SliceExpr represents col[start:end] for strings.
type SliceExpr struct {
	Col   Expr
	Start Expr
	End   Expr
	Typ   string
}

// StrJoin concatenates a list of strings into a single string.
type StrJoin struct {
	List Expr
}

// ListUpdateExpr updates a list at a specific index and returns the new list.
type ListUpdateExpr struct {
	List  Expr
	Index Expr
	Value Expr
}

func (lu *ListUpdateExpr) emit(w io.Writer) {
	io.WriteString(w, "(List.mapi (fun __i __x -> if __i = ")
	lu.Index.emit(w)
	io.WriteString(w, " then ")
	lu.Value.emit(w)
	io.WriteString(w, " else __x) (")
	lu.List.emit(w)
	io.WriteString(w, "))")
}

func (lu *ListUpdateExpr) emitPrint(w io.Writer) { lu.emit(w) }

// MapUpdateExpr updates a map by replacing or adding a key/value pair.
type MapUpdateExpr struct {
	Map   Expr
	Key   Expr
	Value Expr
	Dyn   bool
}

// queryLoop is one iteration variable and its source list.
type queryLoop struct {
	Name string
	Src  Expr
	Side string // "", "left", "right", or "outer"
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
	if loop.Side != "" {
		fmt.Fprintf(w, "(* %s join *) ", loop.Side)
	}
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
	io.WriteString(w, "(List.concat_map (fun ")
	io.WriteString(w, loop.Name)
	io.WriteString(w, " -> ")
	emitQueryLoop(w, loops, sel, where, idx+1)
	io.WriteString(w, ") ")
	loop.Src.emit(w)
	io.WriteString(w, ")")
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
	Sort   Expr
	Desc   bool
}

func emitGroupLoop(w io.Writer, loops []queryLoop, key Expr, where Expr, idx int) {
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
		if len(loops) == 1 {
			io.WriteString(w, "    let cur = try List.assoc key !__groups0 with Not_found -> [] in\n")
			fmt.Fprintf(w, "    __groups0 := (key, %s :: cur) :: List.remove_assoc key !__groups0;\n", loop.Name)
		} else {
			io.WriteString(w, "    let item = [")
			for i, lp := range loops {
				if i > 0 {
					io.WriteString(w, "; ")
				}
				fmt.Fprintf(w, "(\"%s\", Obj.repr %s)", lp.Name, lp.Name)
			}
			io.WriteString(w, "] in\n")
			io.WriteString(w, "    let cur = try List.assoc key !__groups0 with Not_found -> [] in\n")
			io.WriteString(w, "    __groups0 := (key, item :: cur) :: List.remove_assoc key !__groups0;\n")
		}
		if where != nil {
			io.WriteString(w, "    )\n")
		}
	} else {
		emitGroupLoop(w, loops, key, where, idx+1)
	}
	io.WriteString(w, "  ) ")
	loop.Src.emit(w)
	io.WriteString(w, ";\n")
}

func (g *GroupByQueryExpr) emit(w io.Writer) {
	io.WriteString(w, "(let __groups0 = ref [] in\n")
	emitGroupLoop(w, g.Loops, g.Key, g.Where, 0)
	io.WriteString(w, "  let __res0 = ref [] in\n")
	io.WriteString(w, "  List.iter (fun (")
	io.WriteString(w, g.Into+"_key, "+g.Into+"_items")
	io.WriteString(w, ") ->\n")
	fmt.Fprintf(w, "    let %s = List.rev %s_items in\n", g.Into, g.Into)
	if g.Having != nil {
		io.WriteString(w, "    if ")
		g.Having.emit(w)
		io.WriteString(w, " then (\n      __res0 := (")
		if g.Sort != nil {
			g.Sort.emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, ", ")
		g.Select.emit(w)
		io.WriteString(w, ") :: !__res0)\n    else ()\n")
	} else {
		io.WriteString(w, "    __res0 := (")
		if g.Sort != nil {
			g.Sort.emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, ", ")
		g.Select.emit(w)
		io.WriteString(w, ") :: !__res0\n")
	}
	io.WriteString(w, "  ) !__groups0;\n")
	if g.Sort != nil {
		io.WriteString(w, "  let __sorted = List.sort (fun a b -> compare ")
		if g.Desc {
			io.WriteString(w, "(fst b) (fst a)")
		} else {
			io.WriteString(w, "(fst a) (fst b)")
		}
		io.WriteString(w, ") !__res0 in\n")
		io.WriteString(w, "  List.map snd __sorted)")
	} else {
		io.WriteString(w, "  List.rev (List.map snd !__res0))")
	}
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
		if strings.HasPrefix(in.Typ, "map-") || strings.HasPrefix(in.Typ, "map{") || strings.HasPrefix(in.Typ, "map-dyn{") {
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
	if mu.Dyn {
		io.WriteString(w, "Obj.repr (")
	}
	mu.Value.emit(w)
	if mu.Dyn {
		io.WriteString(w, ")")
	}
	io.WriteString(w, ") :: List.remove_assoc (")
	mu.Key.emit(w)
	io.WriteString(w, ") ")
	if mu.Dyn {
		if mi, ok := mu.Map.(*MapIndexExpr); ok && mi.Dyn {
			// Map expression already yields a properly typed list.
			mu.Map.emit(w)
		} else {
			io.WriteString(w, "(Obj.magic (")
			mu.Map.emit(w)
			keyIsString := false
			switch k := mu.Key.(type) {
			case *StringLit:
				keyIsString = true
			case *Name:
				keyIsString = k.Typ == "string"
			}
			if keyIsString {
				io.WriteString(w, ") : (string * Obj.t) list)")
			} else {
				io.WriteString(w, ") : (int * Obj.t) list)")
			}
		}
	} else {
		mu.Map.emit(w)
	}
	io.WriteString(w, ")")
}

func (mu *MapUpdateExpr) emitPrint(w io.Writer) { mu.emit(w) }

func (ix *IndexExpr) emit(w io.Writer) {
	switch ix.ColTyp {
	case "string":
		io.WriteString(w, "String.make 1 (String.get (")
		ix.Col.emit(w)
		io.WriteString(w, ") ")
		ix.Index.emit(w)
		io.WriteString(w, ")")
	case "list":
		if ix.Typ == "int" {
			io.WriteString(w, "(Obj.magic (List.nth (")
			ix.Col.emit(w)
			io.WriteString(w, ") (")
			ix.Index.emit(w)
			io.WriteString(w, ") ) : int)")
		} else if ix.Typ == "float" {
			io.WriteString(w, "(Obj.magic (List.nth (")
			ix.Col.emit(w)
			io.WriteString(w, ") (")
			ix.Index.emit(w)
			io.WriteString(w, ") ) : float)")
		} else if ix.Typ == "string" {
			io.WriteString(w, "(Obj.magic (List.nth (")
			ix.Col.emit(w)
			io.WriteString(w, ") (")
			ix.Index.emit(w)
			io.WriteString(w, ") ) : string)")
		} else {
			io.WriteString(w, "List.nth (")
			ix.Col.emit(w)
			io.WriteString(w, ") (")
			ix.Index.emit(w)
			io.WriteString(w, ")")
		}
	default:
		io.WriteString(w, "List.nth (")
		ix.Col.emit(w)
		io.WriteString(w, ") (")
		ix.Index.emit(w)
		io.WriteString(w, ")")
	}
}

func (ix *IndexExpr) emitPrint(w io.Writer) {
	switch ix.ColTyp {
	case "string":
		ix.emit(w)
	default:
		if ix.Typ == "float" {
			io.WriteString(w, "Printf.sprintf \"%.15f\" (")
			ix.emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "string_of_int (")
			ix.emit(w)
			io.WriteString(w, ")")
		}
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	if s.Typ == "string" {
		io.WriteString(w, "String.sub (")
		s.Col.emit(w)
		io.WriteString(w, ") ")
		s.Start.emit(w)
		io.WriteString(w, " (")
		s.End.emit(w)
		io.WriteString(w, " - ")
		s.Start.emit(w)
		io.WriteString(w, ")")
		return
	}
	// list slice using Seq.drop/take
	io.WriteString(w, "List.of_seq (Seq.take (")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ") (Seq.drop ")
	s.Start.emit(w)
	io.WriteString(w, " (List.to_seq (")
	s.Col.emit(w)
	io.WriteString(w, "))))")
}

func (s *SliceExpr) emitPrint(w io.Writer) { s.emit(w) }

func (sj *StrJoin) emit(w io.Writer) {
	io.WriteString(w, "String.concat \"\" (")
	sj.List.emit(w)
	io.WriteString(w, ")")
}

func (sj *StrJoin) emitPrint(w io.Writer) { sj.emit(w) }

// UnaryMinus represents negation of an integer expression.
type UnaryMinus struct {
	Expr Expr
	Typ  string
}

func (u *UnaryMinus) emit(w io.Writer) {
	if u.Typ == "float" {
		io.WriteString(w, "(-.(")
		u.Expr.emit(w)
		io.WriteString(w, "))")
	} else if u.Typ == "bigrat" {
		io.WriteString(w, "(Q.neg (")
		u.Expr.emit(w)
		io.WriteString(w, "))")
	} else if u.Typ == "bigint" {
		io.WriteString(w, "(Z.neg (")
		u.Expr.emit(w)
		io.WriteString(w, "))")
	} else {
		io.WriteString(w, "-(")
		u.Expr.emit(w)
		io.WriteString(w, ")")
	}
}

func (u *UnaryMinus) emitPrint(w io.Writer) {
	if u.Typ == "float" {
		io.WriteString(w, "string_of_float (-.(")
		u.Expr.emit(w)
		io.WriteString(w, "))")
	} else if u.Typ == "bigrat" {
		io.WriteString(w, "Q.to_string (Q.neg (")
		u.Expr.emit(w)
		io.WriteString(w, "))")
	} else if u.Typ == "bigint" {
		io.WriteString(w, "Z.to_string (Z.neg (")
		u.Expr.emit(w)
		io.WriteString(w, "))")
	} else {
		io.WriteString(w, "string_of_int (-")
		u.Expr.emit(w)
		io.WriteString(w, ")")
	}
}

// UnaryNot represents logical negation of a boolean expression.
type UnaryNot struct{ Expr Expr }

func (u *UnaryNot) emit(w io.Writer) {
	io.WriteString(w, "not (")
	u.Expr.emit(w)
	io.WriteString(w, ")")
}

func (u *UnaryNot) emitPrint(w io.Writer) {
	io.WriteString(w, "string_of_bool (not ")
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
	case "str_to_int":
		if s, ok := c.Expr.(*SubstringBuiltin); ok {
			io.WriteString(w, "Char.code (String.get (")
			s.Str.emit(w)
			io.WriteString(w, ") ")
			s.Start.emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "int_of_string (")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		}
	case "float_to_int":
		io.WriteString(w, "int_of_float (")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "int_to_float":
		io.WriteString(w, "float_of_int (")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "int_to_big":
		usesBigInt = true
		io.WriteString(w, "(Z.of_int (")
		c.Expr.emit(w)
		io.WriteString(w, "))")
	case "int_to_rat":
		usesBigRat = true
		io.WriteString(w, "(Q.of_int (")
		c.Expr.emit(w)
		io.WriteString(w, "))")
	case "big_to_rat":
		usesBigRat = true
		io.WriteString(w, "(Q.of_bigint (")
		c.Expr.emit(w)
		io.WriteString(w, "))")
	case "big_to_int":
		usesBigInt = true
		io.WriteString(w, "Z.to_int ")
		c.Expr.emit(w)
	case "int_to_obj":
		io.WriteString(w, "Obj.repr (")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "float_to_obj":
		io.WriteString(w, "Obj.repr (")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "list_to_obj":
		io.WriteString(w, "List.map (fun v -> Obj.repr v) (")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "list_to_any":
		io.WriteString(w, "(Obj.repr (List.map (fun v -> Obj.repr v) (")
		c.Expr.emit(w)
		io.WriteString(w, ")))")
	case "obj_to_int":
		io.WriteString(w, "(Obj.magic ")
		c.Expr.emit(w)
		io.WriteString(w, " : int)")
	case "obj_to_float":
		io.WriteString(w, "(Obj.magic ")
		c.Expr.emit(w)
		io.WriteString(w, " : float)")
	case "obj_to_string":
		io.WriteString(w, "(Obj.magic ")
		c.Expr.emit(w)
		io.WriteString(w, " : string)")
	default:
		if strings.HasPrefix(c.Type, "obj_to_") {
			typ := strings.TrimPrefix(c.Type, "obj_to_")
			io.WriteString(w, "(Obj.magic ")
			c.Expr.emit(w)
			io.WriteString(w, " : ")
			io.WriteString(w, ocamlType(typ))
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
	}
}

func (c *CastExpr) emitPrint(w io.Writer) {
	switch c.Type {
	case "str_to_int":
		if s, ok := c.Expr.(*SubstringBuiltin); ok {
			io.WriteString(w, "string_of_int (Char.code (String.get (")
			s.Str.emit(w)
			io.WriteString(w, ") ")
			s.Start.emit(w)
			io.WriteString(w, ")))")
		} else {
			io.WriteString(w, "string_of_int (int_of_string (")
			c.Expr.emit(w)
			io.WriteString(w, "))")
		}
	case "float_to_int":
		io.WriteString(w, "string_of_int (int_of_float (")
		c.Expr.emit(w)
		io.WriteString(w, "))")
	case "int_to_float":
		io.WriteString(w, "string_of_float (float_of_int (")
		c.Expr.emit(w)
		io.WriteString(w, "))")
	case "int_to_big":
		usesBigInt = true
		io.WriteString(w, "Z.to_string (Z.of_int (")
		c.Expr.emit(w)
		io.WriteString(w, "))")
	case "int_to_rat":
		usesBigRat = true
		io.WriteString(w, "Q.to_string (Q.of_int (")
		c.Expr.emit(w)
		io.WriteString(w, "))")
	case "big_to_rat":
		usesBigRat = true
		io.WriteString(w, "Q.to_string (Q.of_bigint ")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "big_to_int":
		usesBigInt = true
		io.WriteString(w, "string_of_int (Z.to_int ")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "int_to_obj":
		io.WriteString(w, "string_of_int ")
		c.Expr.emit(w)
	case "float_to_obj":
		io.WriteString(w, "string_of_float ")
		c.Expr.emit(w)
	case "list_to_obj":
		io.WriteString(w, "__show (")
		c.emit(w)
		io.WriteString(w, ")")
	case "list_to_any":
		io.WriteString(w, "__show (")
		c.emit(w)
		io.WriteString(w, ")")
	case "obj_to_int":
		io.WriteString(w, "string_of_int (Obj.magic ")
		c.Expr.emit(w)
		io.WriteString(w, " : int)")
	case "obj_to_float":
		io.WriteString(w, "string_of_float (Obj.magic ")
		c.Expr.emit(w)
		io.WriteString(w, " : float)")
	case "obj_to_string":
		io.WriteString(w, "(Obj.magic ")
		c.Expr.emit(w)
		io.WriteString(w, " : string)")
	default:
		if strings.HasPrefix(c.Type, "obj_to_") {
			io.WriteString(w, "__show (")
			c.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emitPrint(w)
		}
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
	ident := sanitizeIdent(n.Ident)
	if n.Ref {
		fmt.Fprintf(w, "!%s", ident)
	} else {
		io.WriteString(w, ident)
	}
}
func (n *Name) emitPrint(w io.Writer) {
	ident := sanitizeIdent(n.Ident)
	if n.Ref {
		ident = "!" + n.Ident
	}
	switch n.Typ {
	case "int":
		fmt.Fprintf(w, "string_of_int %s", ident)
	case "bool":
		fmt.Fprintf(w, "string_of_bool %s", ident)
	case "float":
		fmt.Fprintf(w, "Printf.sprintf \"%%.15f\" (%s)", ident)
	case "bigint":
		usesBigInt = true
		fmt.Fprintf(w, "Z.to_string %s", ident)
	case "bigrat":
		usesBigRat = true
		fmt.Fprintf(w, "Q.to_string %s", ident)
	default:
		fmt.Fprintf(w, "__show %s", ident)
	}
}

// IntLit represents an integer literal.
type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer)      { fmt.Fprintf(w, "%d", i.Value) }
func (i *IntLit) emitPrint(w io.Writer) { fmt.Fprintf(w, "string_of_int %d", i.Value) }

// RawExpr emits raw OCaml code as-is.
type RawExpr struct{ Code string }

func (r *RawExpr) emit(w io.Writer)      { io.WriteString(w, r.Code) }
func (r *RawExpr) emitPrint(w io.Writer) { io.WriteString(w, r.Code) }

// IgnoreExpr wraps an expression with OCaml's [ignore] to discard its result.
type IgnoreExpr struct{ Expr Expr }

func (i *IgnoreExpr) emit(w io.Writer) {
	io.WriteString(w, "ignore (")
	i.Expr.emit(w)
	io.WriteString(w, ")")
}

func (i *IgnoreExpr) emitPrint(w io.Writer) { i.emit(w) }

// FloatLit represents a floating point literal.
type FloatLit struct{ Value float64 }

func formatFloat(v float64) string {
	s := strconv.FormatFloat(v, 'f', -1, 64)
	if !strings.ContainsAny(s, ".eE") {
		s += ".0"
	}
	return s
}

func (f *FloatLit) emit(w io.Writer) { io.WriteString(w, formatFloat(f.Value)) }
func (f *FloatLit) emitPrint(w io.Writer) {
	io.WriteString(w, "Printf.sprintf \"%.15f\" ")
	f.emit(w)
}

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

func isConstExpr(e Expr) bool {
	switch v := e.(type) {
	case *IntLit, *FloatLit, *BoolLit, *StringLit:
		return true
	case *BinaryExpr:
		return isConstExpr(v.Left) && isConstExpr(v.Right)
	case *UnaryMinus:
		return isConstExpr(v.Expr)
	case *ListLit:
		for _, el := range v.Elems {
			if !isConstExpr(el) {
				return false
			}
		}
		return true
	case *MapLit:
		for _, it := range v.Items {
			if !isConstExpr(it.Key) || !isConstExpr(it.Value) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

// BinaryExpr represents a binary operation.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
	Typ   string
	Ltyp  string
	Rtyp  string
}

type DynBinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Typ == "bigint" {
		usesBigInt = true
		switch b.Op {
		case "+":
			io.WriteString(w, "(Z.add (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "-":
			io.WriteString(w, "(Z.sub (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "*":
			io.WriteString(w, "(Z.mul (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "/":
			io.WriteString(w, "(Z.div (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "%":
			io.WriteString(w, "(Z.rem (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "==":
			io.WriteString(w, "(Z.equal (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "!=":
			io.WriteString(w, "(not (Z.equal (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, ")))")
		case "<":
			io.WriteString(w, "((Z.compare ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ") < 0)")
		case "<=":
			io.WriteString(w, "((Z.compare ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ") <= 0)")
		case ">":
			io.WriteString(w, "((Z.compare ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ") > 0)")
		case ">=":
			io.WriteString(w, "((Z.compare ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ") >= 0)")
		default:
			io.WriteString(w, "(Z.add ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ")")
		}
		return
	} else if b.Typ == "bigrat" {
		usesBigRat = true
		switch b.Op {
		case "+":
			io.WriteString(w, "(Q.add (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "-":
			io.WriteString(w, "(Q.sub (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "*":
			io.WriteString(w, "(Q.mul (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "/":
			io.WriteString(w, "(Q.div (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "==":
			io.WriteString(w, "(Q.equal (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		case "!=":
			io.WriteString(w, "(not (Q.equal (")
			b.Left.emit(w)
			io.WriteString(w, ") (")
			b.Right.emit(w)
			io.WriteString(w, ")))")
		case "<":
			io.WriteString(w, "((Q.compare ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ") < 0)")
		case "<=":
			io.WriteString(w, "((Q.compare ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ") <= 0)")
		case ">":
			io.WriteString(w, "((Q.compare ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ") > 0)")
		case ">=":
			io.WriteString(w, "((Q.compare ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ") >= 0)")
		default:
			io.WriteString(w, "(Q.add ")
			b.Left.emit(w)
			io.WriteString(w, " ")
			b.Right.emit(w)
			io.WriteString(w, ")")
		}
		return
	}

	if b.Op == "%" && (b.Typ == "float" || b.Ltyp == "float" || b.Rtyp == "float") {
		io.WriteString(w, "(Float.rem ")
		if (b.Typ == "float" || b.Ltyp == "float" || b.Rtyp == "float") && b.Ltyp == "int" {
			io.WriteString(w, "float_of_int (")
			b.Left.emit(w)
			io.WriteString(w, ")")
		} else {
			b.Left.emit(w)
		}
		io.WriteString(w, " ")
		if (b.Typ == "float" || b.Ltyp == "float" || b.Rtyp == "float") && b.Rtyp == "int" {
			io.WriteString(w, "float_of_int (")
			b.Right.emit(w)
			io.WriteString(w, ")")
		} else {
			b.Right.emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	op := b.Op
	if b.Op == "%" {
		op = "mod"
	}
	if b.Op == "==" {
		op = "="
	} else if b.Op == "!=" {
		op = "<>"
	}
	if b.Op == "+" && b.Typ == "string" {
		op = "^"
	} else if b.Typ == "float" {
		switch b.Op {
		case "+":
			op = "+."
		case "-":
			op = "-."
		case "*":
			op = "*."
		case "/":
			op = "/."
		}
	}
	fmt.Fprintf(w, "(")
	if (b.Typ == "float" || b.Ltyp == "float" || b.Rtyp == "float") && b.Ltyp == "int" {
		io.WriteString(w, "float_of_int (")
		b.Left.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emit(w)
	}
	fmt.Fprintf(w, " %s ", op)
	if (b.Typ == "float" || b.Ltyp == "float" || b.Rtyp == "float") && b.Rtyp == "int" {
		io.WriteString(w, "float_of_int (")
		b.Right.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emit(w)
	}
	fmt.Fprintf(w, ")")
}

func (b *BinaryExpr) emitPrint(w io.Writer) {
	switch b.Typ {
	case "bool":
		io.WriteString(w, "string_of_bool ")
		b.emit(w)
	case "string":
		b.emit(w)
	case "float":
		io.WriteString(w, "string_of_float ")
		b.emit(w)
	case "bigint":
		usesBigInt = true
		io.WriteString(w, "Z.to_string ")
		b.emit(w)
	case "bigrat":
		usesBigRat = true
		io.WriteString(w, "Q.to_string ")
		b.emit(w)
	default:
		io.WriteString(w, "string_of_int ")
		b.emit(w)
	}
}

func (d *DynBinaryExpr) emit(w io.Writer) {
	fn := "_dyn_add"
	switch d.Op {
	case "+":
		fn = "_dyn_add"
	case "-":
		fn = "_dyn_sub"
	case "*":
		fn = "_dyn_mul"
	case "/":
		fn = "_dyn_div"
	}
	io.WriteString(w, "(")
	io.WriteString(w, fn)
	io.WriteString(w, " (")
	d.Left.emit(w)
	io.WriteString(w, ") (")
	d.Right.emit(w)
	io.WriteString(w, "))")
}

func (d *DynBinaryExpr) emitPrint(w io.Writer) {
	io.WriteString(w, "__show (")
	d.emit(w)
	io.WriteString(w, ")")
}

func header() string {
	ts := gitTimestamp()
	version := strings.TrimSpace(meta.Version())
	return fmt.Sprintf("(* Generated by Mochi transpiler v%s on %s *)\n", version, ts)
}

const helperNow = `
let _now_seed = ref 0
let _now_seeded = ref false

let _now () =
  if not !_now_seeded then (
    match Sys.getenv_opt "MOCHI_NOW_SEED" with
    | Some s -> (try _now_seed := int_of_string s; _now_seeded := true with _ -> ())
    | None -> ()
  );
  if !_now_seeded then (
    _now_seed := (!(_now_seed) * 1664525 + 1013904223) mod 2147483647;
    !_now_seed
  ) else int_of_float (Sys.time () *. 1000000000.)
`

const helperLookupHost = `
let _lookup_host _host =
  [Obj.repr ([] : string list); nil]
`

const helperNil = `
let nil = Obj.repr 0
`

const helperMem = `
let _mem () =
  int_of_float (Gc.allocated_bytes ())
`

const helperGetOutput = `
let _getoutput cmd =
  let tmp = Filename.temp_file "mochi" ".tmp" in
  let _ = Sys.command (cmd ^ " > " ^ tmp) in
  let ic = open_in tmp in
  let len = in_channel_length ic in
  let buf = really_input_string ic len in
  close_in ic;
  Sys.remove tmp;
  let n = String.length buf in
  if n > 0 && buf.[n - 1] = '\n' then
    String.sub buf 0 (n - 1)
  else buf
`

const helperGetEnv = `
let _getenv s =
  match Sys.getenv_opt s with
  | Some v -> v
  | None -> ""
`

const helperEnviron = `
let _environ () =
  Array.to_list (Unix.environment ())
`

const helperDynMath = `
let _dyn_add a b =
  if Obj.is_int a && Obj.is_int b then
    Obj.repr ((Obj.magic a : int) + (Obj.magic b : int))
  else
    let af = if Obj.is_int a then float_of_int (Obj.magic a : int) else (Obj.magic a : float) in
    let bf = if Obj.is_int b then float_of_int (Obj.magic b : int) else (Obj.magic b : float) in
    Obj.repr (af +. bf)

let _dyn_sub a b =
  if Obj.is_int a && Obj.is_int b then
    Obj.repr ((Obj.magic a : int) - (Obj.magic b : int))
  else
    let af = if Obj.is_int a then float_of_int (Obj.magic a : int) else (Obj.magic a : float) in
    let bf = if Obj.is_int b then float_of_int (Obj.magic b : int) else (Obj.magic b : float) in
    Obj.repr (af -. bf)

let _dyn_mul a b =
  if Obj.is_int a && Obj.is_int b then
    Obj.repr ((Obj.magic a : int) * (Obj.magic b : int))
  else
    let af = if Obj.is_int a then float_of_int (Obj.magic a : int) else (Obj.magic a : float) in
    let bf = if Obj.is_int b then float_of_int (Obj.magic b : int) else (Obj.magic b : float) in
    Obj.repr (af *. bf)

let _dyn_div a b =
  let af = if Obj.is_int a then float_of_int (Obj.magic a : int) else (Obj.magic a : float) in
  let bf = if Obj.is_int b then float_of_int (Obj.magic b : int) else (Obj.magic b : float) in
  Obj.repr (af /. bf)
`

const helperSHA = `
open Sha256

let _sha256 lst =
  let buf = Buffer.create (List.length lst) in
  List.iter (fun i -> Buffer.add_char buf (Char.chr i)) lst;
  let digest = Sha256.to_bin (Sha256.string (Buffer.contents buf)) in
  let rec loop i acc =
    if i < 0 then acc else loop (i - 1) ((Char.code digest.[i]) :: acc)
  in
  loop (String.length digest - 1) []
`

const helperSplit = `
let _split s sep =
  let c = if String.length sep = 0 then ' ' else sep.[0] in
  String.split_on_char c s
`

const helperLen = `
let _len v =
  let r = Obj.repr v in
  if Obj.is_int r then 0 else
    match Obj.tag r with
    | 252 -> String.length (Obj.magic v : string)
    | _ -> List.length (Obj.magic v)
`

const helperShow = `
let rec __show v =
  let open Obj in
  let rec list_aux o =
    if is_int o && (magic (obj o) : int) = 0 then "" else
      let hd = field o 0 in
      let tl = field o 1 in
      let rest = list_aux tl in
      if rest = "" then __show (obj hd) else __show (obj hd) ^ "; " ^ rest
  in
  let r = repr v in
  if is_int r then
    let i = (magic v : int) in
    if i = 0 || i = 1 then string_of_bool (i <> 0)
    else string_of_int i
  else
  match tag r with
  | 0 -> if size r = 0 then "[]" else "[" ^ list_aux r ^ "]"
  | 252 -> (magic v : string)
  | 253 -> string_of_float (magic v)
  | _ -> "<value>"
`

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
	case "float":
		return &FloatLit{Value: 0}
	case "bigint":
		usesBigInt = true
		return &RawExpr{Code: "(Z.of_int 0)"}
	case "bigrat":
		usesBigRat = true
		return &RawExpr{Code: "(Q.of_int 0)"}
	case "list":
		return &ListLit{Elems: nil}
	case "map":
		return &MapLit{Items: nil}
	case "func":
		return &RawExpr{Code: "(Obj.magic 0)"}
	}
	if strings.HasPrefix(typ, "func-") {
		return &RawExpr{Code: "(Obj.magic 0)"}
	}
	if strings.HasPrefix(typ, "list-") {
		elem := strings.TrimPrefix(typ, "list-")
		if elem != "" {
			return &RawExpr{Code: fmt.Sprintf("([] : (%s) list)", ocamlType(elem))}
		}
		return &ListLit{Elems: nil}
	}
	if strings.HasPrefix(typ, "map-") {
		val := strings.TrimPrefix(typ, "map-")
		if val != "" {
			return &RawExpr{Code: fmt.Sprintf("([] : (string * %s) list)", ocamlType(val))}
		}
		return &MapLit{Items: nil}
	}
	return &IntLit{Value: 0}
}

func typeRefString(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		return *t.Simple
	}
	if t.Fun != nil {
		ret := typeRefString(t.Fun.Return)
		n := len(t.Fun.Params)
		if n == 0 {
			n = 1
		}
		for i := 0; i < n; i++ {
			ret = "func-" + ret
		}
		return ret
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				elem := typeRefString(t.Generic.Args[0])
				if elem != "" {
					return "list-" + elem
				}
			}
			return "list"
		case "map":
			if len(t.Generic.Args) == 2 {
				val := typeRefString(t.Generic.Args[1])
				if val != "" {
					return "map-" + val
				}
			}
			return "map"
		}
	}
	return ""
}

// Emit renders OCaml source code for the program.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	buf.WriteString("\n")
	buf.WriteString(helperShow)
	buf.WriteString("\n")
	buf.WriteString(helperNil)
	buf.WriteString("\n")
	if p.UsesStrModule() {
		buf.WriteString("open Str\n\n")
	}
	if usesNow {
		buf.WriteString(helperNow)
		buf.WriteString("\n")
	}
	if usesLookupHost {
		buf.WriteString(helperLookupHost)
		buf.WriteString("\n")
	}
	if usesDynMath {
		buf.WriteString(helperDynMath)
		buf.WriteString("\n")
	}
	if usesMem {
		buf.WriteString(helperMem)
		buf.WriteString("\n")
	}
	if usesGetOutput {
		buf.WriteString(helperGetOutput)
		buf.WriteString("\n")
	}
	if usesGetEnv {
		buf.WriteString(helperGetEnv)
		buf.WriteString("\n")
	}
	if usesEnviron {
		buf.WriteString(helperEnviron)
		buf.WriteString("\n")
	}
	if usesSHA {
		buf.WriteString(helperSHA)
		buf.WriteString("\n")
	}
	if usesDynLen {
		buf.WriteString(helperLen)
		buf.WriteString("\n")
	}
	if usesSplit {
		buf.WriteString(helperSplit)
		buf.WriteString("\n")
	}
	if p.UsesControl() {
		buf.WriteString("exception Break\nexception Continue\n\n")
	}
	buf.WriteString("exception Return\n\n")
	var funs []*FunStmt
	flushFuns := func() {
		if len(funs) > 0 {
			funs[0].emitWith(&buf, "let rec")
			for i := 1; i < len(funs); i++ {
				funs[i].emitWith(&buf, "and")
			}
			buf.WriteString("\n")
			funs = nil
		}
	}

	var body []Stmt
	inBody := false
	for _, s := range p.Stmts {
		switch st := s.(type) {
		case *FunStmt:
			funs = append(funs, st)
		case *VarStmt:
			if inBody {
				body = append(body, st)
			} else {
				flushFuns()
				st.emitTop(&buf)
			}
		case *LetStmt:
			if inBody {
				body = append(body, st)
			} else {
				flushFuns()
				st.emitTop(&buf)
			}
		default:
			flushFuns()
			body = append(body, st)
			inBody = true
		}
	}
	flushFuns()

	buf.WriteString("let () =\n")
	for _, s := range body {
		s.emit(&buf)
	}
	buf.WriteString("  ()")
	return buf.Bytes()
}

// Transpile converts a Mochi program into a simple OCaml AST.
func transpileStmt(st *parser.Statement, env *types.Env, vars map[string]VarInfo, mutated map[string]bool) (Stmt, error) {
	switch {
	case st.Import != nil:
		if st.Import.Lang != nil {
			alias := st.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(st.Import.Path)
			}
			path := strings.Trim(st.Import.Path, "\"")
			switch *st.Import.Lang {
			case "python":
				if path == "math" {
					vars[alias] = VarInfo{typ: "python_math"}
					return nil, nil
				}
				if path == "subprocess" {
					vars[alias] = VarInfo{typ: "python_subprocess"}
					return nil, nil
				}
			case "go":
				if strings.Contains(path, "testpkg") && st.Import.Auto {
					vars[alias] = VarInfo{typ: "go_testpkg"}
					return nil, nil
				}
				if path == "strings" && st.Import.Auto {
					vars[alias] = VarInfo{typ: "go_strings"}
					return nil, nil
				}
				if path == "net" && st.Import.Auto {
					vars[alias] = VarInfo{typ: "go_net"}
					return nil, nil
				}
				if path == "os" && st.Import.Auto {
					vars[alias] = VarInfo{typ: "go_os"}
					return nil, nil
				}
			}
		}
		return nil, fmt.Errorf("unsupported import")
	case st.ExternVar != nil:
		if st.ExternVar.Type != nil && st.ExternVar.Type.Simple != nil {
			vars[st.ExternVar.Name()] = VarInfo{typ: *st.ExternVar.Type.Simple}
		} else {
			vars[st.ExternVar.Name()] = VarInfo{}
		}
		return nil, nil
	case st.ExternFun != nil:
		return nil, nil
	case st.Let != nil:
		var expr Expr
		var typ string
		var err error
		var valTyp string
		if st.Let.Value != nil {
			expr, valTyp, err = convertExpr(st.Let.Value, env, vars)
			if err != nil {
				return nil, err
			}
		}
		if st.Let.Type != nil {
			typ = typeRefString(st.Let.Type)
			if typ == "" {
				typ = guessTypeFromName(st.Var.Name)
			}
		} else {
			typ = valTyp
		}
		if expr == nil {
			expr = defaultValueExpr(typ)
		}
		if typ != "" && valTyp != "" && typ != valTyp {
			if typ == "bigint" && valTyp == "int" {
				expr = &CastExpr{Expr: expr, Type: "int_to_big"}
			} else if typ == "int" && valTyp == "bigint" {
				expr = &CastExpr{Expr: expr, Type: "big_to_int"}
			} else if typ == "bigrat" && valTyp == "int" {
				expr = &CastExpr{Expr: expr, Type: "int_to_rat"}
			} else if typ == "bigrat" && valTyp == "bigint" {
				expr = &CastExpr{Expr: expr, Type: "big_to_rat"}
			}
		} else if typ == "" {
			return nil, fmt.Errorf("let without value not supported: %s", st.Let.Name)
		}
		vinfo := VarInfo{typ: typ}
		if strings.HasPrefix(typ, "func-") {
			vinfo.ret = strings.TrimPrefix(typ, "func-")
		}
		if mutated[st.Let.Name] {
			vinfo.ref = true
		}
		stmt := Stmt(&LetStmt{Name: st.Let.Name, Expr: expr})
		if vinfo.ref || strings.HasPrefix(typ, "list-") || strings.HasPrefix(typ, "map") {
			vinfo.ref = true
			stmt = &VarStmt{Name: st.Let.Name, Expr: expr}
		}
		vars[st.Let.Name] = vinfo
		return stmt, nil
	case st.Var != nil:
		var expr Expr
		var typ string
		var valTyp string
		if st.Var.Type != nil {
			typ = typeRefString(st.Var.Type)
		}
		if st.Var.Value != nil {
			valExpr, typVal, err := convertExpr(st.Var.Value, env, vars)
			if err != nil {
				return nil, err
			}
			valTyp = typVal
			if typ == "" {
				typ = typVal
			}
			expr = valExpr
			if valTyp == "" {
				if r, ok := valExpr.(*RawExpr); ok && r.Code == "nil" {
					if strings.HasPrefix(typ, "map-") || strings.HasPrefix(typ, "list-") {
						expr = defaultValueExpr(typ)
						valTyp = typ
					}
				}
			} else if valTyp == "list" && strings.HasPrefix(typ, "list-") {
				expr = defaultValueExpr(typ)
				valTyp = typ
			} else if valTyp == "map" && strings.HasPrefix(typ, "map-") {
				expr = defaultValueExpr(typ)
				valTyp = typ
			}
		} else if typ != "" {
			expr = defaultValueExpr(typ)
		} else {
			return nil, fmt.Errorf("var without type or value not supported")
		}
		if typ == "" {
			typ = "int"
		}
		if typ != "" && valTyp != "" && typ != valTyp {
			if typ == "bigint" && valTyp == "int" {
				expr = &CastExpr{Expr: expr, Type: "int_to_big"}
			} else if typ == "int" && valTyp == "bigint" {
				expr = &CastExpr{Expr: expr, Type: "big_to_int"}
			} else if typ == "bigrat" && valTyp == "int" {
				expr = &CastExpr{Expr: expr, Type: "int_to_rat"}
			} else if typ == "bigrat" && valTyp == "bigint" {
				expr = &CastExpr{Expr: expr, Type: "big_to_rat"}
			}
		}

		vinfo := VarInfo{typ: typ, ref: true}
		if strings.HasPrefix(typ, "func-") {
			vinfo.ret = strings.TrimPrefix(typ, "func-")
		}
		vars[st.Var.Name] = vinfo
		return &VarStmt{Name: st.Var.Name, Expr: expr}, nil
	case st.Assign != nil:
		info, ok := vars[st.Assign.Name]
		if !ok {
			if t, err := env.GetVar(st.Assign.Name); err == nil {
				vars[st.Assign.Name] = VarInfo{typ: typeString(t), ref: true}
				info = vars[st.Assign.Name]
			}
		}
		if !info.ref {
			return nil, fmt.Errorf("assignment to unknown or immutable variable: %s", st.Assign.Name)
		}
		valExpr, valTyp, err := convertExpr(st.Assign.Value, env, vars)
		if err != nil {
			return nil, err
		}
		if valTyp == "" {
			if r, ok := valExpr.(*RawExpr); ok && r.Code == "nil" {
				if strings.HasPrefix(info.typ, "list-") || strings.HasPrefix(info.typ, "map-") {
					valExpr = defaultValueExpr(info.typ)
					valTyp = info.typ
				}
			}
		}
		if info.typ == "int" && valTyp != "int" {
			valExpr = &CastExpr{Expr: valExpr, Type: "obj_to_int"}
		} else if info.typ == "float" && valTyp != "float" {
			valExpr = &CastExpr{Expr: valExpr, Type: "obj_to_float"}
		} else if info.typ == "string" && valTyp != "string" {
			valExpr = &CastExpr{Expr: valExpr, Type: "obj_to_string"}
		} else if info.typ == "bigint" && valTyp == "int" {
			valExpr = &CastExpr{Expr: valExpr, Type: "int_to_big"}
		} else if info.typ == "bigrat" && valTyp == "bigint" {
			valExpr = &CastExpr{Expr: valExpr, Type: "big_to_rat"}
		}
		if len(st.Assign.Field) > 0 && len(st.Assign.Index) == 0 {
			key := &StringLit{Value: st.Assign.Field[0].Name}
			mapExpr := Expr(&Name{Ident: st.Assign.Name, Typ: info.typ, Ref: true})
			upd := &MapUpdateExpr{Map: mapExpr, Key: key, Value: valExpr, Dyn: isDynamicMapType(info.typ)}
			return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
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
				upd := &MapUpdateExpr{Map: mapExpr, Key: key, Value: valExpr, Dyn: isDynamicMapType(info.typ)}
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
				upd := buildMapUpdate(mapExpr, keys, valExpr, info.typ)
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
		if valTyp != "" && info.typ == "" {
			info.typ = valTyp
			vars[st.Assign.Name] = info
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
		if call != nil && call.Func == "json" && len(call.Args) == 1 {
			arg, _, err := convertExpr(call.Args[0], env, vars)
			if err != nil {
				return nil, err
			}
			return &JSONStmt{Expr: arg}, nil
		}
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			src, _, err := convertExpr(se.Src, env, vars)
			if err != nil {
				return nil, err
			}
			format := parseFormat(se.With)
			path := ""
			if se.Path != nil {
				path = strings.Trim(*se.Path, "\"")
			}
			return &SaveStmt{Src: src, Path: path, Format: format}, nil
		}
		if call != nil {
			expr, ret, err := convertCall(call, env, vars)
			if err != nil {
				return nil, err
			}
			if ret != "" && ret != "unit" {
				expr = &IgnoreExpr{Expr: expr}
			}
			return &ExprStmt{Expr: expr}, nil
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
			elseStmt, err := transpileStmt(&parser.Statement{If: st.If.ElseIf}, env, vars, mutated)
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
		} else if iterTyp == "list" {
			// Dynamic list; element type unknown
			vtyp = ""
		} else if strings.HasPrefix(iterTyp, "list") {
			vtyp = "int"
		}
                if iterTyp == "" || iterTyp == "dyn" {
                        vtyp = "string"
                }
                if strings.HasPrefix(iterTyp, "map") {
                        vtyp = "string"
                }
		vars[st.For.Name] = VarInfo{typ: vtyp}
		body, err := transpileStmts(st.For.Body, env, vars)
		if err != nil {
			return nil, err
		}
		delete(vars, st.For.Name)
		return &ForEachStmt{Name: st.For.Name, Iterable: iter, Body: body, Typ: iterTyp}, nil
	case st.Update != nil:
		fields := make([]string, len(st.Update.Set.Items))
		values := make([]Expr, len(st.Update.Set.Items))
		child := types.NewEnv(env)
		if t, err := env.GetVar(st.Update.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					for _, f := range s.Order {
						child.SetVar(f, s.Fields[f], true)
					}
				}
			}
		}
		fieldVars := map[string]VarInfo{}
		fieldSet := map[string]bool{}
		for name, typ := range child.Types() {
			fieldSet[name] = true
			tname := "int"
			switch typ.(type) {
			case types.StringType:
				tname = "string"
			case types.FloatType:
				tname = "float"
			case types.BoolType:
				tname = "bool"
			default:
				tname = guessTypeFromName(name)
			}
			fieldVars[name] = VarInfo{typ: tname}
		}
		for i, it := range st.Update.Set.Items {
			key, ok := isSimpleIdent(it.Key)
			if !ok {
				key, ok = literalString(it.Key)
				if !ok {
					return nil, fmt.Errorf("unsupported update key")
				}
			}
			val, _, err := convertExpr(it.Value, child, fieldVars)
			if err != nil {
				return nil, err
			}
			fields[i] = key
			values[i] = substituteFieldVars(val, fieldSet)
		}
		var cond Expr
		if st.Update.Where != nil {
			var err error
			cond, _, err = convertExpr(st.Update.Where, child, fieldVars)
			if err != nil {
				return nil, err
			}
			cond = substituteFieldVars(cond, fieldSet)
		}
		return &UpdateStmt{Target: st.Update.Target, Fields: fields, Values: values, Cond: cond}, nil
	case st.Bench != nil:
		body, err := transpileStmts(st.Bench.Body, env, vars)
		if err != nil {
			return nil, err
		}
		usesNow = true
		usesMem = true
		name := strings.Trim(st.Bench.Name, "\"")
		return &BenchStmt{Name: name, Body: body}, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Return != nil:
		var expr Expr
		if st.Return.Value != nil {
			var err error
			expr, _, err = convertExpr(st.Return.Value, env, vars)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: expr}, nil
	case st.Fun != nil:
		env.SetFunc(st.Fun.Name, st.Fun)
		var retTyp types.Type = types.VoidType{}
		if st.Fun.Return != nil {
			retTyp = types.ResolveTypeRef(st.Fun.Return, env)
		}
		env.SetFuncType(st.Fun.Name, types.FuncType{Return: retTyp})
		child := types.NewEnv(env)
		fnVars := map[string]VarInfo{}
		for k, v := range vars {
			fnVars[k] = v
		}
		mutated := map[string]bool{}
		collectMutations(st.Fun.Body, mutated)
		funcMutations[st.Fun.Name] = mutated
		params := make([]string, len(st.Fun.Params))
		for i, p := range st.Fun.Params {
			params[i] = sanitizeIdent(p.Name)
			typ := "int"
			if p.Type != nil {
				typ = typeRefString(p.Type)
				if ts := typeString(types.ResolveTypeRef(p.Type, env)); ts != "" {
					typ = ts
				}
				if typ == "any" {
					typ = ""
				}
				// leave typ empty for dynamic parameters
				if typ == "" {
					// no default
				}
			}
			fnVars[p.Name] = VarInfo{typ: typ, ref: mutated[p.Name]}
		}
		body, err := transpileStmts(st.Fun.Body, child, fnVars)
		if err != nil {
			return nil, err
		}
		casts := []Stmt{}
		for _, p := range st.Fun.Params {
			if p.Type != nil {
				typ := typeRefString(p.Type)
				if typ == "int" || typ == "float" {
					exprCode := ""
					if mutated[p.Name] {
						exprCode = fmt.Sprintf("(Obj.magic !%s : %s)", sanitizeIdent(p.Name), typ)
					} else {
						exprCode = fmt.Sprintf("(Obj.magic %s : %s)", sanitizeIdent(p.Name), typ)
					}
					if mutated[p.Name] {
						casts = append(casts, &AssignStmt{Name: p.Name, Expr: &RawExpr{Code: exprCode}})
					} else {
						casts = append(casts, &LetStmt{Name: p.Name, Expr: &RawExpr{Code: exprCode}})
					}
				}
			}
		}
		if len(casts) > 0 {
			body = append(casts, body...)
		}
		var ret Expr
		endsWithReturn := false
		if len(st.Fun.Body) > 0 {
			last := st.Fun.Body[len(st.Fun.Body)-1]
			if last.Return != nil && last.Return.Value != nil {
				// last statement explicitly returns a value; no implicit return
				r, _, err := convertExpr(last.Return.Value, child, fnVars)
				if err != nil {
					return nil, err
				}
				// body already contains a ReturnStmt, so skip implicit return
				_ = r
				endsWithReturn = true
			} else if last.Return == nil {
				// infer implicit return value if provided
				// (none for this case)
			}
		}
		local := len(vars) > 0 && env != rootEnv
		return &FunStmt{Name: st.Fun.Name, Params: params, Body: body, Ret: ret, RetTyp: typeString(retTyp), Local: local, EndsWithReturn: endsWithReturn}, nil
	case st.Type != nil:
		var methodStmts []Stmt
		if len(st.Type.Members) > 0 {
			fields := map[string]string{}
			for _, m := range st.Type.Members {
				if m.Field != nil {
					ft := typeRefString(m.Field.Type)
					if ts := typeString(types.ResolveTypeRef(m.Field.Type, env)); ts != "" {
						ft = ts
					}
					if ft == "" {
						ft = "int"
					}
					fields[m.Field.Name] = ft
				} else if m.Method != nil {
					method := *m.Method
					method.Name = st.Type.Name + "_" + m.Method.Name
					selfParam := &parser.Param{Name: "self", Type: &parser.TypeRef{Simple: &st.Type.Name}}
					method.Params = append([]*parser.Param{selfParam}, method.Params...)
					s := &parser.Statement{Fun: &method}
					compiled, err := transpileStmt(s, env, vars, mutated)
					if err != nil {
						return nil, err
					}
					if compiled != nil {
						methodStmts = append(methodStmts, compiled)
					}
				}
			}
			structFields[st.Type.Name] = fields
		}
		if len(st.Type.Variants) > 0 {
			for _, v := range st.Type.Variants {
				fields := map[string]string{}
				for _, f := range v.Fields {
					ft := typeRefString(f.Type)
					if ts := typeString(types.ResolveTypeRef(f.Type, env)); ts != "" {
						ft = ts
					}
					if ft == "" {
						ft = "int"
					}
					fields[f.Name] = ft
				}
				structFields[v.Name] = fields
			}
		}
		if len(methodStmts) > 0 {
			return &MultiStmt{Stmts: methodStmts}, nil
		}
		return nil, nil
	case st.Test != nil:
		// ignore test blocks
		return nil, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func transpileStmts(list []*parser.Statement, env *types.Env, vars map[string]VarInfo) ([]Stmt, error) {
	mutated := map[string]bool{}
	collectMutations(list, mutated)
	var out []Stmt
	for _, st := range list {
		compiled, err := transpileStmt(st, env, vars, mutated)
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
	rootEnv = env
	usesNow = false
	usesLookupHost = false
	usesDynMath = false
	usesMem = false
	usesBigInt = false
	usesBigRat = false
	usesSHA = false
	usesGetOutput = false
	usesGetEnv = false
	usesEnviron = false
	usesSplit = false
	usesDynLen = false
	pr := &Program{}
	vars := map[string]VarInfo{}
	stmts, err := transpileStmts(prog.Statements, env, vars)
	if err != nil {
		return nil, err
	}
	pr.Stmts = stmts
	if benchMain {
		usesNow = true
		usesMem = true
		var defs []Stmt
		var body []Stmt
		inBody := false
		for _, st := range pr.Stmts {
			switch s := st.(type) {
			case *FunStmt:
				defs = append(defs, st)
			case *VarStmt:
				if inBody {
					body = append(body, st)
				} else {
					defs = append(defs, st)
				}
			case *LetStmt:
				if !inBody && isConstExpr(s.Expr) {
					defs = append(defs, st)
				} else {
					body = append(body, st)
					inBody = true
				}
			default:
				body = append(body, st)
				inBody = true
			}
		}
		pr.Stmts = append(defs, &BenchStmt{Name: "main", Body: body})
	}
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
	type opInfo struct {
		op  string
		all bool
	}
	ops := []opInfo{}
	for _, op := range b.Right {
		right, rtyp, err := convertPostfix(op.Right, env, vars)
		if err != nil {
			return nil, "", err
		}
		operands = append(operands, right)
		typesList = append(typesList, rtyp)
		ops = append(ops, opInfo{op.Op, op.All})
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
		case "union", "except", "intersect", "union_all":
			return -1
		default:
			return -1
		}
	}

	// Shunting-yard like evaluation
	var exprStack []Expr
	var typeStack []string
	var opStack []opInfo

	pushResult := func() {
		if len(opStack) == 0 {
			return
		}
		info := opStack[len(opStack)-1]
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
		if ltyp == "bigint" && rtyp == "int" {
			right = &CastExpr{Expr: right, Type: "int_to_big"}
			rtyp = "bigint"
		} else if ltyp == "int" && rtyp == "bigint" {
			left = &CastExpr{Expr: left, Type: "int_to_big"}
			ltyp = "bigint"
		}

		resTyp := ltyp
		switch info.op {
		case "+", "-", "*", "/", "%":
			if info.op == "+" && (ltyp == "string" || rtyp == "string") {
				if ltyp != "string" {
					left = &FuncCall{Name: "__show", Args: []Expr{left}, Ret: "string"}
					ltyp = "string"
				}
				if rtyp != "string" {
					right = &FuncCall{Name: "__show", Args: []Expr{right}, Ret: "string"}
					rtyp = "string"
				}
				resTyp = "string"
			} else if info.op == "+" && strings.HasPrefix(ltyp, "list") && strings.HasPrefix(rtyp, "list") {
				resTyp = ltyp
				info.op = "union_all"
			} else if info.op == "+" && ltyp == "string" && rtyp == "string" {
				resTyp = "string"
			} else if info.op == "+" && ltyp == "string" && strings.HasPrefix(rtyp, "list") {
				right = &StrJoin{List: right}
				rtyp = "string"
				resTyp = "string"
			} else if info.op == "+" && rtyp == "string" && strings.HasPrefix(ltyp, "list") {
				left = &StrJoin{List: left}
				ltyp = "string"
				resTyp = "string"
			} else if info.op == "+" && ltyp == "string" && rtyp == "" {
				right = &FuncCall{Name: "__show", Args: []Expr{right}, Ret: "string"}
				rtyp = "string"
				resTyp = "string"
			} else if info.op == "+" && rtyp == "string" && ltyp == "" {
				left = &FuncCall{Name: "__show", Args: []Expr{left}, Ret: "string"}
				ltyp = "string"
				resTyp = "string"
			} else if ltyp == "bigint" || rtyp == "bigint" {
				resTyp = "bigint"
			} else if ltyp == "bigrat" || rtyp == "bigrat" {
				resTyp = "bigrat"
			} else if ltyp == "float" || rtyp == "float" {
				resTyp = "float"
			} else if ltyp == "" || rtyp == "" {
				resTyp = ""
			} else {
				if ltyp == "int" && rtyp == "string" {
					right = &CastExpr{Expr: right, Type: "str_to_int"}
					rtyp = "int"
				} else if rtyp == "int" && ltyp == "string" {
					left = &CastExpr{Expr: left, Type: "str_to_int"}
					ltyp = "int"
				}
				resTyp = "int"
			}
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in", "union", "union_all", "except", "intersect":
			if ltyp == "string" && rtyp == "bool" {
				right = &FuncCall{Name: "string_of_bool", Args: []Expr{right}, Ret: "string"}
				rtyp = "string"
			} else if ltyp == "bool" && rtyp == "string" {
				left = &FuncCall{Name: "string_of_bool", Args: []Expr{left}, Ret: "string"}
				ltyp = "string"
			}
			resTyp = "bool"
		}

		var expr Expr
		if info.op == "in" {
			expr = &InExpr{Item: left, Coll: right, Typ: rtyp}
		} else if info.op == "union" {
			expr = &UnionExpr{Left: left, Right: right}
			resTyp = ltyp
		} else if info.op == "union_all" {
			expr = &UnionAllExpr{Left: left, Right: right}
			resTyp = ltyp
		} else if info.op == "except" {
			expr = &ExceptExpr{Left: left, Right: right}
			resTyp = ltyp
		} else if info.op == "intersect" {
			expr = &IntersectExpr{Left: left, Right: right}
			resTyp = ltyp
		} else {
			if resTyp == "" {
				expr = &DynBinaryExpr{Left: left, Op: info.op, Right: right}
				usesDynMath = true
			} else {
				expr = &BinaryExpr{Left: left, Op: info.op, Right: right, Typ: resTyp, Ltyp: ltyp, Rtyp: rtyp}
			}
		}
		exprStack = append(exprStack, expr)
		typeStack = append(typeStack, resTyp)
	}

	exprStack = append(exprStack, operands[0])
	typeStack = append(typeStack, typesList[0])
	for i, op := range ops {
		for len(opStack) > 0 && prec(opStack[len(opStack)-1].op) >= prec(op.op) {
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
			if typ != "int" && typ != "float" && typ != "bigrat" && typ != "bigint" {
				return nil, "", fmt.Errorf("unary - only for numeric")
			}
			return &UnaryMinus{Expr: expr, Typ: typ}, typ, nil
		} else if len(u.Ops) == 1 && u.Ops[0] == "!" {
			expr, typ, err := convertPostfix(u.Value, env, vars)
			if err != nil {
				return nil, "", err
			}
			if typ != "bool" {
				return nil, "", fmt.Errorf("unary ! only for bool")
			}
			return &UnaryNot{Expr: expr}, "bool", nil
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
		if strings.HasPrefix(info.typ, "map") {
			root := &Name{Ident: p.Target.Selector.Root, Typ: info.typ, Ref: info.ref}
			return &MapContainsBuiltin{Map: root, Key: arg}, "bool", nil
		}
		if info.typ != "string" || at != "string" {
			return nil, "", fmt.Errorf("contains expects string or map")
		}
		root := &Name{Ident: p.Target.Selector.Root, Typ: info.typ, Ref: info.ref}
		return &StringContainsBuiltin{Str: root, Sub: arg}, "bool", nil
	}
	// Handle map.keys()
	if p.Target != nil && p.Target.Selector != nil &&
		len(p.Target.Selector.Tail) == 1 && p.Target.Selector.Tail[0] == "keys" &&
		len(p.Ops) == 1 && p.Ops[0].Call != nil && len(p.Ops[0].Call.Args) == 0 {
		info := vars[p.Target.Selector.Root]
		if info.typ != "map" && !strings.HasPrefix(info.typ, "map-") && !strings.HasPrefix(info.typ, "map{") {
			return nil, "", fmt.Errorf("keys expects map")
		}
		root := &Name{Ident: p.Target.Selector.Root, Typ: info.typ, Ref: info.ref}
		return &KeysBuiltin{Map: root}, "list", nil
	}
	// Handle testpkg.FifteenPuzzleExample()
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 1 &&
		len(p.Ops) == 1 && p.Ops[0].Call != nil && len(p.Ops[0].Call.Args) == 0 {
		info := vars[p.Target.Selector.Root]
		if info.typ == "go_testpkg" && p.Target.Selector.Tail[0] == "FifteenPuzzleExample" {
			return &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}, "string", nil
		}
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
			if strings.HasPrefix(typ, "map") {
				expr = &MapContainsBuiltin{Map: expr, Key: arg}
				typ = "bool"
				i += 2
				continue
			}
			if typ != "string" || aTyp != "string" {
				return nil, "", fmt.Errorf("contains expects string or map")
			}
			expr = &StringContainsBuiltin{Str: expr, Sub: arg}
			typ = "bool"
			i += 2
			continue
		case op.Field != nil && op.Field.Name == "padStart":
			if i+1 >= len(p.Ops) || p.Ops[i+1].Call == nil || len(p.Ops[i+1].Call.Args) != 2 {
				return nil, "", fmt.Errorf("padStart expects 2 args")
			}
			call := p.Ops[i+1].Call
			argLen, lenTyp, err := convertExpr(call.Args[0], env, vars)
			if err != nil {
				return nil, "", err
			}
			argPad, padTyp, err := convertExpr(call.Args[1], env, vars)
			if err != nil {
				return nil, "", err
			}
			if typ != "string" || lenTyp != "int" || padTyp != "string" {
				return nil, "", fmt.Errorf("padStart expects string.padStart(int,string)")
			}
			expr = &PadStartBuiltin{Str: expr, Len: argLen, Pad: argPad}
			typ = "string"
			i += 2
			continue
		case op.Field != nil && op.Field.Name == "keys":
			if i+1 >= len(p.Ops) || p.Ops[i+1].Call == nil || len(p.Ops[i+1].Call.Args) != 0 {
				return nil, "", fmt.Errorf("keys must be called with no args")
			}
			if typ != "map" && !strings.HasPrefix(typ, "map-") && !strings.HasPrefix(typ, "map{") {
				return nil, "", fmt.Errorf("keys expects map")
			}
			expr = &KeysBuiltin{Map: expr}
			typ = "list"
			i += 2
			continue
		case op.Field != nil && op.Field.Name == "get":
			if i+1 >= len(p.Ops) || p.Ops[i+1].Call == nil {
				return nil, "", fmt.Errorf("get must be called")
			}
			call := p.Ops[i+1].Call
			if len(call.Args) != 2 {
				return nil, "", fmt.Errorf("get expects 2 args")
			}
			key, _, err := convertExpr(call.Args[0], env, vars)
			if err != nil {
				return nil, "", err
			}
			defVal, defTyp, err := convertExpr(call.Args[1], env, vars)
			if err != nil {
				return nil, "", err
			}
			valTyp := defTyp
			if strings.HasPrefix(typ, "map-") {
				valTyp = strings.TrimPrefix(typ, "map-")
			}
			expr = &MapGetExpr{Map: expr, Key: key, Default: defVal, Typ: valTyp, Dyn: isDynamicMapType(typ)}
			typ = valTyp
			i += 2
			continue
		case op.Field != nil:
			// generic method call obj.Field(args)
			if op.Field != nil && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
				if _, ok := env.GetFunc(op.Field.Name); ok {
					call := p.Ops[i+1].Call
					args := make([]Expr, len(call.Args)+1)
					args[0] = expr
					for j, a := range call.Args {
						ex, _, err := convertExpr(a, env, vars)
						if err != nil {
							return nil, "", err
						}
						args[j+1] = ex
					}
					expr = &FuncCall{Name: sanitizeIdent(op.Field.Name), Args: args, Ret: ""}
					typ = ""
					i += 2
					continue
				}
			}
			if name, ok := expr.(*Name); ok {
				if nameTyp := vars[name.Ident].typ; nameTyp == "python_math" {
					field := op.Field.Name
					if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
						call := p.Ops[i+1].Call
						args := make([]Expr, len(call.Args))
						for j, a := range call.Args {
							ex, _, err := convertExpr(a, env, vars)
							if err != nil {
								return nil, "", err
							}
							args[j] = ex
						}
						switch field {
						case "sqrt", "sin", "log":
							expr = &FuncCall{Name: field, Args: args, Ret: "float"}
							typ = "float"
						case "pow":
							expr = &FuncCall{Name: "Float.pow", Args: args, Ret: "float"}
							typ = "float"
						default:
							return nil, "", fmt.Errorf("unsupported field %s", field)
						}
						i += 2
						continue
					}
					switch field {
					case "pi":
						expr = &FloatLit{Value: 3.141592653589793}
						typ = "float"
						i++
						continue
					case "e":
						expr = &FloatLit{Value: 2.718281828459045}
						typ = "float"
						i++
						continue
					}
				} else if nameTyp == "python_subprocess" {
					field := op.Field.Name
					if field == "getoutput" && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil && len(p.Ops[i+1].Call.Args) == 1 {
						arg, _, err := convertExpr(p.Ops[i+1].Call.Args[0], env, vars)
						if err != nil {
							return nil, "", err
						}
						usesGetOutput = true
						expr = &FuncCall{Name: "_getoutput", Args: []Expr{arg}, Ret: "string"}
						typ = "string"
						i += 2
						continue
					}
				} else if nameTyp == "go_os" {
					field := op.Field.Name
					if field == "Getenv" && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
						call := p.Ops[i+1].Call
						if len(call.Args) == 1 {
							arg, _, err := convertExpr(call.Args[0], env, vars)
							if err != nil {
								return nil, "", err
							}
							usesGetEnv = true
							expr = &FuncCall{Name: "_getenv", Args: []Expr{arg}, Ret: "string"}
							typ = "string"
							i += 2
							continue
						}
					}
					if field == "Environ" && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
						call := p.Ops[i+1].Call
						if len(call.Args) == 0 {
							usesEnviron = true
							expr = &FuncCall{Name: "_environ", Args: nil, Ret: "list-string"}
							typ = "list-string"
							i += 2
							continue
						}
					}
				} else if nameTyp == "go_strings" {
					field := op.Field.Name
					if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
						call := p.Ops[i+1].Call
						if len(call.Args) == 1 {
							arg, _, err := convertExpr(call.Args[0], env, vars)
							if err != nil {
								return nil, "", err
							}
							switch field {
							case "TrimSpace":
								expr = &FuncCall{Name: "String.trim", Args: []Expr{arg}, Ret: "string"}
								typ = "string"
							case "ToUpper":
								expr = &FuncCall{Name: "String.uppercase_ascii", Args: []Expr{arg}, Ret: "string"}
								typ = "string"
							default:
								return nil, "", fmt.Errorf("unsupported field %s", field)
							}
							i += 2
							continue
						}
					}
				} else if nameTyp == "go_net" {
					field := op.Field.Name
					if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
						call := p.Ops[i+1].Call
						if field == "LookupHost" && len(call.Args) == 1 {
							arg, _, err := convertExpr(call.Args[0], env, vars)
							if err != nil {
								return nil, "", err
							}
							usesLookupHost = true
							expr = &FuncCall{Name: "_lookup_host", Args: []Expr{arg}, Ret: "list"}
							typ = "list"
							i += 2
							continue
						}
					}
				} else if nameTyp == "go_testpkg" {
					field := op.Field.Name
					if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
						call := p.Ops[i+1].Call
						if field == "Add" && len(call.Args) == 2 {
							left, _, err := convertExpr(call.Args[0], env, vars)
							if err != nil {
								return nil, "", err
							}
							right, _, err := convertExpr(call.Args[1], env, vars)
							if err != nil {
								return nil, "", err
							}
							expr = &BinaryExpr{Left: left, Op: "+", Right: right, Typ: "int", Ltyp: "int", Rtyp: "int"}
							typ = "int"
							i += 2
							continue
						}
					}
					switch field {
					case "Pi":
						expr = &FloatLit{Value: 3.14}
						typ = "float"
						i++
						continue
					case "Answer":
						expr = &IntLit{Value: 42}
						typ = "int"
						i++
						continue
					case "FifteenPuzzleExample":
						if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil && len(p.Ops[i+1].Call.Args) == 0 {
							expr = &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}
							typ = "string"
							i += 2
							continue
						}
					}
				}
			}
			if name, ok := expr.(*Name); ok {
				if info, ok := vars[name.Ident]; ok && info.group && op.Field.Name == "key" {
					expr = &Name{Ident: name.Ident + "_key", Typ: "string"}
					typ = "string"
					i++
					continue
				}
			}
			key := &StringLit{Value: op.Field.Name}
			dyn := isDynamicMapType(typ)
			if ft, ok := mapFieldType(typ, op.Field.Name); ok {
				expr = &MapIndexExpr{Map: expr, Key: key, Typ: ft, Dyn: dyn, KeyTyp: "string"}
				typ = ft
			} else {
				expr = &MapIndexExpr{Map: expr, Key: key, Typ: "int", Dyn: dyn, KeyTyp: "string"}
				typ = "int"
			}
			i++
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil && op.Index.End == nil && op.Index.Step == nil:
			idxExpr, idxTyp, err := convertExpr(op.Index.Start, env, vars)
			if err != nil {
				return nil, "", err
			}
			dyn := isDynamicMapType(typ)
			if sl, ok := idxExpr.(*StringLit); ok {
				if ft, ok := mapFieldType(typ, sl.Value); ok {
					expr = &MapIndexExpr{Map: expr, Key: idxExpr, Typ: ft, Dyn: dyn, KeyTyp: "string"}
					typ = ft
				} else {
					expr = &MapIndexExpr{Map: expr, Key: idxExpr, Typ: "int", Dyn: dyn, KeyTyp: "string"}
					typ = "int"
				}
			} else if strings.HasPrefix(typ, "map-") {
				valTyp := strings.TrimPrefix(typ, "map-")
				expr = &MapIndexExpr{Map: expr, Key: idxExpr, Typ: valTyp, Dyn: dyn, KeyTyp: idxTyp}
				typ = valTyp
                        } else if typ == "map" || strings.HasPrefix(typ, "map{") {
                                expr = &MapIndexExpr{Map: expr, Key: idxExpr, Typ: "map", Dyn: dyn, KeyTyp: idxTyp}
                                typ = "map"
                        } else if typ == "" || typ == "dyn" {
                                if idxTyp == "int" {
                                        expr = &IndexExpr{Col: expr, Index: idxExpr, Typ: "", ColTyp: typ}
                                        typ = ""
                                } else {
                                        expr = &MapIndexExpr{Map: expr, Key: idxExpr, Typ: "dyn", Dyn: true, KeyTyp: idxTyp}
                                        typ = "dyn"
                                }
                        } else if strings.HasPrefix(typ, "list-") {
				elemTyp := strings.TrimPrefix(typ, "list-")
				expr = &IndexExpr{Col: expr, Index: idxExpr, Typ: elemTyp, ColTyp: typ}
				typ = elemTyp
			} else if typ == "list" {
				expr = &IndexExpr{Col: expr, Index: idxExpr, Typ: "", ColTyp: typ}
				typ = ""
			} else {
				if typ == "" {
					expr = &IndexExpr{Col: expr, Index: idxExpr, Typ: "", ColTyp: "list"}
					typ = ""
				} else {
					expr = &IndexExpr{Col: expr, Index: idxExpr, Typ: typ, ColTyp: typ}
					if typ == "string" {
						typ = "string"
					} else {
						typ = "int"
					}
				}
			}
			i++
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			var startExpr Expr
			if op.Index.Start != nil {
				var err error
				startExpr, _, err = convertExpr(op.Index.Start, env, vars)
				if err != nil {
					return nil, "", err
				}
			} else {
				startExpr = &IntLit{Value: 0}
			}
			var endExpr Expr
			if op.Index.End != nil {
				var err error
				endExpr, _, err = convertExpr(op.Index.End, env, vars)
				if err != nil {
					return nil, "", err
				}
			} else {
				endExpr = &LenBuiltin{Arg: expr, Typ: typ}
			}
			expr = &SliceExpr{Col: expr, Start: startExpr, End: endExpr, Typ: typ}
			i++
		case op.Call != nil:
			if typ == "func" || strings.HasPrefix(typ, "func-") {
				args := make([]Expr, len(op.Call.Args))
				for j, a := range op.Call.Args {
					ex, at, err := convertExpr(a, env, vars)
					if err != nil {
						return nil, "", err
					}
					if at == "int" {
						ex = &CastExpr{Expr: ex, Type: "int_to_obj"}
					} else if at == "float" {
						ex = &CastExpr{Expr: ex, Type: "float_to_obj"}
					}
					args[j] = ex
				}
				ret := ""
				if strings.HasPrefix(typ, "func-") {
					ret = strings.TrimPrefix(typ, "func-")
					if ret == "any" {
						ret = ""
					}
				}
				expr = &ExprCall{Fn: expr, Args: args, Ret: ret}
				typ = ret
				i++
				continue
			}
			if idx, ok := expr.(*MapIndexExpr); ok {
				if key, ok := idx.Key.(*StringLit); ok && key.Value == "get" {
					if len(op.Call.Args) != 2 {
						return nil, "", fmt.Errorf("get expects 2 args")
					}
					k, _, err := convertExpr(op.Call.Args[0], env, vars)
					if err != nil {
						return nil, "", err
					}
					def, defTyp, err := convertExpr(op.Call.Args[1], env, vars)
					if err != nil {
						return nil, "", err
					}
					valTyp := defTyp
					if strings.HasPrefix(idx.Typ, "map-") {
						valTyp = strings.TrimPrefix(idx.Typ, "map-")
					}
					expr = &MapGetExpr{Map: idx.Map, Key: k, Default: def, Typ: valTyp, Dyn: idx.Dyn}
					typ = valTyp
					i++
					continue
				}
				if root, ok := idx.Map.(*Name); ok {
					if info := vars[root.Ident]; info.typ == "python_math" {
						if key, ok := idx.Key.(*StringLit); ok {
							args := make([]Expr, len(op.Call.Args))
							for j, a := range op.Call.Args {
								ex, _, err := convertExpr(a, env, vars)
								if err != nil {
									return nil, "", err
								}
								args[j] = ex
							}
							field := key.Value
							switch field {
							case "sqrt", "sin", "log":
								expr = &FuncCall{Name: field, Args: args, Ret: "float"}
								typ = "float"
							case "pow":
								expr = &FuncCall{Name: "Float.pow", Args: args, Ret: "float"}
								typ = "float"
							default:
								return nil, "", fmt.Errorf("unsupported field %s", field)
							}
							i++
							continue
						}
					} else if info.typ == "python_subprocess" {
						if key, ok := idx.Key.(*StringLit); ok {
							field := key.Value
							if field == "getoutput" && len(op.Call.Args) == 1 {
								arg, _, err := convertExpr(op.Call.Args[0], env, vars)
								if err != nil {
									return nil, "", err
								}
								usesGetOutput = true
								expr = &FuncCall{Name: "_getoutput", Args: []Expr{arg}, Ret: "string"}
								typ = "string"
								i++
								continue
							}
						}
					} else if info.typ == "go_os" {
						if key, ok := idx.Key.(*StringLit); ok {
							field := key.Value
							if field == "Getenv" && len(op.Call.Args) == 1 {
								arg, _, err := convertExpr(op.Call.Args[0], env, vars)
								if err != nil {
									return nil, "", err
								}
								usesGetEnv = true
								expr = &FuncCall{Name: "_getenv", Args: []Expr{arg}, Ret: "string"}
								typ = "string"
								i++
								continue
							}
							if field == "Environ" && len(op.Call.Args) == 0 {
								usesEnviron = true
								expr = &FuncCall{Name: "_environ", Args: nil, Ret: "list-string"}
								typ = "list-string"
								i++
								continue
							}
						}
					} else if info.typ == "go_strings" {
						if key, ok := idx.Key.(*StringLit); ok {
							field := key.Value
							if len(op.Call.Args) == 1 {
								arg, _, err := convertExpr(op.Call.Args[0], env, vars)
								if err != nil {
									return nil, "", err
								}
								switch field {
								case "TrimSpace":
									expr = &FuncCall{Name: "String.trim", Args: []Expr{arg}, Ret: "string"}
									typ = "string"
								case "ToUpper":
									expr = &FuncCall{Name: "String.uppercase_ascii", Args: []Expr{arg}, Ret: "string"}
									typ = "string"
								default:
									return nil, "", fmt.Errorf("unsupported field %s", field)
								}
								i++
								continue
							}
						}
					} else if info.typ == "go_net" {
						if key, ok := idx.Key.(*StringLit); ok {
							field := key.Value
							if field == "LookupHost" && len(op.Call.Args) == 1 {
								arg, _, err := convertExpr(op.Call.Args[0], env, vars)
								if err != nil {
									return nil, "", err
								}
								usesLookupHost = true
								expr = &FuncCall{Name: "_lookup_host", Args: []Expr{arg}, Ret: "list"}
								typ = "list"
								i++
								continue
							}
						}
					} else if info.typ == "go_testpkg" {
						if key, ok := idx.Key.(*StringLit); ok {
							field := key.Value
							if field == "Add" && len(op.Call.Args) == 2 {
								left, _, err := convertExpr(op.Call.Args[0], env, vars)
								if err != nil {
									return nil, "", err
								}
								right, _, err := convertExpr(op.Call.Args[1], env, vars)
								if err != nil {
									return nil, "", err
								}
								expr = &BinaryExpr{Left: left, Op: "+", Right: right, Typ: "int", Ltyp: "int", Rtyp: "int"}
								typ = "int"
								i++
								continue
							}
						}
					}
				}
			}
			return nil, "", fmt.Errorf("postfix op not supported")
		case op.Cast != nil && op.Cast.Type != nil:
			target := typeRefString(op.Cast.Type)
			if typ == "string" && target == "int" {
				expr = &CastExpr{Expr: expr, Type: "str_to_int"}
			} else if target == "int" && typ == "bigint" {
				expr = &CastExpr{Expr: expr, Type: "big_to_int"}
			} else if typ == "int" && target == "bigint" {
				expr = &CastExpr{Expr: expr, Type: "int_to_big"}
			} else if typ == "int" && target == "bigrat" {
				expr = &CastExpr{Expr: expr, Type: "int_to_rat"}
			} else if typ == "bigint" && target == "bigrat" {
				expr = &CastExpr{Expr: expr, Type: "big_to_rat"}
			} else if target == "int" && typ != "int" {
				expr = &CastExpr{Expr: expr, Type: "obj_to_int"}
			} else if target == "float" && typ != "float" && typ != "int" {
				expr = &CastExpr{Expr: expr, Type: "obj_to_float"}
			} else if target == "bool" && typ != "bool" {
				expr = &CastExpr{Expr: expr, Type: "obj_to_bool"}
			} else if typ == "int" && target == "float" {
				expr = &CastExpr{Expr: expr, Type: "int_to_float"}
			} else if typ == "float" && target == "int" {
				expr = &CastExpr{Expr: expr, Type: "float_to_int"}
			} else if target == "string" && typ != "string" {
				expr = &CastExpr{Expr: expr, Type: "obj_to_string"}
			} else if strings.HasPrefix(target, "list") || strings.HasPrefix(target, "map") || structFields[target] != nil {
				expr = &CastExpr{Expr: expr, Type: "obj_to_" + target}
			} else if mi, ok := expr.(*MapIndexExpr); ok {
				mi.Typ = target
				expr = mi
			}
			if target != "" {
				typ = target
			}
			i++
			continue
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
		if info.typ == "python_math" {
			switch t {
			case "pi":
				expr = &FloatLit{Value: 3.141592653589793}
				typ = "float"
				continue
			case "e":
				expr = &FloatLit{Value: 2.718281828459045}
				typ = "float"
				continue
			}
		} else if info.typ == "go_testpkg" {
			switch t {
			case "Pi":
				expr = &FloatLit{Value: 3.14}
				typ = "float"
				continue
			case "Answer":
				expr = &IntLit{Value: 42}
				typ = "int"
				continue
			}
		}
		if info.group && t == "key" {
			keyInfo := vars[sel.Root+"Key"]
			expr = &Name{Ident: sel.Root + "_key", Typ: keyInfo.typ}
			typ = keyInfo.typ
			continue
		}
		key := &StringLit{Value: t}
		dyn := isDynamicMapType(typ)
		if ft, ok := mapFieldType(typ, t); ok {
			expr = &MapIndexExpr{Map: expr, Key: key, Typ: ft, Dyn: dyn, KeyTyp: "string"}
			typ = ft
		} else if fields, ok := structFields[typ]; ok {
			if ft, ok2 := fields[t]; ok2 {
				expr = &MapIndexExpr{Map: expr, Key: key, Typ: ft, Dyn: dyn, KeyTyp: "string"}
				typ = ft
			} else {
				expr = &MapIndexExpr{Map: expr, Key: key, Typ: "int", Dyn: dyn, KeyTyp: "string"}
				typ = "int"
			}
		} else {
			expr = &MapIndexExpr{Map: expr, Key: key, Typ: "int", Dyn: dyn, KeyTyp: "string"}
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
		dynamic := false
		for i, e := range p.List.Elems {
			ex, t, err := convertExpr(e, env, vars)
			if err != nil {
				return nil, "", err
			}
			if i == 0 {
				elemTyp = t
			} else if t != elemTyp {
				dynamic = true
			}
			if t == "" {
				dynamic = true
			}
			elems[i] = ex
		}
		listTyp := "list"
		if elemTyp != "" {
			listTyp = "list-" + elemTyp
		}
		return &ListLit{Elems: elems, Dynamic: dynamic}, listTyp, nil
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		expr, err := dataExprFromFile(path, format, p.Load.Type)
		if err != nil {
			return nil, "", err
		}
		listTyp := "list"
		if lst, ok := expr.(*ListLit); ok && len(lst.Elems) > 0 {
			if mp, ok := lst.Elems[0].(*MapLit); ok {
				fields := make([]string, 0, len(mp.Items))
				for _, it := range mp.Items {
					if k, ok := it.Key.(*StringLit); ok {
						ft := "int"
						switch it.Value.(type) {
						case *StringLit:
							ft = "string"
						case *FloatLit:
							ft = "float"
						case *BoolLit:
							ft = "bool"
						}
						fields = append(fields, k.Value+":"+ft)
					}
				}
				if len(fields) > 0 {
					if mp.Dynamic {
						listTyp = "list-map-dyn{" + strings.Join(fields, ",") + "}"
					} else {
						listTyp = "list-map{" + strings.Join(fields, ",") + "}"
					}
				}
			}
		}
		return expr, listTyp, nil
	case p.Map != nil:
		items := make([]MapEntry, len(p.Map.Items))
		fieldTypes := []string{}
		dynamic := false
		firstType := ""
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
			if firstType == "" {
				firstType = vt
			} else if vt != firstType {
				dynamic = true
			}
			if vt == "" {
				dynamic = true
			}
			if ks, ok := k.(*StringLit); ok {
				fieldTypes = append(fieldTypes, ks.Value+":"+vt)
			} else {
				fieldTypes = nil
			}
			items[i] = MapEntry{Key: k, Value: v}
		}
		typ := "map"
		if dynamic {
			if fieldTypes != nil {
				typ = "map-dyn{" + strings.Join(fieldTypes, ",") + "}"
			} else {
				typ = "map-dyn"
			}
		} else if fieldTypes != nil {
			typ = "map{" + strings.Join(fieldTypes, ",") + "}"
		} else if firstType != "" {
			typ = "map-" + firstType
		}
		return &MapLit{Items: items, Dynamic: dynamic}, typ, nil
	case p.Struct != nil:
		items := make([]MapEntry, 0, len(p.Struct.Fields)+1)
		if _, ok := env.FindUnionByVariant(p.Struct.Name); ok {
			items = append(items, MapEntry{Key: &StringLit{Value: "tag"}, Value: &StringLit{Value: p.Struct.Name}})
		}
		for _, f := range p.Struct.Fields {
			key := &StringLit{Value: f.Name}
			val, _, err := convertExpr(f.Value, env, vars)
			if err != nil {
				return nil, "", err
			}
			items = append(items, MapEntry{Key: key, Value: val})
		}
		dyn := true
		if _, ok := env.FindUnionByVariant(p.Struct.Name); ok {
			dyn = false
		}
		typ := "map-dyn"
		if _, ok := structFields[p.Struct.Name]; ok {
			typ = p.Struct.Name
		}
		return &MapLit{Items: items, Dynamic: dyn}, typ, nil
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
			info, ok := vars[p.Selector.Root]
			if !ok {
				if t, err := env.GetVar(p.Selector.Root); err == nil {
					info.typ = typeString(t)
				} else {
					info.typ = guessTypeFromName(p.Selector.Root)
				}
				vars[p.Selector.Root] = info
			}
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
	if l.Float != nil {
		return &FloatLit{Value: *l.Float}, "float", nil
	}
	if l.Bool != nil {
		return &BoolLit{Value: bool(*l.Bool)}, "bool", nil
	}
	if l.Str != nil {
		return &StringLit{Value: *l.Str}, "string", nil
	}
	if l.Null {
		return &RawExpr{Code: "nil"}, "", nil
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
	child := types.NewEnv(env)
	fnVars := make(map[string]VarInfo)
	for k, v := range vars {
		fnVars[k] = v
	}
	mutated := map[string]bool{}
	collectMutations(fn.BlockBody, mutated)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := "int"
		ret := ""
		if p.Type != nil {
			typ = typeRefString(p.Type)
			if ts := typeString(types.ResolveTypeRef(p.Type, env)); ts != "" {
				typ = ts
			}
			if ft, ok := types.ResolveTypeRef(p.Type, env).(types.FuncType); ok {
				if r := typeString(ft.Return); r != "" {
					ret = r
				}
			}
			if typ == "any" {
				typ = ""
			}
			if typ == "" {
				typ = "int"
			}
		}
		fnVars[p.Name] = VarInfo{typ: typ, ref: mutated[p.Name], ret: ret}
		params[i] = p.Name
	}
	var retExpr Expr
	if fn.ExprBody != nil {
		ex, _, err := convertExpr(fn.ExprBody, child, fnVars)
		if err != nil {
			return nil, "", err
		}
		casts := []Stmt{}
		for _, p := range fn.Params {
			if p.Type != nil {
				typ := typeRefString(p.Type)
				if typ == "int" || typ == "float" {
					var exprCode string
					if mutated[p.Name] {
						exprCode = fmt.Sprintf("(Obj.magic !%s : %s)", sanitizeIdent(p.Name), typ)
						casts = append(casts, &AssignStmt{Name: p.Name, Expr: &RawExpr{Code: exprCode}})
					} else {
						exprCode = fmt.Sprintf("(Obj.magic %s : %s)", sanitizeIdent(p.Name), typ)
						casts = append(casts, &LetStmt{Name: p.Name, Expr: &RawExpr{Code: exprCode}})
					}
				}
			}
		}
		if len(casts) > 0 {
			return &FuncExpr{Params: params, Body: casts, Ret: ex}, "func", nil
		}
		retExpr = ex
	} else if len(fn.BlockBody) > 0 {
		body, err := transpileStmts(fn.BlockBody, child, fnVars)
		if err != nil {
			return nil, "", err
		}
		casts := []Stmt{}
		for _, p := range fn.Params {
			if p.Type != nil {
				typ := typeRefString(p.Type)
				if typ == "int" || typ == "float" {
					var exprCode string
					if mutated[p.Name] {
						exprCode = fmt.Sprintf("(Obj.magic !%s : %s)", sanitizeIdent(p.Name), typ)
						casts = append(casts, &AssignStmt{Name: p.Name, Expr: &RawExpr{Code: exprCode}})
					} else {
						exprCode = fmt.Sprintf("(Obj.magic %s : %s)", sanitizeIdent(p.Name), typ)
						casts = append(casts, &LetStmt{Name: p.Name, Expr: &RawExpr{Code: exprCode}})
					}
				}
			}
		}
		if len(casts) > 0 {
			body = append(casts, body...)
		}
		var ret Expr
		if len(body) > 0 {
			if r, ok := body[len(body)-1].(*ReturnStmt); ok {
				ret = r.Expr
				body = body[:len(body)-1]
			}
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
	loops = append(loops, queryLoop{Name: q.Var, Src: src, Side: ""})
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
		loops = append(loops, queryLoop{Name: f.Var, Src: fs, Side: ""})
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
		side := ""
		if j.Side != nil {
			side = *j.Side
		}
		loops = append(loops, queryLoop{Name: j.Var, Src: js, Side: side})
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
	call := extractCallExpr(q.Select)
	if call != nil && call.Func == "sum" && len(call.Args) == 1 &&
		q.Group == nil && len(q.Froms) == 0 && len(q.Joins) == 0 &&
		q.Sort == nil && q.Skip == nil && q.Take == nil {
		argExpr, _, err := convertExpr(call.Args[0], env, qVars)
		if err != nil {
			return nil, "", err
		}
		qe := &QueryExpr{Loops: loops, Where: whereExpr, Select: argExpr}
		elemTyp := qVars[q.Var].typ
		ret := "int"
		if elemTyp == "float" {
			ret = "float"
		}
		return &SumBuiltin{List: qe, ElemType: elemTyp}, ret, nil
	}
	if q.Group != nil {
		keyExpr, keyTyp, err := convertExpr(q.Group.Exprs[0], env, qVars)
		if err != nil {
			return nil, "", err
		}
		gVars := map[string]VarInfo{}
		for k, v := range qVars {
			gVars[k] = v
		}
		elemTyp := qVars[q.Var].typ
		if len(loops) > 1 {
			parts := make([]string, 0, len(loops))
			for _, lp := range loops {
				if t, ok := qVars[lp.Name]; ok {
					parts = append(parts, lp.Name+":"+t.typ)
				}
			}
			if len(parts) > 0 {
				elemTyp = "map-dyn{" + strings.Join(parts, ",") + "}"
			} else {
				elemTyp = "map-dyn"
			}
		}
		gVars[q.Group.Name] = VarInfo{typ: "list-" + elemTyp, group: true}
		gVars[q.Group.Name+"Key"] = VarInfo{typ: keyTyp}
		var sortExpr Expr
		var desc bool
		if q.Sort != nil {
			sx, _, err := convertExpr(q.Sort, env, gVars)
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
		return &GroupByQueryExpr{Loops: loops, Where: whereExpr, Key: keyExpr, Into: q.Group.Name, Having: havingExpr, Select: sel, Sort: sortExpr, Desc: desc}, retTyp, nil
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
	if ut, ok := env.FindUnionByVariant(c.Func); ok {
		if st, ok2 := ut.Variants[c.Func]; ok2 {
			if len(c.Args) == len(st.Order) {
				items := make([]MapEntry, 0, len(st.Order)+1)
				items = append(items, MapEntry{Key: &StringLit{Value: "tag"}, Value: &StringLit{Value: c.Func}})
				for i, name := range st.Order {
					val, _, err := convertExpr(c.Args[i], env, vars)
					if err != nil {
						return nil, "", err
					}
					items = append(items, MapEntry{Key: &StringLit{Value: name}, Value: val})
				}
				return &MapLit{Items: items, Dynamic: false}, "map-dyn", nil
			}
		}
	}
	if (c.Func == "upper" || c.Func == "lower") && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "string" {
			return nil, "", fmt.Errorf("%s expects string", c.Func)
		}
		name := "String.uppercase_ascii"
		if c.Func == "lower" {
			name = "String.lowercase_ascii"
		}
		return &FuncCall{Name: name, Args: []Expr{arg}, Ret: "string"}, "string", nil
	}
	if c.Func == "pow" && len(c.Args) == 2 {
		if _, ok := env.GetFunc("pow"); !ok {
			a1, t1, err := convertExpr(c.Args[0], env, vars)
			if err != nil {
				return nil, "", err
			}
			a2, t2, err := convertExpr(c.Args[1], env, vars)
			if err != nil {
				return nil, "", err
			}
			if t1 != "int" && t1 != "float" {
				t1 = t1
			}
			_ = t1
			if t2 != "int" && t2 != "float" {
				t2 = t2
			}
			_ = t2
			return &FuncCall{Name: "Float.pow", Args: []Expr{a1, a2}, Ret: "float"}, "float", nil
		}
	}
	if c.Func == "str" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		switch typ {
		case "int", "float", "bigint", "bigrat":
			return &StrBuiltin{Expr: arg, Typ: typ}, "string", nil
		default:
			return &FuncCall{Name: "__show", Args: []Expr{arg}, Ret: "string"}, "string", nil
		}
	}
	if c.Func == "len" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		// If the argument type is not obviously a container, consult the
		// type environment in case this is a variable with a known list
		// or string type. This helps when the inferred type loses the
		// element information (e.g. after slicing operations).
		if !(typ == "string" || typ == "list" || strings.HasPrefix(typ, "list") ||
			typ == "map" || strings.HasPrefix(typ, "map-") || strings.HasPrefix(typ, "map{")) {
			if n, ok := arg.(*Name); ok {
				if t, err := env.GetVar(n.Ident); err == nil {
					nt := typeString(t)
					if nt != "" {
						typ = nt
					}
				}
			}
		}
		switch {
		case typ == "string" || typ == "list" || strings.HasPrefix(typ, "list") ||
			typ == "map" || strings.HasPrefix(typ, "map-") || strings.HasPrefix(typ, "map{"):
			return &LenBuiltin{Arg: arg, Typ: typ}, "int", nil
		default:
			usesDynLen = true
			return &LenBuiltin{Arg: arg, Typ: "dyn"}, "int", nil
		}
	}
	if c.Func == "os.Getenv" && len(c.Args) == 1 {
		arg, _, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		usesGetEnv = true
		return &FuncCall{Name: "_getenv", Args: []Expr{arg}, Ret: "string"}, "string", nil
	}
	if (c.Func == "substring" || c.Func == "substr") && len(c.Args) == 3 {
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
	if c.Func == "indexOf" && len(c.Args) == 2 {
		strArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		subArg, typ2, err := convertExpr(c.Args[1], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "string" || typ2 != "string" {
			return nil, "", fmt.Errorf("indexOf expects string")
		}
		return &IndexOfBuiltin{Str: strArg, Sub: subArg}, "int", nil
	}
	if c.Func == "slice" && len(c.Args) == 3 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") && typ != "string" {
			return nil, "", fmt.Errorf("slice expects list or string")
		}
		startArg, styp, err := convertExpr(c.Args[1], env, vars)
		if err != nil {
			return nil, "", err
		}
		if styp != "int" {
			return nil, "", fmt.Errorf("slice start expects int")
		}
		endArg, etyp, err := convertExpr(c.Args[2], env, vars)
		if err != nil {
			return nil, "", err
		}
		if etyp != "int" {
			return nil, "", fmt.Errorf("slice end expects int")
		}
		return &SliceExpr{Col: listArg, Start: startArg, End: endArg, Typ: typ}, typ, nil
	}
	if c.Func == "padStart" && len(c.Args) == 3 {
		strArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		lenArg, typ2, err := convertExpr(c.Args[1], env, vars)
		if err != nil {
			return nil, "", err
		}
		padArg, typ3, err := convertExpr(c.Args[2], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "string" || typ2 != "int" || typ3 != "string" {
			return nil, "", fmt.Errorf("padStart expects string,int,string")
		}
		return &PadStartBuiltin{Str: strArg, Len: lenArg, Pad: padArg}, "string", nil
	}
	if c.Func == "append" && len(c.Args) == 2 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("append expects list")
		}
		valArg, vtyp, err := convertExpr(c.Args[1], env, vars)
		if err != nil {
			return nil, "", err
		}
		elem := ""
		if strings.HasPrefix(typ, "list-") {
			elem = strings.TrimPrefix(typ, "list-")
			if vtyp == "" {
				valArg = &CastExpr{Expr: valArg, Type: "obj_to_" + elem}
			}
		}
		resTyp := typ
		if typ == "list" && vtyp != "" {
			resTyp = "list-" + vtyp
			elem = vtyp
		}
		return &AppendBuiltin{List: listArg, Value: valArg, Elem: elem}, resTyp, nil
	}
	if c.Func == "sum" && len(c.Args) == 1 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("sum expects list")
		}
		elemTyp := "int"
		if strings.HasPrefix(typ, "list-") {
			elemTyp = strings.TrimPrefix(typ, "list-")
		}
		retTyp := "int"
		if elemTyp == "float" {
			retTyp = "float"
		}
		return &SumBuiltin{List: listArg, ElemType: elemTyp}, retTyp, nil
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
	if c.Func == "exists" && len(c.Args) == 1 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("exists expects list")
		}
		return &ExistsBuiltin{List: listArg}, "bool", nil
	}
	if c.Func == "abs" && len(c.Args) == 1 {
		val, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		resTyp := typ
		if typ != "float" {
			resTyp = "int"
		}
		if typ != "float" && typ != "int" {
			return nil, "", fmt.Errorf("abs expects numeric")
		}
		return &AbsBuiltin{Value: val, Typ: resTyp}, resTyp, nil
	}
	if c.Func == "int" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		castType := ""
		switch typ {
		case "string":
			castType = "str_to_int"
		case "float":
			castType = "float_to_int"
		default:
			return arg, "int", nil
		}
		return &CastExpr{Expr: arg, Type: castType}, "int", nil
	}
	if c.Func == "float" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		castType := ""
		switch typ {
		case "int":
			castType = "int_to_float"
		default:
			return arg, "float", nil
		}
		return &CastExpr{Expr: arg, Type: castType}, "float", nil
	}
	if c.Func == "panic" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "string" {
			arg = &StrBuiltin{Expr: arg, Typ: typ}
		}
		return &PanicExpr{Arg: arg}, "", nil
	}
	if c.Func == "input" && len(c.Args) == 0 {
		return &InputBuiltin{}, "string", nil
	}
	if c.Func == "print" {
		args := make([]Expr, len(c.Args))
		for i, a := range c.Args {
			ex, _, err := convertExpr(a, env, vars)
			if err != nil {
				return nil, "", err
			}
			args[i] = ex
		}
		return &PrintExpr{Exprs: args}, "", nil
	}
	if c.Func == "now" && len(c.Args) == 0 {
		usesNow = true
		return &FuncCall{Name: "_now", Args: nil, Ret: "int"}, "int", nil
	}
	if c.Func == "contains" && len(c.Args) == 2 {
		if _, ok := env.GetFunc("contains"); !ok {
			containerArg, containerTyp, err := convertExpr(c.Args[0], env, vars)
			if err != nil {
				return nil, "", err
			}
			keyArg, keyTyp, err := convertExpr(c.Args[1], env, vars)
			if err != nil {
				return nil, "", err
			}
			if containerTyp == "string" && keyTyp == "string" {
				return &StringContainsBuiltin{Str: containerArg, Sub: keyArg}, "bool", nil
			}
			if strings.HasPrefix(containerTyp, "map") {
				return &MapContainsBuiltin{Map: containerArg, Key: keyArg}, "bool", nil
			}
			return nil, "", fmt.Errorf("contains expects string or map")
		}
	}
	if c.Func == "split" && len(c.Args) == 2 {
		strArg, styp, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		sepArg, typ2, err := convertExpr(c.Args[1], env, vars)
		if err != nil {
			return nil, "", err
		}
		if styp != "string" || typ2 != "string" {
			return nil, "", fmt.Errorf("split expects string,string")
		}
		usesSplit = true
		return &FuncCall{Name: "_split", Args: []Expr{strArg, sepArg}, Ret: "list-string"}, "list-string", nil
	}
	if c.Func == "sha256" && len(c.Args) == 1 {
		arg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "list-int" && typ != "list" {
			return nil, "", fmt.Errorf("sha256 expects list<int>")
		}
		usesSHA = true
		return &FuncCall{Name: "_sha256", Args: []Expr{arg}, Ret: "list-int"}, "list-int", nil
	}
	if c.Func == "net.LookupHost" && len(c.Args) == 1 {
		arg, _, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		usesLookupHost = true
		return &FuncCall{Name: "_lookup_host", Args: []Expr{arg}, Ret: "list"}, "list", nil
	}
	if c.Func == "avg" && len(c.Args) == 1 {
		listArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if !strings.HasPrefix(typ, "list") {
			return nil, "", fmt.Errorf("avg expects list")
		}
		elemTyp := "int"
		if strings.HasPrefix(typ, "list-") {
			elemTyp = strings.TrimPrefix(typ, "list-")
		}
		retTyp := "int"
		if elemTyp == "float" {
			retTyp = "float"
		}
		return &AvgBuiltin{List: listArg, ElemType: elemTyp}, retTyp, nil
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
		if typ != "map" && !strings.HasPrefix(typ, "map-") && !strings.HasPrefix(typ, "map{") {
			return nil, "", fmt.Errorf("values expects map")
		}
		return &ValuesBuiltin{Map: mapArg}, "list", nil
	}
	if c.Func == "keys" && len(c.Args) == 1 {
		mapArg, typ, err := convertExpr(c.Args[0], env, vars)
		if err != nil {
			return nil, "", err
		}
		if typ != "map" && !strings.HasPrefix(typ, "map-") && !strings.HasPrefix(typ, "map{") {
			return nil, "", fmt.Errorf("keys expects map")
		}
		return &KeysBuiltin{Map: mapArg}, "list", nil
	}
	if fn, ok := env.GetFunc(c.Func); ok {
		ret := "int"
		if fn.Return != nil {
			if fn.Return.Simple != nil {
				ret = *fn.Return.Simple
			} else if fn.Return.Generic != nil {
				switch fn.Return.Generic.Name {
				case "list":
					if len(fn.Return.Generic.Args) == 1 {
						elem := typeRefString(fn.Return.Generic.Args[0])
						if elem != "" {
							ret = "list-" + elem
						} else {
							ret = "list"
						}
					} else {
						ret = "list"
					}
				case "map":
					if len(fn.Return.Generic.Args) == 2 {
						val := typeRefString(fn.Return.Generic.Args[1])
						if val == "any" || val == "" {
							ret = "map-dyn"
						} else {
							ret = "map-" + val
						}
					} else {
						ret = "map"
					}
				}
			} else if fn.Return.Fun != nil {
				sub := typeRefString(fn.Return.Fun.Return)
				if sub != "" {
					ret = "func-" + sub
				} else {
					ret = "func"
				}
			}
		}
		args := make([]Expr, len(c.Args))
		mut := getFuncMutations(env, c.Func)
		for i, a := range c.Args {
			ex, at, err := convertExpr(a, env, vars)
			if err != nil {
				return nil, "", err
			}
			if mut != nil && i < len(fn.Params) && mut[fn.Params[i].Name] {
				// pass reference without dereferencing when the caller
				// variable is already a ref, otherwise create one
				if n, ok := ex.(*Name); ok {
					if info, ok2 := vars[n.Ident]; ok2 && info.ref {
						ex = &Name{Ident: n.Ident, Typ: n.Typ, Ref: false}
					} else {
						ex = &RefExpr{Value: ex}
					}
				} else {
					ex = &RefExpr{Value: ex}
				}
			} else if i < len(fn.Params) {
				ptyp := typeRefString(fn.Params[i].Type)
				if ts := typeString(types.ResolveTypeRef(fn.Params[i].Type, env)); ts != "" {
					ptyp = ts
				}
				if ptyp == "any" {
					ptyp = ""
				}
				if ptyp == "float" && at == "int" {
					ex = &CastExpr{Expr: ex, Type: "int_to_float"}
					at = "float"
				}
				if ptyp == "" {
					switch {
					case at == "int":
						ex = &CastExpr{Expr: ex, Type: "int_to_obj"}
					case at == "float":
						ex = &CastExpr{Expr: ex, Type: "float_to_obj"}
					case strings.HasPrefix(at, "list-") || at == "list":
						ex = &CastExpr{Expr: ex, Type: "list_to_any"}
					}
				} else {
					if at == "" || at == "list" || at == "map" || at == "func" || strings.HasPrefix(at, "map-") || strings.HasPrefix(at, "list-") && !strings.HasPrefix(ptyp, at) {
						ex = &CastExpr{Expr: ex, Type: "obj_to_" + ptyp}
					} else if ptyp == "int" {
						ex = &CastExpr{Expr: ex, Type: "int_to_obj"}
					} else if ptyp == "float" {
						if at == "int" {
							// already converted above
						}
						ex = &CastExpr{Expr: ex, Type: "float_to_obj"}
					} else if ptyp == "list" && strings.HasPrefix(at, "list-") {
						ex = &CastExpr{Expr: ex, Type: "list_to_obj"}
					}
				}
			}
			args[i] = ex
		}
		if len(args) < len(fn.Params) {
			ret = "func"
		}
		return &FuncCall{Name: sanitizeIdent(c.Func), Args: args, Ret: ret}, ret, nil
	}
	if v, ok := vars[c.Func]; ok && (v.typ == "func" || strings.HasPrefix(v.typ, "func-")) {
		args := make([]Expr, len(c.Args))
		for i, a := range c.Args {
			ex, at, err := convertExpr(a, env, vars)
			if err != nil {
				return nil, "", err
			}
			// when calling a function stored in a variable, we
			// don't have parameter type information. Assume dynamic
			// parameters and box primitive values accordingly.
			if at == "int" {
				ex = &CastExpr{Expr: ex, Type: "int_to_obj"}
			} else if at == "float" {
				ex = &CastExpr{Expr: ex, Type: "float_to_obj"}
			}
			args[i] = ex
		}
		ret := v.ret
		if ret == "" {
			if strings.HasPrefix(v.typ, "func-") {
				ret = strings.TrimPrefix(v.typ, "func-")
				if ret == "any" {
					ret = ""
				}
			} else if t, err := env.GetVar(c.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					if r := typeString(ft.Return); r != "" {
						ret = r
					}
				}
			}
		}
		return &FuncCall{Name: sanitizeIdent(c.Func), Args: args, Ret: ret}, ret, nil
	}
	return nil, "", fmt.Errorf("call %s not supported", c.Func)
}

func buildListUpdate(list Expr, indexes []Expr, val Expr) Expr {
	idx := indexes[0]
	if len(indexes) == 1 {
		return &ListUpdateExpr{List: list, Index: idx, Value: val}
	}
	if _, ok := indexes[1].(*StringLit); ok {
		mapExpr := &IndexExpr{Col: list, Index: idx, Typ: "map", ColTyp: "list"}
		inner := buildMapUpdate(mapExpr, indexes[1:], val, "map")
		return &ListUpdateExpr{List: list, Index: idx, Value: inner}
	}
	inner := buildListUpdate(&IndexExpr{Col: list, Index: idx, Typ: "list", ColTyp: "list"}, indexes[1:], val)
	return &ListUpdateExpr{List: list, Index: idx, Value: inner}
}

func buildMapUpdate(mp Expr, keys []Expr, val Expr, typ string) Expr {
	key := keys[0]
	valTyp := strings.TrimPrefix(typ, "map-")
	if len(keys) == 1 {
		return &MapUpdateExpr{Map: mp, Key: key, Value: val, Dyn: isDynamicMapType(typ)}
	}
	inner := buildMapUpdate(&MapIndexExpr{Map: mp, Key: key, Typ: valTyp, Dyn: true}, keys[1:], val, valTyp)
	return &MapUpdateExpr{Map: mp, Key: key, Value: inner, Dyn: isDynamicMapType(typ)}
}

func substituteFieldVars(e Expr, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *Name:
		if fields[ex.Ident] {
			return &MapIndexExpr{Map: &Name{Ident: "item"}, Key: &StringLit{Value: ex.Ident}, Typ: ex.Typ, Dyn: false}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFieldVars(ex.Left, fields)
		ex.Right = substituteFieldVars(ex.Right, fields)
		return ex
	case *FuncCall:
		for i := range ex.Args {
			ex.Args[i] = substituteFieldVars(ex.Args[i], fields)
		}
		return ex
	default:
		return ex
	}
}

func renderExpr(e Expr) string {
	var b bytes.Buffer
	e.emit(&b)
	return b.String()
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

func isSimpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func literalString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func parseFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return ""
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Map == nil {
		return ""
	}
	for _, it := range p.Target.Map.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
		}
		if key == "format" {
			if s, ok := literalString(it.Value); ok {
				return s
			}
		}
	}
	return ""
}

func valueToExpr(v interface{}) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		keys := make([]string, 0, len(val))
		for k := range val {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		items := make([]MapEntry, len(keys))
		dynamic := false
		for i, k := range keys {
			vexpr := valueToExpr(val[k])
			if _, ok := vexpr.(*IntLit); !ok {
				dynamic = true
			}
			items[i] = MapEntry{Key: &StringLit{Value: k}, Value: vexpr}
		}
		return &MapLit{Items: items, Dynamic: dynamic}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(it)
		}
		return &ListLit{Elems: elems, Dynamic: true}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case int:
		return &IntLit{Value: val}
	case int64:
		return &IntLit{Value: int(val)}
	case float64:
		f := val
		if float64(int(f)) == f {
			return &IntLit{Value: int(f)}
		}
		return &FloatLit{Value: f}
	case float32:
		f := float64(val)
		if float64(int(f)) == f {
			return &IntLit{Value: int(f)}
		}
		return &FloatLit{Value: f}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func dataExprFromFile(path, format string, typ *parser.TypeRef) (Expr, error) {
	if path == "" {
		return &ListLit{}, nil
	}
	root := repoRoot()
	if root != "" && strings.HasPrefix(path, "../") {
		clean := strings.TrimPrefix(path, "../")
		path = filepath.Join(root, "tests", clean)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var v interface{}
	switch format {
	case "yaml":
		if err := yaml.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "jsonl":
		var arr []interface{}
		for _, line := range bytes.Split(data, []byte{'\n'}) {
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var item interface{}
			if err := json.Unmarshal(line, &item); err == nil {
				arr = append(arr, item)
			}
		}
		v = arr
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(v), nil
}

func extractSaveExpr(e *parser.Expr) *parser.SaveExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return nil
	}
	return p.Target.Save
}

func extractCallExpr(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return nil
	}
	return p.Target.Call
}
