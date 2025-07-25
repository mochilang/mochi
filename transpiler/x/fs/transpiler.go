//go:build slow

package fstrans

import (
	"bytes"
	"fmt"
	"io"
	"os/exec"
	"strconv"
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

type UnionCase struct {
	Name   string
	Fields []string
}

type UnionDef struct {
	Name  string
	Cases []UnionCase
}

type Program struct {
	Structs   []StructDef
	Unions    []UnionDef
	Stmts     []Stmt
	UseNow    bool
	UseBreak  bool
	UseReturn bool
	UseBench  bool
}

// varTypes holds the inferred type for each variable defined during
// transpilation. It is reset for every call to Transpile.
var (
	varTypes     map[string]string
	structDefs   []StructDef
	unionDefs    []UnionDef
	structCount  int
	transpileEnv *types.Env
	neededOpens  map[string]bool
	indentLevel  int
	usesNow      bool
	usesBreak    bool
	usesReturn   bool
	usesBench    bool
)

func copyMap(src map[string]string) map[string]string {
	dst := make(map[string]string, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

const helperNow = `let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)
`

const helperMem = `let _mem () =
    System.GC.Collect()
    System.GC.GetTotalMemory(true)
`

func writeIndent(w io.Writer) {
	for i := 0; i < indentLevel; i++ {
		io.WriteString(w, "    ")
	}
}

func stmtUsesBreak(s Stmt) bool {
	switch st := s.(type) {
	case *BreakStmt, *ContinueStmt:
		return true
	case *IfStmt:
		for _, t := range st.Then {
			if stmtUsesBreak(t) {
				return true
			}
		}
		for _, e := range st.Else {
			if stmtUsesBreak(e) {
				return true
			}
		}
	case *WhileStmt:
		for _, b := range st.Body {
			if stmtUsesBreak(b) {
				return true
			}
		}
	case *ForStmt:
		for _, b := range st.Body {
			if stmtUsesBreak(b) {
				return true
			}
		}
	case *FunDef:
		for _, b := range st.Body {
			if stmtUsesBreak(b) {
				return true
			}
		}
	}
	return false
}

func bodyUsesBreak(body []Stmt) bool {
	for _, st := range body {
		if stmtUsesBreak(st) {
			return true
		}
	}
	return false
}

func fsType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.BigIntType:
		return "bigint"
	case types.BigRatType:
		return "bignum"
	case types.FloatType:
		return "float"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "string"
	case types.ListType:
		return fsType(tt.Elem) + " array"
	case types.MapType:
		return fmt.Sprintf("Map<%s, %s>", fsType(tt.Key), fsType(tt.Value))
	case types.OptionType:
		return fsType(tt.Elem) + " option"
	case types.VoidType:
		return "unit"
	case types.FuncType:
		parts := make([]string, len(tt.Params)+1)
		for i, p := range tt.Params {
			parts[i] = fsType(p)
		}
		parts[len(parts)-1] = fsType(tt.Return)
		return strings.Join(parts, " -> ")
	case types.StructType:
		if tt.Name != "" {
			return tt.Name
		}
	case types.UnionType:
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
	case "bigint":
		return "bigint"
	default:
		if strings.HasPrefix(s, "list<") && strings.HasSuffix(s, ">") {
			elem := strings.TrimSuffix(strings.TrimPrefix(s, "list<"), ">")
			return fsTypeFromString(elem) + " array"
		}
		if strings.HasSuffix(s, " list") {
			return fsTypeFromString(strings.TrimSuffix(s, " list")) + " array"
		}
		if strings.HasPrefix(s, "Map<") {
			return s
		}
		if s == "obj" {
			return "obj"
		}
		return s
	}
}

func typeRefString(t *parser.TypeRef) string {
	if t == nil || transpileEnv == nil {
		return ""
	}
	ft := types.ResolveTypeRef(t, transpileEnv)
	return fsType(ft)
}

func fsIdent(name string) string {
	keywords := map[string]bool{"and": true, "as": true, "assert": true, "begin": true,
		"class": true, "default": true, "delegate": true, "do": true, "done": true,
		"downcast": true, "downto": true, "elif": true, "else": true, "end": true,
		"exception": true, "extern": true, "false": true, "finally": true, "for": true,
		"fun": true, "function": true, "if": true, "in": true, "inherit": true,
		"base":   true,
		"inline": true, "interface": true, "internal": true, "lazy": true, "let": true,
		"match": true, "member": true, "module": true, "mutable": true, "namespace": true,
		"new": true, "null": true, "of": true, "open": true, "or": true, "override": true,
		"private": true, "public": true, "rec": true, "return": true, "sig": true, "static": true,
		"struct": true, "then": true, "to": true, "true": true, "try": true, "type": true,
		"upcast": true, "use": true, "val": true, "void": true, "when": true, "while": true,
		"with": true, "yield": true}
	if keywords[name] || strings.ContainsAny(name, "- ") {
		return "``" + name + "``"
	}
	return name
}

func inferStructFromMapVars(ml *parser.MapLiteral) ([]StructField, bool) {
	if ml == nil || len(ml.Items) == 0 {
		return nil, false
	}
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
		if tgt.Selector == nil || len(tgt.Selector.Tail) == 0 {
			return nil, false
		}
		vname := tgt.Selector.Root
		vtype, ok := varTypes[vname]
		if !ok {
			return nil, false
		}
		fieldName := tgt.Selector.Tail[len(tgt.Selector.Tail)-1]
		if ft, ok := structFieldType(vtype, fieldName); ok {
			fields[i] = StructField{Name: key, Type: ft, Mut: false}
		} else {
			return nil, false
		}
	}
	return fields, true
}

func structFieldType(name, field string) (string, bool) {
	for _, sd := range structDefs {
		if sd.Name == name {
			for _, f := range sd.Fields {
				if f.Name == field {
					return f.Type, true
				}
			}
		}
	}
	return "", false
}

func structFieldNames(name string) ([]string, bool) {
	for _, sd := range structDefs {
		if sd.Name == name {
			names := make([]string, len(sd.Fields))
			for i, f := range sd.Fields {
				names[i] = f.Name
			}
			return names, true
		}
	}
	return nil, false
}

func addStructDef(name string, st types.StructType) {
	def := StructDef{Name: name}
	for _, f := range st.Order {
		def.Fields = append(def.Fields, StructField{Name: f, Type: fsType(st.Fields[f]), Mut: false})
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
	// Handle nested lists
	var elemType string
	if first == "array" {
		if inner, ok := l.Elems[0].(*ListLit); ok {
			elemType = simpleListType(inner)
			if elemType == "" {
				return ""
			}
		} else {
			return ""
		}
		for _, e := range l.Elems[1:] {
			inner, ok := e.(*ListLit)
			if !ok || simpleListType(inner) != elemType {
				return ""
			}
		}
	} else {
		elemType = first
		for _, e := range l.Elems[1:] {
			if inferType(e) != first {
				return ""
			}
		}
	}
	return elemType + " array"
}

func mapValueType(s string) string {
	if strings.HasPrefix(s, "Map<") && strings.HasSuffix(s, ">") {
		parts := strings.TrimSuffix(strings.TrimPrefix(s, "Map<"), ">")
		idx := strings.LastIndex(parts, ",")
		if idx >= 0 {
			return strings.TrimSpace(parts[idx+1:])
		}
	}
	return "obj"
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
			return name + " array"
		}
		if len(ll.Elems) > 0 && ll.Elems[0].Binary != nil && ll.Elems[0].Binary.Left.Value.Target.Map != nil {
			if fields, ok := inferStructFromMapVars(ll.Elems[0].Binary.Left.Value.Target.Map); ok {
				structCount++
				name := fmt.Sprintf("Anon%d", structCount)
				structDefs = append(structDefs, StructDef{Name: name, Fields: fields})
				return name + " array"
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
					return name + " array"
				}
				if fields, ok := inferStructFromMapVars(ml); ok {
					structCount++
					name := fmt.Sprintf("Anon%d", structCount)
					structDefs = append(structDefs, StructDef{Name: name, Fields: fields})
					return name + " array"
				}
			}
		}
	}
	return "array"
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

// OpenStmt represents `open <module>`.
type OpenStmt struct{ Name string }

func (o *OpenStmt) emit(w io.Writer) {
	writeIndent(w)
	fmt.Fprintf(w, "open %s\n", fsIdent(o.Name))
}

// ModuleDef represents `module <name>` with nested declarations.
type ModuleDef struct {
	Name  string
	Stmts []Stmt
	Open  bool
}

func (m *ModuleDef) emit(w io.Writer) {
	writeIndent(w)
	fmt.Fprintf(w, "module %s =\n", fsIdent(m.Name))
	indentLevel++
	if m.Open {
		writeIndent(w)
		io.WriteString(w, "open System\n")
	}
	for i, st := range m.Stmts {
		st.emit(w)
		if i < len(m.Stmts)-1 {
			w.Write([]byte{'\n'})
		}
	}
	indentLevel--
	if len(m.Stmts) > 0 {
		w.Write([]byte{'\n'})
	}
}

// LambdaExpr represents an inline function expression.
type LambdaExpr struct {
	Params []string
	Expr   Expr
	Body   []Stmt
}

func (l *LambdaExpr) emit(w io.Writer) {
	writeIndent(w)
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
	indentLevel++
	for i, st := range l.Body {
		st.emit(w)
		if i < len(l.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
	indentLevel--
}

type FunDef struct {
	Name   string
	Params []string
	Types  []string
	Body   []Stmt
	Return string
}

func (f *FunDef) emitWithPrefix(w io.Writer, prefix string) {
	writeIndent(w)
	io.WriteString(w, prefix)
	io.WriteString(w, " ")
	io.WriteString(w, fsIdent(f.Name))
	if len(f.Params) == 0 {
		io.WriteString(w, " ()")
	}
	for i, p := range f.Params {
		io.WriteString(w, " ")
		name := fsIdent(p)
		if i < len(f.Types) && f.Types[i] != "" {
			io.WriteString(w, "(")
			io.WriteString(w, name)
			io.WriteString(w, ": ")
			io.WriteString(w, f.Types[i])
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, name)
		}
	}
	io.WriteString(w, " =\n")
	indentLevel++
	writeIndent(w)
	if f.Return != "" {
		fmt.Fprintf(w, "let mutable __ret : %s = Unchecked.defaultof<%s>\n", f.Return, f.Return)
	} else {
		io.WriteString(w, "let mutable __ret = ()\n")
	}
	for _, p := range f.Params {
		writeIndent(w)
		fmt.Fprintf(w, "let mutable %s = %s\n", fsIdent(p), fsIdent(p))
	}
	writeIndent(w)
	io.WriteString(w, "try\n")
	indentLevel++
	for i, st := range f.Body {
		st.emit(w)
		if i < len(f.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
	w.Write([]byte{'\n'})
	writeIndent(w)
	io.WriteString(w, "__ret")
	indentLevel--
	w.Write([]byte{'\n'})
	writeIndent(w)
	io.WriteString(w, "with\n")
	indentLevel++
	writeIndent(w)
	io.WriteString(w, "| Return -> __ret")
	indentLevel--
	indentLevel--
}

func (f *FunDef) emit(w io.Writer) { f.emitWithPrefix(w, "let rec") }

type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "__ret <- ")
	if r.Expr != nil {
		r.Expr.emit(w)
	} else {
		io.WriteString(w, "()")
	}
	io.WriteString(w, "\n")
	writeIndent(w)
	io.WriteString(w, "raise Return")
}

type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "raise Break")
}

type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "raise Continue")
}

// BenchStmt represents a benchmark block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (b *BenchStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "let __bench_mem_start = _mem()\n")
	writeIndent(w)
	io.WriteString(w, "let __bench_start = _now()\n")
	for _, st := range b.Body {
		st.emit(w)
		w.Write([]byte{'\n'})
	}
	writeIndent(w)
	io.WriteString(w, "let __bench_end = _now()\n")
	writeIndent(w)
	io.WriteString(w, "let __bench_mem_end = _mem()\n")
	writeIndent(w)
	fmt.Fprintf(w, "printfn \"{\\n  \\\"duration_us\\\": %%d,\\n  \\\"memory_bytes\\\": %%d,\\n  \\\"name\\\": \\\"%s\\\"\\n}\" ((__bench_end - __bench_start) / 1000) (__bench_mem_end - __bench_mem_start)\n", b.Name)
}

// ListLit represents an F# list literal.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[|")
	for i, e := range l.Elems {
		e.emit(w)
		if i < len(l.Elems)-1 {
			io.WriteString(w, "; ")
		}
	}
	io.WriteString(w, "|]")
}

// MapLit represents an F# map literal.
type MapLit struct {
	Items [][2]Expr
	Types []string
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "Map.ofList [")
	same := true
	if len(m.Types) == 0 {
		m.Types = make([]string, len(m.Items))
		prev := ""
		for i, kv := range m.Items {
			t := inferType(kv[1])
			m.Types[i] = t
			if i == 0 {
				prev = t
			} else if t != prev {
				same = false
			}
		}
	} else {
		prev := m.Types[0]
		for _, t := range m.Types[1:] {
			if t != prev {
				same = false
				break
			}
		}
	}
	for i, kv := range m.Items {
		io.WriteString(w, "(")
		kv[0].emit(w)
		io.WriteString(w, ", ")
		if !same && m.Types[i] != "obj" {
			io.WriteString(w, "box ")
		}
		kv[1].emit(w)
		io.WriteString(w, ")")
		if i < len(m.Items)-1 {
			io.WriteString(w, "; ")
		}
	}
	io.WriteString(w, "]")
}

// LoadYamlExpr represents loading a YAML file at runtime.
type LoadYamlExpr struct {
	Path string
	Type string
}

func (l *LoadYamlExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "(let deserializer = DeserializerBuilder().Build()\n    let yamlText = File.ReadAllText(%q)\n    deserializer.Deserialize<%s list>(yamlText))", l.Path, l.Type)
}

// LoadJSONLExpr represents loading a JSONL file at runtime.
type LoadJSONLExpr struct {
	Path string
	Type string
}

func (l *LoadJSONLExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "(File.ReadLines(%q) |> Seq.map (fun line -> JsonSerializer.Deserialize<%s>(line)) |> Seq.toList)", l.Path, l.Type)
}

// SaveJSONLExpr writes a list of records to stdout as JSON lines.
type SaveJSONLExpr struct{ Src Expr }

func (s *SaveJSONLExpr) emit(w io.Writer) {
	io.WriteString(w, "(List.iter (fun row -> printfn \"%s\" (JsonSerializer.Serialize(row))) ")
	s.Src.emit(w)
	io.WriteString(w, ")")
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
	Var       string
	Src       Expr
	Froms     []queryFrom
	Joins     []queryJoin
	Where     Expr
	Key       Expr
	GroupVar  string
	Select    Expr
	ItemName  string
	GroupName string
}

type StructLit struct {
	Name   string
	Fields []StructFieldExpr
}

type StructFieldExpr struct {
	Name  string
	Value Expr
}

type VariantExpr struct {
	Name string
	Args []Expr
}

func (v *VariantExpr) emit(w io.Writer) {
	io.WriteString(w, fsIdent(v.Name))
	if len(v.Args) > 0 {
		io.WriteString(w, "(")
		for i, a := range v.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		io.WriteString(w, ")")
	}
}

func (s *StructLit) emit(w io.Writer) {
	io.WriteString(w, "{ ")
	for i, f := range s.Fields {
		io.WriteString(w, fsIdent(f.Name))
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
	io.WriteString(w, fsIdent(q.Var))
	io.WriteString(w, " in ")
	src := q.Src
	if q.Sort != nil {
		src = &CallExpr{Func: "List.sortBy", Args: []Expr{&LambdaExpr{Params: []string{fsIdent(q.Var)}, Expr: q.Sort}, src}}
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
		io.WriteString(w, fsIdent(j.Var))
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
		io.WriteString(w, fsIdent(f.Var))
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
		io.WriteString(w, fsIdent(g.Var))
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
			io.WriteString(w, fsIdent(n))
			io.WriteString(w, " = ")
			io.WriteString(w, fsIdent(n))
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
		io.WriteString(w, fsIdent(g.Var))
		io.WriteString(w, " in ")
		g.Src.emit(w)
		io.WriteString(w, " do")
		for _, j := range g.Joins {
			io.WriteString(w, " for ")
			io.WriteString(w, fsIdent(j.Var))
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
			io.WriteString(w, fsIdent(f.Var))
			io.WriteString(w, " in ")
			f.Src.emit(w)
			io.WriteString(w, " do")
		}
		if g.Where != nil {
			io.WriteString(w, " if ")
			g.Where.emit(w)
			io.WriteString(w, " then")
		}
		io.WriteString(w, " yield { ")
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
			io.WriteString(w, fsIdent(n))
			io.WriteString(w, " = ")
			io.WriteString(w, fsIdent(n))
		}
		io.WriteString(w, " }")
		if g.ItemName != "" {
			io.WriteString(w, " : ")
			io.WriteString(w, g.ItemName)
		}
		io.WriteString(w, " ]")
	}
	io.WriteString(w, " do\n    let ")
	io.WriteString(w, fsIdent(g.GroupVar))
	io.WriteString(w, " : ")
	io.WriteString(w, g.GroupName)
	io.WriteString(w, " = { key = key; items = items }\n    yield ")
	g.Select.emit(w)
	io.WriteString(w, " ]")
}

func (a *AppendExpr) emit(w io.Writer) {
	io.WriteString(w, "Array.append ")
	a.List.emit(w)
	io.WriteString(w, " [|")
	a.Elem.emit(w)
	io.WriteString(w, "|]")
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
	writeIndent(w)
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then\n")
	indentLevel++
	for idx, st := range i.Then {
		st.emit(w)
		if idx < len(i.Then)-1 {
			w.Write([]byte{'\n'})
		}
	}
	indentLevel--
	if len(i.Else) > 0 {
		w.Write([]byte{'\n'})
		writeIndent(w)
		io.WriteString(w, "else\n")
		indentLevel++
		for idx, st := range i.Else {
			st.emit(w)
			if idx < len(i.Else)-1 {
				w.Write([]byte{'\n'})
			}
		}
		indentLevel--
	}
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) {
	writeIndent(w)
	s.Expr.emit(w)
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, fsIdent(s.Name))
	io.WriteString(w, " <- ")
	s.Expr.emit(w)
}

type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *IndexAssignStmt) emit(w io.Writer) {
	if idx, ok := s.Target.(*IndexExpr); ok {
		if id, ok2 := idx.Target.(*IdentExpr); ok2 {
			t := inferType(idx.Target)
			if strings.Contains(t, "array") || t == "" {
				writeIndent(w)
				name := fsIdent(id.Name)
				io.WriteString(w, name)
				io.WriteString(w, ".[")
				idx.Index.emit(w)
				io.WriteString(w, "] <- ")
				s.Value.emit(w)
				return
			}
		}
	}
	writeIndent(w)
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
	io.WriteString(w, fsIdent(s.Field))
	io.WriteString(w, " = ")
	s.Value.emit(w)
	io.WriteString(w, " }")
}

type WhileStmt struct {
	Cond      Expr
	Body      []Stmt
	WithBreak bool
}

func (wst *WhileStmt) emit(w io.Writer) {
	if wst.WithBreak {
		writeIndent(w)
		io.WriteString(w, "try\n")
		indentLevel++
	}
	writeIndent(w)
	io.WriteString(w, "while ")
	wst.Cond.emit(w)
	io.WriteString(w, " do\n")
	indentLevel++
	for i, st := range wst.Body {
		st.emit(w)
		if i < len(wst.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
	indentLevel--
	if wst.WithBreak {
		indentLevel--
		w.Write([]byte{'\n'})
		writeIndent(w)
		io.WriteString(w, "with\n")
		writeIndent(w)
		io.WriteString(w, "| Break -> ()\n")
		writeIndent(w)
		io.WriteString(w, "| Continue -> ()")
	}
}

type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (u *UpdateStmt) emit(w io.Writer) {
	io.WriteString(w, fsIdent(u.Target))
	io.WriteString(w, " <- List.map (fun item -> ")
	expr := Expr(&IdentExpr{Name: "item"})
	for i, f := range u.Fields {
		expr = &StructUpdateExpr{Target: expr, Field: f, Value: u.Values[i]}
	}
	if u.Cond != nil {
		expr = &IfExpr{Cond: u.Cond, Then: expr, Else: &IdentExpr{Name: "item"}}
	}
	expr.emit(w)
	io.WriteString(w, ") ")
	io.WriteString(w, fsIdent(u.Target))
}

type ForStmt struct {
	Name      string
	Start     Expr
	End       Expr
	Body      []Stmt
	WithBreak bool
}

func (fst *ForStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "for ")
	if fst.End == nil && inferType(fst.Start) == "map" {
		io.WriteString(w, "KeyValue(")
		io.WriteString(w, fsIdent(fst.Name))
		io.WriteString(w, ", _)")
	} else {
		io.WriteString(w, fsIdent(fst.Name))
	}
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
	indentLevel++
	if fst.WithBreak {
		writeIndent(w)
		io.WriteString(w, "try\n")
		indentLevel++
	}
	for i, st := range fst.Body {
		st.emit(w)
		if i < len(fst.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
	if fst.WithBreak {
		indentLevel--
		w.Write([]byte{'\n'})
		writeIndent(w)
		io.WriteString(w, "with\n")
		writeIndent(w)
		io.WriteString(w, "| Break -> ()\n")
		writeIndent(w)
		io.WriteString(w, "| Continue -> ()")
	}
	indentLevel--
}

type LetStmt struct {
	Name    string
	Mutable bool
	Type    string
	Expr    Expr
}

func (s *LetStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "let ")
	if s.Mutable {
		io.WriteString(w, "mutable ")
	}
	io.WriteString(w, fsIdent(s.Name))
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
		if rtyp != "map" {
			if id, ok := b.Right.(*IdentExpr); ok {
				if t, ok2 := varTypes[id.Name]; ok2 && strings.HasPrefix(t, "Map<") {
					rtyp = "map"
				}
			}
		}
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
		} else if rtyp == "array" {
			io.WriteString(w, "Array.contains ")
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
	if b.Op == "+" {
		lt := inferType(b.Left)
		rt := inferType(b.Right)
		leftArr := lt == "array" || lt == "" && (isSlice(b.Left) || isList(b.Left))
		rightArr := rt == "array" || rt == "" && (isSlice(b.Right) || isList(b.Right))
		if lt != "string" && rt != "string" && (leftArr || rightArr) {
			io.WriteString(w, "Array.append ")
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
			return
		}
	}
	lt := inferType(b.Left)
	rt := inferType(b.Right)
	left := b.Left
	right := b.Right
	if lt == "float" && rt == "int" {
		right = &CastExpr{Expr: right, Type: "float"}
	} else if rt == "float" && lt == "int" {
		left = &CastExpr{Expr: left, Type: "float"}
	}
	if needsParen(left) {
		io.WriteString(w, "(")
		left.emit(w)
		io.WriteString(w, ")")
	} else {
		left.emit(w)
	}
	io.WriteString(w, " ")
	io.WriteString(w, mapOp(b.Op))
	io.WriteString(w, " ")
	if needsParen(right) {
		io.WriteString(w, "(")
		right.emit(w)
		io.WriteString(w, ")")
	} else {
		right.emit(w)
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

type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) {
	s := strconv.FormatFloat(f.Value, 'f', -1, 64)
	if !strings.ContainsAny(s, ".eE") {
		s += ".0"
	}
	io.WriteString(w, s)
}

type IdentExpr struct {
	Name string
	Type string
}

func (i *IdentExpr) emit(w io.Writer) { io.WriteString(w, fsIdent(i.Name)) }

type UnitLit struct{}

func (u *UnitLit) emit(w io.Writer) { io.WriteString(w, "()") }

type NullLit struct{}

func (n *NullLit) emit(w io.Writer) { io.WriteString(w, "null") }

// MatchExpr represents a match expression.
type MatchExpr struct {
	Target Expr
	Cases  []MatchCase
}

type MatchCase struct {
	Pattern Expr
	Result  Expr
}

func (m *MatchExpr) emit(w io.Writer) {
	io.WriteString(w, "(match ")
	m.Target.emit(w)
	io.WriteString(w, " with")
	indentLevel++
	for _, c := range m.Cases {
		io.WriteString(w, "\n")
		writeIndent(w)
		io.WriteString(w, "| ")
		c.Pattern.emit(w)
		io.WriteString(w, " -> ")
		c.Result.emit(w)
	}
	indentLevel--
	io.WriteString(w, ")")
}

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
	case *BinaryExpr, *UnaryExpr, *IfExpr, *AppendExpr, *SubstringExpr, *CallExpr, *VariantExpr, *IndexExpr, *LambdaExpr, *FieldExpr, *MethodCallExpr, *SliceExpr, *CastExpr, *MapLit, *MatchExpr:
		return true
	default:
		return false
	}
}

func inferType(e Expr) string {
	switch v := e.(type) {
	case *IntLit:
		return "int"
	case *FloatLit:
		return "float"
	case *StringLit:
		return "string"
	case *BoolLit:
		return "bool"
	case *ListLit:
		return "array"
	case *MapLit:
		return "map"
	case *StructLit:
		return "struct"
	case *VariantExpr:
		if transpileEnv != nil {
			if u, ok := transpileEnv.FindUnionByVariant(v.Name); ok {
				return u.Name
			}
		}
		return ""
	case *QueryExpr, *GroupQueryExpr:
		return "array"
	case *IdentExpr:
		if t := v.Type; t != "" {
			if strings.HasSuffix(t, " list") || strings.HasSuffix(t, " array") || strings.HasPrefix(t, "list<") {
				return "array"
			}
			if strings.HasPrefix(t, "Map<") {
				return "map"
			}
			return t
		}
		if t, ok := varTypes[v.Name]; ok {
			if strings.HasSuffix(t, " list") || strings.HasSuffix(t, " array") || strings.HasPrefix(t, "list<") {
				return "array"
			}
			if strings.HasPrefix(t, "Map<") {
				return "map"
			}
			return t
		}
		return ""
	case *FieldExpr:
		t := inferType(v.Target)
		name := strings.TrimSuffix(strings.TrimSuffix(t, " list"), " array")
		if strings.HasPrefix(name, "list<") {
			name = strings.TrimSuffix(strings.TrimPrefix(name, "list<"), ">")
		}
		if ft, ok := structFieldType(name, v.Name); ok {
			return ft
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
			if (lt == "int" && rt == "float") || (lt == "float" && rt == "int") {
				return "float"
			}
			if v.Op == "+" && (lt == "string" || rt == "string") {
				return "string"
			}
		}
	case *AppendExpr:
		return "array"
	case *SubstringExpr:
		return "string"
	case *SliceExpr:
		return inferType(v.Target)
	case *MatchExpr:
		if len(v.Cases) == 0 {
			return ""
		}
		t := inferType(v.Cases[0].Result)
		for _, c := range v.Cases[1:] {
			if inferType(c.Result) != t {
				return ""
			}
		}
		return t
	case *CallExpr:
		switch v.Func {
		case "string":
			return "string"
		case "System.Console.ReadLine", "input":
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

func valueType(e Expr) string {
	if id, ok := e.(*IdentExpr); ok {
		if id.Type != "" {
			return id.Type
		}
		if t, ok := varTypes[id.Name]; ok {
			return t
		}
	}
	return inferType(e)
}

func isSlice(e Expr) bool {
	_, ok := e.(*SliceExpr)
	return ok
}

func isList(e Expr) bool {
	switch e.(type) {
	case *ListLit, *AppendExpr:
		return true
	default:
		return false
	}
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
	t := inferType(i.Target)
	if strings.Contains(t, "array") || t == "" {
		i.Target.emit(w)
		io.WriteString(w, ".[")
		i.Index.emit(w)
		io.WriteString(w, "]")
		return
	}
	if strings.HasPrefix(t, "Map<") || t == "map" {
		valT := mapValueType(t)
		if id, ok := i.Target.(*IdentExpr); ok {
			if vt := id.Type; vt != "" {
				if alt := mapValueType(vt); alt != "obj" {
					valT = alt
				}
			}
			if vt, ok2 := varTypes[id.Name]; ok2 {
				if alt := mapValueType(vt); alt != "obj" {
					valT = alt
				}
			}
		}
		io.WriteString(w, "(defaultArg (Map.tryFind ")
		i.Index.emit(w)
		io.WriteString(w, " ")
		i.Target.emit(w)
		fmt.Fprintf(w, ") Unchecked.defaultof<%s>)", valT)
		return
	}
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
	io.WriteString(w, fsIdent(f.Name))
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
	Target   Expr
	Start    Expr // may be nil for start of collection
	End      Expr // may be nil for end of collection
	IsString bool
}

func (s *SliceExpr) emit(w io.Writer) {
	start := s.Start
	if start == nil {
		start = &IntLit{Value: 0}
	}
	t := inferType(s.Target)
	if !s.IsString && t != "string" {
		io.WriteString(w, "Array.sub ")
		s.Target.emit(w)
		io.WriteString(w, " ")
		if needsParen(start) {
			io.WriteString(w, "(")
			start.emit(w)
			io.WriteString(w, ")")
		} else {
			start.emit(w)
		}
		io.WriteString(w, " ")
		var endLen Expr
		if s.End != nil {
			endLen = &BinaryExpr{Left: s.End, Op: "-", Right: start}
		} else {
			endLen = &BinaryExpr{Left: &CallExpr{Func: "Array.length", Args: []Expr{s.Target}}, Op: "-", Right: start}
		}
		if needsParen(endLen) {
			io.WriteString(w, "(")
			endLen.emit(w)
			io.WriteString(w, ")")
		} else {
			endLen.emit(w)
		}
		return
	}
	s.Target.emit(w)
	io.WriteString(w, ".Substring(")
	start.emit(w)
	io.WriteString(w, ", ")
	var length Expr
	if s.End != nil {
		length = &BinaryExpr{Left: s.End, Op: "-", Right: start}
	} else {
		length = &BinaryExpr{Left: &CallExpr{Func: "String.length", Args: []Expr{s.Target}}, Op: "-", Right: start}
	}
	length.emit(w)
	io.WriteString(w, ")")
}

// CastExpr represents expr as type.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "float":
		io.WriteString(w, "float ")
		if needsParen(c.Expr) {
			io.WriteString(w, "(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
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
		if needsParen(c.Expr) {
			io.WriteString(w, "(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
		io.WriteString(w, " :?> ")
		io.WriteString(w, c.Type)
	}
}

// DefaultOfExpr represents Unchecked.defaultof<T>.
type DefaultOfExpr struct{ Type string }

func (d *DefaultOfExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "Unchecked.defaultof<%s>", d.Type)
}

// Emit generates formatted F# code from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	indentLevel = 0
	buf.WriteString(header())
	if prog.UseBreak {
		buf.WriteString("exception Break\nexception Continue\n\n")
	}
	if prog.UseReturn {
		buf.WriteString("exception Return\n\n")
	}
	if prog.UseNow {
		buf.WriteString(helperNow)
		buf.WriteString("\n_initNow()\n")
	}
	if prog.UseBench {
		buf.WriteString(helperMem)
	}
	for _, st := range prog.Structs {
		fmt.Fprintf(&buf, "type %s = {\n", fsIdent(st.Name))
		for _, f := range st.Fields {
			if f.Mut {
				fmt.Fprintf(&buf, "    mutable %s: %s\n", fsIdent(f.Name), f.Type)
			} else {
				fmt.Fprintf(&buf, "    %s: %s\n", fsIdent(f.Name), f.Type)
			}
		}
		buf.WriteString("}\n")
	}
	for _, u := range prog.Unions {
		fmt.Fprintf(&buf, "type %s =\n", u.Name)
		for i, c := range u.Cases {
			buf.WriteString("    | ")
			buf.WriteString(c.Name)
			if len(c.Fields) > 0 {
				buf.WriteString(" of ")
				for j, f := range c.Fields {
					if j > 0 {
						buf.WriteString(" * ")
					}
					buf.WriteString(f)
				}
			}
			if i < len(u.Cases)-1 {
				buf.WriteByte('\n')
			} else {
				buf.WriteByte('\n')
			}
		}
	}
	for i := 0; i < len(prog.Stmts); {
		if fd, ok := prog.Stmts[i].(*FunDef); ok {
			fd.emitWithPrefix(&buf, "let rec")
			i++
			for i < len(prog.Stmts) {
				next, ok := prog.Stmts[i].(*FunDef)
				if !ok {
					break
				}
				buf.WriteByte('\n')
				next.emitWithPrefix(&buf, "and")
				i++
			}
		} else {
			prog.Stmts[i].emit(&buf)
			i++
		}
		if i < len(prog.Stmts) {
			buf.WriteByte('\n')
		}
	}
	if b := buf.Bytes(); len(b) > 0 && b[len(b)-1] != '\n' {
		buf.WriteByte('\n')
	}
	return buf.Bytes()
}

func header() string {
	out, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(out))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		ts = t.Format("2006-01-02 15:04 -0700")
	} else {
		ts = time.Now().Format("2006-01-02 15:04 -0700")
	}
	return fmt.Sprintf("// Generated %s\n\n", ts)
}

// Transpile converts a Mochi program to a simple F# AST.
func Transpile(prog *parser.Program, env *types.Env, benchMain bool) (*Program, error) {
	transpileEnv = env
	varTypes = map[string]string{}
	structDefs = nil
	unionDefs = nil
	structCount = 0
	neededOpens = map[string]bool{}
	usesNow = false
	usesBreak = false
	usesReturn = false
	usesBench = false
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
	p.Unions = unionDefs
	if len(neededOpens) > 0 {
		opens := make([]Stmt, 0, len(neededOpens))
		for m := range neededOpens {
			opens = append(opens, &OpenStmt{Name: m})
		}
		p.Stmts = append(opens, p.Stmts...)
	}
	if benchMain {
		usesNow = true
		usesBench = true
		p.Stmts = []Stmt{&BenchStmt{Name: "main", Body: p.Stmts}}
	}
	transpileEnv = nil
	p.UseNow = usesNow
	p.UseBreak = usesBreak
	p.UseReturn = usesReturn
	p.UseBench = usesBench
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
	ml := e.Binary.Left.Value.Target.Map
	if ml == nil {
		return ""
	}
	for _, it := range ml.Items {
		key, ok := literalString(it.Key)
		if !ok || key != "format" {
			continue
		}
		if v, ok := literalString(it.Value); ok {
			return v
		}
	}
	return ""
}

func convertStmt(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Import != nil:
		im, err := convertImport(st.Import)
		if err != nil {
			return nil, err
		}
		return im, nil
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
		if st.Let.Type != nil {
			declared = typeRefString(st.Let.Type)
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
		if declared == "array" {
			if ll, ok := e.(*ListLit); ok {
				if t := simpleListType(ll); t != "" {
					declared = t
				}
			}
		}
		fsDecl := fsTypeFromString(declared)
		if _, ok := e.(*NullLit); ok && fsDecl != "" {
			e = &DefaultOfExpr{Type: fsDecl}
		} else if fsDecl == "bigint" {
			if _, ok := e.(*IntLit); ok {
				e = &CastExpr{Expr: e, Type: "bigint"}
			}
		}
		varTypes[st.Let.Name] = declared
		typ := fsDecl
		if typ == "array" || typ == "map" {
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
		if st.Var.Type != nil {
			declared = typeRefString(st.Var.Type)
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
		if declared == "array" {
			if ll, ok := e.(*ListLit); ok {
				if t := simpleListType(ll); t != "" {
					declared = t
				}
			}
		}
		fsDecl := fsTypeFromString(declared)
		if _, ok := e.(*NullLit); ok && fsDecl != "" {
			e = &DefaultOfExpr{Type: fsDecl}
		} else if fsDecl == "bigint" {
			if _, ok := e.(*IntLit); ok {
				e = &CastExpr{Expr: e, Type: "bigint"}
			}
		}
		varTypes[st.Var.Name] = declared
		typ := fsDecl
		if typ == "array" || typ == "map" {
			typ = ""
		}
		return &LetStmt{Name: st.Var.Name, Expr: e, Type: typ, Mutable: true}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		e, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		if t := inferType(e); t != "" {
			cur := varTypes[st.Assign.Name]
			if cur == "" || cur == "array" || cur == "map" {
				varTypes[st.Assign.Name] = t
			}
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: e}, nil
	case st.Assign != nil && (len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0):
		val, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		typ := varTypes[st.Assign.Name]
		target := Expr(&IdentExpr{Name: st.Assign.Name, Type: typ})
		if len(st.Assign.Index) > 0 {
			target, err = applyIndexOps(target, st.Assign.Index)
			if err != nil {
				return nil, err
			}
			if t := varTypes[st.Assign.Name]; strings.HasPrefix(t, "Map<") {
				if len(st.Assign.Index) == 1 {
					upd := &CallExpr{Func: "Map.add", Args: []Expr{target.(*IndexExpr).Index, val, &IdentExpr{Name: st.Assign.Name, Type: t}}}
					return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
				}
			}
			if t := varTypes[st.Assign.Name]; t == "list" || strings.HasSuffix(t, " list") || strings.HasPrefix(t, "list<") {
				indices := make([]Expr, len(st.Assign.Index))
				for i, ix := range st.Assign.Index {
					idx, err := convertExpr(ix.Start)
					if err != nil {
						return nil, err
					}
					indices[i] = idx
				}
				list := &IdentExpr{Name: st.Assign.Name, Type: t}
				upd := buildListUpdate(list, indices, val)
				return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
			}
			if t := varTypes[st.Assign.Name]; strings.HasPrefix(t, "Map<") && len(st.Assign.Index) > 1 {
				indices := make([]Expr, len(st.Assign.Index))
				for i, ix := range st.Assign.Index {
					idx, err := convertExpr(ix.Start)
					if err != nil {
						return nil, err
					}
					indices[i] = idx
				}
				upd := buildMapUpdate(&IdentExpr{Name: st.Assign.Name, Type: t}, indices, val)
				return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
			}
			return &IndexAssignStmt{Target: target, Value: val}, nil
		}
		if len(st.Assign.Field) == 1 {
			upd := &StructUpdateExpr{Target: target, Field: st.Assign.Field[0].Name, Value: val}
			return &AssignStmt{Name: st.Assign.Name, Expr: upd}, nil
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
		usesReturn = true
		return &ReturnStmt{Expr: e}, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Fun != nil:
		save := varTypes
		varTypes = copyMap(varTypes)
		for _, p := range st.Fun.Params {
			if p.Type != nil && transpileEnv != nil {
				t := types.ResolveTypeRef(p.Type, transpileEnv)
				varTypes[p.Name] = fsType(t)
			} else if p.Type != nil && p.Type.Simple != nil {
				varTypes[p.Name] = fsTypeFromString(*p.Type.Simple)
			} else if transpileEnv != nil {
				if t, err := transpileEnv.GetVar(p.Name); err == nil {
					varTypes[p.Name] = fsType(t)
				}
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
		paramTypes := make([]string, len(st.Fun.Params))
		for i, p := range st.Fun.Params {
			params[i] = p.Name
			if p.Type != nil && transpileEnv != nil {
				ft := types.ResolveTypeRef(p.Type, transpileEnv)
				paramTypes[i] = fsType(ft)
			} else if t, ok := varTypes[p.Name]; ok {
				paramTypes[i] = fsTypeFromString(t)
			} else if transpileEnv != nil {
				if vt, err := transpileEnv.GetVar(p.Name); err == nil {
					paramTypes[i] = fsType(vt)
				}
			}
		}
		var retType string
		if transpileEnv != nil {
			if vt, err := transpileEnv.GetVar(st.Fun.Name); err == nil {
				if ft, ok := vt.(types.FuncType); ok {
					retType = fsType(ft.Return)
				}
			}
		}
		varTypes = save
		return &FunDef{Name: st.Fun.Name, Params: params, Types: paramTypes, Body: body, Return: retType}, nil
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
		wb := bodyUsesBreak(body)
		if wb {
			usesBreak = true
		}
		return &WhileStmt{Cond: cond, Body: body, WithBreak: wb}, nil
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
		wb := bodyUsesBreak(body)
		if wb {
			usesBreak = true
		}
		return &ForStmt{Name: st.For.Name, Start: start, End: end, Body: body, WithBreak: wb}, nil
	case st.Bench != nil:
		usesNow = true
		usesBench = true
		body := make([]Stmt, len(st.Bench.Body))
		for i, s := range st.Bench.Body {
			cs, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			body[i] = cs
		}
		return &BenchStmt{Name: st.Bench.Name, Body: body}, nil
	case st.Update != nil:
		up, err := convertUpdateStmt(st.Update)
		if err != nil {
			return nil, err
		}
		return up, nil
	case st.ExternVar != nil:
		return nil, nil
	case st.ExternFun != nil:
		return nil, nil
	case st.ExternObject != nil:
		return nil, nil
	case st.ExternType != nil:
		return nil, nil
	case st.Type != nil:
		if err := convertTypeDecl(st.Type); err != nil {
			return nil, err
		}
		return nil, nil
	case st.Test != nil:
		for _, s := range st.Test.Body {
			if _, err := convertStmt(s); err != nil {
				return nil, err
			}
		}
		return nil, nil
	case st.Expect != nil:
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
		case op.Index != nil && op.Index.Colon != nil && op.Index.Step == nil && op.Index.Colon2 == nil:
			var start, end Expr
			var err error
			if op.Index.Start != nil {
				start, err = convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
			}
			if op.Index.End != nil {
				end, err = convertExpr(op.Index.End)
				if err != nil {
					return nil, err
				}
			}
			isStr := inferType(expr) == "string"
			expr = &SliceExpr{Target: expr, Start: start, End: end, IsString: isStr}
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
		case op.Cast != nil && op.Cast.Type != nil:
			expr = &CastExpr{Expr: expr, Type: typeRefString(op.Cast.Type)}
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
					b := &IfExpr{Cond: args[0], Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
					return &CallExpr{Func: "printfn \"%d\"", Args: []Expr{b}}, nil
				case "int":
					return &CallExpr{Func: "printfn \"%d\"", Args: []Expr{args[0]}}, nil
				case "float":
					return &CallExpr{Func: "printfn \"%.1f\"", Args: []Expr{args[0]}}, nil
				case "array":
					mapped := &CallExpr{Func: "Array.map string", Args: []Expr{args[0]}}
					concat := &CallExpr{Func: "String.concat", Args: []Expr{&StringLit{Value: " "}, &CallExpr{Func: "Array.toList", Args: []Expr{mapped}}}}
					wrapped := &BinaryExpr{Left: &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: concat}, Op: "+", Right: &StringLit{Value: "]"}}
					return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{wrapped}}, nil
				case "string":
					return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{args[0]}}, nil
				default:
					return &CallExpr{Func: "printfn \"%A\"", Args: []Expr{args[0]}}, nil
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
				case "array":
					mapped := &CallExpr{Func: "Array.map string", Args: []Expr{a}}
					elems[i] = &BinaryExpr{Left: &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: &CallExpr{Func: "String.concat", Args: []Expr{&StringLit{Value: " "}, &CallExpr{Func: "Array.toList", Args: []Expr{mapped}}}}}, Op: "+", Right: &StringLit{Value: "]"}}
				default:
					elems[i] = &CallExpr{Func: "sprintf \"%A\"", Args: []Expr{a}}
				}
			}
			list := &ListLit{Elems: elems}
			concat := &CallExpr{Func: "String.concat", Args: []Expr{&StringLit{Value: " "}, list}}
			return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{concat}}, nil
		case "count", "len":
			fn := "Seq.length"
			if len(args) == 1 {
				switch inferType(args[0]) {
				case "array":
					fn = "Array.length"
				case "string":
					fn = "String.length"
				case "map":
					fn = "Seq.length"
				case "group":
					if id, ok := args[0].(*IdentExpr); ok {
						field := &FieldExpr{Target: id, Name: "items"}
						return &CallExpr{Func: "Array.length", Args: []Expr{field}}, nil
					}
				}
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "str":
			return &CallExpr{Func: "string", Args: args}, nil
		case "sum":
			fn := "Seq.sum"
			if len(args) == 1 && inferType(args[0]) == "array" {
				fn = "Array.sum"
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "avg":
			fn := "Seq.averageBy float"
			if len(args) == 1 && inferType(args[0]) == "array" {
				fn = "Array.averageBy float"
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "exists":
			if len(args) != 1 {
				return nil, fmt.Errorf("exists expects 1 arg")
			}
			fn := "Seq.isEmpty"
			if inferType(args[0]) == "array" {
				fn = "Array.isEmpty"
			}
			return &UnaryExpr{Op: "not", Expr: &CallExpr{Func: fn, Args: args}}, nil
		case "append":
			if len(args) != 2 {
				return nil, fmt.Errorf("append expects 2 args")
			}
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		case "now":
			if len(args) == 0 {
				usesNow = true
				neededOpens["System"] = true
				return &CallExpr{Func: "_now", Args: nil}, nil
			}
		case "input":
			if len(args) == 0 {
				neededOpens["System"] = true
				return &CallExpr{Func: "System.Console.ReadLine", Args: nil}, nil
			}
		case "substring":
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		case "upper":
			if len(args) != 1 {
				return nil, fmt.Errorf("upper expects 1 arg")
			}
			return &MethodCallExpr{Target: args[0], Name: "ToUpper"}, nil
		case "lower":
			if len(args) != 1 {
				return nil, fmt.Errorf("lower expects 1 arg")
			}
			return &MethodCallExpr{Target: args[0], Name: "ToLower"}, nil
		case "keys":
			if len(args) != 1 {
				return nil, fmt.Errorf("keys expects 1 arg")
			}
			t := inferType(args[0])
			if t == "map" {
				inner := &CallExpr{Func: "Map.toList", Args: []Expr{args[0]}}
				return &CallExpr{Func: "List.map fst", Args: []Expr{inner}}, nil
			}
			return &CallExpr{Func: "Seq.map fst", Args: args}, nil
		case "values":
			if len(args) != 1 {
				return nil, fmt.Errorf("values expects 1 arg")
			}
			t := inferType(args[0])
			if t == "map" {
				inner := &CallExpr{Func: "Map.toList", Args: []Expr{args[0]}}
				return &CallExpr{Func: "List.map snd", Args: []Expr{inner}}, nil
			}
			if fields, ok := structFieldNames(t); ok {
				elems := make([]Expr, len(fields))
				for i, f := range fields {
					elems[i] = &FieldExpr{Target: args[0], Name: f}
				}
				return &ListLit{Elems: elems}, nil
			}
			return &CallExpr{Func: "Seq.map snd", Args: args}, nil
		default:
			if transpileEnv != nil {
				if _, ok := transpileEnv.FindUnionByVariant(p.Call.Func); ok {
					return &VariantExpr{Name: p.Call.Func, Args: args}, nil
				}
			}
			return &CallExpr{Func: p.Call.Func, Args: args}, nil
		}
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Float != nil:
		return &FloatLit{Value: *p.Lit.Float}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Lit != nil && p.Lit.Null:
		return &NullLit{}, nil
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
		if transpileEnv != nil {
			if _, ok := transpileEnv.FindUnionByVariant(p.Struct.Name); ok {
				args := make([]Expr, len(p.Struct.Fields))
				for i, f := range p.Struct.Fields {
					v, err := convertExpr(f.Value)
					if err != nil {
						return nil, err
					}
					args[i] = v
				}
				return &VariantExpr{Name: p.Struct.Name, Args: args}, nil
			}
		}
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
		if transpileEnv != nil {
			if _, ok := types.InferSimpleMap(p.Map, transpileEnv); !ok {
				if fields, ok := inferStructFromMapVars(p.Map); ok {
					structCount++
					name := fmt.Sprintf("Anon%d", structCount)
					structDefs = append(structDefs, StructDef{Name: name, Fields: fields})
					vals := make([]StructFieldExpr, len(p.Map.Items))
					for i, it := range p.Map.Items {
						v, err := convertExpr(it.Value)
						if err != nil {
							return nil, err
						}
						key, _ := types.SimpleStringKey(it.Key)
						vals[i] = StructFieldExpr{Name: key, Value: v}
					}
					return &StructLit{Name: name, Fields: vals}, nil
				}
			}
		}
		items := make([][2]Expr, len(p.Map.Items))
		types := make([]string, len(p.Map.Items))
		same := true
		prev := ""
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
			t := valueType(v)
			types[i] = t
			if i == 0 {
				prev = t
			} else if t != prev {
				same = false
			}
		}
		if !same {
			for i := range items {
				items[i][1] = &CallExpr{Func: "box", Args: []Expr{items[i][1]}}
				types[i] = "obj"
			}
		}
		return &MapLit{Items: items, Types: types}, nil
	case p.Match != nil:
		return convertMatchExpr(p.Match)
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
		if p.Selector.Root == "nil" && len(p.Selector.Tail) == 0 {
			return &NullLit{}, nil
		}
		typ := ""
		if t, ok := varTypes[p.Selector.Root]; ok {
			typ = t
		}
		expr := Expr(&IdentExpr{Name: p.Selector.Root, Type: typ})
		for _, name := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: name}
		}
		return expr, nil
	case p.Query != nil:
		return convertQueryExpr(p.Query)
	case p.Group != nil:
		return convertExpr(p.Group)
	case p.Load != nil:
		if p.Load.Path != nil && p.Load.Type != nil && p.Load.With != nil {
			format := parseFormat(p.Load.With)
			if format == "yaml" {
				neededOpens["System"] = true
				neededOpens["System.IO"] = true
				neededOpens["YamlDotNet.Serialization"] = true
				path := strings.Trim(*p.Load.Path, "\"")
				typ := "obj"
				if p.Load.Type.Simple != nil {
					typ = *p.Load.Type.Simple
				}
				return &LoadYamlExpr{Path: path, Type: typ}, nil
			}
			if format == "jsonl" {
				neededOpens["System"] = true
				neededOpens["System.IO"] = true
				neededOpens["System.Text.Json"] = true
				path := strings.Trim(*p.Load.Path, "\"")
				typ := "obj"
				if p.Load.Type.Simple != nil {
					typ = *p.Load.Type.Simple
				}
				return &LoadJSONLExpr{Path: path, Type: typ}, nil
			}
		}
	case p.Save != nil:
		if p.Save.Path != nil && p.Save.With != nil {
			format := parseFormat(p.Save.With)
			if format == "jsonl" && strings.Trim(*p.Save.Path, "\"") == "-" {
				neededOpens["System"] = true
				neededOpens["System.Text.Json"] = true
				src, err := convertExpr(p.Save.Src)
				if err != nil {
					return nil, err
				}
				return &SaveJSONLExpr{Src: src}, nil
			}
		}
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

func convertMatchExpr(in *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(in.Target)
	if err != nil {
		return nil, err
	}
	cases := make([]MatchCase, len(in.Cases))
	for i, c := range in.Cases {
		pat, err := convertExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		cases[i] = MatchCase{Pattern: pat, Result: res}
	}
	return &MatchExpr{Target: target, Cases: cases}, nil
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

func convertUpdateStmt(u *parser.UpdateStmt) (Stmt, error) {
	if transpileEnv == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := transpileEnv.GetVar(u.Target)
	if err != nil {
		return nil, err
	}
	lt, ok := t.(types.ListType)
	if !ok {
		return nil, fmt.Errorf("update target not list")
	}
	st, ok := lt.Elem.(types.StructType)
	if !ok {
		return nil, fmt.Errorf("update element not struct")
	}
	save := varTypes
	varTypes = copyMap(varTypes)
	for _, f := range st.Order {
		varTypes[f] = fsType(st.Fields[f])
	}
	fields := make([]string, len(u.Set.Items))
	values := make([]Expr, len(u.Set.Items))
	for i, it := range u.Set.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			varTypes = save
			return nil, fmt.Errorf("unsupported update key")
		}
		val, err := convertExpr(it.Value)
		if err != nil {
			varTypes = save
			return nil, err
		}
		fields[i] = key
		values[i] = val
	}
	var cond Expr
	if u.Where != nil {
		c, err := convertExpr(u.Where)
		if err != nil {
			varTypes = save
			return nil, err
		}
		cond = c
	}
	varTypes = save
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func convertTypeDecl(td *parser.TypeDecl) error {
	if len(td.Variants) > 0 {
		u := UnionDef{Name: td.Name}
		for _, v := range td.Variants {
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				ft := types.ResolveTypeRef(f.Type, transpileEnv)
				fields[i] = fsType(ft)
			}
			u.Cases = append(u.Cases, UnionCase{Name: v.Name, Fields: fields})
		}
		unionDefs = append(unionDefs, u)
		return nil
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

func convertImport(im *parser.ImportStmt) (Stmt, error) {
	if im.Lang == nil {
		return nil, nil
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	path := strings.Trim(im.Path, "\"")
	switch *im.Lang {
	case "python":
		if path == "math" {
			return &ModuleDef{Open: true, Name: alias, Stmts: []Stmt{
				&LetStmt{Name: "pi", Type: "float", Expr: &FieldExpr{Target: &IdentExpr{Name: "System.Math"}, Name: "PI"}},
				&LetStmt{Name: "e", Type: "float", Expr: &FieldExpr{Target: &IdentExpr{Name: "System.Math"}, Name: "E"}},
				&FunDef{Name: "sqrt", Params: []string{"x"}, Body: []Stmt{&ReturnStmt{Expr: &CallExpr{Func: "System.Math.Sqrt", Args: []Expr{&IdentExpr{Name: "x"}}}}}},
				&FunDef{Name: "pow", Params: []string{"x", "y"}, Body: []Stmt{&ReturnStmt{Expr: &CallExpr{Func: "System.Math.Pow", Args: []Expr{&IdentExpr{Name: "x"}, &IdentExpr{Name: "y"}}}}}},
				&FunDef{Name: "sin", Params: []string{"x"}, Body: []Stmt{&ReturnStmt{Expr: &CallExpr{Func: "System.Math.Sin", Args: []Expr{&IdentExpr{Name: "x"}}}}}},
				&FunDef{Name: "log", Params: []string{"x"}, Body: []Stmt{&ReturnStmt{Expr: &CallExpr{Func: "System.Math.Log", Args: []Expr{&IdentExpr{Name: "x"}}}}}},
			}}, nil
		}
	case "go":
		if path == "mochi/runtime/ffi/go/testpkg" {
			usesReturn = true
			return &ModuleDef{Open: true, Name: alias, Stmts: []Stmt{
				&FunDef{Name: "Add", Params: []string{"a", "b"}, Return: "int", Body: []Stmt{&ReturnStmt{Expr: &BinaryExpr{Left: &IdentExpr{Name: "a"}, Op: "+", Right: &IdentExpr{Name: "b"}}}}},
				&LetStmt{Name: "Pi", Expr: &FloatLit{Value: 3.14}},
				&LetStmt{Name: "Answer", Expr: &IntLit{Value: 42}},
				&FunDef{Name: "FifteenPuzzleExample", Params: nil, Return: "string", Body: []Stmt{&ReturnStmt{Expr: &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}}}},
			}}, nil
		}
		if path == "strings" {
			return &ModuleDef{Open: true, Name: alias, Stmts: []Stmt{
				&FunDef{Name: "ToUpper", Params: []string{"s"}, Body: []Stmt{&ReturnStmt{Expr: &MethodCallExpr{Target: &IdentExpr{Name: "s"}, Name: "ToUpper"}}}},
				&FunDef{Name: "TrimSpace", Params: []string{"s"}, Body: []Stmt{&ReturnStmt{Expr: &MethodCallExpr{Target: &IdentExpr{Name: "s"}, Name: "Trim"}}}},
			}}, nil
		}
		if path == "net" {
			usesReturn = true
			return &ModuleDef{Open: true, Name: alias, Stmts: []Stmt{
				&OpenStmt{Name: "System.Net"},
				&FunDef{Name: "LookupHost", Params: []string{"host"}, Return: "obj array", Body: []Stmt{
					&LetStmt{Name: "addrs", Expr: &CallExpr{Func: "Dns.GetHostAddresses", Args: []Expr{&IdentExpr{Name: "host"}}}},
					&LetStmt{Name: "mapped", Expr: &CallExpr{Func: "Array.map", Args: []Expr{
						&LambdaExpr{Params: []string{"ip"}, Expr: &MethodCallExpr{Target: &IdentExpr{Name: "ip"}, Name: "ToString"}},
						&IdentExpr{Name: "addrs"},
					}}},
					&LetStmt{Name: "lst", Expr: &CallExpr{Func: "Array.toList", Args: []Expr{&IdentExpr{Name: "mapped"}}}},
					&ReturnStmt{Expr: &ListLit{Elems: []Expr{
						&CallExpr{Func: "box", Args: []Expr{&IdentExpr{Name: "lst"}}},
						&NullLit{},
					}}},
				}},
			}}, nil
		}
	}
	return nil, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	saved := copyMap(varTypes)

	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	if id, ok := src.(*IdentExpr); ok {
		if varTypes[id.Name] == "group" {
			src = &FieldExpr{Target: id, Name: "items"}
		}
		if t, ok := varTypes[id.Name]; ok {
			if strings.HasSuffix(t, " list") {
				varTypes[q.Var] = strings.TrimSuffix(t, " list")
			} else if strings.HasPrefix(t, "list<") {
				varTypes[q.Var] = strings.TrimSuffix(strings.TrimPrefix(t, "list<"), ">")
			}
		}
	}
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		e, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		froms[i] = queryFrom{Var: f.Var, Src: e}
		if id, ok := e.(*IdentExpr); ok {
			if t, ok := varTypes[id.Name]; ok {
				if strings.HasSuffix(t, " list") {
					varTypes[f.Var] = strings.TrimSuffix(t, " list")
				} else if strings.HasPrefix(t, "list<") {
					varTypes[f.Var] = strings.TrimSuffix(strings.TrimPrefix(t, "list<"), ">")
				}
			}
		}
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
		if id, ok := src.(*IdentExpr); ok {
			if t, ok := varTypes[id.Name]; ok {
				if strings.HasSuffix(t, " list") {
					varTypes[j.Var] = strings.TrimSuffix(t, " list")
				} else if strings.HasPrefix(t, "list<") {
					varTypes[j.Var] = strings.TrimSuffix(strings.TrimPrefix(t, "list<"), ">")
				}
			}
		}
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
		names := []string{q.Var}
		for _, j := range q.Joins {
			names = append(names, j.Var)
		}
		for _, f := range q.Froms {
			names = append(names, f.Var)
		}
		itemFields := make([]StructField, len(names))
		for i, n := range names {
			typ := "obj"
			if t, ok := varTypes[n]; ok {
				typ = fsTypeFromString(strings.TrimSuffix(strings.TrimPrefix(strings.TrimSuffix(t, " list"), "list<"), ">"))
			}
			itemFields[i] = StructField{Name: n, Type: typ, Mut: false}
		}
		structCount++
		itemName := fmt.Sprintf("Anon%d", structCount)
		structDefs = append(structDefs, StructDef{Name: itemName, Fields: itemFields})
		keyType := "obj"
		if sl, ok := key.(*StructLit); ok && sl.Name != "" {
			keyType = sl.Name
		} else if kt := inferType(key); kt != "" {
			keyType = fsTypeFromString(strings.TrimSuffix(kt, " list"))
		}
		groupFields := []StructField{{Name: "key", Type: keyType, Mut: false}, {Name: "items", Type: itemName + " list", Mut: false}}
		structCount++
		groupName := fmt.Sprintf("Anon%d", structCount)
		structDefs = append(structDefs, StructDef{Name: groupName, Fields: groupFields})
		varTypes = saved
		return &GroupQueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Key: key, GroupVar: q.Group.Name, Select: sel, ItemName: itemName, GroupName: groupName}, nil
	}
	sel, err := convertExpr(q.Select)
	if err != nil {
		return nil, err
	}
	varTypes = saved
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Sort: sort, Skip: skip, Take: take, Select: sel}, nil
}
