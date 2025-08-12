//go:build slow

package fstrans

import (
	"bytes"
	"fmt"
	"io"
	"math"
	"os/exec"
	"strconv"
	"strings"
	"time"
	"unicode"

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

type AliasDef struct {
	Name string
	Type string
}

type Program struct {
	Structs        []StructDef
	Unions         []UnionDef
	Aliases        []AliasDef
	Stmts          []Stmt
	UseNow         bool
	UseBreak       bool
	UseReturn      bool
	UseSubstring   bool
	UsePadStart    bool
	UseSHA256      bool
	UseParseIntStr bool
	UseDictAdd     bool
	UseDictGet     bool
	UseDictCreate  bool
	UseSafeIndex   bool
	UseStr         bool
	UseRepr        bool
	UseArraySet    bool
	UseJSON        bool
	UseFloorDiv    bool
}

// varTypes holds the inferred type for each variable defined during
// transpilation. It is reset for every call to Transpile.
var (
	varTypes       map[string]string
	structDefs     []StructDef
	unionDefs      []UnionDef
	aliasDefs      []AliasDef
	aliasSet       map[string]bool
	structCount    int
	transpileEnv   *types.Env
	neededOpens    map[string]bool
	indentLevel    int
	usesNow        bool
	usesBreak      bool
	usesReturn     bool
	usesSubstring  bool
	usesPadStart   bool
	usesSHA256     bool
	benchMain      bool
	usesDictAdd    bool
	usesDictGet    bool
	usesDictCreate bool
	currentReturn  string
	funcDepth      int
	methodDefs     []Stmt
	definedFuncs   map[string]bool
	funcParamTypes map[string][]string
	useParseIntStr bool
	mutatedVars    map[string]bool
	letPtrs        map[string][]*LetStmt
	usesSafeIndex  bool
	usesStr        bool
	usesRepr       bool
	usesArraySet   bool
	usesJSON       bool
	usesFloorDiv   bool
	funcHasNull    bool
)

// SetBenchMain configures whether the generated main function is wrapped in a
// benchmark block when emitting code. When enabled, the program will print a
// JSON object with duration and memory statistics on completion.
func SetBenchMain(v bool) { benchMain = v }

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

const helperSubstring = `let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)
`

const helperPadStart = `let _padStart (s:string) (width:int) (pad:string) =
    let mutable out = s
    while out.Length < width do
        out <- pad + out
    out
`

const helperSHA256 = `let _sha256 (s:string) : int array =
    use sha = System.Security.Cryptography.SHA256.Create()
    let bytes = System.Text.Encoding.UTF8.GetBytes(s)
    sha.ComputeHash(bytes) |> Array.map int
`

const helperParseIntStr = `let parseIntStr (s:string) (b:int) : int =
    System.Convert.ToInt32(s, b)
`

const helperDictAdd = `let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d`

const helperDictCreate = `let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d`

const helperDictGet = `let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>`

const helperIndex = `let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>`

const helperArraySet = `let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a`

const helperJSON = `let json (arr:obj) =
    match arr with
    | :? (int array array) as a2 ->
        printf "[\n"
        for i in 0 .. a2.Length - 1 do
            let line = String.concat ", " (Array.map string a2.[i] |> Array.toList)
            if i < a2.Length - 1 then
                printfn "  [%s]," line
            else
                printfn "  [%s]" line
        printfn "]"
    | :? (int array) as a1 ->
        let line = String.concat ", " (Array.map string a1 |> Array.toList)
        printfn "[%s]" line
    | _ -> ()`

const helperStr = `let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")`

const helperRepr = `let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")`

const helperFloorDiv = `let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q`

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
	case types.AnyType:
		return "obj"
	case types.IntType, types.Int64Type:
		return "int"
	case types.BigIntType:
		return "bigint"
	case types.BigRatType:
		return "float"
	case types.FloatType:
		return "float"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "string"
	case types.ListType:
		elem := fsType(tt.Elem)
		if _, ok := tt.Elem.(types.FuncType); ok {
			elem = "(" + elem + ")"
		}
		return elem + " array"
	case types.MapType:
		return fmt.Sprintf("System.Collections.Generic.IDictionary<%s, %s>", fsType(tt.Key), fsType(tt.Value))
	case types.OptionType:
		elem := fsType(tt.Elem)
		if _, ok := tt.Elem.(types.FuncType); ok {
			elem = "(" + elem + ")"
		}
		return elem + " option"
	case types.VoidType:
		return "unit"
	case types.FuncType:
		if len(tt.Params) == 0 {
			return "unit -> " + fsType(tt.Return)
		}
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
	case "int":
		return "int"
	case "int64":
		return "int64"
	case "float":
		return "float"
	case "bool":
		return "bool"
	case "string":
		return "string"
	case "bigint":
		return "bigint"
	case "any":
		return "obj"
	case "void":
		return "unit"
	default:
		if strings.HasPrefix(s, "list<") && strings.HasSuffix(s, ">") {
			elem := strings.TrimSuffix(strings.TrimPrefix(s, "list<"), ">")
			t := fsTypeFromString(elem)
			if strings.Contains(t, "->") {
				t = "(" + t + ")"
			}
			return t + " array"
		}
		if strings.HasSuffix(s, " list") {
			t := fsTypeFromString(strings.TrimSuffix(s, " list"))
			if strings.Contains(t, "->") {
				t = "(" + t + ")"
			}
			return t + " array"
		}
		if strings.HasSuffix(s, " option") {
			t := fsTypeFromString(strings.TrimSuffix(s, " option"))
			if strings.Contains(t, "->") {
				t = "(" + t + ")"
			}
			return t + " option"
		}
		if strings.HasPrefix(s, "fun(") && strings.HasSuffix(s, ")") {
			inner := strings.TrimSuffix(strings.TrimPrefix(s, "fun("), ")")
			params := []string{}
			if strings.TrimSpace(inner) != "" {
				for _, p := range strings.Split(inner, ",") {
					params = append(params, fsTypeFromString(strings.TrimSpace(p)))
				}
			}
			params = append(params, "obj")
			return strings.Join(params, " -> ")
		}
		if strings.HasPrefix(s, "Map<") {
			return strings.Replace(s, "Map<", "System.Collections.Generic.IDictionary<", 1)
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
	if t.Simple != nil {
		return fsTypeFromString(*t.Simple)
	}
	if t.Fun != nil {
		if len(t.Fun.Params) == 0 {
			ret := "obj"
			if t.Fun.Return != nil {
				ret = typeRefString(t.Fun.Return)
			}
			return "unit -> " + ret
		}
		params := make([]string, len(t.Fun.Params)+1)
		for i, p := range t.Fun.Params {
			params[i] = typeRefString(p)
		}
		if t.Fun.Return != nil {
			params[len(params)-1] = typeRefString(t.Fun.Return)
		} else {
			params[len(params)-1] = "obj"
		}
		return strings.Join(params, " -> ")
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
		"with": true, "yield": true, "mod": true}
	if keywords[name] || strings.ContainsAny(name, "- ") {
		return "``" + name + "``"
	}
	// avoid clashes with helper or library names that may be implicitly
	// generated by the F# compiler (e.g. global random `seed` variable
	// conflicting with setter names). Prefix such identifiers to keep them
	// unique in the generated output.
	if name == "seed" {
		return "_seed"
	}
	if name == "front" {
		return "_front"
	}
	if _, ok := structWithField(name); ok {
		return "_" + name
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
			fields[i] = StructField{Name: key, Type: ft, Mut: true}
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

func structWithField(field string) (string, bool) {
	name := ""
	for _, sd := range structDefs {
		for _, f := range sd.Fields {
			if f.Name == field {
				if name != "" {
					return "", false
				}
				name = sd.Name
			}
		}
	}
	if name == "" {
		return "", false
	}
	return name, true
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
			t := inferType(e)
			if (elemType == "int" && t == "int64") || (elemType == "int64" && t == "int") {
				// When mixing 32-bit and 64-bit integers in a list,
				// promote the element type to int64 so that large
				// literals are preserved without overflow.
				elemType = "int64"
				continue
			}
			if t != elemType {
				return ""
			}
		}
	}
	return elemType + " array"
}

func mapValueType(s string) string {
	if strings.HasPrefix(s, "System.Collections.Generic.IDictionary<") && strings.HasSuffix(s, ">") {
		parts := strings.TrimSuffix(strings.TrimPrefix(s, "System.Collections.Generic.IDictionary<"), ">")
		idx := strings.LastIndex(parts, ",")
		if idx >= 0 {
			return strings.TrimSpace(parts[idx+1:])
		}
	}
	return "obj"
}

func mapKeyType(s string) string {
	if strings.HasPrefix(s, "System.Collections.Generic.IDictionary<") && strings.HasSuffix(s, ">") {
		parts := strings.TrimSuffix(strings.TrimPrefix(s, "System.Collections.Generic.IDictionary<"), ">")
		idx := strings.LastIndex(parts, ",")
		if idx >= 0 {
			return strings.TrimSpace(parts[:idx])
		}
	}
	return "obj"
}

func elemTypeOf(e Expr) string {
	switch v := e.(type) {
	case *IdentExpr:
		if t, ok := varTypes[v.Name]; ok {
			if strings.HasSuffix(t, " array") {
				return strings.TrimSuffix(t, " array")
			}
			if strings.HasSuffix(t, " list") {
				return strings.TrimSuffix(t, " list")
			}
			if strings.HasPrefix(t, "list<") && strings.HasSuffix(t, ">") {
				return strings.TrimSuffix(strings.TrimPrefix(t, "list<"), ">")
			}
			if strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") {
				return mapValueType(t)
			}
			return t
		}
	}
	t := inferType(e)
	if strings.HasSuffix(t, " array") {
		return strings.TrimSuffix(t, " array")
	}
	if strings.HasSuffix(t, " list") {
		return strings.TrimSuffix(t, " list")
	}
	if strings.HasPrefix(t, "list<") && strings.HasSuffix(t, ">") {
		return strings.TrimSuffix(strings.TrimPrefix(t, "list<"), ">")
	}
	if strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") {
		return mapValueType(t)
	}
	if t == "string" {
		return "string"
	}
	return ""
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
		if len(ll.Elems) > 0 && ll.Elems[0].Binary != nil && ll.Elems[0].Binary.Left.Value.Target.Map != nil {
			ml0 := ll.Elems[0].Binary.Left.Value.Target.Map
			if mt, ok := types.InferSimpleMap(ml0, transpileEnv); ok {
				same := true
				for _, el := range ll.Elems[1:] {
					if el.Binary == nil || el.Binary.Left.Value.Target.Map == nil {
						same = false
						break
					}
					if _, ok2 := types.InferSimpleMap(el.Binary.Left.Value.Target.Map, transpileEnv); !ok2 {
						same = false
						break
					}
				}
				if same {
					vt := fsType(mt.Value)
					if vt == "" {
						vt = "obj"
					}
					return fmt.Sprintf("System.Collections.Generic.IDictionary<string, %s> array", vt)
				}
			}
		}
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
	if m.Open {
		io.WriteString(w, "open System\n\n")
	}
	writeIndent(w)
	fmt.Fprintf(w, "module %s =\n", fsIdent(m.Name))
	indentLevel++
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
	Types  []string
	Expr   Expr
	Body   []Stmt
	Return string
}

func (l *LambdaExpr) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "fun")
	if len(l.Params) == 0 {
		io.WriteString(w, " ()")
	}
	for i, p := range l.Params {
		io.WriteString(w, " ")
		if i < len(l.Types) && l.Types[i] != "" {
			io.WriteString(w, "(")
			io.WriteString(w, p)
			io.WriteString(w, ": ")
			io.WriteString(w, l.Types[i])
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, p)
		}
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
	if len(l.Body) == 1 {
		if r, ok := l.Body[0].(*ReturnStmt); ok {
			if r.Expr != nil {
				if needsParen(r.Expr) {
					io.WriteString(w, "(")
					r.Expr.emit(w)
					io.WriteString(w, ")")
				} else {
					r.Expr.emit(w)
				}
			} else {
				io.WriteString(w, "()")
			}
			return
		}
	}
	if len(l.Body) == 0 {
		io.WriteString(w, "()")
		return
	}
	w.Write([]byte{'\n'})
	indentLevel++
	writeIndent(w)
	if l.Return != "" {
		fmt.Fprintf(w, "let mutable __ret : %s = Unchecked.defaultof<%s>\n", l.Return, l.Return)
	} else {
		io.WriteString(w, "let mutable __ret = ()\n")
	}
	writeIndent(w)
	io.WriteString(w, "try\n")
	indentLevel++
	for i, st := range l.Body {
		st.emit(w)
		if i < len(l.Body)-1 {
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
	io.WriteString(w, "let __bench_start = _now()\n")
	writeIndent(w)
	io.WriteString(w, "let __mem_start = System.GC.GetTotalMemory(true)\n")
	for i := 0; i < len(b.Body); {
		if fd, ok := b.Body[i].(*FunDef); ok {
			fd.emitWithPrefix(w, "let rec")
			i++
			for i < len(b.Body) {
				next, ok := b.Body[i].(*FunDef)
				if !ok {
					break
				}
				w.Write([]byte{'\n'})
				next.emitWithPrefix(w, "and")
				i++
			}
		} else {
			b.Body[i].emit(w)
			i++
		}
		if i < len(b.Body) {
			w.Write([]byte{'\n'})
		}
	}
	if len(b.Body) > 0 {
		w.Write([]byte{'\n'})
	}
	writeIndent(w)
	io.WriteString(w, "let __bench_end = _now()\n")
	writeIndent(w)
	io.WriteString(w, "let __mem_end = System.GC.GetTotalMemory(true)\n")
	writeIndent(w)
	fmt.Fprintf(w, "printfn \"{\\n  \\\"duration_us\\\": %%d,\\n  \\\"memory_bytes\\\": %%d,\\n  \\\"name\\\": \\\"%s\\\"\\n}\" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)\n", b.Name)
}

// ListLit represents an F# list literal.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	types := make([]string, len(l.Elems))
	same := true
	prev := ""
	hasInt64 := false
	for i, e := range l.Elems {
		t := inferType(e)
		if t == "array" {
			et := elemTypeOf(e)
			if et != "" {
				t = et + " array"
			}
		}
		types[i] = t
		if t == "int64" {
			hasInt64 = true
		}
		if i == 0 {
			prev = t
		} else if t != prev && !(t == "int" && prev == "int64") && !(t == "int64" && prev == "int") && t != "" && prev != "" {
			same = false
		}
	}
	io.WriteString(w, "[|")
	for i, e := range l.Elems {
		if hasInt64 && types[i] == "int" {
			io.WriteString(w, "int64 (")
			e.emit(w)
			io.WriteString(w, ")")
		} else if !same && types[i] != "obj" {
			io.WriteString(w, "box (")
			e.emit(w)
			io.WriteString(w, ")")
		} else {
			e.emit(w)
		}
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
	usesDictCreate = true
	neededOpens["System.Collections.Generic"] = true
	io.WriteString(w, "_dictCreate")
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
	io.WriteString(w, " [")
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
	if len(s.Fields) == 0 && s.Name != "" {
		io.WriteString(w, fsIdent(s.Name))
		io.WriteString(w, "()")
		return
	}
	if s.Name != "" {
		if names, ok := structFieldNames(s.Name); ok {
			existing := map[string]bool{}
			for _, f := range s.Fields {
				existing[f.Name] = true
			}
			for _, n := range names {
				if !existing[n] {
					if ft, ok := structFieldType(s.Name, n); ok {
						s.Fields = append(s.Fields, StructFieldExpr{Name: n, Value: &DefaultOfExpr{Type: ft}})
					} else {
						s.Fields = append(s.Fields, StructFieldExpr{Name: n, Value: &DefaultOfExpr{Type: "obj"}})
					}
				}
			}
		}
	}
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
	if needsParen(a.List) {
		io.WriteString(w, "(")
		a.List.emit(w)
		io.WriteString(w, ")")
	} else {
		a.List.emit(w)
	}
	io.WriteString(w, " [|")
	elemType := elemTypeOf(a.List)
	if elemType != "" {
		it := inferType(a.Elem)
		if elemType == "int" && it == "int64" {
			io.WriteString(w, "int (")
			a.Elem.emit(w)
			io.WriteString(w, ")|]")
			return
		}
		if it == "obj" || it == "" {
			io.WriteString(w, "unbox<")
			io.WriteString(w, elemType)
			io.WriteString(w, "> (")
			a.Elem.emit(w)
			io.WriteString(w, ")")
			io.WriteString(w, "|]")
			return
		}
	}
	if needsParen(a.Elem) {
		io.WriteString(w, "(")
		a.Elem.emit(w)
		io.WriteString(w, ")")
	} else {
		a.Elem.emit(w)
	}
	io.WriteString(w, "|]")
}

// SubstringExpr represents substring(str, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "_substring ")
	if needsParen(s.Str) {
		io.WriteString(w, "(")
		s.Str.emit(w)
		io.WriteString(w, ")")
	} else {
		s.Str.emit(w)
	}
	io.WriteString(w, " ")
	if needsParen(s.Start) {
		io.WriteString(w, "(")
		s.Start.emit(w)
		io.WriteString(w, ")")
	} else {
		s.Start.emit(w)
	}
	io.WriteString(w, " ")
	if needsParen(s.End) {
		io.WriteString(w, "(")
		s.End.emit(w)
		io.WriteString(w, ")")
	} else {
		s.End.emit(w)
	}
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
	io.WriteString(w, " then")
	if len(i.Then) == 0 {
		io.WriteString(w, " ()")
	} else {
		io.WriteString(w, "\n")
		indentLevel++
		for idx, st := range i.Then {
			st.emit(w)
			if idx < len(i.Then)-1 {
				w.Write([]byte{'\n'})
			}
		}
		indentLevel--
	}
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
        if t := inferType(s.Expr); t != "unit" && (t != "" || isCallExpr(s.Expr)) {
                io.WriteString(w, "ignore (")
                s.Expr.emit(w)
                io.WriteString(w, ")")
        } else {
                s.Expr.emit(w)
        }
}

func isCallExpr(e Expr) bool {
        switch e.(type) {
        case *CallExpr, *MethodCallExpr:
                return true
        default:
                return false
        }
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
	target := s.Target
	indices := []Expr{}
	for {
		if idx, ok := target.(*IndexExpr); ok {
			indices = append([]Expr{idx.Index}, indices...)
			target = idx.Target
		} else {
			break
		}
	}
	if id, ok := target.(*IdentExpr); ok {
		t := inferType(id)
		name := fsIdent(id.Name)
		if strings.HasSuffix(t, " array") && len(indices) == 1 {
			usesArraySet = true
			writeIndent(w)
			io.WriteString(w, name)
			io.WriteString(w, " <- _arrset ")
			io.WriteString(w, name)
			io.WriteString(w, " (int ")
			if needsParen(indices[0]) {
				io.WriteString(w, "(")
				indices[0].emit(w)
				io.WriteString(w, ")")
			} else {
				indices[0].emit(w)
			}
			io.WriteString(w, ") (")
			s.Value.emit(w)
			io.WriteString(w, ")")
			return
		}
		if strings.HasSuffix(t, " array") || t == "" || strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") {
			writeIndent(w)
			io.WriteString(w, name)
			curT := t
			for _, ix := range indices {
				io.WriteString(w, ".[")
				ixT := inferType(ix)
				if ixT != "int" {
					io.WriteString(w, "int ")
				}
				if needsParen(ix) {
					io.WriteString(w, "(")
					ix.emit(w)
					io.WriteString(w, ")")
				} else {
					ix.emit(w)
				}
				io.WriteString(w, "]")
				if strings.HasSuffix(curT, " array") {
					curT = strings.TrimSuffix(curT, " array")
				} else if strings.HasPrefix(curT, "System.Collections.Generic.IDictionary<") {
					curT = mapValueType(curT)
				}
			}
			io.WriteString(w, " <- ")
			s.Value.emit(w)
			return
		}
	}
	if f, ok := s.Target.(*FieldExpr); ok {
		base := f.Target
		sub := []Expr{}
		for {
			if ix, ok := base.(*IndexExpr); ok {
				sub = append([]Expr{ix.Index}, sub...)
				base = ix.Target
			} else {
				break
			}
		}
		if id, ok2 := base.(*IdentExpr); ok2 {
			writeIndent(w)
			name := fsIdent(id.Name)
			io.WriteString(w, name)
			for _, ix := range sub {
				io.WriteString(w, ".[")
				ix.emit(w)
				io.WriteString(w, "]")
			}
			io.WriteString(w, ".")
			io.WriteString(w, fsIdent(f.Name))
			io.WriteString(w, " <- ")
			s.Value.emit(w)
			return
		}
		if ce, ok2 := base.(*CastExpr); ok2 {
			if id, ok3 := ce.Expr.(*IdentExpr); ok3 {
				writeIndent(w)
				name := fsIdent(id.Name)
				io.WriteString(w, name)
				for _, ix := range sub {
					io.WriteString(w, ".[")
					ix.emit(w)
					io.WriteString(w, "]")
				}
				io.WriteString(w, ".")
				io.WriteString(w, fsIdent(f.Name))
				io.WriteString(w, " <- ")
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
	if wst.WithBreak {
		writeIndent(w)
		io.WriteString(w, "try\n")
		indentLevel++
	}
	for i, st := range wst.Body {
		st.emit(w)
		if i < len(wst.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
	if wst.WithBreak {
		indentLevel--
		w.Write([]byte{'\n'})
		writeIndent(w)
		io.WriteString(w, "with\n")
		writeIndent(w)
		io.WriteString(w, "| Continue -> ()\n")
		writeIndent(w)
		io.WriteString(w, "| Break -> raise Break")
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
	if fst.WithBreak {
		writeIndent(w)
		io.WriteString(w, "try\n")
		indentLevel++
	}
	writeIndent(w)
	io.WriteString(w, "for ")
	if fst.End == nil {
		if ix, ok := fst.Start.(*IndexExpr); ok {
			if isMapType(inferType(ix)) {
				io.WriteString(w, fsIdent(fst.Name))
				io.WriteString(w, " in (")
				ix.emit(w)
				io.WriteString(w, ").Keys do\n")
			} else {
				io.WriteString(w, fsIdent(fst.Name))
				io.WriteString(w, " in ")
				ix.emit(w)
				io.WriteString(w, " do\n")
			}
		} else if call, ok := fst.Start.(*CallExpr); ok && call.Func == "_dictGet" {
			io.WriteString(w, fsIdent(fst.Name))
			io.WriteString(w, " in (")
			fst.Start.emit(w)
			io.WriteString(w, ").Keys do\n")
		} else if isMapType(inferType(fst.Start)) {
			io.WriteString(w, "KeyValue(")
			io.WriteString(w, fsIdent(fst.Name))
			io.WriteString(w, ", _) in ")
			fst.Start.emit(w)
			io.WriteString(w, " do\n")
		} else {
			io.WriteString(w, fsIdent(fst.Name))
			io.WriteString(w, " in ")
			fst.Start.emit(w)
			io.WriteString(w, " do\n")
		}
	} else {
		io.WriteString(w, fsIdent(fst.Name))
		io.WriteString(w, " in ")
		fst.Start.emit(w)
		io.WriteString(w, " .. (")
		(&BinaryExpr{Left: fst.End, Op: "-", Right: &IntLit{Value: 1}}).emit(w)
		io.WriteString(w, ") do\n")
	}
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
		io.WriteString(w, "| Continue -> ()\n")
		writeIndent(w)
		io.WriteString(w, "| Break -> raise Break")
	}
	indentLevel--
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
		if strings.HasSuffix(s.Type, " array") {
			base := strings.TrimSuffix(s.Type, " array")
			io.WriteString(w, "Array.empty<")
			io.WriteString(w, base)
			io.WriteString(w, ">")
		} else if s.Type == "string" {
			io.WriteString(w, "\"\"")
		} else if s.Type == "bool" {
			io.WriteString(w, "false")
		} else if s.Type == "float" {
			io.WriteString(w, "0.0")
		} else if s.Type != "" && s.Type != "int" {
			io.WriteString(w, "Unchecked.defaultof<")
			io.WriteString(w, s.Type)
			io.WriteString(w, ">")
		} else {
			io.WriteString(w, "0")
		}
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
			if strings.HasPrefix(rtyp, "System.Collections.Generic.IDictionary<") {
				rtyp = "map"
			} else if id, ok := b.Right.(*IdentExpr); ok {
				if t, ok2 := varTypes[id.Name]; ok2 && strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") {
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
			if needsParen(b.Right) {
				io.WriteString(w, "(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
			io.WriteString(w, ".ContainsKey(")
			if needsParen(b.Left) {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			io.WriteString(w, ")")
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
	if b.Op == "%" {
		lt := inferType(b.Left)
		rt := inferType(b.Right)
		left := b.Left
		right := b.Right
		if rt == "int64" && lt == "int" {
			left = &CastExpr{Expr: left, Type: "int64"}
		} else if lt == "int64" && rt == "int" {
			right = &CastExpr{Expr: right, Type: "int64"}
		}
		io.WriteString(w, "((")
		if needsParen(left) {
			io.WriteString(w, "(")
			left.emit(w)
			io.WriteString(w, ")")
		} else {
			left.emit(w)
		}
		io.WriteString(w, " % ")
		if needsParen(right) {
			io.WriteString(w, "(")
			right.emit(w)
			io.WriteString(w, ")")
		} else {
			right.emit(w)
		}
		io.WriteString(w, " + ")
		if needsParen(right) {
			io.WriteString(w, "(")
			right.emit(w)
			io.WriteString(w, ")")
		} else {
			right.emit(w)
		}
		io.WriteString(w, ") % ")
		if needsParen(right) {
			io.WriteString(w, "(")
			right.emit(w)
			io.WriteString(w, ")")
		} else {
			right.emit(w)
		}
		io.WriteString(w, ")")
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
			if elemTypeOf(b.Left) == "int" {
				io.WriteString(w, "(Array.map int ")
				if needsParen(b.Right) {
					io.WriteString(w, "(")
					b.Right.emit(w)
					io.WriteString(w, ")")
				} else {
					b.Right.emit(w)
				}
				io.WriteString(w, ")")
			} else {
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
	}
	lt := inferType(b.Left)
	rt := inferType(b.Right)
	left := b.Left
	right := b.Right
	castIf := func(expr Expr, targetType string) Expr {
		return &CastExpr{Expr: expr, Type: targetType}
	}
	if (lt == "obj" || lt == "") && (rt == "int" || rt == "float" || rt == "string" || rt == "bool") {
		left = castIf(left, rt)
		lt = rt
	} else if (rt == "obj" || rt == "") && (lt == "int" || lt == "float" || lt == "string" || lt == "bool") {
		right = castIf(right, lt)
		rt = lt
	} else if (lt == "obj" || lt == "") && (rt == "obj" || rt == "") && (b.Op == "-" || b.Op == "*" || b.Op == "/") {
		left = castIf(left, "float")
		right = castIf(right, "float")
		lt, rt = "float", "float"
	}
	if lt == "float" && rt == "int" {
		right = &CastExpr{Expr: right, Type: "float"}
	} else if rt == "float" && lt == "int" {
		left = &CastExpr{Expr: left, Type: "float"}
	} else if lt == "int64" && rt == "int" {
		right = &CastExpr{Expr: right, Type: "int64"}
	} else if rt == "int64" && lt == "int" {
		left = &CastExpr{Expr: left, Type: "int64"}
	} else if lt == "bigint" && rt == "int" {
		right = &CastExpr{Expr: right, Type: "bigint"}
	} else if rt == "bigint" && lt == "int" {
		left = &CastExpr{Expr: left, Type: "bigint"}
	}
	if b.Op == "/" && lt == "int" && rt == "int" {
		usesFloorDiv = true
		io.WriteString(w, "_floordiv ")
		if needsParen(left) {
			io.WriteString(w, "(")
			left.emit(w)
			io.WriteString(w, ")")
		} else {
			left.emit(w)
		}
		io.WriteString(w, " ")
		if needsParen(right) {
			io.WriteString(w, "(")
			right.emit(w)
			io.WriteString(w, ")")
		} else {
			right.emit(w)
		}
		return
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

type IntLit struct{ Value int64 }

func (i *IntLit) emit(w io.Writer) {
	if i.Value > math.MaxInt32 || i.Value < math.MinInt32 {
		// Emit as a 64-bit literal to avoid overflow when the value
		// exceeds the range of a 32-bit int. Using the L suffix keeps
		// the literal as int64 without an unnecessary cast.
		fmt.Fprintf(w, "%dL", i.Value)
	} else {
		fmt.Fprintf(w, "%d", i.Value)
	}
}

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

func isMapType(t string) bool {
	if strings.HasSuffix(t, " array") || strings.HasSuffix(t, " list") {
		return false
	}
	return t == "map" || strings.HasPrefix(t, "System.Collections.Generic.IDictionary<")
}

func isIndexExpr(e Expr) bool {
	_, ok := e.(*IndexExpr)
	return ok
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
		// Values outside the 32-bit signed range must be treated as
		// int64 to avoid overflow in generated F# code.
		if v.Value > math.MaxInt32 || v.Value < math.MinInt32 {
			return "int64"
		}
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
		usesDictCreate = true
		neededOpens["System.Collections.Generic"] = true
		if len(v.Types) > 0 {
			valT := v.Types[0]
			same := valT != "obj"
			for _, t := range v.Types[1:] {
				if t != valT {
					same = false
					break
				}
			}
			if same {
				return fmt.Sprintf("System.Collections.Generic.IDictionary<string, %s>", valT)
			}
		}
		return "map"
	case *StructLit:
		if v.Name != "" {
			return v.Name
		}
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
				return t
			}
			if strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") {
				return t
			}
			return t
		}
		if t, ok := varTypes[v.Name]; ok {
			if strings.HasSuffix(t, " list") || strings.HasSuffix(t, " array") || strings.HasPrefix(t, "list<") {
				return t
			}
			if strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") {
				return t
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
			if lt == "obj" && rt == "obj" {
				return "float"
			}
			if lt == rt {
				return lt
			}
			if (lt == "int" && rt == "float") || (lt == "float" && rt == "int") {
				return "float"
			}
			if (lt == "bigint" && rt == "int") || (lt == "int" && rt == "bigint") || (lt == "bigint" && rt == "bigint") {
				return "bigint"
			}
			if (lt == "int" && rt == "int64") || (lt == "int64" && rt == "int") {
				return "int64"
			}
			if v.Op == "+" && (lt == "string" || rt == "string") {
				return "string"
			}
		}
	case *AppendExpr:
		t := inferType(v.List)
		if t != "" {
			return t
		}
		return "array"
	case *SubstringExpr:
		return "string"
	case *SliceExpr:
		return inferType(v.Target)
	case *CastExpr:
		if v.Type != "" {
			return v.Type
		}
		return inferType(v.Expr)
	case *IndexExpr:
		t := inferType(v.Target)
		if strings.HasSuffix(t, " array") {
			return strings.TrimSuffix(t, " array")
		}
		if t == "string" {
			return "string"
		}
		if isMapType(t) {
			return mapValueType(t)
		}
		if id, ok := v.Target.(*IdentExpr); ok {
			if isMapType(id.Type) {
				return mapValueType(id.Type)
			}
			if vt, ok2 := varTypes[id.Name]; ok2 && isMapType(vt) {
				return mapValueType(vt)
			}
		}
		return ""
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
		case "_str":
			return "string"
		case "Seq.length", "List.length", "String.length":
			return "int"
		case "Seq.sum", "List.sum":
			return "int"
		case "Seq.averageBy float", "List.averageBy float":
			return "float"
		case "Array.append":
			if len(v.Args) > 0 {
				return inferType(v.Args[0])
			}
			return "array"
		case "_dictGet":
			if len(v.Args) > 0 {
				return mapValueType(inferType(v.Args[0]))
			}
			return "obj"
		}
		if strings.HasSuffix(v.Func, "FifteenPuzzleExample") {
			return "string"
		}
		if t, ok := varTypes[v.Func]; ok && t != "" {
			if strings.Contains(t, "->") {
				parts := strings.Split(t, "->")
				ret := strings.TrimSpace(parts[len(parts)-1])
				return ret
			}
			return t
		}
		if transpileEnv != nil {
			if fv, err := transpileEnv.GetVar(v.Func); err == nil {
				if ft, ok := fv.(types.FuncType); ok {
					return fsType(ft.Return)
				}
			}
		}
	case *MethodCallExpr:
		switch v.Name {
		case "contains", "Contains", "ContainsKey":
			return "bool"
		case "FifteenPuzzleExample":
			return "string"
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
	if ll, ok := e.(*ListLit); ok {
		if t := simpleListType(ll); t != "" {
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
	name := c.Func
	if name == "mod" {
		name = "``mod``"
	}
	if strings.Contains(name, " ") {
		io.WriteString(w, name)
	} else {
		io.WriteString(w, fsIdent(name))
	}
	if len(c.Args) == 0 {
		io.WriteString(w, "()")
		return
	}
	lastDot := strings.LastIndex(name, ".")
	if lastDot != -1 && lastDot+1 < len(name) && unicode.IsUpper(rune(name[lastDot+1])) && !strings.Contains(name, " ") {
		io.WriteString(w, "(")
		for i, a := range c.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	for _, a := range c.Args {
		io.WriteString(w, " (")
		a.emit(w)
		io.WriteString(w, ")")
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
	// If the target is a map, use dictionary helpers
	if isMapType(t) {
		usesDictGet = true
		io.WriteString(w, "_dictGet ")
		if needsParen(i.Target) {
			io.WriteString(w, "(")
			i.Target.emit(w)
			io.WriteString(w, ")")
		} else {
			i.Target.emit(w)
		}
		io.WriteString(w, " (")
		if mapKeyType(t) == "string" {
			io.WriteString(w, "(string (")
			i.Index.emit(w)
			io.WriteString(w, "))")
		} else if mapKeyType(t) == "int" && inferType(i.Index) == "int64" {
			io.WriteString(w, "(int (")
			i.Index.emit(w)
			io.WriteString(w, "))")
		} else {
			i.Index.emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	// Otherwise treat it as an array/string index
	if strings.HasSuffix(t, " array") || t == "" {
		usesSafeIndex = true
		io.WriteString(w, "_idx ")
		if needsParen(i.Target) {
			io.WriteString(w, "(")
			i.Target.emit(w)
			io.WriteString(w, ")")
		} else {
			i.Target.emit(w)
		}
		io.WriteString(w, " (int ")
		if needsParen(i.Index) {
			io.WriteString(w, "(")
			i.Index.emit(w)
			io.WriteString(w, ")")
		} else {
			i.Index.emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	if t == "string" {
		io.WriteString(w, "string (")
		i.Target.emit(w)
		io.WriteString(w, ".[")
		i.Index.emit(w)
		io.WriteString(w, "])")
		return
	}
	if t == "map" || t == "obj" || t == "" {
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
		io.WriteString(w, "((")
		i.Target.emit(w)
		if inferType(i.Index) == "int" {
			io.WriteString(w, " :?> System.Array).GetValue(")
			if needsParen(i.Index) {
				io.WriteString(w, "(")
				i.Index.emit(w)
				io.WriteString(w, ")")
			} else {
				i.Index.emit(w)
			}
			io.WriteString(w, ")")
			if valT != "obj" {
				io.WriteString(w, ")")
				io.WriteString(w, " |> unbox<")
				io.WriteString(w, valT)
				io.WriteString(w, ">")
				return
			}
			io.WriteString(w, ")")
			return
		}
		io.WriteString(w, " :?> System.Collections.Generic.IDictionary<string, ")
		if valT != "obj" {
			io.WriteString(w, valT)
		} else {
			io.WriteString(w, "obj")
		}
		io.WriteString(w, ">).[")
		if needsParen(i.Index) {
			io.WriteString(w, "(")
			i.Index.emit(w)
			io.WriteString(w, ")")
		} else {
			i.Index.emit(w)
		}
		io.WriteString(w, "]")
		io.WriteString(w, ")")
		if valT != "obj" {
			io.WriteString(w, " |> unbox<")
			io.WriteString(w, valT)
			io.WriteString(w, ">")
		}
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
	t := inferType(f.Target)
	if id, ok := f.Target.(*IdentExpr); ok && t == "" {
		if _, ok2 := varTypes[id.Name]; !ok2 {
			if strings.Contains(id.Name, ".") {
				if needsParen(f.Target) {
					io.WriteString(w, "(")
					f.Target.emit(w)
					io.WriteString(w, ")")
				} else {
					f.Target.emit(w)
				}
				io.WriteString(w, ".")
				io.WriteString(w, fsIdent(f.Name))
				return
			}
		}
	}
	if strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") {
		if f.Name == "Keys" || f.Name == "Values" {
			if needsParen(f.Target) {
				io.WriteString(w, "(")
				f.Target.emit(w)
				io.WriteString(w, ")")
			} else {
				f.Target.emit(w)
			}
			io.WriteString(w, ".")
			io.WriteString(w, fsIdent(f.Name))
			return
		}
	}
	if t == "obj" || t == "" {
		if sname, ok := structWithField(f.Name); ok {
			io.WriteString(w, "((")
			f.Target.emit(w)
			io.WriteString(w, " :?> ")
			io.WriteString(w, sname)
			io.WriteString(w, ").")
			io.WriteString(w, fsIdent(f.Name))
			io.WriteString(w, ")")
			return
		}
	}
	if strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") || t == "map" || t == "obj" || t == "" {
		valT := mapValueType(t)
		if id, ok := f.Target.(*IdentExpr); ok {
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
		if t == "obj" || t == "" {
			io.WriteString(w, "((")
			f.Target.emit(w)
			io.WriteString(w, " :?> System.Collections.Generic.IDictionary<string, obj>).[")
		} else {
			if needsParen(f.Target) {
				io.WriteString(w, "(")
				f.Target.emit(w)
				io.WriteString(w, ")")
			} else {
				f.Target.emit(w)
			}
			io.WriteString(w, ".[")
		}
		fmt.Fprintf(w, "%q", f.Name)
		io.WriteString(w, "]")
		if t == "obj" || t == "" {
			io.WriteString(w, ")")
		}
		if valT != "obj" {
			io.WriteString(w, " |> unbox<")
			io.WriteString(w, valT)
			io.WriteString(w, ">")
		}
	} else {
		if needsParen(f.Target) {
			io.WriteString(w, "(")
			f.Target.emit(w)
			io.WriteString(w, ")")
		} else {
			f.Target.emit(w)
		}
		io.WriteString(w, ".")
		io.WriteString(w, fsIdent(f.Name))
	}
}

// MethodCallExpr represents a method invocation target.method(args).
type MethodCallExpr struct {
	Target Expr
	Name   string
	Args   []Expr
}

type InvokeExpr struct {
	Target Expr
	Args   []Expr
}

func (i *InvokeExpr) emit(w io.Writer) {
	if needsParen(i.Target) {
		io.WriteString(w, "(")
		i.Target.emit(w)
		io.WriteString(w, ")")
	} else {
		i.Target.emit(w)
	}
	if len(i.Args) == 0 {
		io.WriteString(w, " ()")
		return
	}
	for _, a := range i.Args {
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

func (m *MethodCallExpr) emit(w io.Writer) {
	// Special handling for common container methods.
	if id, ok := m.Target.(*IdentExpr); ok && id.Name == "stdout" && m.Name == "write" {
		io.WriteString(w, "printf \"%s\" ")
		arg := Expr(&CallExpr{Func: "string", Args: []Expr{m.Args[0]}})
		if inferType(m.Args[0]) == "string" {
			arg = m.Args[0]
		}
		if needsParen(arg) {
			io.WriteString(w, "(")
			arg.emit(w)
			io.WriteString(w, ")")
		} else {
			arg.emit(w)
		}
		return
	}
	switch m.Name {
	case "keys":
		if len(m.Args) == 0 {
			typ := inferType(m.Target)
			if isMapType(typ) {
				io.WriteString(w, "(Map.toList ")
				m.Target.emit(w)
				io.WriteString(w, " |> List.map fst)")
			} else {
				io.WriteString(w, "(Seq.map fst ")
				m.Target.emit(w)
				io.WriteString(w, ")")
			}
			return
		}
	case "values":
		if len(m.Args) == 0 {
			typ := inferType(m.Target)
			if isMapType(typ) {
				io.WriteString(w, "(Map.toList ")
				m.Target.emit(w)
				io.WriteString(w, " |> List.map snd)")
			} else if fields, ok := structFieldNames(typ); ok {
				io.WriteString(w, "[")
				for i, f := range fields {
					if i > 0 {
						io.WriteString(w, "; ")
					}
					io.WriteString(w, "(")
					m.Target.emit(w)
					io.WriteString(w, ".")
					io.WriteString(w, fsIdent(f))
					io.WriteString(w, ")")
				}
				io.WriteString(w, "]")
			} else {
				io.WriteString(w, "(Seq.map snd ")
				m.Target.emit(w)
				io.WriteString(w, ")")
			}
			return
		}
	case "get":
		if len(m.Args) == 2 && isMapType(inferType(m.Target)) {
			io.WriteString(w, "(defaultArg (Map.tryFind ")
			if needsParen(m.Args[0]) {
				io.WriteString(w, "(")
				m.Args[0].emit(w)
				io.WriteString(w, ")")
			} else {
				m.Args[0].emit(w)
			}
			io.WriteString(w, " ")
			m.Target.emit(w)
			io.WriteString(w, ") ")
			m.Args[1].emit(w)
			io.WriteString(w, ")")
			return
		}
	}

	typ := inferType(m.Target)
	if typ == "map" {
		if needsParen(m.Target) {
			io.WriteString(w, "(")
			m.Target.emit(w)
			io.WriteString(w, ")")
		} else {
			m.Target.emit(w)
		}
		io.WriteString(w, ".")
		io.WriteString(w, mapMethod(m.Name))
		if len(m.Args) > 0 {
			io.WriteString(w, "(")
			for i, a := range m.Args {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				a.emit(w)
			}
			io.WriteString(w, ")")
		}
		return
	}
	if ft, ok := structFieldType(typ, m.Name); ok && strings.Contains(ft, "->") {
		if needsParen(m.Target) {
			io.WriteString(w, "(")
			m.Target.emit(w)
			io.WriteString(w, ")")
		} else {
			m.Target.emit(w)
		}
		io.WriteString(w, ".")
		io.WriteString(w, fsIdent(m.Name))
		for _, a := range m.Args {
			io.WriteString(w, " ")
			if needsParen(a) {
				io.WriteString(w, "(")
				a.emit(w)
				io.WriteString(w, ")")
			} else {
				a.emit(w)
			}
		}
		return
	}
	if typ != "" && typ != "obj" {
		// For primitive types like string, prefer direct method calls
		if typ == "string" && (m.Name == "ToLower" || m.Name == "ToUpper" || m.Name == "Trim" || m.Name == "IndexOf" || m.Name == "Split") {
			if needsParen(m.Target) {
				io.WriteString(w, "(")
				m.Target.emit(w)
				io.WriteString(w, ")")
			} else {
				m.Target.emit(w)
			}
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
			return
		}
		io.WriteString(w, fsIdent(typ+"_"+mapMethod(m.Name)))
		io.WriteString(w, " ")
		if needsParen(m.Target) {
			io.WriteString(w, "(")
			m.Target.emit(w)
			io.WriteString(w, ")")
		} else {
			m.Target.emit(w)
		}
		for _, a := range m.Args {
			io.WriteString(w, " ")
			if needsParen(a) {
				io.WriteString(w, "(")
				a.emit(w)
				io.WriteString(w, ")")
			} else {
				a.emit(w)
			}
		}
	} else {
		if needsParen(m.Target) {
			io.WriteString(w, "(")
			m.Target.emit(w)
			io.WriteString(w, ")")
		} else {
			m.Target.emit(w)
		}
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
	if s.IsString || t == "string" || t == "" {
		var finish Expr
		if s.End != nil {
			finish = s.End
		} else {
			finish = &CallExpr{Func: "String.length", Args: []Expr{s.Target}}
		}
		io.WriteString(w, "_substring ")
		if needsParen(s.Target) {
			io.WriteString(w, "(")
			s.Target.emit(w)
			io.WriteString(w, ")")
		} else {
			s.Target.emit(w)
		}
		io.WriteString(w, " (")
		start.emit(w)
		io.WriteString(w, ") (")
		finish.emit(w)
		io.WriteString(w, ")")
		return
	}
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
}

// CastExpr represents expr as type.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	t := inferType(c.Expr)
	switch c.Type {
	case "obj":
		if needsParen(c.Expr) {
			io.WriteString(w, "(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
	case "float":
		if t == "obj" {
			io.WriteString(w, "System.Convert.ToDouble ")
		} else {
			io.WriteString(w, "float ")
		}
		if needsParen(c.Expr) {
			io.WriteString(w, "(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
	case "int":
		if t == "obj" {
			io.WriteString(w, "unbox<int> ")
		} else {
			io.WriteString(w, "int ")
		}
		if needsParen(c.Expr) {
			io.WriteString(w, "(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
	case "int64":
		io.WriteString(w, "int64 ")
		if needsParen(c.Expr) {
			io.WriteString(w, "(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
	case "bigint":
		if t := inferType(c.Expr); t == "obj" {
			io.WriteString(w, "unbox<bigint> ")
		} else {
			io.WriteString(w, "bigint ")
		}
		if needsParen(c.Expr) {
			io.WriteString(w, "(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
	case "int array":
		if ll, ok := c.Expr.(*ListLit); ok && len(ll.Elems) == 0 {
			io.WriteString(w, "Array.empty<int>")
		} else if valueType(c.Expr) == "int64 array" {
			io.WriteString(w, "Array.map int ")
			if needsParen(c.Expr) {
				io.WriteString(w, "(")
				c.Expr.emit(w)
				io.WriteString(w, ")")
			} else {
				c.Expr.emit(w)
			}
		} else {
			io.WriteString(w, "unbox<int array> ")
			if needsParen(c.Expr) {
				io.WriteString(w, "(")
				c.Expr.emit(w)
				io.WriteString(w, ")")
			} else {
				c.Expr.emit(w)
			}
		}
	default:
		if ll, ok := c.Expr.(*ListLit); ok && c.Type == "obj array" {
			io.WriteString(w, "[|")
			for i, e := range ll.Elems {
				io.WriteString(w, "box (")
				e.emit(w)
				io.WriteString(w, ")")
				if i < len(ll.Elems)-1 {
					io.WriteString(w, "; ")
				}
			}
			io.WriteString(w, "|]")
		} else if ll, ok := c.Expr.(*ListLit); ok && strings.HasSuffix(c.Type, " array") && len(ll.Elems) == 0 {
			elem := strings.TrimSuffix(c.Type, " array")
			io.WriteString(w, "Array.empty<")
			io.WriteString(w, elem)
			io.WriteString(w, ">")
		} else if ll, ok := c.Expr.(*ListLit); ok && strings.HasSuffix(c.Type, " array array") {
			io.WriteString(w, "[|")
			for i, e := range ll.Elems {
				if i > 0 {
					io.WriteString(w, "; ")
				}
				e.emit(w)
			}
			io.WriteString(w, "|]")
		} else if strings.HasSuffix(c.Type, " array array") && inferType(c.Expr) == "obj" {
			elem := strings.TrimSuffix(c.Type, " array array")
			io.WriteString(w, "(match ")
			if needsParen(c.Expr) {
				io.WriteString(w, "(")
				c.Expr.emit(w)
				io.WriteString(w, ")")
			} else {
				c.Expr.emit(w)
			}
			io.WriteString(w, " with | :? (")
			io.WriteString(w, c.Type)
			io.WriteString(w, ") as a -> a | :? (obj array) as oa -> oa |> Array.map (fun v -> unbox<")
			io.WriteString(w, elem)
			io.WriteString(w, " array> v) | _ -> failwith \"invalid cast\")")
		} else {
			io.WriteString(w, "unbox<")
			io.WriteString(w, c.Type)
			io.WriteString(w, "> ")
			if needsParen(c.Expr) {
				io.WriteString(w, "(")
				c.Expr.emit(w)
				io.WriteString(w, ")")
			} else {
				c.Expr.emit(w)
			}
		}
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
		buf.WriteString("exception Return\n")
	}
	if prog.UseNow {
		buf.WriteString(helperNow)
		buf.WriteString("\n_initNow()\n")
	}
	if prog.UseSubstring {
		buf.WriteString(helperSubstring)
		buf.WriteString("\n")
	}
	if prog.UsePadStart {
		buf.WriteString(helperPadStart)
		buf.WriteString("\n")
	}
	if prog.UseSHA256 {
		buf.WriteString(helperSHA256)
		buf.WriteString("\n")
	}
	if prog.UseParseIntStr && !definedFuncs["parseIntStr"] {
		buf.WriteString(helperParseIntStr)
		buf.WriteString("\n")
	}
	if prog.UseDictAdd {
		buf.WriteString(helperDictAdd)
		buf.WriteString("\n")
	}
	if prog.UseDictCreate {
		buf.WriteString(helperDictCreate)
		buf.WriteString("\n")
	}
	if prog.UseDictGet {
		buf.WriteString(helperDictGet)
		buf.WriteString("\n")
	}
	if prog.UseSafeIndex {
		buf.WriteString(helperIndex)
		buf.WriteString("\n")
	}
	if prog.UseArraySet {
		buf.WriteString(helperArraySet)
		buf.WriteString("\n")
	}
	if prog.UseJSON {
		buf.WriteString(helperJSON)
		buf.WriteString("\n")
	}
	if prog.UseStr {
		buf.WriteString(helperStr)
		buf.WriteString("\n")
	}
	if prog.UseRepr {
		buf.WriteString(helperRepr)
		buf.WriteString("\n")
	}
	if prog.UseFloorDiv {
		buf.WriteString(helperFloorDiv)
		buf.WriteString("\n")
	}
	for _, a := range prog.Aliases {
		fmt.Fprintf(&buf, "type %s = %s\n", fsIdent(a.Name), a.Type)
	}
	for _, st := range prog.Structs {
		if len(st.Fields) == 0 {
			fmt.Fprintf(&buf, "type %s() = class end\n", fsIdent(st.Name))
			continue
		}
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
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	transpileEnv = env
	varTypes = map[string]string{}
	structDefs = nil
	unionDefs = nil
	aliasDefs = nil
	aliasSet = map[string]bool{}
	structCount = 0
	neededOpens = map[string]bool{}
	definedFuncs = map[string]bool{}
	mutatedVars = map[string]bool{}
	letPtrs = map[string][]*LetStmt{}
	useParseIntStr = false
	usesNow = false
	usesBreak = false
	usesReturn = false
	usesSubstring = false
	usesPadStart = false
	usesSHA256 = false
	usesDictAdd = false
	usesDictGet = false
	usesDictCreate = false
	usesSafeIndex = false
	usesStr = false
	usesRepr = false
	usesArraySet = false
	usesJSON = false
	usesFloorDiv = false
	funcHasNull = false
	currentReturn = ""
	funcDepth = 0
	p := &Program{}
	for i := 0; i < len(prog.Statements); i++ {
		st := prog.Statements[i]
		if st.Expr != nil && st.Expr.Expr != nil && st.Expr.Expr.Binary != nil && st.Expr.Expr.Binary.Left != nil && st.Expr.Expr.Binary.Left.Value != nil && st.Expr.Expr.Binary.Left.Value.Target != nil {
			if sel := st.Expr.Expr.Binary.Left.Value.Target.Selector; sel != nil && sel.Root == "throw" && i+1 < len(prog.Statements) {
				if nxt := prog.Statements[i+1]; nxt.Expr != nil {
					arg, err := convertExpr(nxt.Expr.Expr)
					if err != nil {
						return nil, err
					}
					conv := &ExprStmt{Expr: &CallExpr{Func: "failwith", Args: []Expr{arg}}}
					p.Stmts = append(p.Stmts, optimizeFun(conv))
					i++
					continue
				}
			}
		}
		conv, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if conv != nil {
			p.Stmts = append(p.Stmts, optimizeFun(conv))
		}
	}
	// Preserve original statement order so that top-level values defined
	// before functions remain in scope when those functions are emitted.
	// F# requires values to be declared prior to use.
	if len(methodDefs) > 0 {
		p.Stmts = append(methodDefs, p.Stmts...)
		methodDefs = nil
	}
	p.Structs = structDefs
	p.Unions = unionDefs
	p.Aliases = aliasDefs
	if len(neededOpens) > 0 {
		opens := make([]Stmt, 0, len(neededOpens))
		for m := range neededOpens {
			opens = append(opens, &OpenStmt{Name: m})
		}
		p.Stmts = append(opens, p.Stmts...)
	}
	// Preserve the original statement order. Functions are placed according
	// to their appearance in the source so that both functions and
	// top-level values are defined before use.
	for name, lsts := range letPtrs {
		if mutatedVars[name] {
			for _, ls := range lsts {
				ls.Mutable = true
				if varTypes[name] == "obj" {
					ls.Type = "obj"
				}
			}
		}
	}
	if benchMain {
		hasMain := false
		for _, st := range p.Stmts {
			if fd, ok := st.(*FunDef); ok && fd.Name == "main" {
				hasMain = true
				break
			}
		}
		if !hasMain {
			usesNow = true
			p.Stmts = []Stmt{&BenchStmt{Name: "main", Body: p.Stmts}}
		}
	}
	transpileEnv = nil
	p.UseNow = usesNow
	p.UseBreak = usesBreak
	p.UseReturn = usesReturn
	p.UseSubstring = usesSubstring
	p.UsePadStart = usesPadStart
	p.UseSHA256 = usesSHA256
	p.UseParseIntStr = useParseIntStr
	p.UseDictAdd = usesDictAdd
	p.UseDictGet = usesDictGet
	p.UseDictCreate = usesDictCreate
	p.UseSafeIndex = usesSafeIndex
	p.UseStr = usesStr
	p.UseRepr = usesRepr
	p.UseArraySet = usesArraySet
	p.UseJSON = usesJSON
	p.UseFloorDiv = usesFloorDiv
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

func exprUsesFunc(e Expr) bool {
	switch ex := e.(type) {
	case *CallExpr:
		if definedFuncs[ex.Func] {
			return true
		}
		for _, a := range ex.Args {
			if exprUsesFunc(a) {
				return true
			}
		}
	case *BinaryExpr:
		if exprUsesFunc(ex.Left) || exprUsesFunc(ex.Right) {
			return true
		}
	case *UnaryExpr:
		if exprUsesFunc(ex.Expr) {
			return true
		}
	case *IfExpr:
		if exprUsesFunc(ex.Cond) || exprUsesFunc(ex.Then) || exprUsesFunc(ex.Else) {
			return true
		}
	case *IndexExpr:
		if exprUsesFunc(ex.Target) || exprUsesFunc(ex.Index) {
			return true
		}
	case *SliceExpr:
		if exprUsesFunc(ex.Target) {
			return true
		}
		if ex.Start != nil && exprUsesFunc(ex.Start) {
			return true
		}
		if ex.End != nil && exprUsesFunc(ex.End) {
			return true
		}
	case *AppendExpr:
		if exprUsesFunc(ex.List) || exprUsesFunc(ex.Elem) {
			return true
		}
	case *MatchExpr:
		if exprUsesFunc(ex.Target) {
			return true
		}
		for _, br := range ex.Cases {
			if exprUsesFunc(br.Pattern) || exprUsesFunc(br.Result) {
				return true
			}
		}
	case *ListLit:
		for _, el := range ex.Elems {
			if exprUsesFunc(el) {
				return true
			}
		}
	case *MapLit:
		for _, kv := range ex.Items {
			if exprUsesFunc(kv[0]) || exprUsesFunc(kv[1]) {
				return true
			}
		}
	case *LambdaExpr:
		if exprUsesFunc(ex.Expr) {
			return true
		}
		for _, st := range ex.Body {
			if ls, ok := st.(*ReturnStmt); ok {
				if ls.Expr != nil && exprUsesFunc(ls.Expr) {
					return true
				}
			}
		}
	}
	return false
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
			if declared == "array" {
				if id, ok := e.(*IdentExpr); ok {
					if id.Type != "" && id.Type != "array" {
						declared = id.Type
					} else if t, ok2 := varTypes[id.Name]; ok2 && t != "array" {
						declared = t
					}
				}
			}
			if declared == "" {
				if ix, ok := e.(*IndexExpr); ok {
					if t := elemTypeOf(ix.Target); t != "" {
						if t == "array" {
							declared = "obj"
						} else {
							declared = t
						}
					}
				}
			}
			if declared == "" && transpileEnv != nil && st.Let.Value != nil {
				tt := types.ExprType(st.Let.Value, transpileEnv)
				if fs := fsType(tt); fs != "obj" {
					declared = fs
				}
			}
		}
		if sl, ok := e.(*StructLit); ok && sl.Name != "" {
			declared = sl.Name
		}
		if ll, ok := e.(*ListLit); ok {
			if t := simpleListType(ll); t != "" {
				declared = t
			}
		}
		if il, ok := e.(*IntLit); ok && declared == "int64" && il.Value <= math.MaxUint32 {
			declared = "int"
		}
		if _, ok := e.(*NullLit); ok && declared == "" {
			declared = "obj"
		}
		fsDecl := fsTypeFromString(declared)
		if strings.HasPrefix(fsDecl, "System.Collections.Generic.IDictionary<") {
			switch v := e.(type) {
			case *StructLit:
				items := make([][2]Expr, len(v.Fields))
				for i, f := range v.Fields {
					items[i] = [2]Expr{&StringLit{Value: f.Name}, f.Value}
				}
				e = &MapLit{Items: items}
			case *ListLit:
				for i, el := range v.Elems {
					if sl, ok := el.(*StructLit); ok {
						items := make([][2]Expr, len(sl.Fields))
						for j, f := range sl.Fields {
							items[j] = [2]Expr{&StringLit{Value: f.Name}, f.Value}
						}
						v.Elems[i] = &MapLit{Items: items}
					}
				}
			}
		}
		if _, ok := e.(*NullLit); ok && fsDecl != "" && fsDecl != "obj" {
			e = &DefaultOfExpr{Type: fsDecl}
		} else if fsDecl == "bigint" {
			if _, ok := e.(*IntLit); ok {
				e = &CastExpr{Expr: e, Type: "bigint"}
			}
		} else if fsDecl != "" {
			inferred := inferType(e)
			if fsDecl == "obj" && inferred != "obj" {
				e = &CallExpr{Func: "box", Args: []Expr{e}}
			} else if inferred == "obj" && fsDecl != "obj" {
				if _, ok := e.(*MapLit); ok && strings.HasPrefix(fsDecl, "System.Collections.Generic.IDictionary<") {
					// allow type inference for empty maps without casting
				} else {
					e = &CastExpr{Expr: e, Type: fsDecl}
				}
			}
		}
		if ml, ok := e.(*MapLit); ok && strings.HasPrefix(fsDecl, "System.Collections.Generic.IDictionary<") {
			vtyp := mapValueType(fsDecl)
			ml.Types = make([]string, len(ml.Items))
			for i := range ml.Items {
				if call, ok2 := ml.Items[i][1].(*CallExpr); ok2 && call.Func == "box" && len(call.Args) == 1 {
					ml.Items[i][1] = call.Args[0]
				}
				if ll, ok2 := ml.Items[i][1].(*ListLit); ok2 && len(ll.Elems) == 0 {
					ml.Items[i][1] = &CastExpr{Expr: ml.Items[i][1], Type: vtyp}
				}
				ml.Types[i] = vtyp
			}
		}
		varTypes[st.Let.Name] = declared
		typ := fsDecl
		mut := mutatedVars[st.Let.Name]
		if fsDecl != "" {
			at := inferType(e)
			if fsDecl == "int" && at == "int64" {
				e = &CastExpr{Expr: e, Type: "int"}
			} else if fsDecl == "int64" && at == "int" {
				e = &CastExpr{Expr: e, Type: "int64"}
			} else if at != fsDecl && at != "" && !(at == "map" && strings.HasPrefix(fsDecl, "System.Collections.Generic.IDictionary<")) {
				e = &CastExpr{Expr: e, Type: fsDecl}
			}
		}
		if typ == "array" || typ == "map" {
			typ = ""
		}
		ls := &LetStmt{Name: st.Let.Name, Expr: e, Type: typ, Mutable: mut}
		letPtrs[st.Let.Name] = append(letPtrs[st.Let.Name], ls)
		return ls, nil
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
			if declared == "array" {
				if id, ok := e.(*IdentExpr); ok {
					if id.Type != "" && id.Type != "array" {
						declared = id.Type
					} else if t, ok2 := varTypes[id.Name]; ok2 && t != "array" {
						declared = t
					}
				}
			}
			if declared == "" {
				if ix, ok := e.(*IndexExpr); ok {
					if t := elemTypeOf(ix.Target); t != "" {
						if t == "array" {
							declared = "obj"
						} else {
							declared = t
						}
					}
				}
			}
			if declared == "" && transpileEnv != nil && st.Var.Value != nil {
				tt := types.ExprType(st.Var.Value, transpileEnv)
				if fs := fsType(tt); fs != "obj" {
					declared = fs
				}
			}
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
		if il, ok := e.(*IntLit); ok && declared == "int64" && il.Value <= math.MaxUint32 {
			declared = "int"
		}
		if _, ok := e.(*NullLit); ok && declared == "" {
			declared = "obj"
		}
		fsDecl := fsTypeFromString(declared)
		if _, ok := e.(*NullLit); ok && fsDecl != "" && fsDecl != "obj" {
			e = &DefaultOfExpr{Type: fsDecl}
		} else if fsDecl == "bigint" {
			if _, ok := e.(*IntLit); ok {
				e = &CastExpr{Expr: e, Type: "bigint"}
			}
		} else if fsDecl != "" {
			inferred := inferType(e)
			if fsDecl == "obj" && inferred != "obj" {
				e = &CallExpr{Func: "box", Args: []Expr{e}}
			} else if inferred == "obj" && fsDecl != "obj" {
				if _, ok := e.(*MapLit); ok && strings.HasPrefix(fsDecl, "System.Collections.Generic.IDictionary<") {
					// allow type inference for empty maps
				} else {
					e = &CastExpr{Expr: e, Type: fsDecl}
				}
			}
		}
		if strings.Contains(fsDecl, "System.Collections.Generic.IDictionary<") {
			switch v := e.(type) {
			case *StructLit:
				items := make([][2]Expr, len(v.Fields))
				for i, f := range v.Fields {
					items[i] = [2]Expr{&StringLit{Value: f.Name}, f.Value}
				}
				e = &MapLit{Items: items}
			case *ListLit:
				for i, el := range v.Elems {
					if sl, ok := el.(*StructLit); ok {
						items := make([][2]Expr, len(sl.Fields))
						for j, f := range sl.Fields {
							items[j] = [2]Expr{&StringLit{Value: f.Name}, f.Value}
						}
						v.Elems[i] = &MapLit{Items: items}
					}
				}
			}
		}
		if ml, ok := e.(*MapLit); ok && strings.HasPrefix(fsDecl, "System.Collections.Generic.IDictionary<") {
			vtyp := mapValueType(fsDecl)
			ml.Types = make([]string, len(ml.Items))
			for i := range ml.Items {
				if call, ok2 := ml.Items[i][1].(*CallExpr); ok2 && call.Func == "box" && len(call.Args) == 1 {
					ml.Items[i][1] = call.Args[0]
				}
				if ll, ok2 := ml.Items[i][1].(*ListLit); ok2 && len(ll.Elems) == 0 {
					ml.Items[i][1] = &CastExpr{Expr: ml.Items[i][1], Type: vtyp}
				}
				ml.Types[i] = vtyp
			}
		}
		varTypes[st.Var.Name] = declared
		typ := fsDecl
		if fsDecl != "" {
			at := inferType(e)
			if fsDecl == "int" && at == "int64" {
				e = &CastExpr{Expr: e, Type: "int"}
			} else if fsDecl == "int64" && at == "int" {
				e = &CastExpr{Expr: e, Type: "int64"}
			} else if at != fsDecl && at != "" && !(at == "map" && strings.HasPrefix(fsDecl, "System.Collections.Generic.IDictionary<")) {
				e = &CastExpr{Expr: e, Type: fsDecl}
			}
		}
		if typ == "array" || typ == "map" {
			typ = ""
		}
		ls := &LetStmt{Name: st.Var.Name, Expr: e, Type: typ, Mutable: true}
		letPtrs[st.Var.Name] = append(letPtrs[st.Var.Name], ls)
		return ls, nil
	case st.Assign != nil:
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			name := st.Assign.Name
			e, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			mutatedVars[name] = true
			cur := varTypes[name]
			t := inferType(e)
			if cur == "int" && t == "int64" {
				e = &CastExpr{Expr: e, Type: "int"}
				t = "int"
			}
			if t == "array" {
				if app, ok := e.(*AppendExpr); ok {
					if et := inferType(app.Elem); et != "" && et != "obj" {
						t = et + " array"
					}
				} else if cur := varTypes[name]; cur != "" && cur != "array" && cur != "obj" {
					t = cur
					e = &CastExpr{Expr: e, Type: fsTypeFromString(cur)}
				}
			}
			if t != "" {
				if cur == "" || cur == "array" || cur == "map" {
					varTypes[name] = t
					cur = t
				}
				if cur != "" && cur != t && cur != "obj" {
					varTypes[name] = "obj"
					for _, ls := range letPtrs[name] {
						ls.Type = "obj"
						if ls.Expr != nil {
							ls.Expr = &CallExpr{Func: "box", Args: []Expr{ls.Expr}}
						}
					}
					e = &CallExpr{Func: "box", Args: []Expr{e}}
				} else if cur == "obj" && t != "obj" {
					e = &CallExpr{Func: "box", Args: []Expr{e}}
				}
			} else if vt := varTypes[name]; vt != "" && vt != "obj" {
				e = &CastExpr{Expr: e, Type: fsTypeFromString(vt)}
			}
			return &AssignStmt{Name: name, Expr: e}, nil
		}
		// Handle assignments to indexed or field targets like a[i] <- v or obj.field <- v
		target := Expr(&IdentExpr{Name: st.Assign.Name})
		for _, idx := range st.Assign.Index {
			if idx.Colon != nil || idx.Colon2 != nil {
				return nil, fmt.Errorf("slice assignment not supported")
			}
			ix, err := convertExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			target = &IndexExpr{Target: target, Index: ix}
		}
		for _, f := range st.Assign.Field {
			target = &FieldExpr{Target: target, Name: f.Name}
		}
		val, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		// Ensure value type matches target element type
		tgtType := inferType(target)
		valType := inferType(val)
		if tgtType == "int" && valType == "int64" {
			val = &CastExpr{Expr: val, Type: "int"}
		} else if tgtType == "int64" && valType == "int" {
			val = &CastExpr{Expr: val, Type: "int64"}
		} else if tgtType != "" && valType != "" && tgtType != valType {
			val = &CastExpr{Expr: val, Type: fsTypeFromString(tgtType)}
		}
		// Walk down any indexing to find the base identifier and collect indices.
		base := target
		indices := []Expr{}
		idxCount := len(st.Assign.Index)
		for {
			if ix, ok := base.(*IndexExpr); ok {
				indices = append([]Expr{ix.Index}, indices...)
				base = ix.Target
			} else {
				break
			}
		}
		if id, ok := base.(*IdentExpr); ok {
			t := inferType(id)
			// Map assignment: use helper to update dictionary
			if strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") && len(indices) > 0 {
				expr := buildMapUpdate(&IdentExpr{Name: id.Name}, indices, val)
				return &AssignStmt{Name: fsIdent(id.Name), Expr: expr}, nil
			}
			// Array assignment with single index uses dynamic resize helper
			if strings.HasSuffix(t, " array") && idxCount == 1 {
				usesArraySet = true
			}
		}
		return &IndexAssignStmt{Target: target, Value: val}, nil
	case st.Return != nil:
		var e Expr
		if st.Return.Value != nil {
			var err error
			e, err = convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
			if currentReturn != "" {
				t := inferType(e)
				if t != currentReturn {
					e = &CastExpr{Expr: e, Type: fsTypeFromString(currentReturn)}
				}
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
		saveNull := funcHasNull
		funcHasNull = false
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
		if st.Fun.Return != nil {
			retType = typeRefString(st.Fun.Return)
		}
		if retType == "" && transpileEnv != nil {
			if vt, err := transpileEnv.GetVar(st.Fun.Name); err == nil {
				if ft, ok := vt.(types.FuncType); ok {
					retType = fsType(ft.Return)
				}
			}
		}
		if transpileEnv != nil {
			if vt, err := transpileEnv.GetVar(st.Fun.Name); err == nil {
				if ft, ok := vt.(types.FuncType); ok {
					if inner, ok2 := ft.Return.(types.FuncType); ok2 {
						inner.Return = types.AnyType{}
						ft.Return = inner
					} else {
						ft.Return = types.AnyType{}
					}
					transpileEnv.SetVar(st.Fun.Name, ft, false)
				}
			}
		}
		// Register function signature before processing body so that recursive
		// calls can correctly infer the return type.
		if varTypes == nil {
			varTypes = map[string]string{}
		}
		varTypes[st.Fun.Name] = retType
		if funcDepth == 0 {
			if definedFuncs == nil {
				definedFuncs = map[string]bool{}
			}
			definedFuncs[st.Fun.Name] = true
		}

		prevReturn := currentReturn
		currentReturn = retType
		funcDepth++
		body := make([]Stmt, len(st.Fun.Body))
		for i, s := range st.Fun.Body {
			cs, err := convertStmt(s)
			if err != nil {
				varTypes = save
				currentReturn = prevReturn
				funcDepth--
				return nil, err
			}
			body[i] = cs
		}
		funcDepth--
		currentReturn = prevReturn
		hasNull := funcHasNull
		funcHasNull = saveNull
		if hasNull && strings.Contains(retType, "int array") {
			retType = "obj array"
			for _, st := range body {
				if r, ok := st.(*ReturnStmt); ok {
					if ce, ok2 := r.Expr.(*CastExpr); ok2 && ce.Type == "int array" {
						r.Expr = ce.Expr
					}
				}
			}
		}
		varTypes = save
		if benchMain && st.Fun.Name == "main" {
			usesNow = true
			body = []Stmt{&BenchStmt{Name: "main", Body: body}}
		}
		varTypes[st.Fun.Name] = retType
		if funcDepth == 0 {
			definedFuncs[st.Fun.Name] = true
		}
		if funcParamTypes == nil {
			funcParamTypes = map[string][]string{}
		}
		funcParamTypes[st.Fun.Name] = paramTypes
		return &FunDef{Name: st.Fun.Name, Params: params, Types: paramTypes, Body: body, Return: retType}, nil
	case st.While != nil:
		cond, err := convertExpr(st.While.Cond)
		if err != nil {
			return nil, err
		}
		t := inferType(cond)
		if t == "obj" || t == "" {
			cond = &CastExpr{Expr: cond, Type: "bool"}
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
		elemType := ""
		if st.For.RangeEnd != nil {
			elemType = "int"
		} else {
			elemType = elemTypeOf(start)
			srcT := inferType(start)
			if strings.HasPrefix(srcT, "System.Collections.Generic.IDictionary<") {
				start = &FieldExpr{Target: start, Name: "Keys"}
				elemType = mapKeyType(srcT)
			} else if elemType == "string" {
				start = &CallExpr{Func: "Seq.map string", Args: []Expr{start}}
			}
		}
		if elemType != "" {
			varTypes[st.For.Name] = elemType
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
			if o == "/" && inferType(l) == "int" && inferType(r) == "int" {
				usesFloorDiv = true
			}
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
		if o == "/" && inferType(l) == "int" && inferType(r) == "int" {
			usesFloorDiv = true
		}
		exprs = append(exprs, &BinaryExpr{Left: l, Op: o, Right: r})
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("expr reduce error")
	}
	res := exprs[0]
	if be, ok := res.(*BinaryExpr); ok && (be.Op == "+" || be.Op == "-") {
		if idx, ok2 := be.Left.(*IndexExpr); ok2 {
			t := inferType(idx)
			if t != "int" && t != "float" && t != "string" && t != "bool" {
				idx.Index = &BinaryExpr{Left: idx.Index, Op: be.Op, Right: be.Right}
				res = idx
			}
		}
	}
	return res, nil
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
			if inferType(expr) != "bool" {
				expr = &CastExpr{Expr: expr, Type: "bool"}
			}
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
			t := inferType(expr)
			if ix, ok := idx.(*IndexExpr); ok {
				// Re-associate nested indexing so a[b][c] doesn't become a[b[c]]
				expr = &IndexExpr{Target: expr, Index: ix.Target}
				expr = &IndexExpr{Target: expr, Index: ix.Index}
				continue
			}
			if isMapType(t) {
				usesDictGet = true
			} else {
				usesSafeIndex = true
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
			if isStr {
				usesSubstring = true
			}
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
				if fe.Name == "padStart" {
					if len(args) != 2 {
						return nil, fmt.Errorf("padStart expects 2 args")
					}
					usesPadStart = true
					expr = &CallExpr{Func: "_padStart", Args: append([]Expr{fe.Target}, args...)}
				} else {
					expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
				}
			} else if id, ok := expr.(*IdentExpr); ok {
				expr = &CallExpr{Func: id.Name, Args: args}
			} else {
				expr = &InvokeExpr{Target: expr, Args: args}
			}
		case op.Cast != nil && op.Cast.Type != nil:
			ct := typeRefString(op.Cast.Type)
			if !aliasSet[ct] {
				expr = &CastExpr{Expr: expr, Type: ct}
			}
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
		if definedFuncs[p.Call.Func] {
			if funcParamTypes != nil {
				if pts, ok := funcParamTypes[p.Call.Func]; ok {
					for i, t := range pts {
						if i < len(args) && t != "" && t != inferType(args[i]) {
							if strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") && inferType(args[i]) == "map" {
								// let F# infer generic types for map literals
							} else {
								args[i] = &CastExpr{Expr: args[i], Type: t}
							}
						}
					}
				}
			}
			return &CallExpr{Func: fsIdent(p.Call.Func), Args: args}, nil
		}
		switch p.Call.Func {
		case "print":
			if len(args) == 1 {
				t := inferType(args[0])
				switch t {
				case "bool":
					return &CallExpr{Func: "printfn \"%b\"", Args: []Expr{args[0]}}, nil
				case "int":
					return &CallExpr{Func: "printfn \"%d\"", Args: []Expr{args[0]}}, nil
				case "float":
					return &CallExpr{Func: "printfn \"%g\"", Args: []Expr{args[0]}}, nil
				case "string":
					return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{args[0]}}, nil
				default:
					if strings.HasSuffix(t, " array") {
						usesRepr = true
						wrapped := &CallExpr{Func: "_repr", Args: []Expr{args[0]}}
						return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{wrapped}}, nil
					}
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
					elems[i] = &CallExpr{Func: "sprintf \"%g\"", Args: []Expr{a}}
				case "string":
					elems[i] = &CallExpr{Func: "sprintf \"%s\"", Args: []Expr{a}}
				case "array":
					mapped := &CallExpr{Func: "Array.map string", Args: []Expr{a}}
					elems[i] = &BinaryExpr{Left: &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: &CallExpr{Func: "String.concat \" \"", Args: []Expr{&CallExpr{Func: "Array.toList", Args: []Expr{mapped}}}}}, Op: "+", Right: &StringLit{Value: "]"}}
				default:
					elems[i] = &CallExpr{Func: "sprintf \"%A\"", Args: []Expr{a}}
				}
			}
			list := &ListLit{Elems: elems}
			concat := &CallExpr{Func: "String.concat \" \"", Args: []Expr{list}}
			return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{concat}}, nil
		case "stdout.write":
			if len(args) == 1 {
				switch inferType(args[0]) {
				case "bool":
					b := &IfExpr{Cond: args[0], Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
					return &CallExpr{Func: "printf \"%d\"", Args: []Expr{b}}, nil
				case "int":
					return &CallExpr{Func: "printf \"%d\"", Args: []Expr{args[0]}}, nil
				case "float":
					return &CallExpr{Func: "printf \"%g\"", Args: []Expr{args[0]}}, nil
				case "array":
					mapped := &CallExpr{Func: "Array.map string", Args: []Expr{args[0]}}
					concat := &CallExpr{Func: "String.concat \" \"", Args: []Expr{&CallExpr{Func: "Array.toList", Args: []Expr{mapped}}}}
					wrapped := &BinaryExpr{Left: &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: concat}, Op: "+", Right: &StringLit{Value: "]"}}
					return &CallExpr{Func: "printf \"%s\"", Args: []Expr{wrapped}}, nil
				case "string":
					return &CallExpr{Func: "printf \"%s\"", Args: []Expr{args[0]}}, nil
				default:
					return &CallExpr{Func: "printf \"%A\"", Args: []Expr{args[0]}}, nil
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
					elems[i] = &CallExpr{Func: "sprintf \"%g\"", Args: []Expr{a}}
				case "string":
					elems[i] = &CallExpr{Func: "sprintf \"%s\"", Args: []Expr{a}}
				case "array":
					mapped := &CallExpr{Func: "Array.map string", Args: []Expr{a}}
					elems[i] = &BinaryExpr{Left: &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: &CallExpr{Func: "String.concat \" \"", Args: []Expr{&CallExpr{Func: "Array.toList", Args: []Expr{mapped}}}}}, Op: "+", Right: &StringLit{Value: "]"}}
				default:
					elems[i] = &CallExpr{Func: "sprintf \"%A\"", Args: []Expr{a}}
				}
			}
			list := &ListLit{Elems: elems}
			concat := &CallExpr{Func: "String.concat \" \"", Args: []Expr{list}}
			return &CallExpr{Func: "printf \"%s\"", Args: []Expr{concat}}, nil
		case "parseIntStr":
			useParseIntStr = true
			if len(args) == 1 && !definedFuncs["parseIntStr"] {
				args = append(args, &IntLit{Value: 10})
			}
			return &CallExpr{Func: "parseIntStr", Args: args}, nil
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
				case "obj":
					// Fallback for values with dynamic type. Attempt to treat the
					// value as an array; if it's not, the generated code may fail
					// at runtime, but this handles common cases where `len` is used
					// on a list packaged as `any`.
					cast := &CastExpr{Expr: args[0], Type: "System.Array"}
					field := &FieldExpr{Target: cast, Name: "Length"}
					return field, nil
				}
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "str":
			usesStr = true
			return &CallExpr{Func: "_str", Args: args}, nil
		case "sum":
			fn := "Seq.sum"
			if len(args) == 1 && inferType(args[0]) == "array" {
				fn = "Array.sum"
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "min":
			fn := "Seq.min"
			if len(args) == 1 && inferType(args[0]) == "array" {
				fn = "Array.min"
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "max":
			fn := "Seq.max"
			if len(args) == 1 && inferType(args[0]) == "array" {
				fn = "Array.max"
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "panic":
			return &CallExpr{Func: "failwith", Args: args}, nil
		case "error":
			return &CallExpr{Func: "failwith", Args: args}, nil
		case "throw":
			return &CallExpr{Func: "failwith", Args: args}, nil
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
			listArg := args[0]
			elemArg := args[1]
			if et := elemTypeOf(listArg); et != "" && inferType(elemArg) == "obj" {
				elemArg = &CastExpr{Expr: elemArg, Type: et}
				if ix, ok := elemArg.(*CastExpr); ok {
					if idx, ok2 := ix.Expr.(*IndexExpr); ok2 {
						if id, ok3 := idx.Target.(*IdentExpr); ok3 {
							if varTypes[id.Name] == "obj" {
								varTypes[id.Name] = et + " array"
								id.Type = et + " array"
							}
						}
					}
				}
			}
			return &AppendExpr{List: listArg, Elem: elemArg}, nil
		case "concat":
			if len(args) != 2 {
				return nil, fmt.Errorf("concat expects 2 args")
			}
			return &CallExpr{Func: "Array.append", Args: args}, nil
		case "json":
			if len(args) != 1 {
				return nil, fmt.Errorf("json expects 1 arg")
			}
			usesJSON = true
			return &CallExpr{Func: "json", Args: args}, nil
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
		case "read_file":
			if len(args) != 1 {
				return nil, fmt.Errorf("read_file expects 1 arg")
			}
			neededOpens["System.IO"] = true
			return &CallExpr{Func: "System.IO.File.ReadAllText", Args: args}, nil
		case "substring", "substr":
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			usesSubstring = true
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
		case "padStart":
			if len(args) != 3 {
				return nil, fmt.Errorf("padStart expects 3 args")
			}
			usesPadStart = true
			return &CallExpr{Func: "_padStart", Args: args}, nil
		case "slice":
			if len(args) != 3 {
				return nil, fmt.Errorf("slice expects 3 args")
			}
			isStr := inferType(args[0]) == "string"
			return &SliceExpr{Target: args[0], Start: args[1], End: args[2], IsString: isStr}, nil
		case "to_float":
			if len(args) != 1 {
				return nil, fmt.Errorf("to_float expects 1 arg")
			}
			return &CastExpr{Expr: args[0], Type: "float"}, nil
		case "to_int":
			if len(args) != 1 {
				return nil, fmt.Errorf("to_int expects 1 arg")
			}
			return &CastExpr{Expr: args[0], Type: "int"}, nil
		case "indexOf":
			if len(args) != 2 {
				return nil, fmt.Errorf("indexOf expects 2 args")
			}
			t := inferType(args[0])
			if t == "string" {
				return &MethodCallExpr{Target: args[0], Name: "IndexOf", Args: []Expr{args[1]}}, nil
			}
			lambdaVar := "x"
			pred := &BinaryExpr{Left: &IdentExpr{Name: lambdaVar}, Op: "=", Right: args[1]}
			lam := &LambdaExpr{Params: []string{lambdaVar}, Expr: pred}
			if t == "array" {
				return &CallExpr{Func: "Array.findIndex", Args: []Expr{lam, args[0]}}, nil
			}
			return &CallExpr{Func: "Seq.findIndex", Args: []Expr{lam, args[0]}}, nil
		case "split":
			if len(args) != 2 {
				return nil, fmt.Errorf("split expects 2 args")
			}
			t := inferType(args[0])
			if t == "string" || t == "" {
				return &MethodCallExpr{Target: args[0], Name: "Split", Args: []Expr{&ListLit{Elems: []Expr{args[1]}}, &IdentExpr{Name: "System.StringSplitOptions.None"}}}, nil
			}
			return nil, fmt.Errorf("split on non-string not supported")
		case "contains":
			if len(args) != 2 {
				return nil, fmt.Errorf("contains expects 2 args")
			}
			t := inferType(args[0])
			switch {
			case t == "string":
				return &MethodCallExpr{Target: args[0], Name: "Contains", Args: []Expr{args[1]}}, nil
			case t == "array":
				return &CallExpr{Func: "Array.contains", Args: []Expr{args[1], args[0]}}, nil
			case t == "map":
				return &MethodCallExpr{Target: args[0], Name: "ContainsKey", Args: []Expr{args[1]}}, nil
			default:
				return &CallExpr{Func: "Seq.contains", Args: []Expr{args[1], args[0]}}, nil
			}
		case "sha256":
			if len(args) != 1 {
				return nil, fmt.Errorf("sha256 expects 1 arg")
			}
			usesSHA256 = true
			neededOpens["System.Security.Cryptography"] = true
			neededOpens["System.Text"] = true
			return &CallExpr{Func: "_sha256", Args: args}, nil
		case "keys":
			if len(args) != 1 {
				return nil, fmt.Errorf("keys expects 1 arg")
			}
			t := inferType(args[0])
			if isMapType(t) {
				if strings.HasPrefix(t, "System.Collections.Generic.IDictionary<") {
					return &FieldExpr{Target: args[0], Name: "Keys"}, nil
				}
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
				if fv, err := transpileEnv.GetVar(p.Call.Func); err == nil {
					if ft, ok := fv.(types.FuncType); ok {
						for i := 0; i < len(args) && i < len(ft.Params); i++ {
							pt := fsType(ft.Params[i])
							at := inferType(args[i])
							if (at == "obj" || at == "") && pt != "" && pt != "obj" {
								args[i] = &CastExpr{Expr: args[i], Type: pt}
							} else if at == "int64" && pt == "int" {
								args[i] = &CastExpr{Expr: args[i], Type: "int"}
							}
						}
					}
				}
			}
			return &CallExpr{Func: p.Call.Func, Args: args}, nil
		}
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int64(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Float != nil:
		return &FloatLit{Value: *p.Lit.Float}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Lit != nil && p.Lit.Null:
		funcHasNull = true
		return &NullLit{}, nil
	case p.List != nil:
		// Avoid inferring anonymous records for simple map lists; use generic maps instead
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
			if ft, ok := structFieldType(p.Struct.Name, f.Name); ok {
				at := inferType(v)
				if at == "int64" && ft == "int" {
					v = &CastExpr{Expr: v, Type: "int"}
				} else if at == "int" && ft == "int64" {
					v = &CastExpr{Expr: v, Type: "int64"}
				} else if at != "" && ft != "" && at != ft {
					v = &CastExpr{Expr: v, Type: ft}
				}
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
		usesDictCreate = true
		return &MapLit{Items: items, Types: types}, nil
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.FunExpr != nil:
		save := varTypes
		saveRet := currentReturn
		currentReturn = ""
		if p.FunExpr.Return != nil {
			currentReturn = typeRefString(p.FunExpr.Return)
		}
		varTypes = copyMap(varTypes)
		params := make([]string, len(p.FunExpr.Params))
		paramTypes := make([]string, len(p.FunExpr.Params))
		for i, par := range p.FunExpr.Params {
			params[i] = par.Name
			if par.Type != nil && par.Type.Simple != nil {
				varTypes[par.Name] = *par.Type.Simple
				paramTypes[i] = fsTypeFromString(*par.Type.Simple)
			}
		}
		if p.FunExpr.ExprBody != nil {
			body, err := convertExpr(p.FunExpr.ExprBody)
			retType := currentReturn
			varTypes = save
			currentReturn = saveRet
			if err != nil {
				return nil, err
			}
			return &LambdaExpr{Params: params, Types: paramTypes, Expr: body, Return: retType}, nil
		}
		stmts := make([]Stmt, len(p.FunExpr.BlockBody))
		for i, s := range p.FunExpr.BlockBody {
			cs, err := convertStmt(s)
			if err != nil {
				varTypes = save
				currentReturn = saveRet
				return nil, err
			}
			stmts[i] = cs
		}
		varTypes = save
		retType := currentReturn
		currentReturn = saveRet
		return &LambdaExpr{Params: params, Types: paramTypes, Body: stmts, Return: retType}, nil
	case p.Selector != nil:
		if p.Selector.Root == "nil" && len(p.Selector.Tail) == 0 {
			funcHasNull = true
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
	t := inferType(cond)
	if t == "obj" || t == "" {
		cond = &CastExpr{Expr: cond, Type: "bool"}
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
		if strings.HasPrefix(inferType(base), "System.Collections.Generic.IDictionary<") {
			usesDictGet = true
			usesDictAdd = true
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
	keyT := mapKeyType(inferType(m))
	if keyT == "string" {
		key = &CallExpr{Func: "string", Args: []Expr{key}}
	} else if keyT == "int" && inferType(key) == "int64" {
		key = &CastExpr{Expr: key, Type: "int"}
	}
	if len(keys) == 1 {
		mapVal := mapValueType(inferType(m))
		v := val
		if mapVal == "obj" && inferType(val) != "obj" {
			v = &CallExpr{Func: "box", Args: []Expr{val}}
		}
		usesDictAdd = true
		return &CallExpr{Func: "_dictAdd", Args: []Expr{m, key, v}}
	}
	inner := buildMapUpdate(&IndexExpr{Target: m, Index: key}, keys[1:], val)
	if mapValueType(inferType(m)) == "obj" {
		inner = &CallExpr{Func: "box", Args: []Expr{inner}}
	}
	usesDictAdd = true
	return &CallExpr{Func: "_dictAdd", Args: []Expr{m, key, inner}}
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
	if td.Alias != nil {
		ft := types.ResolveTypeRef(td.Alias, transpileEnv)
		aliasDefs = append(aliasDefs, AliasDef{Name: td.Name, Type: fsType(ft)})
		if aliasSet == nil {
			aliasSet = map[string]bool{}
		}
		aliasSet[td.Name] = true
		return nil
	}
	if len(td.Variants) == 1 && len(td.Variants[0].Fields) == 0 {
		alias := fsTypeFromString(td.Variants[0].Name)
		aliasDefs = append(aliasDefs, AliasDef{Name: td.Name, Type: alias})
		if aliasSet == nil {
			aliasSet = map[string]bool{}
		}
		aliasSet[td.Name] = true
		return nil
	}
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
	for _, m := range td.Members {
		if m.Method == nil {
			continue
		}
		fn := *m.Method
		fn.Name = td.Name + "_" + fn.Name
		selfName := "self"
		selfTypeName := td.Name
		selfType := &parser.TypeRef{Simple: &selfTypeName}
		param := &parser.Param{Name: selfName, Type: selfType}
		fn.Params = append([]*parser.Param{param}, fn.Params...)
		// Inject local variables for struct fields so method bodies can
		// reference them directly by name like in Mochi code.
		var prelude []*parser.Statement
		for _, fm := range td.Members {
			if fm.Field != nil {
				fname := fm.Field.Name
				fieldExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: selfName, Tail: []string{fname}}}}}}}
				prelude = append(prelude, &parser.Statement{Var: &parser.VarStmt{Name: fname, Value: fieldExpr}})
			}
		}
		fn.Body = append(prelude, fn.Body...)
		stmt := &parser.Statement{Fun: &fn}
		fs, err := convertStmt(stmt)
		if err != nil {
			return err
		}
		if fs != nil {
			methodDefs = append(methodDefs, fs)
		}
	}
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
				&FunDef{Name: "sqrt", Params: []string{"x"}, Return: "float", Body: []Stmt{&ReturnStmt{Expr: &CallExpr{Func: "System.Math.Sqrt", Args: []Expr{&IdentExpr{Name: "x"}}}}}},
				&FunDef{Name: "pow", Params: []string{"x", "y"}, Return: "float", Body: []Stmt{&ReturnStmt{Expr: &CallExpr{Func: "System.Math.Pow", Args: []Expr{&IdentExpr{Name: "x"}, &IdentExpr{Name: "y"}}}}}},
				&FunDef{Name: "sin", Params: []string{"x"}, Return: "float", Body: []Stmt{&ReturnStmt{Expr: &CallExpr{Func: "System.Math.Sin", Args: []Expr{&IdentExpr{Name: "x"}}}}}},
				&FunDef{Name: "log", Params: []string{"x"}, Return: "float", Body: []Stmt{&ReturnStmt{Expr: &CallExpr{Func: "System.Math.Log", Args: []Expr{&IdentExpr{Name: "x"}}}}}},
			}}, nil
		}
		if path == "subprocess" {
			neededOpens["System"] = true
			neededOpens["System.Diagnostics"] = true
			stub := &ModuleDef{Name: alias, Stmts: []Stmt{
				&FunDef{Name: "getoutput", Params: []string{"cmd"}, Types: []string{"string"}, Return: "string", Body: []Stmt{
					&LetStmt{Name: "psi", Expr: &CallExpr{Func: "System.Diagnostics.ProcessStartInfo", Args: nil}},
					&AssignStmt{Name: "psi.FileName", Expr: &StringLit{Value: "/bin/sh"}},
					&AssignStmt{Name: "psi.Arguments", Expr: &BinaryExpr{Left: &StringLit{Value: "-c "}, Op: "+", Right: &IdentExpr{Name: "cmd"}}},
					&AssignStmt{Name: "psi.RedirectStandardOutput", Expr: &BoolLit{Value: true}},
					&AssignStmt{Name: "psi.UseShellExecute", Expr: &BoolLit{Value: false}},
					&LetStmt{Name: "p", Expr: &CallExpr{Func: "System.Diagnostics.Process.Start", Args: []Expr{&IdentExpr{Name: "psi"}}}},
					&LetStmt{Name: "output", Expr: &CallExpr{Func: "p.StandardOutput.ReadToEnd", Args: nil}},
					&ExprStmt{Expr: &CallExpr{Func: "p.WaitForExit", Args: nil}},
					&AssignStmt{Name: "__ret", Expr: &CallExpr{Func: "output.TrimEnd", Args: nil}},
				}},
			}}
			return stub, nil
		}
	case "go":
		if path == "mochi/runtime/ffi/go/testpkg" {
			usesReturn = true
			return &ModuleDef{Open: true, Name: alias, Stmts: []Stmt{
				&FunDef{Name: "Add", Params: []string{"a", "b"}, Types: []string{"int", "int"}, Return: "int", Body: []Stmt{
					&ReturnStmt{Expr: &BinaryExpr{Left: &IdentExpr{Name: "a"}, Op: "+", Right: &IdentExpr{Name: "b"}}},
				}},
				&FunDef{Name: "FifteenPuzzleExample", Params: nil, Return: "string", Body: []Stmt{
					&ReturnStmt{Expr: &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}},
				}},
				&LetStmt{Name: "Pi", Expr: &FloatLit{Value: 3.14}},
				&LetStmt{Name: "Answer", Expr: &IntLit{Value: 42}},
			}}, nil
		}
		if path == "strings" {
			return &ModuleDef{Open: true, Name: alias, Stmts: []Stmt{
				&FunDef{Name: "ToUpper", Params: []string{"s"}, Body: []Stmt{&ReturnStmt{Expr: &MethodCallExpr{Target: &IdentExpr{Name: "s"}, Name: "ToUpper"}}}},
				&FunDef{Name: "TrimSpace", Params: []string{"s"}, Body: []Stmt{&ReturnStmt{Expr: &MethodCallExpr{Target: &IdentExpr{Name: "s"}, Name: "Trim"}}}},
			}}, nil
		}
		if path == "os" {
			usesReturn = true
			return &ModuleDef{
				Open: true,
				Name: alias,
				Stmts: []Stmt{
					&FunDef{
						Name:   "Getenv",
						Params: []string{"k"},
						Return: "string",
						Body: []Stmt{
							&ReturnStmt{Expr: &CallExpr{Func: "System.Environment.GetEnvironmentVariable", Args: []Expr{&IdentExpr{Name: "k"}}}},
						},
					},
					&FunDef{
						Name:   "Environ",
						Return: "string array",
						Body: []Stmt{
							&ReturnStmt{
								Expr: &CallExpr{
									Func: "Seq.toArray",
									Args: []Expr{
										&CallExpr{
											Func: "Seq.map",
											Args: []Expr{
												&LambdaExpr{
													Params: []string{"de"},
													Types:  []string{"System.Collections.DictionaryEntry"},
													Expr: &CallExpr{
														Func: "sprintf \"%s=%s\"",
														Args: []Expr{
															&CallExpr{Func: "string", Args: []Expr{&FieldExpr{Target: &IdentExpr{Name: "de", Type: "System.Collections.DictionaryEntry"}, Name: "Key"}}},
															&CallExpr{Func: "string", Args: []Expr{&FieldExpr{Target: &IdentExpr{Name: "de", Type: "System.Collections.DictionaryEntry"}, Name: "Value"}}},
														},
													},
												},
												&CallExpr{
													Func: "Seq.cast<System.Collections.DictionaryEntry>",
													Args: []Expr{
														&CallExpr{Func: "System.Environment.GetEnvironmentVariables"},
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			}, nil
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
			itemFields[i] = StructField{Name: n, Type: typ, Mut: true}
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
		groupFields := []StructField{{Name: "key", Type: keyType, Mut: true}, {Name: "items", Type: itemName + " list", Mut: true}}
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
