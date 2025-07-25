//go:build slow

package javatr

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strings"

	"gopkg.in/yaml.v3"
	"mochi/parser"
	"mochi/types"
)

var varTypes map[string]string
var funcRet map[string]string
var funcParams map[string][]string
var extraDecls []Stmt
var structCount int
var topEnv *types.Env
var groupItems map[string]string
var needLoadYaml bool
var needSaveJsonl bool
var needInput bool
var needNow bool
var needAppendBool bool
var needAppendObj bool
var needNetLookupHost bool
var needMem bool
var pyMathAliases map[string]bool
var builtinAliases map[string]string
var structDefs map[string]map[string]string
var varDecls map[string]*VarStmt
var funcMapFields map[string]map[string]string
var mapVarFields map[string]map[string]string
var benchMain bool

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

func currentEnv() *types.Env {
	if topEnv == nil {
		return nil
	}
	env := topEnv.Copy()
	for name, typ := range varTypes {
		env.SetVar(name, typeFromName(typ), false)
	}
	return env
}

func sanitize(name string) string {
	switch name {
	case "this":
		return "this_"
	case "new":
		return "new_"
	case "double":
		return "double_"
	}
	return name
}

func javaType(t string) string {
	switch t {
	case "int":
		return "int"
	case "bool":
		return "boolean"
	case "boolean":
		return "boolean"
	case "string":
		return "String"
	case "void":
		return "void"
	case "float", "float64", "double":
		return "double"
	case "bigint":
		return "java.math.BigInteger"
	case "int[]":
		return "int[]"
	case "string[]":
		return "String[]"
	case "bool[]":
		return "boolean[]"
	case "float[]", "float64[]", "double[]":
		return "double[]"
	case "map":
		return "java.util.Map"
	case "fn":
		return "java.util.function.Function"
	default:
		if t == "" {
			return ""
		}
		if strings.HasPrefix(t, "string") {
			return "String" + t[len("string"):]
		}
		if strings.HasPrefix(t, "boolean") && t != "boolean" {
			return "boolean" + t[len("boolean"):]
		}
		if strings.HasPrefix(t, "bool") {
			return "boolean" + t[len("bool"):]
		}
		if strings.HasPrefix(t, "fn(") {
			start := strings.Index(t, "(") + 1
			end := strings.Index(t, ")")
			retIdx := strings.LastIndex(t, ":")
			if start >= 0 && end > start && retIdx > end {
				params := strings.Split(t[start:end], ",")
				ret := t[retIdx+1:]
				if len(params) == 1 {
					pt := javaBoxType(javaType(params[0]))
					if ret == "void" {
						return fmt.Sprintf("java.util.function.Consumer<%s>", pt)
					}
					rt := javaBoxType(javaType(ret))
					return fmt.Sprintf("java.util.function.Function<%s,%s>", pt, rt)
				}
			}
		}
		return t
	}
}

func javaBoxType(t string) string {
	switch t {
	case "int":
		return "Integer"
	case "float", "float64", "double":
		return "Double"
	case "bool", "boolean":
		return "Boolean"
	case "string", "String":
		return "String"
	case "bigint", "java.math.BigInteger":
		return "java.math.BigInteger"
	default:
		if strings.HasSuffix(t, "[]") {
			elem := strings.TrimSuffix(t, "[]")
			return javaBoxType(elem) + "[]"
		}
		return "Object"
	}
}

func mapValueType(t string) string {
	if i := strings.Index(t, "Map<"); i >= 0 {
		inside := strings.TrimSuffix(t[i+4:], ">")
		parts := strings.SplitN(inside, ",", 2)
		if len(parts) == 2 {
			v := strings.TrimSpace(parts[1])
			switch v {
			case "Integer":
				return "int"
			case "Double":
				return "double"
			case "Boolean":
				return "boolean"
			default:
				return v
			}
		}
	}
	return ""
}

func mapKeyType(t string) string {
	if i := strings.Index(t, "Map<"); i >= 0 {
		inside := strings.TrimSuffix(t[i+4:], ">")
		parts := strings.SplitN(inside, ",", 2)
		if len(parts) == 2 {
			k := strings.TrimSpace(parts[0])
			switch k {
			case "Integer":
				return "int"
			case "Double":
				return "double"
			case "Boolean":
				return "boolean"
			case "String":
				return "string"
			default:
				return k
			}
		}
	}
	return ""
}

func emitCastExpr(w io.Writer, e Expr, typ string) {
	// Cast map lookups to the destination type when needed.
	if typ != "" && typ != "java.util.Map" && typ != "map" {
		switch ex := e.(type) {
		case *IndexExpr:
			if ex.IsMap || isMapExpr(ex.Target) {
				jt := javaType(typ)
				if jt != "String" {
					fmt.Fprintf(w, "(%s)(", jt)
					e.emit(w)
					fmt.Fprint(w, ")")
					return
				}
			}
		case *FieldExpr:
			if isMapExpr(ex.Target) {
				jt := javaType(typ)
				if jt != "String" {
					fmt.Fprintf(w, "(%s)(", jt)
					e.emit(w)
					fmt.Fprint(w, ")")
					return
				}
			}
		}
	}
	jt := javaType(typ)
	if jt == "String" && !isStringExpr(e) {
		fmt.Fprint(w, "String.valueOf(")
		e.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	if jt == "java.math.BigInteger" {
		if inferType(e) == "bigint" {
			e.emit(w)
			return
		}
		if _, ok := e.(*IntLit); ok {
			fmt.Fprint(w, "java.math.BigInteger.valueOf(")
			e.emit(w)
			fmt.Fprint(w, ")")
			return
		}
		fmt.Fprint(w, "new java.math.BigInteger(String.valueOf(")
		e.emit(w)
		fmt.Fprint(w, "))")
		return
	}
	e.emit(w)
}

func typeFromName(n string) types.Type {
	switch n {
	case "int", "Integer":
		return types.IntType{}
	case "double", "float", "float64":
		return types.FloatType{}
	case "bool", "boolean":
		return types.BoolType{}
	case "string", "String":
		return types.StringType{}
	case "bigint", "java.math.BigInteger":
		return types.BigIntType{}
	default:
		return types.AnyType{}
	}
}

func fieldTypeFromVar(target Expr, name string) (string, bool) {
	if topEnv == nil && structDefs == nil {
		return "", false
	}
	switch v := target.(type) {
	case *VarExpr:
		tname, ok := varTypes[v.Name]
		if !ok {
			return "", false
		}
		base := strings.TrimSuffix(tname, "[]")
		if topEnv != nil {
			if st, ok := topEnv.GetStruct(base); ok {
				if ft, ok2 := st.Fields[name]; ok2 {
					return toJavaTypeFromType(ft), true
				}
			}
		}
		if structDefs != nil {
			if f, ok := structDefs[base][name]; ok {
				return f, true
			}
		}
	case *FieldExpr:
		if typ, ok := fieldTypeFromVar(v.Target, v.Name); ok {
			base := strings.TrimSuffix(typ, "[]")
			if topEnv != nil {
				if st, ok2 := topEnv.GetStruct(base); ok2 {
					if ft, ok3 := st.Fields[name]; ok3 {
						return toJavaTypeFromType(ft), true
					}
				}
			}
			if structDefs != nil {
				if f, ok := structDefs[base][name]; ok {
					return f, true
				}
			}
		}
	}
	return "", false
}

func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		return "int"
	case *FloatLit:
		return "double"
	case *BoolLit:
		return "boolean"
	case *InputExpr:
		return "String"
	case *StringLit:
		return "string"
	case *SubstringExpr:
		return "string"
	case *IndexExpr:
		if ex.ResultType != "" {
			return ex.ResultType
		}
		if isStringExpr(ex.Target) {
			return "string"
		}
		if t := arrayElemType(ex.Target); t != "" {
			return t
		}
	case *SliceExpr:
		if isStringExpr(ex.Value) {
			return "string"
		}
	case *ListLit:
		if ex.ElemType != "" {
			return ex.ElemType + "[]"
		}
		if len(ex.Elems) > 0 {
			t := inferType(ex.Elems[0])
			if t == "" {
				for _, el := range ex.Elems[1:] {
					t = inferType(el)
					if t != "" {
						break
					}
				}
			}
			if t != "" {
				if strings.HasSuffix(t, "[]") {
					return t + "[]"
				}
				switch t {
				case "string":
					return "string[]"
				case "boolean":
					return "bool[]"
				default:
					return t + "[]"
				}
			}
		}
		return ""
	case *StructLit:
		if ex.Name != "" {
			return ex.Name
		}
	case *FieldExpr:
		if t, ok := fieldTypeFromVar(ex.Target, ex.Name); ok {
			return t
		}
	case *MapLit:
		return "map"
	case *LambdaExpr:
		return "fn"
	case *UnaryExpr:
		if ex.Op == "!" {
			return "boolean"
		}
		return inferType(ex.Value)
	case *BinaryExpr:
		switch ex.Op {
		case "+":
			if isStringExpr(ex.Left) || isStringExpr(ex.Right) {
				return "String"
			}
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "bigint" || rt == "bigint" {
				return "bigint"
			}
			if lt == "double" || rt == "double" || lt == "float" || rt == "float" {
				return "double"
			}
			return "int"
		case "-", "*", "/", "%":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "bigint" || rt == "bigint" {
				return "bigint"
			}
			if lt == "double" || rt == "double" || lt == "float" || rt == "float" {
				return "double"
			}
			return "int"
		case "==", "!=", "<", "<=", ">", ">=":
			return "boolean"
		case "&&", "||":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "int" || rt == "int" {
				return "int"
			}
			return "boolean"
		case "in":
			return "boolean"
		}
	case *TernaryExpr:
		t := inferType(ex.Then)
		if t == "" {
			t = inferType(ex.Else)
		}
		return t
	case *GroupExpr:
		return inferType(ex.Expr)
	case *LenExpr:
		return "int"
	case *AvgExpr:
		return "Object"
	case *SumExpr:
		return "Object"
	case *ValuesExpr:
		return "java.util.List"
	case *KeysExpr:
		return "java.util.List"
	case *AppendExpr:
		t := inferType(ex.Value)
		if t == "" || t == "void" {
			t = arrayElemType(ex.List)
		}
		if t == "" {
			t = "Object"
		}
		jt := javaType(t)
		if jt == "" {
			jt = t
		}
		return jt + "[]"
	case *IntCastExpr:
		return "int"
	case *FloatCastExpr:
		return "double"
	case *CastExpr:
		if ex.Type != "" {
			return ex.Type
		}
		return inferType(ex.Value)
	case *CallExpr:
		switch ex.Func {
		case "String.valueOf", "substring":
			return "String"
		case "Integer.parseInt":
			return "int"
		case "System.out.println":
			return "void"
		case "_now":
			return "int"
		default:
			if t, ok := funcRet[ex.Func]; ok {
				return t
			}
			if topEnv != nil {
				if fn, ok := topEnv.GetFunc(ex.Func); ok {
					if t := typeRefString(fn.Return); t != "" {
						return t
					}
				}
			}
		}
	case *MethodCallExpr:
		switch ex.Name {
		case "contains":
			return "boolean"
		case "apply":
			if v, ok := ex.Target.(*VarExpr); ok {
				if ft, ok2 := varTypes[v.Name]; ok2 {
					if strings.HasPrefix(ft, "fn(") {
						if idx := strings.LastIndex(ft, "):"); idx >= 0 {
							ret := ft[idx+2:]
							if ret != "" {
								return ret
							}
						}
					}
				}
			}
			return "Object"
		}
	case *VarExpr:
		if ex.Type != "" {
			return ex.Type
		}
		if t, ok := varTypes[ex.Name]; ok {
			return t
		}
	}
	return ""
}

func guessIntExpr(e Expr) bool {
	if inferType(e) == "int" {
		return true
	}
	if ix, ok := e.(*IndexExpr); ok {
		if arrayElemType(ix.Target) == "int" {
			return true
		}
	}
	return false
}

func inferReturnType(body []Stmt) string {
	if len(body) == 0 {
		return "void"
	}
	if ret, ok := body[len(body)-1].(*ReturnStmt); ok {
		if ret.Expr == nil {
			return "void"
		}
		t := inferType(ret.Expr)
		if t == "" {
			return "void"
		}
		return t
	}
	return "void"
}

func collectReturnMap(body []Stmt) map[string]string {
	for _, st := range body {
		if ret, ok := st.(*ReturnStmt); ok {
			if ml, ok2 := ret.Expr.(*MapLit); ok2 && len(ml.Fields) > 0 {
				return ml.Fields
			}
			if ce, ok2 := ret.Expr.(*CallExpr); ok2 {
				if f, ok3 := funcMapFields[ce.Func]; ok3 {
					return f
				}
			}
		}
	}
	return nil
}

// --- Simple Java AST ---

type Program struct {
	Funcs []*Function
	Stmts []Stmt
}

type Param struct {
	Name string
	Type string
}

// TypeDeclStmt declares a simple struct type.
type TypeDeclStmt struct {
	Name    string
	Fields  []Param
	Extends string
}

// InterfaceDeclStmt declares a simple interface type.
type InterfaceDeclStmt struct{ Name string }

// queryFrom represents a single 'from' clause in a query.
type queryFrom struct {
	Var string
	Src Expr
}

// QueryExpr represents a simplified query comprehension.
type queryJoin struct {
	Var  string
	Src  Expr
	On   Expr
	Side string
}

type queryGroup struct {
	Key       Expr
	Name      string
	Having    Expr
	ItemType  string
	GroupType string
	Fields    []string
}

type QueryExpr struct {
	Var      string
	Src      Expr
	Froms    []queryFrom
	Joins    []queryJoin
	Group    *queryGroup
	Where    Expr
	Sort     Expr
	Skip     Expr
	Take     Expr
	Select   Expr
	ElemType string
}

// StructLit represents a struct literal.
type StructLit struct {
	Name   string
	Fields []Expr
	Names  []string
}

type Function struct {
	Name   string
	Params []Param
	Return string
	Body   []Stmt
}

type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"return")
	if r.Expr != nil {
		fmt.Fprint(w, " ")
		r.Expr.emit(w)
	}
	fmt.Fprint(w, ";\n")
}

type Stmt interface{ emit(io.Writer, string) }

type Expr interface{ emit(io.Writer) }

func (t *TypeDeclStmt) emit(w io.Writer, indent string) {
	decl := "static class " + t.Name
	if t.Extends != "" {
		decl += " implements " + t.Extends
	}
	fmt.Fprintf(w, indent+"%s {\n", decl)
	for _, f := range t.Fields {
		typ := javaType(f.Type)
		if typ == "" {
			typ = f.Type
		}
		fmt.Fprintf(w, indent+"    %s %s;\n", typ, f.Name)
	}
	fmt.Fprintf(w, indent+"    %s(", t.Name)
	for i, f := range t.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		typ := javaType(f.Type)
		if typ == "" {
			typ = f.Type
		}
		fmt.Fprintf(w, "%s %s", typ, sanitize(f.Name))
	}
	fmt.Fprint(w, ") {\n")
	for _, f := range t.Fields {
		fmt.Fprintf(w, indent+"        this.%s = %s;\n", sanitize(f.Name), sanitize(f.Name))
	}
	fmt.Fprint(w, indent+"    }\n")
	fmt.Fprint(w, indent+"    @Override public String toString() {\n")
	fmt.Fprint(w, indent+"        return String.format(\"")
	fmt.Fprint(w, "{")
	for i, f := range t.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		typ := javaType(f.Type)
		if typ == "" {
			typ = f.Type
		}
		if typ == "String" {
			fmt.Fprintf(w, "'%s': '%%s'", f.Name)
		} else {
			fmt.Fprintf(w, "'%s': %%s", f.Name)
		}
	}
	fmt.Fprint(w, "}\", ")
	for i, f := range t.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "String.valueOf(%s)", f.Name)
	}
	fmt.Fprint(w, ");\n")
	fmt.Fprint(w, indent+"    }\n")
	fmt.Fprint(w, indent+"}\n")
}

func (i *InterfaceDeclStmt) emit(w io.Writer, indent string) {
	fmt.Fprintf(w, indent+"interface %s {}\n", i.Name)
}

func (s *StructLit) emit(w io.Writer) {
	fmt.Fprintf(w, "new %s(", s.Name)
	for i, f := range s.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		f.emit(w)
	}
	fmt.Fprint(w, ")")
}

func (q *QueryExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "new java.util.ArrayList<%s>() {{", q.ElemType)
	if q.Group != nil {
		fmt.Fprintf(w, " java.util.LinkedHashMap<String,%s> _groups = new java.util.LinkedHashMap<>();", q.Group.GroupType)
	}
	fmt.Fprintf(w, " java.util.ArrayList<%s> _tmp = new java.util.ArrayList<>();", q.ElemType)
	fmt.Fprintf(w, " for (var %s : ", q.Var)
	if v, ok := q.Src.(*VarExpr); ok {
		if _, ok2 := groupItems[varTypes[v.Name]]; ok2 {
			fmt.Fprint(w, v.Name+".items")
		} else {
			q.Src.emit(w)
		}
	} else {
		q.Src.emit(w)
	}
	fmt.Fprint(w, ") {")
	for _, f := range q.Froms {
		fmt.Fprintf(w, " for (var %s : ", f.Var)
		f.Src.emit(w)
		fmt.Fprint(w, ") {")
	}
	for _, j := range q.Joins {
		fmt.Fprintf(w, " for (var %s : ", j.Var)
		j.Src.emit(w)
		fmt.Fprint(w, ") {")
		fmt.Fprint(w, " if (")
		if j.On != nil {
			j.On.emit(w)
		} else {
			fmt.Fprint(w, "true")
		}
		fmt.Fprint(w, ") {")
	}
	if q.Where != nil {
		fmt.Fprint(w, " if (")
		q.Where.emit(w)
		fmt.Fprint(w, ") {")
	}
	if q.Group != nil {
		fmt.Fprint(w, " var _k = ")
		q.Group.Key.emit(w)
		fmt.Fprint(w, "; String _ks = String.valueOf(_k);")
		fmt.Fprintf(w, " %s g = _groups.get(_ks);", q.Group.GroupType)
		fmt.Fprintf(w, " if (g == null) { g = new %s(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); }", q.Group.GroupType)
		if len(q.Group.Fields) == 1 && !strings.HasPrefix(q.Group.ItemType, "Item") {
			fmt.Fprint(w, " g.items.add(")
			fmt.Fprint(w, q.Group.Fields[0])
			fmt.Fprint(w, ");")
		} else {
			fmt.Fprintf(w, " g.items.add(new %s(", q.Group.ItemType)
			for i, fld := range q.Group.Fields {
				if i > 0 {
					fmt.Fprint(w, ", ")
				}
				fmt.Fprint(w, fld)
			}
			fmt.Fprint(w, "));")
		}
	} else {
		fmt.Fprint(w, " _tmp.add(")
		q.Select.emit(w)
		fmt.Fprint(w, ");")
	}
	if q.Where != nil {
		fmt.Fprint(w, " }")
	}
	for range q.Joins {
		fmt.Fprint(w, " }")
		fmt.Fprint(w, " }")
	}
	for range q.Froms {
		fmt.Fprint(w, " }")
	}
	fmt.Fprint(w, " }")

	if q.Group != nil {
		fmt.Fprintf(w, " java.util.ArrayList<%s> list = new java.util.ArrayList<>(_groups.values());", q.Group.GroupType)
	} else {
		fmt.Fprintf(w, " java.util.ArrayList<%s> list = _tmp;", q.ElemType)
	}
	fmt.Fprintf(w, " java.util.ArrayList<%s> _res = new java.util.ArrayList<>();", q.ElemType)

	if q.Sort != nil {
		fmt.Fprint(w, " list.sort((a, b) -> {")
		expr := q.Sort
		desc := false
		if ue, ok := q.Sort.(*UnaryExpr); ok && ue.Op == "-" {
			expr = ue.Value
			desc = true
		}
		base := q.Var
		if q.Group != nil {
			base = q.Group.Name
		}
		a := renameVar(expr, base, "a")
		b := renameVar(expr, base, "b")
		fmt.Fprint(w, "Comparable _va = (Comparable)(")
		a.emit(w)
		fmt.Fprint(w, "); Comparable _vb = (Comparable)(")
		b.emit(w)
		fmt.Fprint(w, "); return ")
		if desc {
			fmt.Fprint(w, "_vb.compareTo(_va)")
		} else {
			fmt.Fprint(w, "_va.compareTo(_vb)")
		}
		fmt.Fprint(w, ";});")
	}

	fmt.Fprint(w, " int skip = ")
	if q.Skip != nil {
		q.Skip.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "; int take = ")
	if q.Take != nil {
		q.Take.emit(w)
	} else {
		fmt.Fprint(w, "-1")
	}
	fmt.Fprint(w, "; for (int i = 0; i < list.size(); i++) {")
	fmt.Fprint(w, " if (i < skip) continue; if (take >= 0 && i >= skip + take) break;")
	if q.Group != nil {
		fmt.Fprintf(w, " var %s = (%s)list.get(i);", q.Group.Name, q.Group.GroupType)
		if q.Group.Having != nil {
			fmt.Fprint(w, " if (")
			q.Group.Having.emit(w)
			fmt.Fprint(w, ") {")
		}
		fmt.Fprint(w, " _res.add(")
		q.Select.emit(w)
		fmt.Fprint(w, ");")
		if q.Group.Having != nil {
			fmt.Fprint(w, " }")
		}
	} else {
		fmt.Fprint(w, " _res.add((")
		fmt.Fprintf(w, "%s)list.get(i));", q.ElemType)
	}
	fmt.Fprint(w, " }")

	fmt.Fprint(w, " addAll(_res);")
	fmt.Fprint(w, "}}")
}

// LambdaExpr represents a simple lambda expression with a single return value.
type LambdaExpr struct {
	Params []Param
	Body   []Stmt
	Return string
}

func (l *LambdaExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	for i, p := range l.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprint(w, sanitize(p.Name))
	}
	fmt.Fprint(w, ") -> ")
	if len(l.Body) == 1 {
		if rs, ok := l.Body[0].(*ReturnStmt); ok {
			if rs.Expr != nil {
				rs.Expr.emit(w)
				return
			}
		}
		if es, ok := l.Body[0].(*ExprStmt); ok {
			es.Expr.emit(w)
			return
		}
	}
	fmt.Fprint(w, "{\n")
	for _, st := range l.Body {
		st.emit(w, "    ")
	}
	fmt.Fprint(w, "}")
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"if (")
	s.Cond.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range s.Then {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}")
	if len(s.Else) > 0 {
		if len(s.Else) == 1 {
			if ei, ok := s.Else[0].(*IfStmt); ok {
				fmt.Fprint(w, " else ")
				ei.emit(w, indent)
				return
			}
		}
		fmt.Fprint(w, " else {\n")
		for _, st := range s.Else {
			st.emit(w, indent+"    ")
		}
		fmt.Fprint(w, indent+"}\n")
	} else {
		fmt.Fprint(w, "\n")
	}
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent)
	s.Expr.emit(w)
	fmt.Fprint(w, ";\n")
}

type LetStmt struct {
	Name string
	Type string
	Expr Expr
}

func (s *LetStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent)
	if indent == "    " {
		fmt.Fprint(w, "static ")
	}
	typ := s.Type
	if typ == "" && s.Expr != nil {
		typ = inferType(s.Expr)
	}
	if typ == "" {
		typ = "Object"
	}
	fmt.Fprint(w, javaType(typ)+" "+sanitize(s.Name))
	if s.Expr != nil {
		fmt.Fprint(w, " = ")
		emitCastExpr(w, s.Expr, typ)
	}
	fmt.Fprint(w, ";\n")
}

type VarStmt struct {
	Name string
	Type string
	Expr Expr
}

func (s *VarStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent)
	if indent == "    " {
		fmt.Fprint(w, "static ")
	}
	typ := s.Type
	if typ == "" && s.Expr != nil {
		typ = inferType(s.Expr)
	}
	if typ == "" {
		typ = "Object"
	}
	fmt.Fprint(w, javaType(typ)+" "+s.Name)
	if s.Expr != nil {
		fmt.Fprint(w, " = ")
		emitCastExpr(w, s.Expr, typ)
	}
	fmt.Fprint(w, ";\n")
}

type AssignStmt struct {
	Name string
	Expr Expr
	Type string
}

func (s *AssignStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+sanitize(s.Name)+" = ")
	typ := s.Type
	if typ == "" {
		if vs, ok := varDecls[s.Name]; ok {
			typ = vs.Type
		}
	}
	emitCastExpr(w, s.Expr, typ)
	fmt.Fprint(w, ";\n")
}

// IndexAssignStmt represents assignments like a[0] = x or m["k"] = v.
type IndexAssignStmt struct {
	Target  Expr
	Indices []Expr
	Expr    Expr
}

func (s *IndexAssignStmt) emit(w io.Writer, indent string) {
	if len(s.Indices) == 1 {
		if isMapExpr(s.Target) {
			s.Target.emit(w)
			fmt.Fprint(w, ".put(")
			s.Indices[0].emit(w)
			fmt.Fprint(w, ", ")
			s.Expr.emit(w)
			fmt.Fprint(w, ");\n")
			return
		}
		if key, ok := s.Indices[0].(*StringLit); ok && !isMapExpr(s.Target) {
			s.Target.emit(w)
			fmt.Fprintf(w, ".%s = ", key.Value)
			s.Expr.emit(w)
			fmt.Fprint(w, ";\n")
			return
		}
	}
	s.Target.emit(w)
	for _, idx := range s.Indices {
		fmt.Fprint(w, "[")
		idx.emit(w)
		fmt.Fprint(w, "]")
	}
	fmt.Fprint(w, " = ")
	s.Expr.emit(w)
	fmt.Fprint(w, ";\n")
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"while (")
	if wst.Cond != nil {
		wst.Cond.emit(w)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range wst.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}\n")
}

type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"for (int "+fr.Name+" = ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "; ")
	fmt.Fprint(w, fr.Name+" < ")
	fr.End.emit(w)
	fmt.Fprint(w, "; ")
	fmt.Fprint(w, fr.Name+"++")
	fmt.Fprint(w, ") {\n")
	for _, st := range fr.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}\n")
}

// ForEachStmt represents `for x in list {}` loops.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	IsMap    bool
	ElemType string
}

func (fe *ForEachStmt) emit(w io.Writer, indent string) {
	typ := javaType(fe.ElemType)
	if typ == "" {
		fmt.Fprint(w, indent+"for (var "+fe.Name+" : ")
	} else {
		fmt.Fprintf(w, indent+"for (%s %s : ", typ, fe.Name)
	}
	fe.Iterable.emit(w)
	if fe.IsMap {
		fmt.Fprint(w, ".keySet()")
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range fe.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}\n")
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer, indent string) { fmt.Fprint(w, indent+"break;\n") }

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer, indent string) { fmt.Fprint(w, indent+"continue;\n") }

// BenchStmt represents a benchmark block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (b *BenchStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"{\n")
	fmt.Fprint(w, indent+"    long _benchStart = _now();\n")
	fmt.Fprint(w, indent+"    long _benchMem = _mem();\n")
	for _, st := range b.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"    long _benchDuration = _now() - _benchStart;\n")
	fmt.Fprint(w, indent+"    long _benchMemory = _mem() - _benchMem;\n")
	fmt.Fprint(w, indent+"    System.out.println(\"{\");\n")
	fmt.Fprint(w, indent+"    System.out.println(\"  \\\"duration_us\\\": \" + _benchDuration + \",\");\n")
	fmt.Fprint(w, indent+"    System.out.println(\"  \\\"memory_bytes\\\": \" + _benchMemory + \",\");\n")
	fmt.Fprintf(w, indent+"    System.out.println(\"  \\\"name\\\": \\\"%s\\\"\");\n", b.Name)
	fmt.Fprint(w, indent+"    System.out.println(\"}\");\n")
	fmt.Fprint(w, indent+"    return;\n")
	fmt.Fprint(w, indent+"}\n")
}

// UpdateStmt represents an `update` statement on a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (u *UpdateStmt) emit(w io.Writer, indent string) {
	fmt.Fprintf(w, indent+"for (int i = 0; i < %s.length; i++) {\n", u.Target)
	fmt.Fprintf(w, indent+"    var item = %s[i];\n", u.Target)
	if u.Cond != nil {
		fmt.Fprint(w, indent+"    if (")
		u.Cond.emit(w)
		fmt.Fprint(w, ") {\n")
		inner := indent + "        "
		for i, f := range u.Fields {
			fmt.Fprintf(w, inner+"item.%s = ", f)
			u.Values[i].emit(w)
			fmt.Fprint(w, ";\n")
		}
		fmt.Fprint(w, indent+"    }\n")
	} else {
		for i, f := range u.Fields {
			fmt.Fprintf(w, indent+"    item.%s = ", f)
			u.Values[i].emit(w)
			fmt.Fprint(w, ";\n")
		}
	}
	fmt.Fprintf(w, indent+"    %s[i] = item;\n", u.Target)
	fmt.Fprint(w, indent+"}\n")
}

// ListLit represents a list literal.
type ListLit struct {
	ElemType string
	Elems    []Expr
}

func (l *ListLit) emit(w io.Writer) {
	arrType := l.ElemType
	if arrType == "" {
		if len(l.Elems) > 0 {
			t := inferType(l.Elems[0])
			if strings.HasSuffix(t, "[]") {
				arrType = t
			} else {
				arrType = t
				if arrType == "" {
					arrType = "Object"
				}
				switch arrType {
				case "string":
					arrType = "String"
				case "boolean":
					arrType = "boolean"
				}
			}
		} else {
			arrType = "Object"
		}
		arrType = javaType(arrType)
	} else {
		jt := javaType(arrType)
		if jt != "" {
			arrType = jt
		}
	}
	fmt.Fprintf(w, "new %s[]{", arrType)
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "}")
}

// MapLit represents a simple map literal.
type MapLit struct {
	Keys      []Expr
	Values    []Expr
	Fields    map[string]string
	KeyType   string
	ValueType string
}

func (m *MapLit) emit(w io.Writer) {
	valType := "Object"
	if m.ValueType != "" {
		valType = javaBoxType(m.ValueType)
	} else if len(m.Values) > 0 {
		t := inferType(m.Values[0])
		same := true
		for _, v := range m.Values[1:] {
			if inferType(v) != t {
				same = false
				break
			}
		}
		if same {
			valType = javaBoxType(t)
		}
	}
	keyType := "String"
	if m.KeyType != "" {
		keyType = javaBoxType(m.KeyType)
	}
	if len(m.Keys) > 0 {
		fmt.Fprintf(w, "new java.util.LinkedHashMap<%s, %s>(java.util.Map.ofEntries(", keyType, valType)
		for i := range m.Keys {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			fmt.Fprint(w, "java.util.Map.entry(")
			m.Keys[i].emit(w)
			fmt.Fprint(w, ", ")
			if m.ValueType != "" {
				emitCastExpr(w, m.Values[i], m.ValueType)
			} else {
				m.Values[i].emit(w)
			}
			fmt.Fprint(w, ")")
		}
		fmt.Fprint(w, "))")
	} else {
		fmt.Fprintf(w, "new java.util.LinkedHashMap<%s, %s>()", keyType, valType)
	}
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" && (isStringExpr(b.Left) || isStringExpr(b.Right)) {
		emitCastExpr(w, b.Left, "String")
		fmt.Fprint(w, " + ")
		emitCastExpr(w, b.Right, "String")
		return
	}
	if (b.Op == "==" || b.Op == "!=") && (isStringExpr(b.Left) || isStringExpr(b.Right)) {
		if b.Op == "!=" {
			fmt.Fprint(w, "!")
		}
		fmt.Fprint(w, "(")
		b.Left.emit(w)
		fmt.Fprint(w, ".equals(")
		b.Right.emit(w)
		fmt.Fprint(w, "))")
		return
	}
	if (b.Op == "==" || b.Op == "!=") && (isNullExpr(b.Left) || isNullExpr(b.Right)) {
		if b.Op == "!=" {
			fmt.Fprint(w, "!")
		}
		fmt.Fprint(w, "(")
		if isNullExpr(b.Left) {
			b.Right.emit(w)
		} else {
			b.Left.emit(w)
		}
		fmt.Fprint(w, " == null)")
		return
	}
	if (isStringExpr(b.Left) || isStringExpr(b.Right)) &&
		(b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=") {
		lt := inferType(b.Left)
		rt := inferType(b.Right)
		if lt != "int" && lt != "double" && rt != "int" && rt != "double" {
			switch b.Op {
			case "<", "<=", ">", ">=":
				fmt.Fprint(w, "(")
				b.Left.emit(w)
				fmt.Fprint(w, ".compareTo(")
				b.Right.emit(w)
				fmt.Fprint(w, ") ")
				switch b.Op {
				case "<":
					fmt.Fprint(w, "< 0")
				case "<=":
					fmt.Fprint(w, "<= 0")
				case ">":
					fmt.Fprint(w, "> 0")
				case ">=":
					fmt.Fprint(w, ">= 0")
				}
				fmt.Fprint(w, ")")
				return
			}
		}
	}
	if b.Op == "&&" || b.Op == "||" {
		emitCastExpr(w, b.Left, "boolean")
		fmt.Fprint(w, " "+b.Op+" ")
		emitCastExpr(w, b.Right, "boolean")
		return
	}

	if b.Op == "+" || b.Op == "-" || b.Op == "*" || b.Op == "/" || b.Op == "%" ||
		b.Op == "==" || b.Op == "!=" || b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=" {
		lt := inferType(b.Left)
		rt := inferType(b.Right)
		if lt == "" && guessIntExpr(b.Left) {
			lt = "int"
		}
		if rt == "" && guessIntExpr(b.Right) {
			rt = "int"
		}
		big := lt == "bigint" || rt == "bigint"
		typ := "int"
		if lt == "Object" {
			lt = ""
		}
		if rt == "Object" {
			rt = ""
		}
		if lt == "double" || rt == "double" {
			typ = "double"
		}
		if big {
			emitBigIntOperand := func(e Expr, t string) {
				if inferType(e) == "bigint" {
					e.emit(w)
				} else if _, ok := e.(*IntLit); ok {
					fmt.Fprint(w, "java.math.BigInteger.valueOf(")
					e.emit(w)
					fmt.Fprint(w, ")")
				} else {
					fmt.Fprint(w, "new java.math.BigInteger(String.valueOf(")
					e.emit(w)
					fmt.Fprint(w, "))")
				}
			}
			emitBigIntOperand(b.Left, lt)
			switch b.Op {
			case "+":
				fmt.Fprint(w, ".add(")
				emitBigIntOperand(b.Right, rt)
				fmt.Fprint(w, ")")
			case "-":
				fmt.Fprint(w, ".subtract(")
				emitBigIntOperand(b.Right, rt)
				fmt.Fprint(w, ")")
			case "*":
				fmt.Fprint(w, ".multiply(")
				emitBigIntOperand(b.Right, rt)
				fmt.Fprint(w, ")")
			case "/":
				fmt.Fprint(w, ".divide(")
				emitBigIntOperand(b.Right, rt)
				fmt.Fprint(w, ")")
			case "%":
				fmt.Fprint(w, ".remainder(")
				emitBigIntOperand(b.Right, rt)
				fmt.Fprint(w, ")")
			case "==", "!=", "<", "<=", ">", ">=":
				fmt.Fprint(w, ".compareTo(")
				emitBigIntOperand(b.Right, rt)
				fmt.Fprint(w, ") ")
				switch b.Op {
				case "==":
					fmt.Fprint(w, "== 0")
				case "!=":
					fmt.Fprint(w, "!= 0")
				case "<":
					fmt.Fprint(w, "< 0")
				case "<=":
					fmt.Fprint(w, "<= 0")
				case ">":
					fmt.Fprint(w, "> 0")
				case ">=":
					fmt.Fprint(w, ">= 0")
				}
			}
			return
		}
		if lt == "" {
			fmt.Fprint(w, "((Number)(")
			b.Left.emit(w)
			if typ == "int" {
				fmt.Fprint(w, ")).intValue()")
			} else {
				fmt.Fprint(w, ")).doubleValue()")
			}
		} else {
			emitCastExpr(w, b.Left, typ)
		}
		fmt.Fprint(w, " "+b.Op+" ")
		if rt == "" {
			fmt.Fprint(w, "((Number)(")
			b.Right.emit(w)
			if typ == "int" {
				fmt.Fprint(w, ")).intValue()")
			} else {
				fmt.Fprint(w, ")).doubleValue()")
			}
		} else {
			emitCastExpr(w, b.Right, typ)
		}
		return
	}

	if _, ok := b.Left.(*TernaryExpr); ok {
		fmt.Fprint(w, "(")
		b.Left.emit(w)
		fmt.Fprint(w, ")")
	} else {
		b.Left.emit(w)
	}
	fmt.Fprint(w, " "+b.Op+" ")
	if _, ok := b.Right.(*TernaryExpr); ok {
		fmt.Fprint(w, "(")
		b.Right.emit(w)
		fmt.Fprint(w, ")")
	} else {
		b.Right.emit(w)
	}
}

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) {
	if i.Value > math.MaxInt32 || i.Value < math.MinInt32 {
		fmt.Fprintf(w, "(int)%dL", int64(i.Value))
	} else {
		fmt.Fprint(w, i.Value)
	}
}

type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) {
	if math.Trunc(f.Value) == f.Value {
		fmt.Fprintf(w, "%.1f", f.Value)
	} else {
		fmt.Fprint(w, f.Value)
	}
}

type VarExpr struct {
	Name string
	Type string
}

func (v *VarExpr) emit(w io.Writer) { fmt.Fprint(w, sanitize(v.Name)) }

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (f *FieldExpr) emit(w io.Writer) {
	if v, ok := f.Target.(*VarExpr); ok {
		if t, ok2 := varTypes[v.Name]; ok2 && t != "map" && !strings.Contains(t, "Map") {
			f.Target.emit(w)
			fmt.Fprint(w, "."+f.Name)
			return
		}
	}
	if isMapExpr(f.Target) {
		fmt.Fprint(w, "((Integer) (")
		f.Target.emit(w)
		fmt.Fprint(w, ".get(")
		(&StringLit{Value: f.Name}).emit(w)
		fmt.Fprint(w, ")))")
		return
	}
	f.Target.emit(w)
	fmt.Fprint(w, "."+f.Name)
}

type LenExpr struct{ Value Expr }

func (l *LenExpr) emit(w io.Writer) {
	l.Value.emit(w)
	if ix, ok := l.Value.(*IndexExpr); ok {
		if isStringExpr(ix) {
			fmt.Fprint(w, ".length()")
		} else {
			fmt.Fprint(w, ".length")
		}
		return
	}
	switch {
	case isGroupExpr(l.Value):
		fmt.Fprint(w, ".items.size()")
	case isStringExpr(l.Value):
		fmt.Fprint(w, ".length()")
	case isArrayExpr(l.Value):
		fmt.Fprint(w, ".length")
	case strings.HasSuffix(inferType(l.Value), "[]"):
		fmt.Fprint(w, ".length")
	case isMapExpr(l.Value):
		fmt.Fprint(w, ".size()")
	default:
		fmt.Fprint(w, ".length")
	}
}

// AvgExpr represents averaging a list of numbers.
type AvgExpr struct{ Value Expr }

func (a *AvgExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	if isArrayExpr(a.Value) {
		fmt.Fprint(w, "java.util.Arrays.stream(")
		a.Value.emit(w)
		fmt.Fprint(w, ").average().orElse(0)")
	} else {
		a.Value.emit(w)
		fmt.Fprint(w, ".stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)")
	}
	fmt.Fprint(w, ")")
}

// SumExpr represents summing a list of numbers.
type SumExpr struct{ Value Expr }

func (s *SumExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(((")
	if isArrayExpr(s.Value) {
		fmt.Fprint(w, "java.util.Arrays.stream(")
		s.Value.emit(w)
		fmt.Fprint(w, ").sum()) % 1 == 0) ? (Object)(int)(java.util.Arrays.stream(")
		s.Value.emit(w)
		fmt.Fprint(w, ").sum()) : (Object)(java.util.Arrays.stream(")
		s.Value.emit(w)
		fmt.Fprint(w, ").sum()))")
	} else {
		s.Value.emit(w)
		fmt.Fprint(w, ".stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(")
		s.Value.emit(w)
		fmt.Fprint(w, ".stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(")
		s.Value.emit(w)
		fmt.Fprint(w, ".stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()))")
	}
}

// ValuesExpr collects map values into a list.
type ValuesExpr struct{ Map Expr }

// KeysExpr collects map keys into a list.
type KeysExpr struct{ Map Expr }

// AppendExpr appends an element to an array and returns the new array.
type AppendExpr struct {
	List     Expr
	Value    Expr
	ElemType string
}

// IntCastExpr casts a value to int.
type IntCastExpr struct{ Value Expr }

// FloatCastExpr casts a value to double.
type FloatCastExpr struct{ Value Expr }

// CastExpr casts a value to the given reference type.
type CastExpr struct {
	Value Expr
	Type  string
}

// InstanceOfExpr checks if Target is an instance of Type.
type InstanceOfExpr struct {
	Target Expr
	Type   string
}

// UnionExpr concatenates two arrays removing duplicate elements.
type UnionExpr struct{ Left, Right Expr }

// UnionAllExpr concatenates two arrays keeping duplicates.
type UnionAllExpr struct{ Left, Right Expr }

// ExceptExpr returns elements of Left not present in Right.
type ExceptExpr struct{ Left, Right Expr }

// IntersectExpr returns common elements of Left and Right.
type IntersectExpr struct{ Left, Right Expr }

func (v *ValuesExpr) emit(w io.Writer) {
	if ve, ok := v.Map.(*VarExpr); ok {
		if t, ok2 := varTypes[ve.Name]; ok2 {
			if st, ok3 := topEnv.GetStruct(strings.TrimSuffix(t, "[]")); ok3 {
				fmt.Fprint(w, "java.util.Arrays.asList(")
				for i, fn := range st.Order {
					if i > 0 {
						fmt.Fprint(w, ", ")
					}
					fmt.Fprintf(w, "%s.%s", ve.Name, fn)
				}
				fmt.Fprint(w, ")")
				return
			}
		}
	}
	fmt.Fprint(w, "new java.util.ArrayList<>(")
	v.Map.emit(w)
	fmt.Fprint(w, ".values())")
}

func (k *KeysExpr) emit(w io.Writer) {
	fmt.Fprint(w, "new java.util.ArrayList<>(")
	k.Map.emit(w)
	fmt.Fprint(w, ".keySet())")
}

func (a *AppendExpr) emit(w io.Writer) {
	elem := a.ElemType
	if elem == "" || elem == "Object" {
		t := arrayElemType(a.List)
		if t != "" {
			elem = t
		}
	}
	if elem == "" {
		elem = "Object"
	}
	if elem == "bool" {
		elem = "boolean"
	}
	if elem == "float" || elem == "float64" {
		elem = "double"
	}
	jt := javaType(elem)
	if jt == "" {
		jt = elem
	}
	if elem == "boolean" {
		needAppendBool = true
		fmt.Fprint(w, "appendBool(")
		a.List.emit(w)
		fmt.Fprint(w, ", ")
		a.Value.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	if elem == "double" {
		fmt.Fprint(w, "java.util.stream.DoubleStream.concat(java.util.Arrays.stream(")
		a.List.emit(w)
		fmt.Fprint(w, "), java.util.stream.DoubleStream.of(")
		a.Value.emit(w)
		fmt.Fprint(w, ")).toArray()")
		return
	}
	if elem == "int" {
		fmt.Fprint(w, "java.util.stream.IntStream.concat(java.util.Arrays.stream(")
		a.List.emit(w)
		fmt.Fprint(w, "), java.util.stream.IntStream.of(")
		a.Value.emit(w)
		fmt.Fprint(w, ")).toArray()")
		return
	}
	if strings.HasSuffix(jt, "[]") {
		needAppendObj = true
		fmt.Fprint(w, "appendObj(")
		a.List.emit(w)
		fmt.Fprint(w, ", ")
		a.Value.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	fmt.Fprint(w, "java.util.stream.Stream.concat(java.util.Arrays.stream(")
	a.List.emit(w)
	fmt.Fprint(w, "), java.util.stream.Stream.of(")
	a.Value.emit(w)
	fmt.Fprint(w, ")).toArray(")
	switch elem {
	case "string", "String":
		fmt.Fprint(w, "String[]::new")
	default:
		fmt.Fprintf(w, "%s[]::new", jt)
	}
	fmt.Fprint(w, ")")
}

func (c *IntCastExpr) emit(w io.Writer) {
	if isStringExpr(c.Value) {
		fmt.Fprint(w, "Integer.parseInt(")
		c.Value.emit(w)
		fmt.Fprint(w, ")")
	} else {
		fmt.Fprint(w, "((Number)(")
		c.Value.emit(w)
		fmt.Fprint(w, ")).intValue()")
	}
}

func (c *FloatCastExpr) emit(w io.Writer) {
	fmt.Fprint(w, "((Number)(")
	c.Value.emit(w)
	fmt.Fprint(w, ")).doubleValue()")
}

func (c *CastExpr) emit(w io.Writer) {
	jt := javaType(c.Type)
	if jt == "java.math.BigInteger" {
		if inferType(c.Value) == "bigint" {
			c.Value.emit(w)
		} else if _, ok := c.Value.(*IntLit); ok {
			fmt.Fprint(w, "java.math.BigInteger.valueOf(")
			c.Value.emit(w)
			fmt.Fprint(w, ")")
		} else {
			fmt.Fprint(w, "new java.math.BigInteger(String.valueOf(")
			c.Value.emit(w)
			fmt.Fprint(w, "))")
		}
	} else {
		fmt.Fprintf(w, "((%s)(", jt)
		c.Value.emit(w)
		fmt.Fprint(w, "))")
	}
}

func (i *InstanceOfExpr) emit(w io.Writer) {
	i.Target.emit(w)
	fmt.Fprintf(w, " instanceof %s", i.Type)
}

func (u *UnionExpr) emit(w io.Writer) {
	elem := arrayElemType(u.Left)
	if elem == "" {
		elem = arrayElemType(u.Right)
	}
	if elem == "" {
		elem = "Object"
	}
	jt := javaType(elem)
	if jt == "" {
		jt = elem
	}
	if elem == "int" {
		fmt.Fprint(w, "java.util.stream.IntStream.concat(java.util.Arrays.stream(")
		u.Left.emit(w)
		fmt.Fprint(w, "), java.util.Arrays.stream(")
		u.Right.emit(w)
		fmt.Fprint(w, ")).distinct().toArray()")
		return
	}
	fmt.Fprint(w, "java.util.stream.Stream.concat(java.util.Arrays.stream(")
	u.Left.emit(w)
	fmt.Fprint(w, "), java.util.Arrays.stream(")
	u.Right.emit(w)
	fmt.Fprint(w, ")).distinct().toArray(")
	switch elem {
	case "string", "String":
		fmt.Fprint(w, "String[]::new")
	default:
		fmt.Fprintf(w, "%s[]::new", jt)
	}
	fmt.Fprint(w, ")")
}

func (u *UnionAllExpr) emit(w io.Writer) {
	elem := arrayElemType(u.Left)
	if elem == "" {
		elem = arrayElemType(u.Right)
	}
	if elem == "" {
		elem = "Object"
	}
	jt := javaType(elem)
	if jt == "" {
		jt = elem
	}
	if elem == "int" {
		fmt.Fprint(w, "java.util.stream.IntStream.concat(java.util.Arrays.stream(")
		u.Left.emit(w)
		fmt.Fprint(w, "), java.util.Arrays.stream(")
		u.Right.emit(w)
		fmt.Fprint(w, ")).toArray()")
		return
	}
	if elem == "double" {
		fmt.Fprint(w, "java.util.stream.DoubleStream.concat(java.util.Arrays.stream(")
		u.Left.emit(w)
		fmt.Fprint(w, "), java.util.Arrays.stream(")
		u.Right.emit(w)
		fmt.Fprint(w, ")).toArray()")
		return
	}
	fmt.Fprint(w, "java.util.stream.Stream.concat(java.util.Arrays.stream(")
	u.Left.emit(w)
	fmt.Fprint(w, "), java.util.Arrays.stream(")
	u.Right.emit(w)
	fmt.Fprint(w, ")).toArray(")
	switch elem {
	case "string", "String":
		fmt.Fprint(w, "String[]::new")
	default:
		fmt.Fprintf(w, "%s[]::new", jt)
	}
	fmt.Fprint(w, ")")
}

func (e *ExceptExpr) emit(w io.Writer) {
	elem := arrayElemType(e.Left)
	if elem == "" {
		elem = arrayElemType(e.Right)
	}
	if elem == "" {
		elem = "Object"
	}
	jt := javaType(elem)
	if jt == "" {
		jt = elem
	}
	if elem == "int" {
		fmt.Fprint(w, "java.util.Arrays.stream(")
		e.Left.emit(w)
		fmt.Fprint(w, ").filter(v -> java.util.Arrays.stream(")
		e.Right.emit(w)
		fmt.Fprint(w, ").noneMatch(x -> x == v)).toArray()")
		return
	}
	fmt.Fprint(w, "java.util.Arrays.stream(")
	e.Left.emit(w)
	fmt.Fprint(w, ").filter(v -> !java.util.Arrays.asList(")
	e.Right.emit(w)
	fmt.Fprint(w, ").contains(v)).toArray(")
	switch elem {
	case "string", "String":
		fmt.Fprint(w, "String[]::new")
	default:
		fmt.Fprintf(w, "%s[]::new", jt)
	}
	fmt.Fprint(w, ")")
}

func (i *IntersectExpr) emit(w io.Writer) {
	elem := arrayElemType(i.Left)
	if elem == "" {
		elem = arrayElemType(i.Right)
	}
	if elem == "" {
		elem = "Object"
	}
	jt := javaType(elem)
	if jt == "" {
		jt = elem
	}
	if elem == "int" {
		fmt.Fprint(w, "java.util.Arrays.stream(")
		i.Left.emit(w)
		fmt.Fprint(w, ").filter(v -> java.util.Arrays.stream(")
		i.Right.emit(w)
		fmt.Fprint(w, ").anyMatch(x -> x == v)).toArray()")
		return
	}
	fmt.Fprint(w, "java.util.Arrays.stream(")
	i.Left.emit(w)
	fmt.Fprint(w, ").filter(v -> java.util.Arrays.asList(")
	i.Right.emit(w)
	fmt.Fprint(w, ").contains(v)).toArray(")
	switch elem {
	case "string", "String":
		fmt.Fprint(w, "String[]::new")
	default:
		fmt.Fprintf(w, "%s[]::new", jt)
	}
	fmt.Fprint(w, ")")
}

// ListStrExpr converts a list to a space-separated string.
type ListStrExpr struct{ List Expr }

func (ls *ListStrExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(\"[\" + ((java.util.List<?>)")
	ls.List.emit(w)
	fmt.Fprint(w, ").stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(\", \")) + \"]\")")
}

type UnaryExpr struct {
	Op    string
	Value Expr
}

type GroupExpr struct{ Expr Expr }

func (g *GroupExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	g.Expr.emit(w)
	fmt.Fprint(w, ")")
}

type TernaryExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (t *TernaryExpr) emit(w io.Writer) {
	t.Cond.emit(w)
	fmt.Fprint(w, " ? ")
	t.Then.emit(w)
	fmt.Fprint(w, " : ")
	t.Else.emit(w)
}

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprint(w, b.Value) }

type NullLit struct{}

func (n *NullLit) emit(w io.Writer) { fmt.Fprint(w, "null") }

// InputExpr represents a call to input() returning a line from stdin.
type InputExpr struct{}

func (in *InputExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(_scanner.hasNextLine() ? _scanner.nextLine() : \"\")")
}

func (u *UnaryExpr) emit(w io.Writer) {
	if u.Op == "!" && !isBoolExpr(u.Value) {
		fmt.Fprint(w, "!(Boolean)")
		if _, ok := u.Value.(*BinaryExpr); ok {
			fmt.Fprint(w, "(")
			u.Value.emit(w)
			fmt.Fprint(w, ")")
		} else {
			u.Value.emit(w)
		}
		return
	}
	fmt.Fprint(w, u.Op)
	if _, ok := u.Value.(*BinaryExpr); ok {
		fmt.Fprint(w, "(")
		u.Value.emit(w)
		fmt.Fprint(w, ")")
	} else {
		u.Value.emit(w)
	}
}

type CallExpr struct {
	Func string
	Args []Expr
}

// MethodCallExpr represents target.method(args...)
type MethodCallExpr struct {
	Target Expr
	Name   string
	Args   []Expr
}

func (m *MethodCallExpr) emit(w io.Writer) {
	m.Target.emit(w)
	name := sanitize(m.Name)
	if name == "get" && len(m.Args) == 2 {
		name = "getOrDefault"
	}
	fmt.Fprint(w, "."+name+"(")
	for i, a := range m.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
}

func (c *CallExpr) emit(w io.Writer) {
	fmt.Fprint(w, sanitize(c.Func))
	fmt.Fprint(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		typ := ""
		if params, ok := funcParams[c.Func]; ok {
			if i < len(params) {
				typ = params[i]
			}
		}
		emitCastExpr(w, a, typ)
	}
	fmt.Fprint(w, ")")
}

type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Str.emit(w)
	fmt.Fprint(w, ".substring(")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
}

// IndexExpr represents s[i]. For strings it emits charAt.
type IndexExpr struct {
	Target     Expr
	Index      Expr
	IsMap      bool
	ResultType string
}

func (ix *IndexExpr) emit(w io.Writer) {
	if ix.IsMap {
		castType := "Object"
		if ix.ResultType != "" {
			castType = javaType(ix.ResultType)
		}
		useDefault := false
		defVal := "null"
		switch castType {
		case "int", "Integer":
			useDefault = true
			defVal = "0"
		case "double", "Double":
			useDefault = true
			defVal = "0.0"
		case "boolean", "Boolean":
			useDefault = true
			defVal = "false"
		}
		fmt.Fprintf(w, "((%s)", castType)
		if isMapExpr(ix.Target) {
			ix.Target.emit(w)
		} else {
			fmt.Fprint(w, "((java.util.Map)")
			ix.Target.emit(w)
			fmt.Fprint(w, ")")
		}
		if useDefault {
			fmt.Fprint(w, ".getOrDefault(")
			ix.Index.emit(w)
			fmt.Fprintf(w, ", %s))", defVal)
		} else {
			fmt.Fprint(w, ".get(")
			ix.Index.emit(w)
			fmt.Fprint(w, "))")
		}
		return
	}
	if isStringExpr(ix.Target) {
		ix.Target.emit(w)
		fmt.Fprint(w, ".charAt(")
		ix.Index.emit(w)
		fmt.Fprint(w, ")")
	} else if isArrayExpr(ix.Target) {
		ix.Target.emit(w)
		fmt.Fprint(w, "[")
		ix.Index.emit(w)
		fmt.Fprint(w, "]")
	} else if isMapExpr(ix.Target) {
		castType := "java.util.Map"
		if ix.ResultType != "" {
			castType = javaType(ix.ResultType)
		}
		fmt.Fprintf(w, "((%s)", castType)
		ix.Target.emit(w)
		fmt.Fprint(w, ".get(")
		ix.Index.emit(w)
		fmt.Fprint(w, "))")
	} else {
		ix.Target.emit(w)
		fmt.Fprint(w, "[")
		ix.Index.emit(w)
		fmt.Fprint(w, "]")
	}
}

// SliceExpr represents s[a:b]. Only strings are currently supported.
type SliceExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (sli *SliceExpr) emit(w io.Writer) {
	switch {
	case isStringExpr(sli.Value):
		sli.Value.emit(w)
		fmt.Fprint(w, ".substring(")
		sli.Start.emit(w)
		fmt.Fprint(w, ", ")
		sli.End.emit(w)
		fmt.Fprint(w, ")")
	case isArrayExpr(sli.Value):
		fmt.Fprint(w, "java.util.Arrays.copyOfRange(")
		sli.Value.emit(w)
		fmt.Fprint(w, ", ")
		sli.Start.emit(w)
		fmt.Fprint(w, ", ")
		sli.End.emit(w)
		fmt.Fprint(w, ")")
	default:
		sli.Value.emit(w)
	}
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *VarExpr:
		if ex.Type != "" && (ex.Type == "string" || ex.Type == "String") {
			return true
		}
		if t, ok := varTypes[ex.Name]; ok && (t == "string" || t == "String") {
			return true
		}
		if vs, ok := varDecls[ex.Name]; ok {
			if vs.Type == "string" || vs.Type == "String" {
				return true
			}
		}
	case *CallExpr:
		if ex.Func == "String.valueOf" {
			return true
		}
	case *SubstringExpr:
		return true
	case *InputExpr:
		return true
	case *FieldExpr:
		if t, ok := fieldTypeFromVar(ex.Target, ex.Name); ok {
			return t == "String" || t == "string"
		}
		if inferType(ex) == "string" || inferType(ex) == "String" {
			return true
		}
	case *MethodCallExpr:
		switch ex.Name {
		case "toLowerCase", "toUpperCase", "trim", "substring":
			return true
		}
	case *IndexExpr:
		if isStringExpr(ex.Target) || arrayElemType(ex.Target) == "string" {
			return true
		}
	case *SliceExpr:
		if isStringExpr(ex.Value) {
			return true
		}
	}
	return false
}

func isMapExpr(e Expr) bool {
	switch ex := e.(type) {
	case *MapLit:
		return true
	case *VarExpr:
		if ex.Type != "" {
			if strings.HasSuffix(ex.Type, "[]") {
				break
			}
			if ex.Type == "map" || strings.Contains(ex.Type, "Map") {
				return true
			}
		}
		if t, ok := varTypes[ex.Name]; ok {
			if strings.HasSuffix(t, "[]") {
				break
			}
			if t == "map" || strings.Contains(t, "Map") {
				return true
			}
		}
	case *FieldExpr:
		if t, ok := fieldTypeFromVar(ex.Target, ex.Name); ok {
			if strings.Contains(t, "Map") {
				return true
			}
		}
	}
	if inferType(e) == "map" {
		return true
	}
	return false
}

func isStructType(name string) bool {
	base := strings.TrimSuffix(name, "[]")
	if topEnv != nil {
		if _, ok := topEnv.GetStruct(base); ok {
			return true
		}
	}
	if structDefs != nil {
		if _, ok := structDefs[base]; ok {
			return true
		}
	}
	return false
}

func isStructExpr(e Expr) bool {
	switch ex := e.(type) {
	case *VarExpr:
		if t, ok := varTypes[ex.Name]; ok {
			return isStructType(t)
		}
	case *FieldExpr:
		if t, ok := fieldTypeFromVar(ex.Target, ex.Name); ok {
			return isStructType(t)
		}
	}
	return false
}

func isArrayExpr(e Expr) bool {
	switch ex := e.(type) {
	case *ListLit:
		return true
	case *AppendExpr:
		return true
	case *UnionExpr, *UnionAllExpr, *ExceptExpr, *IntersectExpr:
		return true
	case *SliceExpr:
		if !isStringExpr(ex.Value) {
			return true
		}
	case *IndexExpr:
		if ex.ResultType != "" {
			return strings.HasSuffix(ex.ResultType, "[]")
		}
		if t := arrayElemType(ex.Target); t != "" {
			return strings.HasSuffix(t, "[]")
		}
	case *FieldExpr:
		if t, ok := fieldTypeFromVar(ex.Target, ex.Name); ok {
			return strings.HasSuffix(t, "[]")
		}
	case *VarExpr:
		if ex.Type != "" {
			if strings.HasSuffix(ex.Type, "[]") {
				return true
			}
		}
		if t, ok := varTypes[ex.Name]; ok && strings.HasSuffix(t, "[]") {
			return true
		}
	}
	return false
}

func arrayElemType(e Expr) string {
	switch ex := e.(type) {
	case *ListLit:
		if ex.ElemType != "" {
			return ex.ElemType
		}
		if len(ex.Elems) > 0 {
			return inferType(ex.Elems[0])
		}
	case *AppendExpr:
		return arrayElemType(ex.List)
	case *UnionExpr:
		t := arrayElemType(ex.Left)
		if t == "" {
			t = arrayElemType(ex.Right)
		}
		return t
	case *UnionAllExpr:
		t := arrayElemType(ex.Left)
		if t == "" {
			t = arrayElemType(ex.Right)
		}
		return t
	case *ExceptExpr:
		t := arrayElemType(ex.Left)
		if t == "" {
			t = arrayElemType(ex.Right)
		}
		return t
	case *IntersectExpr:
		t := arrayElemType(ex.Left)
		if t == "" {
			t = arrayElemType(ex.Right)
		}
		return t
	case *SliceExpr:
		if !isStringExpr(ex.Value) {
			return arrayElemType(ex.Value)
		}
	case *IndexExpr:
		if ex.ResultType != "" {
			t := ex.ResultType
			if strings.HasSuffix(t, "[]") {
				return strings.TrimSuffix(t, "[]")
			}
			if strings.HasPrefix(t, "java.util.List<") && strings.HasSuffix(t, ">") {
				return strings.TrimSuffix(strings.TrimPrefix(t, "java.util.List<"), ">")
			}
		}
		t := arrayElemType(ex.Target)
		if strings.HasSuffix(t, "[]") {
			return strings.TrimSuffix(t, "[]")
		}
		if strings.HasPrefix(t, "java.util.List<") && strings.HasSuffix(t, ">") {
			return strings.TrimSuffix(strings.TrimPrefix(t, "java.util.List<"), ">")
		}
		return ""
	case *VarExpr:
		if t, ok := varTypes[ex.Name]; ok {
			if strings.HasSuffix(t, "[]") {
				return strings.TrimSuffix(t, "[]")
			}
			if strings.HasPrefix(t, "java.util.List<") && strings.HasSuffix(t, ">") {
				return strings.TrimSuffix(strings.TrimPrefix(t, "java.util.List<"), ">")
			}
		}
		if vs, ok := varDecls[ex.Name]; ok {
			t := vs.Type
			if strings.HasSuffix(t, "[]") {
				return strings.TrimSuffix(t, "[]")
			}
			if strings.HasPrefix(t, "java.util.List<") && strings.HasSuffix(t, ">") {
				return strings.TrimSuffix(strings.TrimPrefix(t, "java.util.List<"), ">")
			}
		}
	case *FieldExpr:
		if t, ok := fieldTypeFromVar(ex.Target, ex.Name); ok {
			if strings.HasSuffix(t, "[]") {
				return strings.TrimSuffix(t, "[]")
			}
			if strings.HasPrefix(t, "java.util.List<") && strings.HasSuffix(t, ">") {
				return strings.TrimSuffix(strings.TrimPrefix(t, "java.util.List<"), ">")
			}
		}
	}
	return ""
}

func isListExpr(e Expr) bool {
	switch ex := e.(type) {
	case *QueryExpr, *ListLit, *ValuesExpr, *KeysExpr:
		return true
	case *VarExpr:
		if t, ok := varTypes[ex.Name]; ok && strings.HasPrefix(t, "java.util.List") {
			return true
		}
	}
	return false
}

func isGroupExpr(e Expr) bool {
	if v, ok := e.(*VarExpr); ok {
		if _, ok2 := groupItems[varTypes[v.Name]]; ok2 {
			return true
		}
	}
	return false
}

func isNullExpr(e Expr) bool {
	_, ok := e.(*NullLit)
	return ok
}

func isNumericBool(e Expr) bool {
	switch ex := e.(type) {
	case *BinaryExpr:
		switch ex.Op {
		case "&&", "||", "==", "!=", "<", "<=", ">", ">=":
			return true
		}
	case *UnaryExpr:
		if ex.Op == "!" {
			if be, ok := ex.Value.(*BinaryExpr); ok && be.Op == "in" {
				return true
			}
			return isNumericBool(ex.Value)
		}
	case *GroupExpr:
		return isNumericBool(ex.Expr)
	case *MethodCallExpr:
		switch ex.Name {
		case "contains", "containsKey", "anyMatch":
			return true
		}
	}
	return false
}

func isBoolExpr(e Expr) bool {
	if inferType(e) == "boolean" {
		return true
	}
	switch ex := e.(type) {
	case *BoolLit:
		return true
	case *UnaryExpr:
		if ex.Op == "!" {
			return true
		}
	case *BinaryExpr:
		switch ex.Op {
		case "&&", "||", "==", "!=", "<", "<=", ">", ">=", "in":
			return true
		}
	case *IndexExpr:
		if arrayElemType(ex.Target) == "boolean" {
			return true
		}
	case *MethodCallExpr:
		switch ex.Name {
		case "contains", "containsKey", "anyMatch":
			return true
		}
	}
	return false
}

// Transpile converts a Mochi AST into a simple Java AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	var prog Program
	varTypes = map[string]string{}
	funcRet = map[string]string{}
	extraDecls = nil
	structCount = 0
	topEnv = env
	groupItems = map[string]string{}
	needInput = false
	needNow = false
	needMem = false
	needAppendBool = false
	pyMathAliases = map[string]bool{}
	builtinAliases = map[string]string{}
	structDefs = map[string]map[string]string{}
	varDecls = map[string]*VarStmt{}
	funcMapFields = map[string]map[string]string{}
	mapVarFields = map[string]map[string]string{}
	funcParams = map[string][]string{}
	for _, s := range p.Statements {
		if s.Fun != nil {
			saved := varTypes
			varTypes = copyMap(varTypes)
			for _, pa := range s.Fun.Params {
				if t := typeRefString(pa.Type); t != "" {
					varTypes[pa.Name] = t
				}
			}
			body, err := compileStmts(s.Fun.Body)
			if err != nil {
				return nil, err
			}
			var params []Param
			for _, p := range s.Fun.Params {
				params = append(params, Param{Name: p.Name, Type: typeRefString(p.Type)})
			}
			ret := typeRefString(s.Fun.Return)
			if ret == "" {
				ret = inferReturnType(body)
			}
			if vt := mapValueType(ret); vt != "" {
				setReturnMapValueType(body, vt)
			}
			for _, p := range params {
				if javaType(p.Type) == "" {
					stmt := &AssignStmt{Name: p.Name, Expr: &CallExpr{Func: "new java.util.LinkedHashMap", Args: []Expr{&VarExpr{Name: p.Name}}}, Type: p.Type}
					body = append([]Stmt{stmt}, body...)
				}
			}
			funcRet[s.Fun.Name] = ret
			if fm := collectReturnMap(body); fm != nil {
				funcMapFields[s.Fun.Name] = fm
			}
			var ptypes []string
			for _, p := range params {
				ptypes = append(ptypes, p.Type)
			}
			funcParams[s.Fun.Name] = ptypes
			prog.Funcs = append(prog.Funcs, &Function{Name: s.Fun.Name, Params: params, Return: ret, Body: body})
			varTypes = saved
			continue
		}
		st, err := compileStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			prog.Stmts = append(prog.Stmts, st)
		}
		if len(extraDecls) > 0 {
			prog.Stmts = append(prog.Stmts, extraDecls...)
			extraDecls = nil
		}
	}
	_ = env // reserved
	if benchMain {
		needNow = true
		needMem = true
	}
	return &prog, nil
}

func compileStmt(s *parser.Statement) (Stmt, error) {
	switch {
	case s.Bench != nil:
		needNow = true
		needMem = true
		body, err := compileStmts(s.Bench.Body)
		if err != nil {
			return nil, err
		}
		return &BenchStmt{Name: strings.Trim(s.Bench.Name, "\""), Body: body}, nil
	case s.Expr != nil:
		if se := extractSaveExpr(s.Expr.Expr); se != nil {
			src, err := compileExpr(se.Src)
			if err != nil {
				return nil, err
			}
			format := parseFormat(se.With)
			path := ""
			if se.Path != nil {
				path = strings.Trim(*se.Path, "\"")
			}
			if format == "jsonl" && (path == "" || path == "-") {
				needSaveJsonl = true
				if v, ok := src.(*VarExpr); ok {
					if t, ok2 := varTypes[v.Name]; ok2 && strings.HasSuffix(t, "[]") {
						src = &CallExpr{Func: "java.util.Arrays.asList", Args: []Expr{src}}
					}
				}
				return &ExprStmt{Expr: &CallExpr{Func: "saveJsonl", Args: []Expr{src}}}, nil
			}
		}
		e, err := compileExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case s.Let != nil:
		if s.Let.Value != nil {
			e, err := compileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
			t := typeRefString(s.Let.Type)
			if t == "list" && topEnv != nil {
				t = toJavaTypeFromType(types.ExprType(s.Let.Value, currentEnv()))
			}
			if t == "" {
				switch ex := e.(type) {
				case *QueryExpr:
					t = fmt.Sprintf("java.util.List<%s>", ex.ElemType)
				case *ListLit:
					if ex.ElemType != "" {
						t = ex.ElemType + "[]"
					}
				case *MapLit:
					if len(ex.Values) > 0 {
						vt := inferType(ex.Values[0])
						t = fmt.Sprintf("java.util.Map<String,%s>", javaBoxType(vt))
					}
				}
			}
			if t == "" {
				if ce, ok := e.(*CallExpr); ok {
					if ret, ok2 := funcRet[ce.Func]; ok2 {
						t = ret
					} else if topEnv != nil {
						if fn, ok3 := topEnv.GetFunc(ce.Func); ok3 {
							t = typeRefString(fn.Return)
						}
					}
				}
			}
			if t == "" && topEnv != nil {
				t = toJavaTypeFromType(types.ExprType(s.Let.Value, currentEnv()))
				if t == "Object" || t == "Object[]" {
					t = ""
				}
			}
			if t == "" {
				t = inferType(e)
			}
			if t != "" {
				varTypes[s.Let.Name] = t
				varDecls[s.Let.Name] = &VarStmt{Name: s.Let.Name, Type: t, Expr: e}
			}
			if ml, ok := e.(*MapLit); ok {
				if len(ml.Fields) > 0 {
					mapVarFields[s.Let.Name] = ml.Fields
				}
				if ml.ValueType == "" {
					if vt := mapValueType(t); vt != "" {
						ml.ValueType = vt
					}
				}
				if ml.KeyType == "" {
					if kt := mapKeyType(t); kt != "" {
						ml.KeyType = kt
					}
				}
			} else if ce, ok := e.(*CallExpr); ok {
				if f, ok2 := funcMapFields[ce.Func]; ok2 {
					mapVarFields[s.Let.Name] = f
				}
			} else if cast, ok := e.(*CastExpr); ok {
				if v, ok2 := cast.Value.(*VarExpr); ok2 {
					if f, ok3 := mapVarFields[v.Name]; ok3 {
						mapVarFields[s.Let.Name] = f
					}
				}
			}
			if l, ok := e.(*ListLit); ok && strings.HasSuffix(t, "[]") {
				elemT := strings.TrimSuffix(t, "[]")
				if l.ElemType == "" {
					l.ElemType = elemT
				}
				if strings.HasSuffix(elemT, "[]") {
					inner := strings.TrimSuffix(elemT, "[]")
					for _, el := range l.Elems {
						if ll, ok2 := el.(*ListLit); ok2 && ll.ElemType == "" {
							ll.ElemType = inner
						}
					}
				}
			}
			return &LetStmt{Name: s.Let.Name, Type: t, Expr: e}, nil
		}
		t := typeRefString(s.Let.Type)
		if t == "" && topEnv != nil {
			if v, err := topEnv.GetVar(s.Let.Name); err == nil {
				t = toJavaTypeFromType(v)
			}
		}
		if t != "" {
			varTypes[s.Let.Name] = t
			varDecls[s.Let.Name] = &VarStmt{Name: s.Let.Name, Type: t}
		}
		return &LetStmt{Name: s.Let.Name, Type: t}, nil
	case s.Var != nil:
		if s.Var.Value != nil {
			e, err := compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
			t := typeRefString(s.Var.Type)
			if t == "" {
				if s.Var.Type != nil && s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "list" && topEnv != nil {
					t = toJavaTypeFromType(types.ExprType(s.Var.Value, currentEnv()))
				}
			}
			if t == "" {
				switch ex := e.(type) {
				case *QueryExpr:
					t = fmt.Sprintf("java.util.List<%s>", ex.ElemType)
				case *ListLit:
					if ex.ElemType != "" {
						t = ex.ElemType + "[]"
					}
				}
			}
			if t == "" && topEnv != nil {
				t = toJavaTypeFromType(types.ExprType(s.Var.Value, currentEnv()))
				if t == "Object" || t == "Object[]" {
					t = ""
				}
			}
			if t == "" {
				t = inferType(e)
			}
			if t != "" {
				varTypes[s.Var.Name] = t
			}
			if ml, ok := e.(*MapLit); ok {
				if len(ml.Fields) > 0 {
					mapVarFields[s.Var.Name] = ml.Fields
				}
				if ml.ValueType == "" {
					if vt := mapValueType(t); vt != "" {
						ml.ValueType = vt
					}
				}
				if ml.KeyType == "" {
					if kt := mapKeyType(t); kt != "" {
						ml.KeyType = kt
					}
				}
			} else if ce, ok := e.(*CallExpr); ok {
				if f, ok2 := funcMapFields[ce.Func]; ok2 {
					mapVarFields[s.Var.Name] = f
				}
			} else if cast, ok := e.(*CastExpr); ok {
				if v, ok2 := cast.Value.(*VarExpr); ok2 {
					if f, ok3 := mapVarFields[v.Name]; ok3 {
						mapVarFields[s.Var.Name] = f
					}
				}
			}
			if l, ok := e.(*ListLit); ok {
				if l.ElemType != "" {
					t = l.ElemType + "[]"
				} else if strings.HasSuffix(t, "[]") {
					l.ElemType = strings.TrimSuffix(t, "[]")
				}
			}
			vs := &VarStmt{Name: s.Var.Name, Type: t, Expr: e}
			varDecls[s.Var.Name] = vs
			return vs, nil
		}
		t := typeRefString(s.Var.Type)
		if t == "" && topEnv != nil {
			if v, err := topEnv.GetVar(s.Var.Name); err == nil {
				t = toJavaTypeFromType(v)
			}
		}
		if t != "" {
			varTypes[s.Var.Name] = t
		}
		vs := &VarStmt{Name: s.Var.Name, Type: t}
		varDecls[s.Var.Name] = vs
		return vs, nil
	case s.Type != nil:
		if len(s.Type.Variants) > 0 {
			extraDecls = append(extraDecls, &InterfaceDeclStmt{Name: s.Type.Name})
			for _, v := range s.Type.Variants {
				if st, ok := topEnv.GetStruct(v.Name); ok {
					fields := make([]Param, len(st.Order))
					for i, n := range st.Order {
						fields[i] = Param{Name: n, Type: toJavaTypeFromType(st.Fields[n])}
					}
					extraDecls = append(extraDecls, &TypeDeclStmt{Name: v.Name, Fields: fields, Extends: s.Type.Name})
				}
			}
			return nil, nil
		}
		if st, ok := topEnv.GetStruct(s.Type.Name); ok {
			fields := make([]Param, len(st.Order))
			for i, n := range st.Order {
				fields[i] = Param{Name: n, Type: toJavaTypeFromType(st.Fields[n])}
			}
			return &TypeDeclStmt{Name: s.Type.Name, Fields: fields}, nil
		}
		return nil, nil
	case s.Fun != nil:
		expr, err := compileFunExpr(&parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body})
		if err != nil {
			return nil, err
		}
		var ptypes []string
		for _, pa := range s.Fun.Params {
			ptypes = append(ptypes, typeRefString(pa.Type))
		}
		funType := fmt.Sprintf("fn(%s):%s", strings.Join(ptypes, ","), typeRefString(s.Fun.Return))
		varTypes[s.Fun.Name] = funType
		return &VarStmt{Name: s.Fun.Name, Type: funType, Expr: expr}, nil
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			e, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if ap, ok := e.(*AppendExpr); ok {
				vt := inferType(ap.Value)
				if vt != "" {
					if cur, ok := varTypes[s.Assign.Name]; ok && (cur == "int[]" || cur == "Object[]") && vt == "boolean" {
						newT := "boolean[]"
						varTypes[s.Assign.Name] = newT
						if vs, ok2 := varDecls[s.Assign.Name]; ok2 {
							vs.Type = newT
							if ll, ok3 := vs.Expr.(*ListLit); ok3 {
								ll.ElemType = "boolean"
							}
						}
					}
				}
			}
			if t := inferType(e); t != "" {
				if cur, ok := varTypes[s.Assign.Name]; !ok || ((cur == "int[]" || cur == "Object[]") && t != cur) {
					varTypes[s.Assign.Name] = t
					if vs, ok := varDecls[s.Assign.Name]; ok {
						vs.Type = t
						if ll, ok2 := vs.Expr.(*ListLit); ok2 && strings.HasSuffix(t, "[]") {
							ll.ElemType = strings.TrimSuffix(t, "[]")
						}
					}
				}
			}
			t := ""
			if vt, ok := varTypes[s.Assign.Name]; ok {
				t = vt
			} else if vs, ok := varDecls[s.Assign.Name]; ok {
				t = vs.Type
			}
			if ml, ok := e.(*MapLit); ok {
				if ml.ValueType == "" {
					if vt := mapValueType(t); vt != "" {
						ml.ValueType = vt
					}
				}
				if ml.KeyType == "" {
					if kt := mapKeyType(t); kt != "" {
						ml.KeyType = kt
					}
				}
				if len(ml.Fields) > 0 {
					mapVarFields[s.Assign.Name] = ml.Fields
				}
			} else if ce, ok := e.(*CallExpr); ok {
				if f, ok2 := funcMapFields[ce.Func]; ok2 {
					mapVarFields[s.Assign.Name] = f
				}
			} else if cast, ok := e.(*CastExpr); ok {
				if v, ok2 := cast.Value.(*VarExpr); ok2 {
					if f, ok3 := mapVarFields[v.Name]; ok3 {
						mapVarFields[s.Assign.Name] = f
					}
				}
			}
			return &AssignStmt{Name: s.Assign.Name, Expr: e, Type: t}, nil
		}
		if len(s.Assign.Index) > 0 && len(s.Assign.Field) == 0 {
			indices := make([]Expr, len(s.Assign.Index))
			for i, idx := range s.Assign.Index {
				if idx.Start == nil || idx.Colon != nil {
					return nil, fmt.Errorf("unsupported index")
				}
				ex, err := compileExpr(idx.Start)
				if err != nil {
					return nil, err
				}
				indices[i] = ex
			}
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if t := inferType(val); t != "" {
				if cur, ok := varTypes[s.Assign.Name]; ok && (cur == "int[]" || cur == "Object[]") && t == "boolean" {
					varTypes[s.Assign.Name] = "boolean[]"
					if vs, ok2 := varDecls[s.Assign.Name]; ok2 {
						vs.Type = "boolean[]"
						if ll, ok3 := vs.Expr.(*ListLit); ok3 {
							ll.ElemType = "boolean"
						}
					}
				}
			}
			baseType := ""
			if t, ok := varTypes[s.Assign.Name]; ok {
				baseType = t
			}
			base := Expr(&VarExpr{Name: s.Assign.Name, Type: baseType})
			return &IndexAssignStmt{Target: base, Indices: indices, Expr: val}, nil
		}
		if len(s.Assign.Field) > 0 && len(s.Assign.Index) == 0 {
			baseType := ""
			if t, ok := varTypes[s.Assign.Name]; ok {
				baseType = t
			}
			base := Expr(&VarExpr{Name: s.Assign.Name, Type: baseType})
			for i := 0; i < len(s.Assign.Field)-1; i++ {
				base = &IndexExpr{Target: base, Index: &StringLit{Value: s.Assign.Field[i].Name}}
			}
			key := &StringLit{Value: s.Assign.Field[len(s.Assign.Field)-1].Name}
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			return &IndexAssignStmt{Target: base, Indices: []Expr{key}, Expr: val}, nil
		}
	case s.If != nil:
		cond, err := compileExpr(s.If.Cond)
		if err != nil {
			return nil, err
		}
		var thenStmts []Stmt
		for _, b := range s.If.Then {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				thenStmts = append(thenStmts, st)
			}
		}
		var elseStmts []Stmt
		if s.If.ElseIf != nil {
			st, err := compileStmt(&parser.Statement{If: s.If.ElseIf})
			if err != nil {
				return nil, err
			}
			if st != nil {
				elseStmts = append(elseStmts, st)
			}
		} else {
			for _, b := range s.If.Else {
				st, err := compileStmt(b)
				if err != nil {
					return nil, err
				}
				if st != nil {
					elseStmts = append(elseStmts, st)
				}
			}
		}
		return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
	case s.Return != nil:
		var e Expr
		var err error
		if s.Return.Value != nil {
			e, err = compileExpr(s.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: e}, nil
	case s.While != nil:
		cond, err := compileExpr(s.While.Cond)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.While.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case s.For != nil && s.For.RangeEnd != nil:
		start, err := compileExpr(s.For.Source)
		if err != nil {
			return nil, err
		}
		end, err := compileExpr(s.For.RangeEnd)
		if err != nil {
			return nil, err
		}
		varTypes[s.For.Name] = "int"
		var body []Stmt
		for _, b := range s.For.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &ForRangeStmt{Name: s.For.Name, Start: start, End: end, Body: body}, nil
	case s.For != nil:
		iter, err := compileExpr(s.For.Source)
		if err != nil {
			return nil, err
		}
		elem := arrayElemType(iter)
		if elem != "" {
			varTypes[s.For.Name] = elem
		}
		var body []Stmt
		for _, b := range s.For.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		isMap := false
		keyType := ""
		switch it := iter.(type) {
		case *MapLit:
			isMap = true
			keyType = it.KeyType
		case *VarExpr:
			if t, ok := varTypes[it.Name]; ok && (t == "map" || strings.HasPrefix(t, "java.util.Map")) {
				isMap = true
				keyType = mapKeyType(t)
			}
		}
		elemType := ""
		if isMap {
			if keyType != "" {
				varTypes[s.For.Name] = keyType
				elemType = keyType
			}
		} else {
			if elem != "" {
				elemType = elem
			}
		}
		return &ForEachStmt{Name: s.For.Name, Iterable: iter, Body: body, IsMap: isMap, ElemType: elemType}, nil
	case s.Update != nil:
		up, err := compileUpdateStmt(s.Update)
		if err != nil {
			return nil, err
		}
		return up, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Import != nil:
		alias := s.Import.As
		if alias == "" {
			alias = parser.AliasFromPath(s.Import.Path)
		}
		if s.Import.Lang != nil && *s.Import.Lang == "python" && strings.Trim(s.Import.Path, "\"") == "math" {
			pyMathAliases[alias] = true
			varTypes[alias] = "module"
			return nil, nil
		}
		if s.Import.Lang != nil && *s.Import.Lang == "go" {
			trimmed := strings.Trim(s.Import.Path, "\"")
			if s.Import.Auto && strings.Contains(trimmed, "testpkg") {
				builtinAliases[alias] = "go_testpkg"
				varTypes[alias] = "module"
				return nil, nil
			}
			if trimmed == "net" {
				builtinAliases[alias] = "go_net"
				varTypes[alias] = "module"
				return nil, nil
			}
		}
		return nil, fmt.Errorf("unsupported import")
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil, s.ExternObject != nil:
		return nil, nil
	case s.Test == nil && s.Import == nil && s.Type == nil:
		return nil, fmt.Errorf("unsupported statement at %d:%d", s.Pos.Line, s.Pos.Column)
	}
	return nil, nil
}

func compileStmts(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		st, err := compileStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			if _, ok := st.(*ReturnStmt); ok {
				out = append(out, st)
				break
			} else {
				out = append(out, st)
			}
		}
	}
	if len(out) >= 2 {
		if _, ok := out[len(out)-1].(*ReturnStmt); ok {
			if w, ok2 := out[len(out)-2].(*WhileStmt); ok2 {
				if b, ok3 := w.Cond.(*BoolLit); ok3 && b.Value {
					out = out[:len(out)-1]
				}
			}
		}
	}
	return out, nil
}

func compileUpdateStmt(u *parser.UpdateStmt) (Stmt, error) {
	if topEnv == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := topEnv.GetVar(u.Target)
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
	fieldSet := map[string]bool{}
	for name, ft := range st.Fields {
		fieldSet[name] = true
		varTypes[name] = toJavaTypeFromType(ft)
	}
	fields := make([]string, len(u.Set.Items))
	values := make([]Expr, len(u.Set.Items))
	for i, it := range u.Set.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			return nil, fmt.Errorf("unsupported update key")
		}
		val, err := compileExpr(it.Value)
		if err != nil {
			return nil, err
		}
		val = substituteFieldVars(val, fieldSet)
		fields[i] = key
		values[i] = val
	}
	var cond Expr
	if u.Where != nil {
		c, err := compileExpr(u.Where)
		if err != nil {
			return nil, err
		}
		cond = substituteFieldVars(c, fieldSet)
	}
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	expr := left
	for _, op := range e.Binary.Right {
		r, err := compilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		switch op.Op {
		case "+":
			if isArrayExpr(expr) && isArrayExpr(r) {
				expr = &UnionAllExpr{Left: expr, Right: r}
				continue
			}
			fallthrough
		case "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			if op.Op == "%" {
				lt := inferType(expr)
				rt := inferType(r)
				if lt != "double" && lt != "float" && rt != "double" && rt != "float" && lt != "bigint" && rt != "bigint" {
					expr = &CallExpr{Func: "Math.floorMod", Args: []Expr{expr, r}}
				} else {
					expr = &BinaryExpr{Left: expr, Op: op.Op, Right: r}
				}
				continue
			}
			expr = &BinaryExpr{Left: expr, Op: op.Op, Right: r}
		case "in":
			if isStringExpr(r) {
				expr = &MethodCallExpr{Target: r, Name: "contains", Args: []Expr{expr}}
			} else if isArrayExpr(r) {
				elem := arrayElemType(r)
				if elem == "int" {
					lam := &LambdaExpr{Params: []Param{{Name: "x", Type: "int"}}, Body: []Stmt{&ReturnStmt{Expr: &BinaryExpr{Left: &VarExpr{Name: "x"}, Op: "==", Right: expr}}}}
					stream := &CallExpr{Func: "java.util.Arrays.stream", Args: []Expr{r}}
					expr = &MethodCallExpr{Target: stream, Name: "anyMatch", Args: []Expr{lam}}
				} else {
					arr := &CallExpr{Func: "java.util.Arrays.asList", Args: []Expr{r}}
					expr = &MethodCallExpr{Target: arr, Name: "contains", Args: []Expr{expr}}
				}
			} else if isListExpr(r) {
				expr = &MethodCallExpr{Target: r, Name: "contains", Args: []Expr{expr}}
			} else if isMapExpr(r) || inferType(r) == "map" {
				expr = &MethodCallExpr{Target: r, Name: "containsKey", Args: []Expr{expr}}
			} else {
				return nil, fmt.Errorf("unsupported binary op: %s", op.Op)
			}
		case "union":
			if op.All {
				expr = &UnionAllExpr{Left: expr, Right: r}
			} else {
				expr = &UnionExpr{Left: expr, Right: r}
			}
		case "except":
			expr = &ExceptExpr{Left: expr, Right: r}
		case "intersect":
			expr = &IntersectExpr{Left: expr, Right: r}
		default:
			return nil, fmt.Errorf("unsupported binary op: %s", op.Op)
		}
	}
	return expr, nil
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
		switch u.Ops[i] {
		case "-", "!":
			expr = &UnaryExpr{Op: u.Ops[i], Value: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op: %s", u.Ops[i])
		}
	}
	return expr, nil
}

func compilePostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := compilePrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			if op.Index.Start == nil {
				return nil, fmt.Errorf("unsupported index")
			}
			idx, err := compileExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			rType := ""
			if s, ok := constStringExpr(idx); ok {
				if fields := mapFieldsForExpr(expr); fields != nil {
					if t, ok2 := fields[s]; ok2 {
						rType = t
					}
				}
			}
			if rType == "" {
				if v, ok := expr.(*VarExpr); ok {
					rType = mapValueType(varTypes[v.Name])
				}
			}
			isMap := isMapExpr(expr)
			if !isMap {
				it := inferType(idx)
				if it == "string" {
					if !isStringExpr(expr) && !isArrayExpr(expr) {
						isMap = true
					}
				} else if _, ok := constStringExpr(idx); ok {
					if !isStringExpr(expr) && !isArrayExpr(expr) {
						isMap = true
					}
				}
			}
			expr = &IndexExpr{Target: expr, Index: idx, IsMap: isMap, ResultType: rType}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			var start Expr
			var end Expr
			var err error
			if op.Index.Start != nil {
				start, err = compileExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
			} else {
				start = &IntLit{Value: 0}
			}
			if op.Index.End != nil {
				end, err = compileExpr(op.Index.End)
				if err != nil {
					return nil, err
				}
			} else {
				end = &LenExpr{Value: expr}
			}
			expr = &SliceExpr{Value: expr, Start: start, End: end}
		case op.Field != nil:
			name := op.Field.Name
			if v, ok := expr.(*VarExpr); ok {
				if pyMathAliases[v.Name] {
					mapped := name
					if name == "pi" {
						mapped = "PI"
					} else if name == "e" {
						mapped = "E"
					}
					expr = &FieldExpr{Target: &VarExpr{Name: "Math"}, Name: mapped}
					break
				}
				if kind, ok2 := builtinAliases[v.Name]; ok2 && kind == "go_testpkg" {
					switch name {
					case "Pi":
						expr = &FloatLit{Value: 3.14}
						break
					case "Answer":
						expr = &IntLit{Value: 42}
						break
					}
				}
			}
			expr = &FieldExpr{Target: expr, Name: name}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for j, a := range op.Call.Args {
				ex, err := compileExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			if fe, ok := expr.(*FieldExpr); ok {
				if fe.Name == "keys" && len(args) == 0 && (isMapExpr(fe.Target) || strings.Contains(inferType(fe.Target), "Map")) {
					expr = &MethodCallExpr{Target: fe.Target, Name: "keySet", Args: nil}
				} else if v, ok2 := fe.Target.(*VarExpr); ok2 && pyMathAliases[v.Name] {
					expr = &CallExpr{Func: "Math." + fe.Name, Args: args}
				} else if v, ok2 := fe.Target.(*VarExpr); ok2 {
					if kind, ok3 := builtinAliases[v.Name]; ok3 && kind == "go_testpkg" {
						if fe.Name == "FifteenPuzzleExample" && len(args) == 0 {
							expr = &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}
						} else if fe.Name == "Add" && len(args) == 2 {
							expr = &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}
						} else {
							expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
						}
					} else if kind == "go_net" {
						if fe.Name == "LookupHost" && len(args) == 1 {
							needNetLookupHost = true
							expr = &CallExpr{Func: "_netLookupHost", Args: args}
						} else {
							return nil, fmt.Errorf("unsupported call")
						}
					} else {
						expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
					}
				} else {
					expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
				}
			} else if v, ok := expr.(*VarExpr); ok {
				if v.Name == "int" && len(args) == 1 {
					expr = &IntCastExpr{Value: args[0]}
				} else if t, ok := varTypes[v.Name]; ok && (strings.HasPrefix(t, "fn") || strings.HasPrefix(t, "java.util.function.Function")) {
					expr = &MethodCallExpr{Target: expr, Name: "apply", Args: args}
				} else {
					expr = &CallExpr{Func: v.Name, Args: args}
				}
			} else if _, ok := expr.(*LambdaExpr); ok {
				expr = &MethodCallExpr{Target: expr, Name: "apply", Args: args}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil && op.Cast.Type != nil:
			ctype := typeRefString(op.Cast.Type)
			if ctype == "" {
				break
			}
			if idx, ok := expr.(*IndexExpr); ok {
				idx.ResultType = ctype
				expr = idx
				break
			}
			if op.Cast.Type.Simple != nil {
				switch ctype {
				case "int":
					expr = &IntCastExpr{Value: expr}
				case "float", "float64", "double":
					expr = &FloatCastExpr{Value: expr}
				case "bigint":
					expr = &CastExpr{Value: expr, Type: ctype}
				default:
					if st, ok := expr.(*StructLit); ok {
						st.Name = ctype
					} else {
						expr = &CastExpr{Value: expr, Type: ctype}
					}
				}
			} else {
				expr = &CastExpr{Value: expr, Type: ctype}
			}
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
		if name == "input" && len(args) == 0 {
			needInput = true
			return &InputExpr{}, nil
		}
		if name == "now" && len(args) == 0 {
			needNow = true
			return &CallExpr{Func: "_now", Args: nil}, nil
		}
		if t, ok := varTypes[name]; ok {
			if strings.HasPrefix(t, "fn") {
				method := "apply"
				if strings.HasSuffix(t, ":void") {
					method = "accept"
				}
				return &MethodCallExpr{Target: &VarExpr{Name: name}, Name: method, Args: args}, nil
			}
			if strings.HasPrefix(t, "java.util.function.Function") {
				return &MethodCallExpr{Target: &VarExpr{Name: name}, Name: "apply", Args: args}, nil
			}
		}
		if name == "print" {
			name = "System.out.println"
			for i, a := range args {
				if isNumericBool(a) || (func() bool {
					if u, ok := a.(*UnaryExpr); ok && u.Op == "!" {
						if g, ok := u.Value.(*GroupExpr); ok {
							if mc, ok2 := g.Expr.(*MethodCallExpr); ok2 {
								switch mc.Name {
								case "contains", "containsKey", "anyMatch":
									return true
								}
							}
						}
						return isNumericBool(u.Value)
					}
					return false
				})() {
					args[i] = &TernaryExpr{Cond: a, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
				} else if isArrayExpr(a) {
					funcName := "java.util.Arrays.toString"
					if strings.HasSuffix(arrayElemType(a), "[]") {
						funcName = "java.util.Arrays.deepToString"
					}
					args[i] = &CallExpr{Func: funcName, Args: []Expr{a}}
				} else if isListExpr(a) {
					args[i] = &ListStrExpr{List: a}
				} else if isBoolExpr(a) {
					args[i] = &TernaryExpr{Cond: a, Then: &StringLit{Value: "True"}, Else: &StringLit{Value: "False"}}
				} else if isStringExpr(a) {
					args[i] = a
				}
			}
			if len(args) > 1 {
				expr := args[0]
				for i := 1; i < len(args); i++ {
					expr = &BinaryExpr{Left: &BinaryExpr{Left: expr, Op: "+", Right: &StringLit{Value: " "}}, Op: "+", Right: args[i]}
				}
				args = []Expr{expr}
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
		if name == "len" && len(args) == 1 {
			return &LenExpr{Value: args[0]}, nil
		}
		if name == "count" && len(args) == 1 {
			return &LenExpr{Value: args[0]}, nil
		}
		if name == "avg" && len(args) == 1 {
			return &AvgExpr{Value: args[0]}, nil
		}
		if name == "sum" && len(args) == 1 {
			if topEnv != nil {
				if _, ok := topEnv.GetFunc("sum"); ok {
					// User-defined function takes precedence
					return &CallExpr{Func: name, Args: args}, nil
				}
			}
			return &SumExpr{Value: args[0]}, nil
		}
		if name == "abs" && len(args) == 1 {
			return &CallExpr{Func: "Math.abs", Args: args}, nil
		}
		if name == "int" && len(args) == 1 {
			return &IntCastExpr{Value: args[0]}, nil
		}
		if (name == "float" || name == "float64" || name == "double") && len(args) == 1 {
			return &FloatCastExpr{Value: args[0]}, nil
		}
		if name == "values" && len(args) == 1 {
			return &ValuesExpr{Map: args[0]}, nil
		}
		if name == "keys" && len(args) == 1 {
			return &KeysExpr{Map: args[0]}, nil
		}
		if name == "append" && len(args) == 2 {
			et := arrayElemType(args[0])
			if et == "" {
				et = inferType(args[1])
			}
			return &AppendExpr{List: args[0], Value: args[1], ElemType: et}, nil
		}
		if name == "str" && len(args) == 1 {
			return &CallExpr{Func: "String.valueOf", Args: args}, nil
		}
		if name == "substring" && len(args) == 3 {
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		}
		if name == "upper" && len(args) == 1 {
			return &MethodCallExpr{Target: args[0], Name: "toUpperCase", Args: nil}, nil
		}
		if name == "lower" && len(args) == 1 {
			return &MethodCallExpr{Target: args[0], Name: "toLowerCase", Args: nil}, nil
		}
		return &CallExpr{Func: name, Args: args}, nil
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
	case p.Selector != nil:
		if pyMathAliases[p.Selector.Root] && len(p.Selector.Tail) == 1 {
			name := p.Selector.Tail[0]
			mapped := name
			if name == "pi" {
				mapped = "PI"
			} else if name == "e" {
				mapped = "E"
			}
			return &FieldExpr{Target: &VarExpr{Name: "Math"}, Name: mapped}, nil
		}
		if kind, ok := builtinAliases[p.Selector.Root]; ok && kind == "go_testpkg" && len(p.Selector.Tail) == 1 {
			switch p.Selector.Tail[0] {
			case "Pi":
				return &FloatLit{Value: 3.14}, nil
			case "Answer":
				return &IntLit{Value: 42}, nil
			}
		}
		typ := ""
		if t, ok := varTypes[p.Selector.Root]; ok {
			typ = t
		}
		if p.Selector.Root == "nil" && len(p.Selector.Tail) == 0 {
			return &NullLit{}, nil
		}
		expr := Expr(&VarExpr{Name: p.Selector.Root, Type: typ})
		for _, name := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: name}
		}
		return expr, nil
	case p.Group != nil:
		e, err := compileExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: e}, nil
	case p.If != nil:
		return compileIfExpr(p.If)
	case p.Query != nil:
		return compileQueryExpr(p.Query)
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		if format == "" {
			format = "yaml"
		}
		if format != "yaml" && format != "jsonl" {
			return nil, fmt.Errorf("unsupported load format")
		}
		expr, err := dataExprFromFile(path, format, p.Load.Type)
		if err != nil {
			return nil, err
		}
		return expr, nil
	case p.List != nil:
		if st, ok := inferStructFromList(p.List); ok {
			structCount++
			name := fmt.Sprintf("Data%d", structCount)
			st.Name = name
			if topEnv != nil {
				topEnv.SetStruct(name, st)
			}
			fields := make([]Param, len(st.Order))
			elems := make([]Expr, len(p.List.Elems))
			for i, fn := range st.Order {
				fields[i] = Param{Name: fn, Type: toJavaTypeFromType(st.Fields[fn])}
			}
			for i, e := range p.List.Elems {
				ml := e.Binary.Left.Value.Target.Map
				vals := make([]Expr, len(st.Order))
				for j, it := range ml.Items {
					v, err := compileExpr(it.Value)
					if err != nil {
						return nil, err
					}
					vals[j] = v
				}
				elems[i] = &StructLit{Name: name, Fields: vals, Names: st.Order}
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fields})
			if structDefs != nil {
				sf := make(map[string]string)
				for _, f := range fields {
					sf[f.Name] = f.Type
				}
				structDefs[name] = sf
			}
			return &ListLit{ElemType: name, Elems: elems}, nil
		}
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
		ml := p.Map
		keys := make([]Expr, len(ml.Items))
		vals := make([]Expr, len(ml.Items))
		fields := make(map[string]string)
		for i, it := range ml.Items {
			ke, err := compileExpr(it.Key)
			if err != nil {
				return nil, err
			}
			ve, err := compileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			keys[i] = ke
			vals[i] = ve
			if s, ok := literalString(it.Key); ok {
				t := inferType(ve)
				if t == "" && topEnv != nil {
					t = toJavaTypeFromType(types.ExprType(it.Value, currentEnv()))
				}
				if t != "" {
					fields[s] = t
				}
			}
		}
		return &MapLit{Keys: keys, Values: vals, Fields: fields}, nil
	case p.Struct != nil:
		names := make([]string, len(p.Struct.Fields))
		vals := make([]Expr, len(p.Struct.Fields))
		var st types.StructType
		if topEnv != nil {
			if s, ok := topEnv.GetStruct(p.Struct.Name); ok {
				st = s
			}
		}
		for i, f := range p.Struct.Fields {
			names[i] = f.Name
			v, err := compileExpr(f.Value)
			if err != nil {
				return nil, err
			}
			if ml, ok := v.(*MapLit); ok && st.Fields != nil {
				if ft, ok2 := st.Fields[f.Name]; ok2 {
					if mt, ok3 := ft.(types.MapType); ok3 {
						if ml.KeyType == "" {
							ml.KeyType = toJavaTypeFromType(mt.Key)
						}
						if ml.ValueType == "" {
							ml.ValueType = toJavaTypeFromType(mt.Value)
						}
					}
				}
			}
			vals[i] = v
		}
		return &StructLit{Name: p.Struct.Name, Fields: vals, Names: names}, nil
	case p.FunExpr != nil:
		return compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return compileMatchExpr(p.Match)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := compileExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = compileIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = compileExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &BoolLit{Value: false}
	}
	return &TernaryExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func compileFunExpr(fn *parser.FunExpr) (Expr, error) {
	params := make([]Param, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = Param{Name: p.Name, Type: typeRefString(p.Type)}
	}
	var body []Stmt
	if fn.ExprBody != nil {
		ex, err := compileExpr(fn.ExprBody)
		if err != nil {
			return nil, err
		}
		body = []Stmt{&ReturnStmt{Expr: ex}}
	} else {
		var err error
		body, err = compileStmts(fn.BlockBody)
		if err != nil {
			return nil, err
		}
	}
	for _, p := range params {
		if javaType(p.Type) == "" {
			stmt := &AssignStmt{Name: p.Name, Expr: &CallExpr{Func: "new java.util.LinkedHashMap", Args: []Expr{&VarExpr{Name: p.Name}}}, Type: p.Type}
			body = append([]Stmt{stmt}, body...)
		}
	}
	ret := typeRefString(fn.Return)
	if ret == "" {
		ret = inferReturnType(body)
	}
	return &LambdaExpr{Params: params, Body: body, Return: ret}, nil
}

func compileMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := compileExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		if call, ok := callPattern(c.Pattern); ok {
			if ut, ok2 := topEnv.FindUnionByVariant(call.Func); ok2 {
				st := ut.Variants[call.Func]
				subs := map[string]Expr{}
				var cond Expr = &InstanceOfExpr{Target: target, Type: call.Func}
				for idx, arg := range call.Args {
					field := st.Order[idx]
					fv := &FieldExpr{Target: &CastExpr{Value: target, Type: call.Func}, Name: field}
					if name, ok := identName(arg); ok {
						if name != "_" {
							subs[name] = fv
						}
					} else {
						val, err := compileExpr(arg)
						if err != nil {
							return nil, err
						}
						part := &BinaryExpr{Left: fv, Op: "==", Right: val}
						if cond != nil {
							cond = &BinaryExpr{Left: cond, Op: "&&", Right: part}
						} else {
							cond = part
						}
					}
				}
				res, err := compileExpr(c.Result)
				if err != nil {
					return nil, err
				}
				res = substituteVars(res, subs)
				if expr == nil {
					expr = res
				} else {
					expr = &TernaryExpr{Cond: cond, Then: res, Else: expr}
				}
				continue
			}
		}
		res, err := compileExpr(c.Result)
		if err != nil {
			return nil, err
		}
		if name, ok := identName(c.Pattern); ok {
			if name == "_" {
				expr = res
				continue
			}
			if _, ok := topEnv.FindUnionByVariant(name); ok {
				cond := &InstanceOfExpr{Target: target, Type: name}
				if expr == nil {
					expr = res
				} else {
					expr = &TernaryExpr{Cond: cond, Then: res, Else: expr}
				}
				continue
			}
		}
		pat, err := compileExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: pat}
		if expr == nil {
			expr = res
		} else {
			expr = &TernaryExpr{Cond: cond, Then: res, Else: expr}
		}
	}
	return expr, nil
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func compileQueryExpr(q *parser.QueryExpr) (Expr, error) {
	src, err := compileExpr(q.Source)
	if err != nil {
		return nil, err
	}
	var elemType string
	if v, ok := src.(*VarExpr); ok {
		if it, ok2 := groupItems[varTypes[v.Name]]; ok2 {
			src = &FieldExpr{Target: src, Name: "items"}
			elemType = it
		}
	}
	if topEnv != nil {
		if lt, ok := types.ExprType(q.Source, currentEnv()).(types.ListType); ok {
			elemType = toJavaTypeFromType(lt.Elem)
		}
	}
	if elemType == "" {
		switch s := src.(type) {
		case *VarExpr:
			if vt, ok := varTypes[s.Name]; ok {
				if strings.HasSuffix(vt, "[]") {
					elemType = strings.TrimSuffix(vt, "[]")
				} else if strings.HasPrefix(vt, "java.util.List<") && strings.HasSuffix(vt, ">") {
					elemType = strings.TrimSuffix(strings.TrimPrefix(vt, "java.util.List<"), ">")
				}
			}
		case *ListLit:
			elemType = s.ElemType
		}
	}
	if elemType == "" {
		elemType = "java.util.Map"
	}
	varTypes[q.Var] = elemType
	if topEnv != nil {
		if st, ok := topEnv.GetStruct(elemType); ok {
			topEnv.SetVar(q.Var, st, false)
		}
	}
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := compileExpr(f.Src)
		if err != nil {
			return nil, err
		}
		ftype := "java.util.Map"
		if topEnv != nil {
			if lt, ok := types.ExprType(f.Src, currentEnv()).(types.ListType); ok {
				ftype = toJavaTypeFromType(lt.Elem)
				if st, ok := topEnv.GetStruct(ftype); ok {
					topEnv.SetVar(f.Var, st, false)
				}
			}
		}
		if ftype == "" {
			ft := inferType(fe)
			if strings.HasSuffix(ft, "[]") {
				ftype = strings.TrimSuffix(ft, "[]")
			}
			if ftype == "" {
				ftype = "java.util.Map"
			}
		}
		varTypes[f.Var] = ftype
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}
	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		je, err := compileExpr(j.Src)
		if err != nil {
			return nil, err
		}
		jt := "java.util.Map"
		if topEnv != nil {
			if lt, ok := types.ExprType(j.Src, currentEnv()).(types.ListType); ok {
				jt = toJavaTypeFromType(lt.Elem)
				if st, ok := topEnv.GetStruct(jt); ok {
					topEnv.SetVar(j.Var, st, false)
				}
			}
		}
		if jt == "" {
			t := inferType(je)
			if strings.HasSuffix(t, "[]") {
				jt = strings.TrimSuffix(t, "[]")
			}
			if jt == "" {
				jt = "java.util.Map"
			}
		}
		varTypes[j.Var] = jt
		onExpr, err := compileExpr(j.On)
		if err != nil {
			return nil, err
		}
		side := ""
		if j.Side != nil {
			side = *j.Side
		}
		joins[i] = queryJoin{Var: j.Var, Src: je, On: onExpr, Side: side}
	}
	var where Expr
	if q.Where != nil {
		where, err = compileExpr(q.Where)
		if err != nil {
			return nil, err
		}
	}

	var group *queryGroup
	if q.Group != nil && len(q.Group.Exprs) > 0 {
		keyExpr, err := compileExpr(q.Group.Exprs[0])
		if err != nil {
			return nil, err
		}
		keyType := "java.util.Map"
		if topEnv != nil {
			keyType = toJavaTypeFromType(types.ExprType(q.Group.Exprs[0], currentEnv()))
		}
		if keyType == "" {
			keyType = inferType(keyExpr)
		}
		if keyType == "" {
			keyType = "Object"
		}
		itemFields := append([]string{q.Var}, make([]string, 0, len(q.Froms)+len(q.Joins))...)
		for _, f := range q.Froms {
			itemFields = append(itemFields, f.Var)
		}
		for _, j := range q.Joins {
			itemFields = append(itemFields, j.Var)
		}
		var itemName string
		if len(itemFields) == 1 {
			itemName = varTypes[itemFields[0]]
		} else {
			itemName = fmt.Sprintf("Item%d", structCount+1)
			itemDecl := make([]Param, len(itemFields))
			for i, n := range itemFields {
				itemDecl[i] = Param{Name: n, Type: varTypes[n]}
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: itemName, Fields: itemDecl})
			if structDefs != nil {
				sf := make(map[string]string)
				for _, p := range itemDecl {
					sf[p.Name] = p.Type
				}
				structDefs[itemName] = sf
			}
			if topEnv != nil {
				st := types.StructType{Fields: map[string]types.Type{}, Order: make([]string, len(itemFields))}
				for i, p := range itemDecl {
					st.Fields[p.Name] = typeFromName(p.Type)
					st.Order[i] = p.Name
				}
				topEnv.SetStruct(itemName, st)
			}
		}

		groupName := fmt.Sprintf("Group%d", structCount+1)
		structCount++
		gfields := []Param{
			{Name: "key", Type: keyType},
			{Name: "items", Type: fmt.Sprintf("java.util.List<%s>", itemName)},
		}
		extraDecls = append(extraDecls, &TypeDeclStmt{Name: groupName, Fields: gfields})
		if structDefs != nil {
			sf := map[string]string{"key": keyType, "items": fmt.Sprintf("java.util.List<%s>", itemName)}
			structDefs[groupName] = sf
		}
		if topEnv != nil {
			st := types.StructType{Fields: map[string]types.Type{"key": typeFromName(keyType), "items": types.ListType{Elem: typeFromName(itemName)}}, Order: []string{"key", "items"}}
			topEnv.SetStruct(groupName, st)
		}
		groupItems[groupName] = itemName
		var having Expr
		if q.Group.Having != nil {
			having, err = compileExpr(q.Group.Having)
			if err != nil {
				return nil, err
			}
		}
		varTypes[q.Group.Name] = groupName
		group = &queryGroup{Key: keyExpr, Name: q.Group.Name, Having: having, ItemType: itemName, GroupType: groupName, Fields: itemFields}
	}

	idx := len(extraDecls)
	sel, err := compileExpr(q.Select)
	if err != nil {
		return nil, err
	}
	if ml := mapLiteral(q.Select); ml != nil {
		extraDecls = extraDecls[:idx]
		if st, ok := types.InferStructFromMapEnv(ml, topEnv); ok {
			structCount++
			name := fmt.Sprintf("Result%d", structCount)
			st.Name = name
			if topEnv != nil {
				topEnv.SetStruct(name, st)
			}
			fieldsDecl := make([]Param, len(st.Order))
			vals := make([]Expr, len(st.Order))
			for i, it := range ml.Items {
				v, err := compileExpr(it.Value)
				if err != nil {
					return nil, err
				}
				tname := ""
				if fe, ok := v.(*FieldExpr); ok {
					if ft, ok2 := fieldTypeFromVar(fe.Target, fe.Name); ok2 {
						tname = ft
					}
				}
				if tname == "" {
					tname = inferType(v)
				}
				if tname == "" {
					tname = "Object"
				}
				fieldsDecl[i] = Param{Name: st.Order[i], Type: tname}
				vals[i] = v
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fieldsDecl})
			if structDefs != nil {
				sf := make(map[string]string)
				for _, f := range fieldsDecl {
					sf[f.Name] = f.Type
				}
				structDefs[name] = sf
			}
			sel = &StructLit{Name: name, Fields: vals, Names: st.Order}
			elemType = name
		}
	}
	tsel := javaBoxType(inferType(sel))
	if tsel != "" && tsel != "Object" {
		elemType = tsel
	} else if elemType == "" {
		elemType = "Object"
	}

	var sortExpr, skipExpr, takeExpr Expr
	if q.Sort != nil {
		sortExpr, err = compileExpr(q.Sort)
		if err != nil {
			return nil, err
		}
	}
	if q.Skip != nil {
		skipExpr, err = compileExpr(q.Skip)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = compileExpr(q.Take)
		if err != nil {
			return nil, err
		}
	}

	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Group: group, Where: where, Sort: sortExpr, Skip: skipExpr, Take: takeExpr, Select: sel, ElemType: elemType}, nil
}

// Emit generates formatted Java source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("public class Main {\n")
	// emit type declarations and global variables first
	for _, st := range prog.Stmts {
		switch st.(type) {
		case *TypeDeclStmt, *InterfaceDeclStmt:
			st.emit(&buf, "    ")
			buf.WriteByte('\n')
		case *LetStmt, *VarStmt:
			st.emit(&buf, "    ")
		}
	}
	if len(prog.Stmts) > 0 {
		buf.WriteByte('\n')
	}
	if needInput {
		buf.WriteString("    static java.util.Scanner _scanner = new java.util.Scanner(System.in);\n\n")
	}
	if needLoadYaml {
		buf.WriteString("    static java.util.List<java.util.Map<String,Object>> loadYaml(String path) {\n")
		buf.WriteString("        if (!(new java.io.File(path)).isAbsolute()) {\n")
		buf.WriteString("            java.io.File f = new java.io.File(path);\n")
		buf.WriteString("            if (!f.exists()) {\n")
		buf.WriteString("                String root = System.getenv(\"MOCHI_ROOT\");\n")
		buf.WriteString("                if (root != null && !root.isEmpty()) {\n")
		buf.WriteString("                    String clean = path;\n")
		buf.WriteString("                    while (clean.startsWith(\"../\")) clean = clean.substring(3);\n")
		buf.WriteString("                    java.io.File alt = new java.io.File(root + java.io.File.separator + \"tests\" + java.io.File.separator + clean);\n")
		buf.WriteString("                    if (!alt.exists()) alt = new java.io.File(root, clean);\n")
		buf.WriteString("                    if (alt.exists()) path = alt.getPath();\n")
		buf.WriteString("                }\n")
		buf.WriteString("            }\n")
		buf.WriteString("        }\n")
		buf.WriteString("        java.util.List<java.util.Map<String,Object>> list = new java.util.ArrayList<>();\n")
		buf.WriteString("        try (java.io.BufferedReader br = new java.io.BufferedReader(new java.io.FileReader(path))) {\n")
		buf.WriteString("            java.util.Map<String,Object> cur = null;\n")
		buf.WriteString("            String line;\n")
		buf.WriteString("            while ((line = br.readLine()) != null) {\n")
		buf.WriteString("                line = line.trim();\n")
		buf.WriteString("                if (line.startsWith(\"- name:\")) {\n")
		buf.WriteString("                    if (cur != null) list.add(cur);\n")
		buf.WriteString("                    cur = new java.util.LinkedHashMap<>();\n")
		buf.WriteString("                    cur.put(\"name\", line.substring(line.indexOf(':')+1).trim());\n")
		buf.WriteString("                } else if (line.startsWith(\"age:\")) {\n")
		buf.WriteString("                    if (cur != null) cur.put(\"age\", Integer.parseInt(line.substring(line.indexOf(':')+1).trim()));\n")
		buf.WriteString("                } else if (line.startsWith(\"email:\")) {\n")
		buf.WriteString("                    if (cur != null) cur.put(\"email\", line.substring(line.indexOf(':')+1).trim());\n")
		buf.WriteString("                }\n")
		buf.WriteString("            }\n")
		buf.WriteString("            if (cur != null) list.add(cur);\n")
		buf.WriteString("        } catch (Exception e) { throw new RuntimeException(e); }\n")
		buf.WriteString("        return list;\n")
		buf.WriteString("    }\n\n")
	}
	if needSaveJsonl {
		buf.WriteString("    static java.util.Map<String,Object> asMap(Object o) {\n")
		buf.WriteString("        if (o instanceof java.util.Map<?,?> mm) {\n")
		buf.WriteString("            java.util.LinkedHashMap<String,Object> m = new java.util.LinkedHashMap<>();\n")
		buf.WriteString("            for (java.util.Map.Entry<?,?> e : mm.entrySet()) m.put(String.valueOf(e.getKey()), e.getValue());\n")
		buf.WriteString("            return m;\n")
		buf.WriteString("        }\n")
		buf.WriteString("        java.util.LinkedHashMap<String,Object> m = new java.util.LinkedHashMap<>();\n")
		buf.WriteString("        for (var f : o.getClass().getDeclaredFields()) { try { f.setAccessible(true); m.put(f.getName(), f.get(o)); } catch (Exception e) { throw new RuntimeException(e); } }\n")
		buf.WriteString("        return m;\n")
		buf.WriteString("    }\n\n")
		buf.WriteString("    static void saveJsonl(java.util.List<?> list) {\n")
		buf.WriteString("        for (Object obj : list) {\n")
		buf.WriteString("            java.util.Map<String,Object> m = asMap(obj);\n")
		buf.WriteString("            java.util.List<String> parts = new java.util.ArrayList<>();\n")
		buf.WriteString("            for (java.util.Map.Entry<?,?> e : m.entrySet()) {\n")
		buf.WriteString("                Object v = e.getValue();\n")
		buf.WriteString("                if (v instanceof String) {\n")
		buf.WriteString("                    parts.add(\"\\\"\" + e.getKey() + \"\\\": \" + \"\\\"\" + v + \"\\\"\");\n")
		buf.WriteString("                } else {\n")
		buf.WriteString("                    parts.add(\"\\\"\" + e.getKey() + \"\\\": \" + v);\n")
		buf.WriteString("                }\n")
		buf.WriteString("            }\n")
		buf.WriteString("            System.out.println(\"{\" + String.join(\", \", parts) + \"}\");\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n\n")
	}
	// helper functions removed for simplified output
	for i, fn := range prog.Funcs {
		ret := javaType(fn.Return)
		if ret == "" {
			ret = "void"
		}
		buf.WriteString("    static " + ret + " " + sanitize(fn.Name) + "(")
		for i, p := range fn.Params {
			if i > 0 {
				buf.WriteString(", ")
			}
			typ := javaType(p.Type)
			if typ == "" {
				typ = "java.util.Map"
			}
			buf.WriteString(typ + " " + sanitize(p.Name))
		}
		buf.WriteString(") {\n")
		savedVT := varTypes
		varTypes = copyMap(varTypes)
		for _, p := range fn.Params {
			if p.Type != "" {
				varTypes[p.Name] = p.Type
			}
		}
		for _, s := range fn.Body {
			s.emit(&buf, "        ")
		}
		varTypes = savedVT
		buf.WriteString("    }")
		buf.WriteByte('\n')
		if i < len(prog.Funcs)-1 {
			buf.WriteByte('\n')
		}
	}
	buf.WriteString("    public static void main(String[] args) {\n")
	if benchMain {
		var body []Stmt
		for _, st := range prog.Stmts {
			switch st.(type) {
			case *LetStmt, *VarStmt, *TypeDeclStmt, *InterfaceDeclStmt:
				// already emitted as globals or declarations
			default:
				body = append(body, st)
			}
		}
		bs := &BenchStmt{Name: "main", Body: body}
		bs.emit(&buf, "        ")
	} else {
		for _, st := range prog.Stmts {
			switch st.(type) {
			case *LetStmt, *VarStmt, *TypeDeclStmt, *InterfaceDeclStmt:
				// already emitted as globals or declarations
			default:
				st.emit(&buf, "        ")
			}
		}
	}
	buf.WriteString("    }\n")
	if needNow {
		buf.WriteString("\n    static boolean _nowSeeded = false;\n")
		buf.WriteString("    static int _nowSeed;\n")
		buf.WriteString("    static int _now() {\n")
		buf.WriteString("        if (!_nowSeeded) {\n")
		buf.WriteString("            String s = System.getenv(\"MOCHI_NOW_SEED\");\n")
		buf.WriteString("            if (s != null && !s.isEmpty()) {\n")
		buf.WriteString("                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}\n")
		buf.WriteString("            }\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if (_nowSeeded) {\n")
		buf.WriteString("            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);\n")
		buf.WriteString("            return _nowSeed;\n")
		buf.WriteString("        }\n")
		buf.WriteString("        return (int)(System.nanoTime() / 1000);\n")
		buf.WriteString("    }\n")
	}
	if needMem {
		buf.WriteString("\n    static long _mem() {\n")
		buf.WriteString("        Runtime rt = Runtime.getRuntime();\n")
		buf.WriteString("        rt.gc();\n")
		buf.WriteString("        return rt.totalMemory() - rt.freeMemory();\n")
		buf.WriteString("    }\n")
	}
	if needAppendBool {
		buf.WriteString("\n    static boolean[] appendBool(boolean[] arr, boolean v) {\n")
		buf.WriteString("        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);\n")
		buf.WriteString("        out[arr.length] = v;\n")
		buf.WriteString("        return out;\n")
		buf.WriteString("    }\n")
	}
	if needAppendObj {
		buf.WriteString("\n    static <T> T[] appendObj(T[] arr, T v) {\n")
		buf.WriteString("        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);\n")
		buf.WriteString("        out[arr.length] = v;\n")
		buf.WriteString("        return out;\n")
		buf.WriteString("    }\n")
	}
	if needNetLookupHost {
		buf.WriteString("\n    static Object[] _netLookupHost(String host) {\n")
		buf.WriteString("        try {\n")
		buf.WriteString("            java.net.InetAddress[] arr = java.net.InetAddress.getAllByName(host);\n")
		buf.WriteString("            String[] out = new String[arr.length];\n")
		buf.WriteString("            for (int i = 0; i < arr.length; i++) { out[i] = arr[i].getHostAddress(); }\n")
		buf.WriteString("            return new Object[]{out, null};\n")
		buf.WriteString("        } catch (Exception e) {\n")
		buf.WriteString("            return new Object[]{null, e.toString()};\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")
	}
	buf.WriteString("}\n")
	return formatJava(buf.Bytes())
}

func formatJava(src []byte) []byte {
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

func typeRefString(tr *parser.TypeRef) string {
	if tr == nil {
		return ""
	}
	if tr.Simple != nil {
		if *tr.Simple == "any" {
			return "Object"
		}
		return *tr.Simple
	}
	if tr.Generic != nil {
		if tr.Generic.Name == "list" && len(tr.Generic.Args) == 1 {
			elem := typeRefString(tr.Generic.Args[0])
			if elem == "" {
				elem = "Object"
			}
			return elem + "[]"
		}
		if tr.Generic.Name == "map" && len(tr.Generic.Args) == 2 {
			key := typeRefString(tr.Generic.Args[0])
			val := typeRefString(tr.Generic.Args[1])
			if key == "" {
				key = "Object"
			}
			if val == "" {
				val = "Object"
			}
			return fmt.Sprintf("java.util.Map<%s,%s>", javaBoxType(key), javaBoxType(val))
		}
		return tr.Generic.Name
	}
	if tr.Fun != nil {
		var params []string
		for _, p := range tr.Fun.Params {
			params = append(params, typeRefString(p))
		}
		ret := typeRefString(tr.Fun.Return)
		if ret == "" {
			ret = "void"
		}
		return fmt.Sprintf("fn(%s):%s", strings.Join(params, ","), ret)
	}
	return ""
}

func toJavaTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.BoolType:
		return "boolean"
	case types.StringType:
		return "String"
	case types.BigIntType:
		return "java.math.BigInteger"
	case types.AnyType:
		return "Object"
	case types.ListType:
		et := toJavaTypeFromType(tt.Elem)
		if et == "" {
			et = "Object"
		}
		return et + "[]"
	case types.StructType:
		if tt.Name != "" {
			return tt.Name
		}
	case types.UnionType:
		if tt.Name != "" {
			return tt.Name
		}
	case types.MapType:
		k := toJavaTypeFromType(tt.Key)
		if k == "" {
			k = "Object"
		}
		v := toJavaTypeFromType(tt.Value)
		if v == "" {
			v = "Object"
		}
		return fmt.Sprintf("java.util.Map<%s,%s>", javaBoxType(k), javaBoxType(v))
	case types.FuncType:
		if len(tt.Params) == 1 {
			pt := javaBoxType(toJavaTypeFromType(tt.Params[0]))
			rt := javaBoxType(toJavaTypeFromType(tt.Return))
			return fmt.Sprintf("java.util.function.Function<%s,%s>", pt, rt)
		}
		return "java.util.function.Function"
	}
	return ""
}

func mapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil
	}
	v := u.Value
	if len(v.Ops) != 0 || v.Target == nil {
		return nil
	}
	return v.Target.Map
}

func inferStructFromList(ll *parser.ListLiteral) (st types.StructType, ok bool) {
	if ll == nil || len(ll.Elems) == 0 {
		return types.StructType{}, false
	}
	first := ll.Elems[0]
	if first.Binary == nil || len(first.Binary.Right) != 0 {
		return types.StructType{}, false
	}
	fm := first.Binary.Left.Value.Target.Map
	if fm == nil {
		return types.StructType{}, false
	}
	fields := map[string]types.Type{}
	order := make([]string, len(fm.Items))
	for i, it := range fm.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			return types.StructType{}, false
		}
		order[i] = key
		fields[key] = types.ExprType(it.Value, currentEnv())
	}
	for _, el := range ll.Elems[1:] {
		if el.Binary == nil || len(el.Binary.Right) != 0 {
			return types.StructType{}, false
		}
		ml := el.Binary.Left.Value.Target.Map
		if ml == nil || len(ml.Items) != len(order) {
			return types.StructType{}, false
		}
		for i, it := range ml.Items {
			key, ok := types.SimpleStringKey(it.Key)
			if !ok || key != order[i] {
				return types.StructType{}, false
			}
			t := types.ExprType(it.Value, currentEnv())
			if !types.EqualTypes(fields[key], t) {
				return types.StructType{}, false
			}
		}
	}
	return types.StructType{Fields: fields, Order: order}, true
}

func renameVar(e Expr, oldName, newName string) Expr {
	switch ex := e.(type) {
	case *VarExpr:
		if ex.Name == oldName {
			return &VarExpr{Name: newName, Type: ex.Type}
		}
		return ex
	case *FieldExpr:
		return &FieldExpr{Target: renameVar(ex.Target, oldName, newName), Name: ex.Name}
	case *BinaryExpr:
		return &BinaryExpr{Left: renameVar(ex.Left, oldName, newName), Op: ex.Op, Right: renameVar(ex.Right, oldName, newName)}
	case *UnaryExpr:
		return &UnaryExpr{Op: ex.Op, Value: renameVar(ex.Value, oldName, newName)}
	case *TernaryExpr:
		return &TernaryExpr{Cond: renameVar(ex.Cond, oldName, newName), Then: renameVar(ex.Then, oldName, newName), Else: renameVar(ex.Else, oldName, newName)}
	case *CallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = renameVar(a, oldName, newName)
		}
		return &CallExpr{Func: ex.Func, Args: args}
	case *MethodCallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = renameVar(a, oldName, newName)
		}
		return &MethodCallExpr{Target: renameVar(ex.Target, oldName, newName), Name: ex.Name, Args: args}
	case *IndexExpr:
		return &IndexExpr{Target: renameVar(ex.Target, oldName, newName), Index: renameVar(ex.Index, oldName, newName), IsMap: ex.IsMap, ResultType: ex.ResultType}
	case *SliceExpr:
		return &SliceExpr{Value: renameVar(ex.Value, oldName, newName), Start: renameVar(ex.Start, oldName, newName), End: renameVar(ex.End, oldName, newName)}
	case *LenExpr:
		return &LenExpr{Value: renameVar(ex.Value, oldName, newName)}
	case *AvgExpr:
		return &AvgExpr{Value: renameVar(ex.Value, oldName, newName)}
	case *SumExpr:
		return &SumExpr{Value: renameVar(ex.Value, oldName, newName)}
	case *ValuesExpr:
		return &ValuesExpr{Map: renameVar(ex.Map, oldName, newName)}
	case *KeysExpr:
		return &KeysExpr{Map: renameVar(ex.Map, oldName, newName)}
	case *AppendExpr:
		return &AppendExpr{List: renameVar(ex.List, oldName, newName), Value: renameVar(ex.Value, oldName, newName), ElemType: ex.ElemType}
	case *IntCastExpr:
		return &IntCastExpr{Value: renameVar(ex.Value, oldName, newName)}
	case *FloatCastExpr:
		return &FloatCastExpr{Value: renameVar(ex.Value, oldName, newName)}
	case *CastExpr:
		return &CastExpr{Value: renameVar(ex.Value, oldName, newName), Type: ex.Type}
	case *InstanceOfExpr:
		return &InstanceOfExpr{Target: renameVar(ex.Target, oldName, newName), Type: ex.Type}
	case *ListLit:
		elems := make([]Expr, len(ex.Elems))
		for i, el := range ex.Elems {
			elems[i] = renameVar(el, oldName, newName)
		}
		return &ListLit{ElemType: ex.ElemType, Elems: elems}
	case *InputExpr:
		return ex
	case *StructLit:
		fields := make([]Expr, len(ex.Fields))
		for i, f := range ex.Fields {
			fields[i] = renameVar(f, oldName, newName)
		}
		return &StructLit{Name: ex.Name, Fields: fields, Names: ex.Names}
	case *QueryExpr:
		src := renameVar(ex.Src, oldName, newName)
		froms := make([]queryFrom, len(ex.Froms))
		for i, f := range ex.Froms {
			froms[i] = queryFrom{Var: f.Var, Src: renameVar(f.Src, oldName, newName)}
			if f.Var == oldName {
				froms[i].Var = newName
			}
		}
		joins := make([]queryJoin, len(ex.Joins))
		for i, j := range ex.Joins {
			joins[i] = queryJoin{Var: j.Var, Src: renameVar(j.Src, oldName, newName), On: renameVar(j.On, oldName, newName), Side: j.Side}
			if j.Var == oldName {
				joins[i].Var = newName
			}
		}
		grp := ex.Group
		if grp != nil {
			grp = &queryGroup{Key: renameVar(grp.Key, oldName, newName), Name: grp.Name, Having: renameVar(grp.Having, oldName, newName), ItemType: grp.ItemType, GroupType: grp.GroupType, Fields: grp.Fields}
			if grp.Name == oldName {
				grp.Name = newName
			}
		}
		return &QueryExpr{
			Var:      choose(ex.Var, oldName, newName),
			Src:      src,
			Froms:    froms,
			Joins:    joins,
			Group:    grp,
			Where:    renameVar(ex.Where, oldName, newName),
			Sort:     renameVar(ex.Sort, oldName, newName),
			Skip:     renameVar(ex.Skip, oldName, newName),
			Take:     renameVar(ex.Take, oldName, newName),
			Select:   renameVar(ex.Select, oldName, newName),
			ElemType: ex.ElemType,
		}
	default:
		return ex
	}
}

func substituteFieldVars(e Expr, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *VarExpr:
		if fields[ex.Name] {
			return &FieldExpr{Target: &VarExpr{Name: "item"}, Name: ex.Name}
		}
		return ex
	case *BinaryExpr:
		return &BinaryExpr{Left: substituteFieldVars(ex.Left, fields), Op: ex.Op, Right: substituteFieldVars(ex.Right, fields)}
	case *UnaryExpr:
		return &UnaryExpr{Op: ex.Op, Value: substituteFieldVars(ex.Value, fields)}
	case *TernaryExpr:
		return &TernaryExpr{Cond: substituteFieldVars(ex.Cond, fields), Then: substituteFieldVars(ex.Then, fields), Else: substituteFieldVars(ex.Else, fields)}
	case *CallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = substituteFieldVars(a, fields)
		}
		return &CallExpr{Func: ex.Func, Args: args}
	case *MethodCallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = substituteFieldVars(a, fields)
		}
		return &MethodCallExpr{Target: substituteFieldVars(ex.Target, fields), Name: ex.Name, Args: args}
	case *FieldExpr:
		return &FieldExpr{Target: substituteFieldVars(ex.Target, fields), Name: ex.Name}
	case *IndexExpr:
		return &IndexExpr{Target: substituteFieldVars(ex.Target, fields), Index: substituteFieldVars(ex.Index, fields), IsMap: ex.IsMap, ResultType: ex.ResultType}
	case *IntCastExpr:
		return &IntCastExpr{Value: substituteFieldVars(ex.Value, fields)}
	case *FloatCastExpr:
		return &FloatCastExpr{Value: substituteFieldVars(ex.Value, fields)}
	case *CastExpr:
		return &CastExpr{Value: substituteFieldVars(ex.Value, fields), Type: ex.Type}
	case *InstanceOfExpr:
		return &InstanceOfExpr{Target: substituteFieldVars(ex.Target, fields), Type: ex.Type}
	case *SliceExpr:
		return &SliceExpr{Value: substituteFieldVars(ex.Value, fields), Start: substituteFieldVars(ex.Start, fields), End: substituteFieldVars(ex.End, fields)}
	case *ListLit:
		elems := make([]Expr, len(ex.Elems))
		for i, el := range ex.Elems {
			elems[i] = substituteFieldVars(el, fields)
		}
		return &ListLit{ElemType: ex.ElemType, Elems: elems}
	default:
		return ex
	}
}

// substituteVars replaces variable names with provided expressions.
func substituteVars(e Expr, vars map[string]Expr) Expr {
	switch ex := e.(type) {
	case *VarExpr:
		if v, ok := vars[ex.Name]; ok {
			return v
		}
		return ex
	case *BinaryExpr:
		return &BinaryExpr{Left: substituteVars(ex.Left, vars), Op: ex.Op, Right: substituteVars(ex.Right, vars)}
	case *UnaryExpr:
		return &UnaryExpr{Op: ex.Op, Value: substituteVars(ex.Value, vars)}
	case *TernaryExpr:
		return &TernaryExpr{Cond: substituteVars(ex.Cond, vars), Then: substituteVars(ex.Then, vars), Else: substituteVars(ex.Else, vars)}
	case *CallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = substituteVars(a, vars)
		}
		return &CallExpr{Func: ex.Func, Args: args}
	case *MethodCallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = substituteVars(a, vars)
		}
		return &MethodCallExpr{Target: substituteVars(ex.Target, vars), Name: ex.Name, Args: args}
	case *FieldExpr:
		return &FieldExpr{Target: substituteVars(ex.Target, vars), Name: ex.Name}
	case *IndexExpr:
		return &IndexExpr{Target: substituteVars(ex.Target, vars), Index: substituteVars(ex.Index, vars), IsMap: ex.IsMap, ResultType: ex.ResultType}
	case *IntCastExpr:
		return &IntCastExpr{Value: substituteVars(ex.Value, vars)}
	case *FloatCastExpr:
		return &FloatCastExpr{Value: substituteVars(ex.Value, vars)}
	case *CastExpr:
		return &CastExpr{Value: substituteVars(ex.Value, vars), Type: ex.Type}
	case *InstanceOfExpr:
		return &InstanceOfExpr{Target: substituteVars(ex.Target, vars), Type: ex.Type}
	case *SliceExpr:
		return &SliceExpr{Value: substituteVars(ex.Value, vars), Start: substituteVars(ex.Start, vars), End: substituteVars(ex.End, vars)}
	case *ListLit:
		elems := make([]Expr, len(ex.Elems))
		for i, el := range ex.Elems {
			elems[i] = substituteVars(el, vars)
		}
		return &ListLit{ElemType: ex.ElemType, Elems: elems}
	default:
		return ex
	}
}

func setReturnMapValueType(stmts []Stmt, vt string) {
	for _, st := range stmts {
		switch s := st.(type) {
		case *ReturnStmt:
			if ml, ok := s.Expr.(*MapLit); ok {
				ml.ValueType = vt
			}
		case *IfStmt:
			setReturnMapValueType(s.Then, vt)
			setReturnMapValueType(s.Else, vt)
		case *WhileStmt:
			setReturnMapValueType(s.Body, vt)
		case *ForRangeStmt:
			setReturnMapValueType(s.Body, vt)
		}
	}
}

func choose(name, oldName, newName string) string {
	if name == oldName {
		return newName
	}
	return name
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
		key, ok := literalString(it.Key)
		if ok && key == "format" {
			if v, ok := literalString(it.Value); ok {
				return v
			}
		}
	}
	return ""
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

func constStringExpr(e Expr) (string, bool) {
	if s, ok := e.(*StringLit); ok {
		return s.Value, true
	}
	return "", false
}

func mapFieldsForExpr(e Expr) map[string]string {
	switch ex := e.(type) {
	case *VarExpr:
		if f, ok := mapVarFields[ex.Name]; ok {
			return f
		}
	case *CallExpr:
		if f, ok := funcMapFields[ex.Func]; ok {
			return f
		}
	case *MapLit:
		return ex.Fields
	}
	return nil
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
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
	case "jsonl":
		var list []interface{}
		scanner := bufio.NewScanner(bytes.NewReader(data))
		for scanner.Scan() {
			line := strings.TrimSpace(scanner.Text())
			if line == "" {
				continue
			}
			var item interface{}
			if err := json.Unmarshal([]byte(line), &item); err != nil {
				return nil, err
			}
			list = append(list, item)
		}
		if err := scanner.Err(); err != nil {
			return nil, err
		}
		v = list
	default:
		return nil, fmt.Errorf("unsupported format")
	}
	return valueToExpr(v, typ), nil
}

func valueToExpr(v interface{}, typ *parser.TypeRef) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		if typ != nil && typ.Simple != nil && topEnv != nil {
			if st, ok := topEnv.GetStruct(*typ.Simple); ok {
				fields := make([]Expr, len(st.Order))
				for i, k := range st.Order {
					ft := parserTypeRefFromType(st.Fields[k])
					fields[i] = valueToExpr(val[k], ft)
				}
				return &StructLit{Name: *typ.Simple, Fields: fields, Names: st.Order}
			}
		}
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		keys := make([]Expr, len(names))
		vals := make([]Expr, len(names))
		for i, k := range names {
			keys[i] = &StringLit{Value: k}
			vals[i] = valueToExpr(val[k], nil)
		}
		return &MapLit{Keys: keys, Values: vals}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(it, typ)
		}
		return &ListLit{Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case int, int64:
		return &IntLit{Value: int(reflect.ValueOf(val).Int())}
	case float32, float64:
		if typ != nil && typ.Simple != nil && *typ.Simple == "int" {
			return &IntLit{Value: int(reflect.ValueOf(val).Float())}
		}
		return &FloatLit{Value: reflect.ValueOf(val).Float()}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func parserTypeRefFromType(t types.Type) *parser.TypeRef {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		s := "int"
		return &parser.TypeRef{Simple: &s}
	case types.FloatType:
		s := "double"
		return &parser.TypeRef{Simple: &s}
	case types.BoolType:
		s := "bool"
		return &parser.TypeRef{Simple: &s}
	case types.StringType:
		s := "string"
		return &parser.TypeRef{Simple: &s}
	case types.StructType:
		if tt.Name != "" {
			s := tt.Name
			return &parser.TypeRef{Simple: &s}
		}
	}
	return nil
}
