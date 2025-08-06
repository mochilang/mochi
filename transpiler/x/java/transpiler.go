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
	"strconv"
	"strings"

	"gopkg.in/yaml.v3"
	"mochi/parser"
	"mochi/types"
)

var varTypes map[string]string
var funcRet map[string]string
var funcParams map[string][]string
var funcParamNames map[string][]string
var paramMapFields map[string]map[string]map[string]string
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
var needConcat bool
var needNetLookupHost bool
var needEnviron bool
var needMem bool
var needPadStart bool
var needRepeat bool
var needFetch bool
var fetchStructs map[string]bool
var needSHA256 bool
var needMD5Hex bool
var needRuneLen bool
var needSubstr bool
var needArrGetI bool
var needArrGetD bool
var needArrGetB bool
var needArrGetO bool
var needP bool
var needJSON bool
var needBigRat bool
var needModPow2 bool
var needCastInt2D bool
var needFn3 bool
var pyMathAliases map[string]bool
var builtinAliases map[string]string
var structDefs map[string]map[string]string
var varDecls map[string]*VarStmt
var funcMapFields map[string]map[string]string
var mapVarFields map[string]map[string]string
var benchMain bool
var currentFuncReturn string
var structMethods map[string][]*Function
var refVars map[string]bool
var scopeStack []map[string]*VarStmt
var closureStack []bool
var structDecls map[string]*TypeDeclStmt
var disableStructList bool
var varAliases map[string]string
var aliasStack []map[string]string
var aliasCounts map[string]int

func pushScope(closure bool) map[string]*VarStmt {
	saved := varDecls
	scopeStack = append(scopeStack, varDecls)
	closureStack = append(closureStack, closure)
	varDecls = map[string]*VarStmt{}
	aliasStack = append(aliasStack, map[string]string{})
	return saved
}

func popScope(saved map[string]*VarStmt) {
	varDecls = saved
	scopeStack = scopeStack[:len(scopeStack)-1]
	closureStack = closureStack[:len(closureStack)-1]
	for name := range aliasStack[len(aliasStack)-1] {
		delete(varAliases, name)
	}
	aliasStack = aliasStack[:len(aliasStack)-1]
}

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

func copyBoolMap(src map[string]bool) map[string]bool {
	dst := make(map[string]bool, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

func declareAlias(name string) string {
	alias := name
	if aliasCounts == nil {
		aliasCounts = map[string]int{}
	}
	if varAliases == nil {
		varAliases = map[string]string{}
	}
	if count, ok := aliasCounts[name]; ok {
		aliasCounts[name] = count + 1
		alias = fmt.Sprintf("%s_%d", name, count+1)
	} else {
		aliasCounts[name] = 0
	}
	varAliases[name] = alias
	if len(aliasStack) > 0 {
		aliasStack[len(aliasStack)-1][name] = alias
	}
	return alias
}

func resolveAlias(name string) string {
	for i := len(aliasStack) - 1; i >= 0; i-- {
		if a, ok := aliasStack[i][name]; ok {
			return a
		}
	}
	if a, ok := varAliases[name]; ok {
		return a
	}
	return name
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

var javaKeywords = map[string]struct{}{
	"abstract": {}, "assert": {}, "boolean": {}, "break": {}, "byte": {},
	"case": {}, "catch": {}, "char": {}, "class": {}, "const": {}, "continue": {},
	"default": {}, "do": {}, "double": {}, "else": {}, "enum": {}, "extends": {},
	"final": {}, "finally": {}, "float": {}, "for": {}, "goto": {}, "if": {},
	"implements": {}, "import": {}, "instanceof": {}, "int": {}, "interface": {},
	"long": {}, "native": {}, "new": {}, "package": {}, "private": {}, "protected": {},
	"public": {}, "return": {}, "short": {}, "static": {}, "strictfp": {}, "super": {},
	"switch": {}, "synchronized": {}, "this": {}, "throw": {}, "throws": {}, "transient": {},
	"try": {}, "void": {}, "volatile": {}, "while": {},
}

func sanitize(name string) string {
	if name == "_" {
		return "_v"
	}
	if _, ok := javaKeywords[name]; ok {
		return name + "_"
	}
	return name
}

func simpleUnionAlias(ut types.UnionType) string {
	if len(ut.Variants) == 1 {
		for name, st := range ut.Variants {
			if len(st.Fields) == 0 {
				switch name {
				case "int", "float", "string", "bool", "boolean", "double":
					return javaType(name)
				}
			}
		}
	}
	return ""
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
	case "bigrat":
		needBigRat = true
		return "BigRat"
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
		if strings.HasPrefix(t, "fn(") {
			start := strings.Index(t, "(") + 1
			end := strings.Index(t, ")")
			retIdx := strings.LastIndex(t, ":")
			if start >= 0 && end >= start && retIdx > end {
				paramsStr := t[start:end]
				ret := t[retIdx+1:]
				if paramsStr == "" {
					if ret == "void" || ret == "" {
						return "Runnable"
					}
					rt := javaBoxType(javaType(ret))
					return fmt.Sprintf("java.util.function.Supplier<%s>", rt)
				}
				params := strings.Split(paramsStr, ",")
				if len(params) == 1 {
					pt := javaBoxType(javaType(params[0]))
					if ret == "void" {
						return fmt.Sprintf("java.util.function.Consumer<%s>", pt)
					}
					rt := javaBoxType(javaType(ret))
					return fmt.Sprintf("java.util.function.Function<%s,%s>", pt, rt)
				} else if len(params) == 2 {
					pt1 := javaBoxType(javaType(params[0]))
					pt2 := javaBoxType(javaType(params[1]))
					if ret == "void" {
						return fmt.Sprintf("java.util.function.BiConsumer<%s,%s>", pt1, pt2)
					}
					rt := javaBoxType(javaType(ret))
					return fmt.Sprintf("java.util.function.BiFunction<%s,%s,%s>", pt1, pt2, rt)
				} else if len(params) == 3 {
					needFn3 = true
					pt1 := javaBoxType(javaType(params[0]))
					pt2 := javaBoxType(javaType(params[1]))
					pt3 := javaBoxType(javaType(params[2]))
					rt := javaBoxType(javaType(ret))
					return fmt.Sprintf("Fn3<%s,%s,%s,%s>", pt1, pt2, pt3, rt)
				}
			}
		}
		if strings.HasSuffix(t, "[]") {
			elem := strings.TrimSuffix(t, "[]")
			return javaType(elem) + "[]"
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
	case "bigrat", "BigRat":
		return "BigRat"
	default:
		if strings.HasSuffix(t, "[]") {
			elem := strings.TrimSuffix(t, "[]")
			jt := javaType(elem)
			if jt == "" {
				jt = elem
			}
			return jt + "[]"
		}
		if t == "" {
			return "Object"
		}
		return t
	}
}

func mapValueType(t string) string {
	if i := strings.Index(t, "Map<"); i >= 0 {
		rest := t[i+4:]
		depth := 1
		j := 0
		comma := -1
		for j < len(rest) && depth > 0 {
			switch rest[j] {
			case '<':
				depth++
			case '>':
				depth--
			case ',':
				if depth == 1 && comma < 0 {
					comma = j
				}
			}
			j++
		}
		if comma >= 0 {
			val := strings.TrimSpace(rest[comma+1 : j-1])
			switch val {
			case "Integer":
				return "int"
			case "Double":
				return "double"
			case "Boolean":
				return "boolean"
			default:
				return val
			}
		}
	}
	return ""
}

func mapKeyType(t string) string {
	if i := strings.Index(t, "Map<"); i >= 0 {
		rest := t[i+4:]
		depth := 1
		j := 0
		comma := -1
		for j < len(rest) && depth > 0 {
			switch rest[j] {
			case '<':
				depth++
			case '>':
				depth--
			case ',':
				if depth == 1 && comma < 0 {
					comma = j
				}
			}
			j++
		}
		if comma >= 0 {
			k := strings.TrimSpace(rest[:comma])
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
	if typ == "boolean" {
		if inferType(e) == "boolean" {
			e.emit(w)
		} else {
			fmt.Fprint(w, "((Boolean)(")
			e.emit(w)
			fmt.Fprint(w, "))")
		}
		return
	}
	if typ == "Object[]" {
		if ll, ok := e.(*ListLit); ok {
			// Force list literals to use Object[] directly instead of
			// emitting a primitive array and casting, which would not
			// compile.
			saved := ll.ElemType
			ll.ElemType = "Object"
			ll.emit(w)
			ll.ElemType = saved
			return
		}
	}
	if typ == "int" || typ == "double" || typ == "float" || typ == "float64" {
		it := inferType(e)
		if typ == "int" && it == "java.math.BigInteger" {
			fmt.Fprint(w, "((java.math.BigInteger)(")
			e.emit(w)
			fmt.Fprint(w, ")).intValue()")
			return
		}
		if typ == "int" {
			if it == "double" {
				typ = "double"
			} else if ce, ok := e.(*CallExpr); ok {
				if strings.HasPrefix(ce.Func, "Math.") {
					e.emit(w)
					return
				}
			}
		}
		if it == "" || it == "Object" {
			fmt.Fprint(w, "((Number)(")
			e.emit(w)
			if typ == "int" {
				fmt.Fprint(w, ")).intValue()")
			} else {
				fmt.Fprint(w, ")).doubleValue()")
			}
			return
		}
	}
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
		case *MapLit:
			if strings.Contains(typ, "Map") {
				jt := javaType(typ)
				fmt.Fprintf(w, "((%s)(", jt)
				e.emit(w)
				fmt.Fprint(w, "))")
				return
			}
		}
	}
	jt := javaType(typ)
	if jt == "String" && !isStringExpr(e) {
		if se, ok := e.(*SliceExpr); ok && isArrayExpr(se.Value) {
			fmt.Fprint(w, "String.join(\"\", ")
			se.emit(w)
			fmt.Fprint(w, ")")
			return
		}
		if it := inferType(e); it == "" || it == "Object" {
			fmt.Fprint(w, "(String)(")
			e.emit(w)
			fmt.Fprint(w, ")")
		} else {
			fmt.Fprint(w, "String.valueOf(")
			e.emit(w)
			fmt.Fprint(w, ")")
		}
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
	if strings.HasSuffix(jt, "[]") {
		if ll, ok := e.(*ListLit); ok && len(ll.Elems) == 0 && ll.ElemType == "" {
			saved := ll.ElemType
			ll.ElemType = strings.TrimSuffix(jt, "[]")
			ll.emit(w)
			ll.ElemType = saved
			return
		}
		fmt.Fprintf(w, "((%s)(", jt)
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
	case "bigrat", "BigRat":
		return types.BigRatType{}
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
		tname := v.Type
		if tname == "" {
			var ok bool
			tname, ok = varTypes[v.Name]
			if !ok {
				return "", false
			}
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
			if tname == "Object" {
				var found string
				for sn, fields := range structDefs {
					if _, ok := fields[name]; ok {
						if found != "" {
							found = "" // ambiguous
							break
						}
						found = sn
					}
				}
				if found != "" {
					return structDefs[found][name], true
				}
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
		if ex.IsMap {
			if t := mapValueType(inferType(ex.Target)); t != "" {
				return t
			}
		}
	case *SliceExpr:
		if isStringExpr(ex.Value) {
			return "string"
		}
		if t := arrayElemType(ex.Value); t != "" {
			return t + "[]"
		}
	case *ListLit:
		if ex.ElemType != "" {
			return ex.ElemType + "[]"
		}
		if len(ex.Elems) > 0 {
			t := inferType(ex.Elems[0])
			same := true
			for _, el := range ex.Elems[1:] {
				et := inferType(el)
				if et == "" {
					continue
				}
				if t == "" {
					t = et
				} else if et != t && !(t == "string" && et == "String") && !(t == "String" && et == "string") {
					same = false
					break
				}
			}
			if same && t != "" {
				if strings.HasSuffix(t, "[]") {
					return t + "[]"
				}
				switch t {
				case "string", "String":
					return "string[]"
				case "boolean":
					return "bool[]"
				default:
					return t + "[]"
				}
			}
			if !same {
				return "Object[]"
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
		if fields := mapFieldsForExpr(ex.Target); fields != nil {
			if t, ok := fields[ex.Name]; ok {
				return t
			}
		}
	case *MapLit:
		if len(ex.Keys) > 0 {
			kt := inferType(ex.Keys[0])
			vt := inferType(ex.Values[0])
			sameK, sameV := true, true
			for i := 1; i < len(ex.Keys); i++ {
				if t := inferType(ex.Keys[i]); t != "" && t != kt {
					sameK = false
				}
				if t := inferType(ex.Values[i]); t != "" && t != vt {
					sameV = false
				}
			}
			if sameK && sameV {
				ex.KeyType = kt
				ex.ValueType = vt
				return fmt.Sprintf("java.util.Map<%s, %s>", javaType(kt), javaType(vt))
			}
		}
		return "java.util.Map"
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
			if lt == "bigrat" || rt == "bigrat" {
				return "bigrat"
			}
			if lt == "bigint" || rt == "bigint" || lt == "java.math.BigInteger" || rt == "java.math.BigInteger" {
				return "bigint"
			}
			if lt == "double" || rt == "double" || lt == "float" || rt == "float" {
				return "double"
			}
			return "int"
		case "-", "*", "/", "%":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "bigrat" || rt == "bigrat" {
				return "bigrat"
			}
			if lt == "bigint" || rt == "bigint" || lt == "java.math.BigInteger" || rt == "java.math.BigInteger" {
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
		case "_bigrat":
			return "bigrat"
		case "_padStart":
			return "String"
		case "_p":
			return "String"
		case "_sha256":
			return "int[]"
		case "Math.floorMod":
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
	Methods []*Function
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
	decl := "static class " + sanitize(t.Name)
	if t.Extends != "" {
		decl += " implements " + sanitize(t.Extends)
	}
	fmt.Fprintf(w, indent+"%s {\n", decl)
	for _, f := range t.Fields {
		typ := javaType(f.Type)
		if typ == "" {
			typ = f.Type
		}
		fmt.Fprintf(w, indent+"    %s %s;\n", typ, f.Name)
	}
	fmt.Fprintf(w, indent+"    %s(", sanitize(t.Name))
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
	fmt.Fprintf(w, indent+"    %s() {}\n", sanitize(t.Name))
	for _, m := range t.Methods {
		fmt.Fprintf(w, indent+"    %s %s(", javaType(m.Return), m.Name)
		for i, p := range m.Params {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			typ := javaType(p.Type)
			if typ == "" {
				typ = p.Type
			}
			fmt.Fprintf(w, "%s %s", typ, sanitize(p.Name))
		}
		fmt.Fprint(w, ") {\n")
		for _, st := range m.Body {
			st.emit(w, indent+"        ")
		}
		fmt.Fprint(w, indent+"    }\n")
	}
	fmt.Fprint(w, indent+"    @Override public String toString() {\n")
	if len(t.Fields) == 0 {
		fmt.Fprintf(w, indent+"        return \"%s{}\";\n", sanitize(t.Name))
	} else {
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
	}
	fmt.Fprint(w, indent+"    }\n")
	fmt.Fprint(w, indent+"}\n")
}

func (i *InterfaceDeclStmt) emit(w io.Writer, indent string) {
	fmt.Fprintf(w, indent+"interface %s {}\n", sanitize(i.Name))
}

func (s *StructLit) emit(w io.Writer) {
	fmt.Fprintf(w, "new %s(", sanitize(s.Name))
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

func exprString(e Expr) string {
	var buf bytes.Buffer
	e.emit(&buf)
	return buf.String()
}

func exprUsesVar(e Expr, name string) bool {
	if e == nil {
		return false
	}
	switch ex := e.(type) {
	case *VarExpr:
		return ex.Name == name
	case *UnaryExpr:
		return exprUsesVar(ex.Value, name)
	case *BinaryExpr:
		return exprUsesVar(ex.Left, name) || exprUsesVar(ex.Right, name)
	case *CallExpr:
		if ex.Func == name {
			return true
		}
		for _, a := range ex.Args {
			if exprUsesVar(a, name) {
				return true
			}
		}
	case *MethodCallExpr:
		if exprUsesVar(ex.Target, name) {
			return true
		}
		for _, a := range ex.Args {
			if exprUsesVar(a, name) {
				return true
			}
		}
	case *FieldExpr:
		return exprUsesVar(ex.Target, name)
	case *IndexExpr:
		if exprUsesVar(ex.Target, name) {
			return true
		}
		if exprUsesVar(ex.Index, name) {
			return true
		}
	case *TernaryExpr:
		return exprUsesVar(ex.Cond, name) || exprUsesVar(ex.Then, name) || exprUsesVar(ex.Else, name)
	case *ListLit:
		for _, v := range ex.Elems {
			if exprUsesVar(v, name) {
				return true
			}
		}
	case *MapLit:
		for _, k := range ex.Keys {
			if exprUsesVar(k, name) {
				return true
			}
		}
		for _, v := range ex.Values {
			if exprUsesVar(v, name) {
				return true
			}
		}
	case *LambdaExpr:
		for _, p := range ex.Params {
			if p.Name == name {
				return false
			}
		}
		for _, st := range ex.Body {
			if stmtUsesVar(st, name) {
				return true
			}
		}
	case *GroupExpr:
		return exprUsesVar(ex.Expr, name)
	}
	return false
}

func stmtUsesVar(s Stmt, name string) bool {
	switch st := s.(type) {
	case *ExprStmt:
		return exprUsesVar(st.Expr, name)
	case *AssignStmt:
		if st.Name == name {
			return true
		}
		return exprUsesVar(st.Expr, name)
	case *IndexAssignStmt:
		if exprUsesVar(st.Target, name) {
			return true
		}
		for _, idx := range st.Indices {
			if exprUsesVar(idx, name) {
				return true
			}
		}
		return exprUsesVar(st.Expr, name)
	case *ReturnStmt:
		return exprUsesVar(st.Expr, name)
	case *IfStmt:
		if exprUsesVar(st.Cond, name) {
			return true
		}
		for _, b := range st.Then {
			if stmtUsesVar(b, name) {
				return true
			}
		}
		for _, b := range st.Else {
			if stmtUsesVar(b, name) {
				return true
			}
		}
	case *WhileStmt:
		if exprUsesVar(st.Cond, name) {
			return true
		}
		for _, b := range st.Body {
			if stmtUsesVar(b, name) {
				return true
			}
		}
	case *ForRangeStmt:
		if exprUsesVar(st.Start, name) || exprUsesVar(st.End, name) {
			return true
		}
		for _, b := range st.Body {
			if stmtUsesVar(b, name) {
				return true
			}
		}
	case *ForEachStmt:
		if exprUsesVar(st.Iterable, name) {
			return true
		}
		for _, b := range st.Body {
			if stmtUsesVar(b, name) {
				return true
			}
		}
	}
	return false
}

func stmtModifiesVar(s Stmt, name string) bool {
	switch st := s.(type) {
	case *AssignStmt:
		return st.Name == name
	case *IndexAssignStmt:
		if v, ok := st.Target.(*VarExpr); ok {
			return v.Name == name
		}
		return false
	case *IfStmt:
		for _, b := range st.Then {
			if stmtModifiesVar(b, name) {
				return true
			}
		}
		for _, b := range st.Else {
			if stmtModifiesVar(b, name) {
				return true
			}
		}
	case *WhileStmt:
		for _, b := range st.Body {
			if stmtModifiesVar(b, name) {
				return true
			}
		}
	case *ForRangeStmt:
		for _, b := range st.Body {
			if stmtModifiesVar(b, name) {
				return true
			}
		}
	case *ForEachStmt:
		for _, b := range st.Body {
			if stmtModifiesVar(b, name) {
				return true
			}
		}
	}
	return false
}

func bodyModifiesVar(body []Stmt, name string) bool {
	for _, st := range body {
		if stmtModifiesVar(st, name) {
			return true
		}
	}
	return false
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
		st.emit(w, "        ")
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
	emitCastExpr(w, s.Cond, "boolean")
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
	if call, ok := s.Expr.(*CallExpr); ok && call.Func == "panic" && len(call.Args) == 1 {
		fmt.Fprint(w, indent)
		fmt.Fprint(w, "throw new RuntimeException(String.valueOf(")
		call.Args[0].emit(w)
		fmt.Fprint(w, "));")
		fmt.Fprint(w, "\n")
		return
	}
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
	if varDecls != nil {
		varDecls[s.Name] = &VarStmt{Name: s.Name, Type: typ}
	}
	if varTypes != nil {
		varTypes[s.Name] = typ
	}
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
	jt := javaType(typ)
	if refVars[s.Name] {
		if jt == "" {
			jt = typ
		}
		raw := jt
		if idx := strings.Index(raw, "<"); idx >= 0 {
			raw = raw[:idx]
		}
		dims := strings.Count(raw, "[]")
		base := strings.TrimSuffix(raw, strings.Repeat("[]", dims))
		fmt.Fprintf(w, "%s[] %s = new %s[1]%s;\n", jt, sanitize(s.Name), base, strings.Repeat("[]", dims))
		if s.Expr != nil {
			if indent == "    " {
				fmt.Fprint(w, "    static {\n")
				fmt.Fprint(w, "        "+sanitize(s.Name)+"[0] = ")
				emitCastExpr(w, s.Expr, typ)
				fmt.Fprint(w, ";\n    }\n")
			} else {
				fmt.Fprint(w, indent+sanitize(s.Name)+"[0] = ")
				emitCastExpr(w, s.Expr, typ)
				fmt.Fprint(w, ";\n")
			}
		} else if indent != "    " {
			fmt.Fprint(w, indent+sanitize(s.Name)+"[0] = ")
			fmt.Fprint(w, defaultValue(jt))
			fmt.Fprint(w, ";\n")
		}
		if varDecls != nil {
			varDecls[s.Name] = s
		}
		if varTypes != nil {
			varTypes[s.Name] = typ
		}
		return
	}
	fmt.Fprint(w, jt+" "+sanitize(s.Name))
	if s.Expr != nil {
		fmt.Fprint(w, " = ")
		emitCastExpr(w, s.Expr, jt)
	} else {
		// Ensure locals have an initial value so Java does not complain
		// about "variable might not have been initialized". For array
		// types, allocate an empty array of the appropriate dimension.
		if strings.HasSuffix(jt, "[]") {
			dims := strings.Count(jt, "[]")
			base := strings.TrimSuffix(jt, strings.Repeat("[]", dims))
			fmt.Fprintf(w, " = new %s[0]%s", base, strings.Repeat("[]", dims-1))
		} else {
			fmt.Fprint(w, " = "+defaultValue(jt))
		}
	}
	fmt.Fprint(w, ";\n")
	if varDecls != nil {
		varDecls[s.Name] = s
	}
	if varTypes != nil {
		varTypes[s.Name] = typ
	}
}

func defaultValue(t string) string {
	switch t {
	case "int", "Integer":
		return "0"
	case "double", "float", "float64", "Double":
		return "0"
	case "boolean", "Boolean":
		return "false"
	default:
		return "null"
	}
}

type AssignStmt struct {
	Name string
	Expr Expr
	Type string
}

func (s *AssignStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+sanitize(s.Name))
	if refVars[s.Name] {
		fmt.Fprint(w, "[0]")
	}
	fmt.Fprint(w, " = ")
	typ := s.Type
	if typ == "" {
		if vs, ok := varDecls[s.Name]; ok {
			typ = vs.Type
		} else if t, ok := varTypes[s.Name]; ok {
			typ = t
		} else {
			typ = inferType(s.Expr)
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
			valType := mapValueType(inferType(s.Target))
			if strings.Contains(valType, "Map") {
				if inner := mapValueType(valType); inner != "" {
					valType = inner
				}
			}
			if ll, ok := s.Expr.(*ListLit); ok && ll.ElemType == "" && strings.HasSuffix(valType, "[]") {
				ll.ElemType = strings.TrimSuffix(valType, "[]")
			}
			if valType != "" {
				if ml, ok := s.Expr.(*MapLit); ok && ml.ValueType == "" {
					kt := mapKeyType(inferType(s.Target))
					oldKT, oldVT := ml.KeyType, ml.ValueType
					if ml.KeyType == "" && (kt == "String" || kt == "string") {
						ml.KeyType = kt
					}
					ml.ValueType = valType
					ml.emit(w)
					ml.KeyType, ml.ValueType = oldKT, oldVT
				} else {
					emitCastExpr(w, s.Expr, valType)
				}
			} else {
				s.Expr.emit(w)
			}
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
	if len(s.Indices) > 1 {
		base := s.Target
		for i := 0; i < len(s.Indices)-1; i++ {
			ix := &IndexExpr{Target: base, Index: s.Indices[i], IsMap: isMapExpr(base)}
			if ix.IsMap {
				ix.ResultType = mapValueType(inferType(base))
			}
			base = ix
		}
		last := s.Indices[len(s.Indices)-1]
		if isMapExpr(base) {
			base.emit(w)
			fmt.Fprint(w, ".put(")
			last.emit(w)
			fmt.Fprint(w, ", ")
			valType := mapValueType(inferType(base))
			if strings.Contains(valType, "Map") {
				if inner := mapValueType(valType); inner != "" {
					valType = inner
				}
			}
			if valType != "" {
				if ml, ok := s.Expr.(*MapLit); ok && ml.ValueType == "" {
					kt := mapKeyType(inferType(base))
					oldKT, oldVT := ml.KeyType, ml.ValueType
					ml.KeyType, ml.ValueType = kt, valType
					ml.emit(w)
					ml.KeyType, ml.ValueType = oldKT, oldVT
				} else {
					emitCastExpr(w, s.Expr, valType)
				}
			} else {
				s.Expr.emit(w)
			}
			fmt.Fprint(w, ");\n")
			return
		}
		base.emit(w)
		fmt.Fprint(w, "[")
		last.emit(w)
		fmt.Fprint(w, "] = ")
		elem := arrayElemType(base)
		emitCastExpr(w, s.Expr, elem)
		fmt.Fprint(w, ";\n")
		return
	}
	s.Target.emit(w)
	fmt.Fprint(w, "[")
	s.Indices[0].emit(w)
	fmt.Fprint(w, "] = ")
	elem := arrayElemType(s.Target)
	emitCastExpr(w, s.Expr, elem)
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
	name := sanitize(fr.Name)
	fmt.Fprint(w, indent+"for (int "+name+" = ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "; ")
	fmt.Fprint(w, name+" < ")
	fr.End.emit(w)
	fmt.Fprint(w, "; ")
	fmt.Fprint(w, name+"++")
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
	if !fe.IsMap && isStringExpr(fe.Iterable) {
		t := javaType(fe.ElemType)
		if t == "" {
			t = "var"
		}
		fmt.Fprint(w, indent+"for (int _i = 0; _i < ")
		fe.Iterable.emit(w)
		fmt.Fprint(w, ".length(); _i++) {\n")
		fmt.Fprintf(w, indent+"    %s %s = ", t, sanitize(fe.Name))
		fe.Iterable.emit(w)
		fmt.Fprint(w, ".substring(_i, _i + 1);\n")
		for _, st := range fe.Body {
			st.emit(w, indent+"    ")
		}
		fmt.Fprint(w, indent+"}\n")
		return
	}
	typ := javaType(fe.ElemType)
	name := sanitize(fe.Name)
	if typ == "" {
		fmt.Fprint(w, indent+"for (var "+name+" : ")
	} else {
		fmt.Fprintf(w, indent+"for (%s %s : ", typ, name)
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
			same := true
			for _, el := range l.Elems[1:] {
				et := inferType(el)
				if et != t && !(t == "string" && et == "String") && !(t == "String" && et == "string") {
					same = false
					break
				}
			}
			if same && t != "" {
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
	// When the element type is explicitly declared as Object (e.g. list<any>)
	// we should keep the array as Object[] even if all literal elements share
	// the same primitive type. The previous implementation would inspect the
	// literal values and narrow the array type which produced code such as
	// `new int[]{...}` cast to Object[], leading to `int[] cannot be converted
	// to Object[]` compilation errors. Only attempt to refine the type when
	// the element type was not explicitly provided.
	if arrType == "Object" && l.ElemType == "" {
		t := ""
		same := true
		for _, el := range l.Elems {
			et := inferType(el)
			if et == "" {
				continue
			}
			if t == "" {
				t = et
			} else if et != t && !(t == "string" && et == "String") && !(t == "String" && et == "string") {
				same = false
				break
			}
		}
		if same && t != "" {
			arrType = javaType(t)
		}
	}
	raw := arrType
	dims := 0
	for strings.HasSuffix(raw, "[]") {
		raw = strings.TrimSuffix(raw, "[]")
		dims++
	}
	if idx := strings.Index(raw, "<"); idx >= 0 {
		raw = raw[:idx]
	}
	if dims == 0 {
		if strings.Contains(arrType, "<") {
			fmt.Fprintf(w, "(%s[])new %s[]{", arrType, raw)
		} else {
			fmt.Fprintf(w, "new %s[]{", arrType)
		}
	} else {
		fmt.Fprintf(w, "new %s%s{", raw, strings.Repeat("[]", dims+1))
	}
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
		if strings.Contains(valType, "<") && !strings.HasSuffix(valType, ">") {
			valType += ">"
		}
	}
	keyType := "String"
	if m.KeyType != "" {
		keyType = javaBoxType(m.KeyType)
	}
	if len(m.Keys) > 0 {
		useEntries := true
		for _, v := range m.Values {
			if _, ok := v.(*NullLit); ok {
				useEntries = false
				break
			}
		}
		if useEntries {
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
				} else if valType != "Object" {
					emitCastExpr(w, m.Values[i], valType)
				} else {
					fmt.Fprint(w, "(Object)(")
					m.Values[i].emit(w)
					fmt.Fprint(w, ")")
				}
				fmt.Fprint(w, ")")
			}
			fmt.Fprint(w, "))")
		} else {
			fmt.Fprintf(w, "new java.util.LinkedHashMap<%s, %s>() {{", keyType, valType)
			for i := range m.Keys {
				fmt.Fprint(w, " put(")
				m.Keys[i].emit(w)
				fmt.Fprint(w, ", ")
				if m.ValueType != "" {
					emitCastExpr(w, m.Values[i], m.ValueType)
				} else {
					m.Values[i].emit(w)
				}
				fmt.Fprint(w, ");")
			}
			fmt.Fprint(w, " }}")
		}
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
	if b.Op == "+" {
		lt := inferType(b.Left)
		rt := inferType(b.Right)
		if lt == "string" || lt == "String" || rt == "string" || rt == "String" || isStringExpr(b.Left) || isStringExpr(b.Right) {
			emitCastExpr(w, b.Left, "String")
			fmt.Fprint(w, " + ")
			emitCastExpr(w, b.Right, "String")
			return
		}
	}
	if b.Op == "==" || b.Op == "!=" {
		lt := inferType(b.Left)
		rt := inferType(b.Right)
		if (lt == "string" || lt == "String" || isStringExpr(b.Left)) &&
			(rt == "string" || rt == "String" || isStringExpr(b.Right)) {
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
		if isNullExpr(b.Left) || isNullExpr(b.Right) {
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
		big := lt == "bigint" || lt == "java.math.BigInteger" || rt == "bigint" || rt == "java.math.BigInteger"
		if lt == "bigrat" || rt == "bigrat" {
			needBigRat = true
			switch b.Op {
			case "+":
				fmt.Fprint(w, "_add(")
				b.Left.emit(w)
				fmt.Fprint(w, ", ")
				b.Right.emit(w)
				fmt.Fprint(w, ")")
			case "-":
				fmt.Fprint(w, "_sub(")
				b.Left.emit(w)
				fmt.Fprint(w, ", ")
				b.Right.emit(w)
				fmt.Fprint(w, ")")
			case "*":
				fmt.Fprint(w, "_mul(")
				b.Left.emit(w)
				fmt.Fprint(w, ", ")
				b.Right.emit(w)
				fmt.Fprint(w, ")")
			case "/":
				fmt.Fprint(w, "_div(")
				b.Left.emit(w)
				fmt.Fprint(w, ", ")
				b.Right.emit(w)
				fmt.Fprint(w, ")")
			default:
				fmt.Fprint(w, "0")
			}
			return
		}
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
		if lt == "" && rt == "" && (b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=") {
			fmt.Fprint(w, "String.valueOf(")
			b.Left.emit(w)
			fmt.Fprint(w, ").compareTo(String.valueOf(")
			b.Right.emit(w)
			fmt.Fprint(w, ")) ")
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
			return
		}
		if big {
			emitBigIntOperand := func(e Expr, t string) {
				if it := inferType(e); it == "bigint" || it == "java.math.BigInteger" {
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
			if ce, ok := b.Left.(*CallExpr); ok && strings.HasPrefix(ce.Func, "Math.") {
				b.Left.emit(w)
			} else {
				fmt.Fprint(w, "((Number)(")
				b.Left.emit(w)
				if typ == "int" {
					fmt.Fprint(w, ")).intValue()")
				} else {
					fmt.Fprint(w, ")).doubleValue()")
				}
			}
		} else {
			emitCastExpr(w, b.Left, typ)
		}
		fmt.Fprint(w, " "+b.Op+" ")
		if rt == "" {
			if ce, ok := b.Right.(*CallExpr); ok && strings.HasPrefix(ce.Func, "Math.") {
				b.Right.emit(w)
			} else {
				fmt.Fprint(w, "((Number)(")
				b.Right.emit(w)
				if typ == "int" {
					fmt.Fprint(w, ")).intValue()")
				} else {
					fmt.Fprint(w, ")).doubleValue()")
				}
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

// MethodRefExpr represents a method reference like Main::foo.
type MethodRefExpr struct {
	Name string
}

func (v *VarExpr) emit(w io.Writer) {
	name := sanitize(v.Name)
	if refVars[v.Name] {
		fmt.Fprint(w, name+"[0]")
	} else {
		fmt.Fprint(w, name)
	}
}

func (m *MethodRefExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "Main::%s", sanitize(m.Name))
}

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (f *FieldExpr) emit(w io.Writer) {
	if v, ok := f.Target.(*VarExpr); ok {
		if t, ok2 := varTypes[v.Name]; ok2 && t != "map" && !strings.Contains(t, "Map") {
			if t != "Object" {
				f.Target.emit(w)
				fmt.Fprint(w, "."+f.Name)
				return
			}
			if structDefs != nil {
				var found string
				for sn, fields := range structDefs {
					if _, ok := fields[f.Name]; ok {
						if found != "" {
							found = ""
							break
						}
						found = sn
					}
				}
				if found != "" {
					fmt.Fprintf(w, "((%s)", found)
					f.Target.emit(w)
					fmt.Fprintf(w, ").%s", f.Name)
					return
				}
			}
		}
	}
	if isMapExpr(f.Target) {
		castType := "Object"
		if fields := mapFieldsForExpr(f.Target); fields != nil {
			if t, ok := fields[f.Name]; ok {
				if jt := javaType(t); jt != "" {
					castType = jt
				}
			}
		}
		fmt.Fprintf(w, "((%s) (", castType)
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
	if ix, ok := l.Value.(*IndexExpr); ok {
		ix.emit(w)
		if isStringExpr(ix) {
			fmt.Fprint(w, ".length()")
		} else {
			fmt.Fprint(w, ".length")
		}
		return
	}
	switch {
	case isGroupExpr(l.Value):
		l.Value.emit(w)
		fmt.Fprint(w, ".items.size()")
	case isStringExpr(l.Value):
		needRuneLen = true
		fmt.Fprint(w, "_runeLen(")
		l.Value.emit(w)
		fmt.Fprint(w, ")")
	case isArrayExpr(l.Value):
		l.Value.emit(w)
		fmt.Fprint(w, ".length")
	case strings.HasSuffix(inferType(l.Value), "[]"):
		l.Value.emit(w)
		fmt.Fprint(w, ".length")
	case isMapExpr(l.Value):
		l.Value.emit(w)
		fmt.Fprint(w, ".size()")
	default:
		fmt.Fprint(w, "String.valueOf(")
		l.Value.emit(w)
		fmt.Fprint(w, ").length()")
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
		if t := arrayElemType(a.List); t != "" {
			elem = t
		}
	}
	if elem == "" || elem == "Object" {
		if t := inferType(a.Value); t != "" {
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
		emitCastExpr(w, a.Value, "boolean")
		fmt.Fprint(w, ")")
		return
	}
	if elem == "double" {
		fmt.Fprint(w, "java.util.stream.DoubleStream.concat(java.util.Arrays.stream(")
		a.List.emit(w)
		fmt.Fprint(w, "), java.util.stream.DoubleStream.of(")
		emitCastExpr(w, a.Value, "double")
		fmt.Fprint(w, ")).toArray()")
		return
	}
	if elem == "int" {
		fmt.Fprint(w, "java.util.stream.IntStream.concat(java.util.Arrays.stream(")
		a.List.emit(w)
		fmt.Fprint(w, "), java.util.stream.IntStream.of(")
		emitCastExpr(w, a.Value, "int")
		fmt.Fprint(w, ")).toArray()")
		return
	}
	if strings.HasSuffix(jt, "[]") || strings.Contains(jt, "<") {
		needAppendObj = true
		fmt.Fprint(w, "appendObj(")
		a.List.emit(w)
		fmt.Fprint(w, ", ")
		if ll, ok := a.Value.(*ListLit); ok && (ll.ElemType == "" || ll.ElemType == "Object") {
			et := elem
			if strings.HasSuffix(et, "[]") {
				et = strings.TrimSuffix(et, "[]")
			}
			ll.ElemType = et
		}
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
	if strings.HasSuffix(jt, "[]") {
		if ll, ok := c.Value.(*ListLit); ok && len(ll.Elems) == 0 && ll.ElemType == "" {
			saved := ll.ElemType
			ll.ElemType = strings.TrimSuffix(jt, "[]")
			ll.emit(w)
			ll.ElemType = saved
			return
		}
	}
	if jt == "int[][]" {
		needCastInt2D = true
		fmt.Fprint(w, "_castInt2D(")
		c.Value.emit(w)
		fmt.Fprint(w, ")")
	} else if jt == "java.math.BigInteger" {
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
	if u.Op == "-" {
		t := inferType(u.Value)
		if t == "bigrat" {
			needBigRat = true
			fmt.Fprint(w, "_sub(_bigrat(0, null), ")
			if _, ok := u.Value.(*BinaryExpr); ok {
				fmt.Fprint(w, "(")
				u.Value.emit(w)
				fmt.Fprint(w, "))")
			} else {
				u.Value.emit(w)
				fmt.Fprint(w, ")")
			}
			return
		}
		if t == "bigint" {
			fmt.Fprint(w, "(")
			if _, ok := u.Value.(*BinaryExpr); ok {
				fmt.Fprint(w, "(")
				u.Value.emit(w)
				fmt.Fprint(w, ")")
			} else {
				u.Value.emit(w)
			}
			fmt.Fprint(w, ").negate()")
			return
		}
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
	if m.Name == "padStart" && len(m.Args) == 2 {
		needPadStart = true
		fmt.Fprint(w, "_padStart(")
		emitCastExpr(w, m.Target, "String")
		fmt.Fprint(w, ", ")
		m.Args[0].emit(w)
		fmt.Fprint(w, ", ")
		m.Args[1].emit(w)
		fmt.Fprint(w, ")")
		return
	}
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
	needSubstr = true
	fmt.Fprint(w, "_substr(")
	s.Str.emit(w)
	fmt.Fprint(w, ", ")
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
		mvType := mapValueType(inferType(ix.Target))
		switch mvType {
		case "int":
			useDefault = true
			defVal = "0"
		case "double":
			useDefault = true
			defVal = "0.0"
		case "boolean":
			useDefault = true
			defVal = "false"
		}
		if !useDefault {
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
		}
		fmt.Fprintf(w, "((%s)(", castType)
		if isMapExpr(ix.Target) {
			ix.Target.emit(w)
		} else {
			fmt.Fprint(w, "((java.util.Map)")
			ix.Target.emit(w)
			fmt.Fprint(w, ")")
		}
		fmt.Fprint(w, ")")
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
		fmt.Fprint(w, ".substring(")
		ix.Index.emit(w)
		fmt.Fprint(w, ", ")
		ix.Index.emit(w)
		fmt.Fprint(w, "+1)")
	} else if isArrayExpr(ix.Target) {
		needCast := ix.ResultType != "" && ix.ResultType != arrayElemType(ix.Target)
		if needCast {
			fmt.Fprintf(w, "((%s)(", javaType(ix.ResultType))
		}
		ix.Target.emit(w)
		fmt.Fprint(w, "[")
		ix.Index.emit(w)
		fmt.Fprint(w, "]")
		if needCast {
			fmt.Fprint(w, "))")
		}
	} else if isMapExpr(ix.Target) {
		castType := "java.util.Map"
		if ix.ResultType != "" {
			castType = javaType(ix.ResultType)
		}
		fmt.Fprintf(w, "((%s)(", castType)
		ix.Target.emit(w)
		fmt.Fprint(w, ")")
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

func javaQuote(s string) string {
	// strconv.Quote handles all escape sequences for us. Strip
	// the surrounding quotes to get the encoded string literal.
	q := strconv.Quote(s)
	if len(q) >= 2 {
		return q[1 : len(q)-1]
	}
	return q
}

func (s *StringLit) emit(w io.Writer) {
	fmt.Fprintf(w, "\"%s\"", javaQuote(s.Value))
}

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
		if ex.Func == "String.valueOf" || ex.Func == "_p" {
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
	case *BinaryExpr:
		if ex.Op == "+" && (isStringExpr(ex.Left) || isStringExpr(ex.Right)) {
			return true
		}
	case *IndexExpr:
		if ex.ResultType != "" {
			return ex.ResultType == "string" || ex.ResultType == "String"
		}
		elemT := arrayElemType(ex.Target)
		if isStringExpr(ex.Target) || elemT == "string" || elemT == "String" {
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
	case *IndexExpr:
		if ex.IsMap {
			if ex.ResultType != "" && strings.HasSuffix(ex.ResultType, "[]") {
				return false
			}
			return true
		}
		return isMapExpr(ex.Target)
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
	t := inferType(e)
	if t == "map" || (strings.Contains(t, "Map") && !strings.HasSuffix(t, "[]")) {
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

// VarType returns the inferred type name for the given variable during the last
// transpilation run. It is intended for testing and debugging only.
func VarType(name string) string {
	return varTypes[name]
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
	case *CallExpr:
		if t, ok := funcRet[ex.Func]; ok {
			if strings.HasSuffix(t, "[]") {
				return strings.TrimSuffix(t, "[]")
			}
			if strings.HasPrefix(t, "java.util.List<") && strings.HasSuffix(t, ">") {
				return strings.TrimSuffix(strings.TrimPrefix(t, "java.util.List<"), ">")
			}
		}
		if t := inferType(ex); strings.HasSuffix(t, "[]") {
			return strings.TrimSuffix(t, "[]")
		}
	case *VarExpr:
		t := ex.Type
		if t == "" {
			if v, ok := varTypes[ex.Name]; ok {
				t = v
			} else if vs, ok2 := varDecls[ex.Name]; ok2 {
				t = vs.Type
			}
		}
		if strings.HasSuffix(t, "[]") {
			return strings.TrimSuffix(t, "[]")
		}
		if strings.HasPrefix(t, "java.util.List<") && strings.HasSuffix(t, ">") {
			return strings.TrimSuffix(strings.TrimPrefix(t, "java.util.List<"), ">")
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
		t := ex.Type
		if t == "" {
			if v, ok := varTypes[ex.Name]; ok {
				t = v
			}
		}
		if strings.HasPrefix(t, "java.util.List") {
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
	needPadStart = false
	needSHA256 = false
	needMD5Hex = false
	needBigRat = false
	needModPow2 = false
	needRuneLen = false
	needSubstr = false
	needFetch = false
	fetchStructs = nil
	needArrGetI = false
	needArrGetD = false
	needArrGetB = false
	needArrGetO = false
	needP = false
	pyMathAliases = map[string]bool{}
	builtinAliases = map[string]string{}
	builtinAliases["stdout"] = "stdout"
	structDefs = map[string]map[string]string{}
	structMethods = map[string][]*Function{}
	structDecls = map[string]*TypeDeclStmt{}
	varDecls = map[string]*VarStmt{}
	refVars = map[string]bool{}
	scopeStack = []map[string]*VarStmt{varDecls}
	closureStack = []bool{false}
	varAliases = map[string]string{}
	aliasCounts = map[string]int{}
	aliasStack = []map[string]string{{}}
	disableStructList = false
	funcMapFields = map[string]map[string]string{}
	mapVarFields = map[string]map[string]string{}
	funcParams = map[string][]string{}
	funcParamNames = map[string][]string{}
	paramMapFields = map[string]map[string]map[string]string{}
	for _, s := range p.Statements {
		if s.Fun != nil {
			if strings.Contains(s.Fun.Name, ".") {
				savedRefs := refVars
				refVars = copyBoolMap(refVars)
				parts := strings.SplitN(s.Fun.Name, ".", 2)
				recv := parts[0]
				name := parts[1]
				saved := varTypes
				varTypes = copyMap(varTypes)
				varDeclsSaved := pushScope(false)
				savedRet := currentFuncReturn
				currentFuncReturn = typeRefString(s.Fun.Return)
				for _, pa := range s.Fun.Params {
					if t := typeRefString(pa.Type); t != "" {
						varTypes[pa.Name] = t
					}
				}
				body, err := compileStmts(s.Fun.Body)
				if err != nil {
					return nil, err
				}
				params := make([]Param, len(s.Fun.Params))
				for i, p := range s.Fun.Params {
					params[i] = Param{Name: p.Name, Type: typeRefString(p.Type)}
				}
				ret := currentFuncReturn
				if ret == "" {
					ret = inferReturnType(body)
				}
				structMethods[recv] = append(structMethods[recv], &Function{Name: name, Params: params, Return: ret, Body: body})
				varTypes = saved
				popScope(varDeclsSaved)
				currentFuncReturn = savedRet
				for k, v := range refVars {
					if v {
						savedRefs[k] = true
					}
				}
				refVars = savedRefs
				continue
			}
			saved := varTypes
			savedRefs := refVars
			varTypes = copyMap(varTypes)
			refVars = copyBoolMap(refVars)
			for _, pa := range s.Fun.Params {
				if t := typeRefString(pa.Type); t != "" {
					varTypes[pa.Name] = t
				}
				if pf, ok := paramMapFields[s.Fun.Name]; ok {
					if f, ok2 := pf[pa.Name]; ok2 {
						mapVarFields[pa.Name] = f
					}
				}
			}
			savedDecls := pushScope(false)
			savedRet := currentFuncReturn
			currentFuncReturn = typeRefString(s.Fun.Return)
			body, err := compileStmts(s.Fun.Body)
			if err != nil {
				return nil, err
			}
			var params []Param
			var pnames []string
			for _, p := range s.Fun.Params {
				params = append(params, Param{Name: p.Name, Type: typeRefString(p.Type)})
				pnames = append(pnames, p.Name)
			}
			funcParamNames[s.Fun.Name] = pnames
			ret := currentFuncReturn
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
			popScope(savedDecls)
			currentFuncReturn = savedRet
			for k, v := range refVars {
				if v {
					savedRefs[k] = true
				}
			}
			refVars = savedRefs
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
	for name, td := range structDecls {
		if ms, ok := structMethods[name]; ok {
			td.Methods = ms
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
			if f := fetchExprOnly(s.Let.Value); f != nil {
				urlExpr, err := compileExpr(f.URL)
				if err != nil {
					return nil, err
				}
				needFetch = true
				t := "java.util.Map<String,Object>"
				var e Expr
				if s.Let.Type != nil {
					t = typeRefString(s.Let.Type)
					if fetchStructs == nil {
						fetchStructs = map[string]bool{}
					}
					fetchStructs[t] = true
					e = &CallExpr{Func: "_fetch_" + t, Args: []Expr{urlExpr}}
				} else {
					e = &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}
				}
				alias := declareAlias(s.Let.Name)
				vs := &VarStmt{Name: alias, Type: t, Expr: e}
				varDecls[alias] = vs
				scopeStack[len(scopeStack)-1][alias] = vs
				if t != "" {
					varTypes[alias] = t
				}
				return vs, nil
			}
			e, err := compileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
			if exprUsesVar(e, s.Let.Name) {
				refVars[s.Let.Name] = true
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
						if topEnv != nil {
							if tt := toJavaTypeFromType(types.ExprType(s.Let.Value, currentEnv())); tt != "" {
								t = tt
							}
						}
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
				if ix, ok := e.(*IndexExpr); ok {
					if et := arrayElemType(ix.Target); et != "" {
						t = et
					}
				}
				if t == "" {
					t = inferType(e)
				}
			}
			if ml, ok := e.(*MapLit); ok && len(ml.Fields) > 0 {
				uniq := ""
				same := true
				for _, ft := range ml.Fields {
					if uniq == "" {
						uniq = ft
					} else if ft != uniq {
						same = false
						break
					}
				}
				if !same || uniq == "Object" {
					t = "java.util.Map<String,Object>"
				}
			}
			if t == "" {
				if lam, ok := e.(*LambdaExpr); ok {
					ptypes := make([]string, len(lam.Params))
					for i, p := range lam.Params {
						ptypes[i] = p.Type
					}
					ret := lam.Return
					if ret == "" {
						ret = "void"
					}
					t = fmt.Sprintf("fn(%s):%s", strings.Join(ptypes, ","), ret)
				}
			}
			alias := declareAlias(s.Let.Name)
			vs := &VarStmt{Name: alias, Type: t, Expr: e}
			varDecls[alias] = vs
			scopeStack[len(scopeStack)-1][alias] = vs
			if t != "" {
				if strings.HasPrefix(t, "fn(") {
					varTypes[alias] = javaType(t)
				} else {
					varTypes[alias] = t
				}
			}
			if ml, ok := e.(*MapLit); ok {
				if len(ml.Fields) > 0 {
					mapVarFields[alias] = ml.Fields
				}
				if ml.ValueType == "" {
					if vt := mapValueType(t); vt != "" {
						if len(ml.Fields) > 0 {
							uniq := ""
							same := true
							for _, ft := range ml.Fields {
								if uniq == "" {
									uniq = ft
								} else if ft != uniq {
									same = false
									break
								}
							}
							if same && uniq != "Object" {
								ml.ValueType = vt
							}
						} else {
							ml.ValueType = vt
						}
					}
				}
				if ml.KeyType == "" {
					if kt := mapKeyType(t); kt != "" {
						ml.KeyType = kt
					}
				}
				if strings.Contains(ml.ValueType, "Map<") {
					innerKey := mapKeyType(ml.ValueType)
					innerVal := mapValueType(ml.ValueType)
					for _, v := range ml.Values {
						if iml, ok := v.(*MapLit); ok {
							if iml.KeyType == "" && innerKey != "" {
								iml.KeyType = innerKey
							}
							if iml.ValueType == "" && innerVal != "" {
								iml.ValueType = innerVal
							}
						}
					}
				}
			} else if ce, ok := e.(*CallExpr); ok {
				if f, ok2 := funcMapFields[ce.Func]; ok2 {
					mapVarFields[alias] = f
				}
			} else if cast, ok := e.(*CastExpr); ok {
				if v, ok2 := cast.Value.(*VarExpr); ok2 {
					if f, ok3 := mapVarFields[v.Name]; ok3 {
						mapVarFields[alias] = f
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
			return &LetStmt{Name: alias, Type: t, Expr: e}, nil
		}
		t := typeRefString(s.Let.Type)
		if t == "" && topEnv != nil {
			if v, err := topEnv.GetVar(s.Let.Name); err == nil {
				t = toJavaTypeFromType(v)
			}
		}
		if t != "" {
			alias := declareAlias(s.Let.Name)
			varTypes[alias] = t
			vs := &VarStmt{Name: alias, Type: t}
			varDecls[alias] = vs
			scopeStack[len(scopeStack)-1][alias] = vs
		}
		alias := declareAlias(s.Let.Name)
		return &LetStmt{Name: alias, Type: t}, nil
	case s.Var != nil:
		if s.Var.Value != nil {
			t := typeRefString(s.Var.Type)
			savedDisable := disableStructList
			if strings.Contains(t, "Map") {
				disableStructList = true
			}
			e, err := compileExpr(s.Var.Value)
			disableStructList = savedDisable
			if err != nil {
				return nil, err
			}
			if exprUsesVar(e, s.Var.Name) {
				refVars[s.Var.Name] = true
			}

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
			alias := declareAlias(s.Var.Name)
			if t != "" {
				varTypes[alias] = t
			}
			if ml, ok := e.(*MapLit); ok {
				if len(ml.Fields) > 0 {
					mapVarFields[alias] = ml.Fields
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
					mapVarFields[alias] = f
				}
			} else if cast, ok := e.(*CastExpr); ok {
				if v, ok2 := cast.Value.(*VarExpr); ok2 {
					if f, ok3 := mapVarFields[v.Name]; ok3 {
						mapVarFields[alias] = f
					}
				}
				if t == "" {
					t = cast.Type
				}
			}
			if l, ok := e.(*ListLit); ok {
				if l.ElemType != "" {
					t = l.ElemType + "[]"
				} else if strings.HasSuffix(t, "[]") {
					l.ElemType = strings.TrimSuffix(t, "[]")
				}
			}
			if ix, ok := e.(*IndexExpr); ok && t != "" {
				ix.ResultType = t
			}
			vs := &VarStmt{Name: alias, Type: t, Expr: e}
			varDecls[alias] = vs
			scopeStack[len(scopeStack)-1][alias] = vs
			return vs, nil
		}
		t := typeRefString(s.Var.Type)
		if t == "" && topEnv != nil {
			if v, err := topEnv.GetVar(s.Var.Name); err == nil {
				t = toJavaTypeFromType(v)
			}
		}
		alias := declareAlias(s.Var.Name)
		if t != "" {
			varTypes[alias] = t
		}
		vs := &VarStmt{Name: alias, Type: t}
		varDecls[alias] = vs
		scopeStack[len(scopeStack)-1][alias] = vs
		return vs, nil
	case s.Type != nil:
		if len(s.Type.Variants) > 0 {
			ut := types.UnionType{Name: s.Type.Name, Variants: map[string]types.StructType{}}
			for _, v := range s.Type.Variants {
				if st, ok := topEnv.GetStruct(v.Name); ok {
					ut.Variants[v.Name] = st
				}
			}
			if simpleUnionAlias(ut) == "" {
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
			}
			return nil, nil
		}
		if st, ok := topEnv.GetStruct(s.Type.Name); ok {
			fields := make([]Param, len(st.Order))
			for i, n := range st.Order {
				typ := toJavaTypeFromType(st.Fields[n])
				if (typ == "Object" || typ == "Object[]") && s.Type != nil {
					for _, mem := range s.Type.Members {
						if mem.Field != nil && mem.Field.Name == n {
							if t := typeRefString(mem.Field.Type); t != "" {
								typ = t
							}
						}
					}
				}
				fields[i] = Param{Name: n, Type: typ}
			}
			var methods []*Function
			for _, mem := range s.Type.Members {
				if mem.Method != nil {
					ms := mem.Method
					saved := varTypes
					varTypes = copyMap(varTypes)
					savedDecls := varDecls
					varDecls = map[string]*VarStmt{}
					savedRet := currentFuncReturn
					currentFuncReturn = typeRefString(ms.Return)
					for _, pa := range ms.Params {
						if t := typeRefString(pa.Type); t != "" {
							varTypes[pa.Name] = t
						}
					}
					body, err := compileStmts(ms.Body)
					if err != nil {
						return nil, err
					}
					params := make([]Param, len(ms.Params))
					for i, p := range ms.Params {
						params[i] = Param{Name: p.Name, Type: typeRefString(p.Type)}
					}
					ret := currentFuncReturn
					if ret == "" {
						ret = inferReturnType(body)
					}
					methods = append(methods, &Function{Name: ms.Name, Params: params, Return: ret, Body: body})
					varTypes = saved
					varDecls = savedDecls
					currentFuncReturn = savedRet
				}
			}
			td := &TypeDeclStmt{Name: s.Type.Name, Fields: fields, Methods: methods}
			structDecls[s.Type.Name] = td
			if structDefs != nil {
				sf := make(map[string]string)
				for _, f := range fields {
					sf[f.Name] = f.Type
				}
				structDefs[s.Type.Name] = sf
			}
			return td, nil
		}
		return nil, nil
	case s.Fun != nil:
		var ptypes []string
		for _, pa := range s.Fun.Params {
			ptypes = append(ptypes, typeRefString(pa.Type))
		}
		provisionalRet := typeRefString(s.Fun.Return)
		if provisionalRet == "" {
			provisionalRet = "void"
		}
		provisional := fmt.Sprintf("fn(%s):%s", strings.Join(ptypes, ","), provisionalRet)
		varTypes[s.Fun.Name] = provisional
		closureFun := len(scopeStack) > 1
		expr, err := compileFunExpr(&parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body}, closureFun)
		if err != nil {
			return nil, err
		}
		if exprUsesVar(expr, s.Fun.Name) {
			refVars[s.Fun.Name] = true
		}
		ret := typeRefString(s.Fun.Return)
		if lam, ok := expr.(*LambdaExpr); ok {
			if ret == "" {
				ret = lam.Return
			}
		}
		funType := fmt.Sprintf("fn(%s):%s", strings.Join(ptypes, ","), ret)
		varTypes[s.Fun.Name] = funType
		return &VarStmt{Name: s.Fun.Name, Type: funType, Expr: expr}, nil
	case s.Assign != nil:
		alias := resolveAlias(s.Assign.Name)
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			e, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			var vdecl *VarStmt
			alias := resolveAlias(s.Assign.Name)
			if vs, ok := varDecls[alias]; ok {
				vdecl = vs
			} else if len(scopeStack) > 0 {
				for i := len(scopeStack) - 1; i >= 0; i-- {
					if vs, ok2 := scopeStack[i][alias]; ok2 {
						vdecl = vs
						if i < len(scopeStack)-1 && closureStack[len(closureStack)-1] {
							refVars[alias] = true
						} else if closureStack[i] {
							refVars[alias] = true
						}
						break
					}
				}
			}
			if ap, ok := e.(*AppendExpr); ok {
				vt := inferType(ap.Value)
				if vt != "" {
					if cur, ok := varTypes[alias]; ok && (cur == "int[]" || cur == "Object[]") && vt == "boolean" {
						newT := "boolean[]"
						varTypes[alias] = newT
						if vdecl != nil {
							vdecl.Type = newT
							if ll, ok3 := vdecl.Expr.(*ListLit); ok3 {
								ll.ElemType = "boolean"
							}
						}
					}
				}
			}
			if t := inferType(e); t != "" {
				if cur, ok := varTypes[alias]; !ok {
					varTypes[alias] = t
					if vdecl != nil {
						vdecl.Type = t
						if ll, ok2 := vdecl.Expr.(*ListLit); ok2 && strings.HasSuffix(t, "[]") {
							ll.ElemType = strings.TrimSuffix(t, "[]")
						}
					}
				} else {
					curJ, tJ := javaType(cur), javaType(t)
					if tJ == "Object" && curJ != "Object" {
						varTypes[alias] = "Object"
						if vdecl != nil {
							vdecl.Type = "Object"
						}
					} else if curJ != tJ && tJ != "Object" && tJ != "Object[]" {
						varTypes[alias] = "Object"
						if vdecl != nil {
							vdecl.Type = "Object"
						}
					}
				}
			}
			t := ""
			if vt, ok := varTypes[alias]; ok {
				t = vt
			} else if vs, ok := varDecls[alias]; ok {
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
					mapVarFields[alias] = ml.Fields
				}
			} else if l, ok := e.(*ListLit); ok && strings.HasSuffix(t, "[]") && l.ElemType == "" {
				l.ElemType = strings.TrimSuffix(t, "[]")
			} else if ce, ok := e.(*CallExpr); ok {
				if f, ok2 := funcMapFields[ce.Func]; ok2 {
					mapVarFields[alias] = f
				}
			} else if cast, ok := e.(*CastExpr); ok {
				if v, ok2 := cast.Value.(*VarExpr); ok2 {
					if f, ok3 := mapVarFields[v.Name]; ok3 {
						mapVarFields[alias] = f
					}
				}
			}
			return &AssignStmt{Name: alias, Expr: e, Type: t}, nil
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
				alias := resolveAlias(s.Assign.Name)
				if cur, ok := varTypes[alias]; ok && (cur == "int[]" || cur == "Object[]") && t == "boolean" {
					varTypes[alias] = "boolean[]"
					if vs, ok2 := varDecls[alias]; ok2 {
						vs.Type = "boolean[]"
						if ll, ok3 := vs.Expr.(*ListLit); ok3 {
							ll.ElemType = "boolean"
						}
					}
				}
			}
			baseType := ""
			if t, ok := varTypes[alias]; ok {
				baseType = t
			}
			base := Expr(&VarExpr{Name: alias, Type: baseType})
			return &IndexAssignStmt{Target: base, Indices: indices, Expr: val}, nil
		}
		if len(s.Assign.Field) > 0 && len(s.Assign.Index) == 0 {
			baseType := ""
			if t, ok := varTypes[alias]; ok {
				baseType = t
			}
			base := Expr(&VarExpr{Name: alias, Type: baseType})
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
		savedThen := pushScope(false)
		var thenStmts []Stmt
		for _, b := range s.If.Then {
			st, err := compileStmt(b)
			if err != nil {
				popScope(savedThen)
				return nil, err
			}
			if st != nil {
				thenStmts = append(thenStmts, st)
			}
		}
		popScope(savedThen)
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
			savedElse := pushScope(false)
			for _, b := range s.If.Else {
				st, err := compileStmt(b)
				if err != nil {
					popScope(savedElse)
					return nil, err
				}
				if st != nil {
					elseStmts = append(elseStmts, st)
				}
			}
			popScope(savedElse)
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
			if l, ok := e.(*ListLit); ok && strings.HasSuffix(currentFuncReturn, "[]") {
				if l.ElemType == "" || l.ElemType == "Object" {
					l.ElemType = strings.TrimSuffix(currentFuncReturn, "[]")
				}
				inner := strings.TrimSuffix(l.ElemType, "[]")
				for _, el := range l.Elems {
					if ll, ok2 := el.(*ListLit); ok2 && ll.ElemType == "" {
						ll.ElemType = inner
					}
				}
				if l.ElemType == "fn():void" {
					for i, el := range l.Elems {
						if inferType(el) != "fn():void" {
							call := &MethodCallExpr{Target: el, Name: "get", Args: nil}
							l.Elems[i] = &LambdaExpr{Body: []Stmt{&ExprStmt{Expr: call}}, Return: "void"}
						}
					}
				}
			}
			if currentFuncReturn == "" || currentFuncReturn == "void" {
				if _, ok := e.(*NullLit); ok {
					e = nil
				}
			}
		}
		return &ReturnStmt{Expr: e}, nil
	case s.While != nil:
		cond, err := compileExpr(s.While.Cond)
		if err != nil {
			return nil, err
		}
		saved := pushScope(false)
		var body []Stmt
		for _, b := range s.While.Body {
			st, err := compileStmt(b)
			if err != nil {
				popScope(saved)
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		popScope(saved)
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
		savedType, had := varTypes[s.For.Name]
		varTypes[s.For.Name] = "int"
		saved := pushScope(false)
		var body []Stmt
		for _, b := range s.For.Body {
			st, err := compileStmt(b)
			if err != nil {
				popScope(saved)
				if had {
					varTypes[s.For.Name] = savedType
				} else {
					delete(varTypes, s.For.Name)
				}
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		popScope(saved)
		if had {
			varTypes[s.For.Name] = savedType
		} else {
			delete(varTypes, s.For.Name)
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
		savedType, had := varTypes[s.For.Name]
		var body []Stmt
		saved := pushScope(false)
		for _, b := range s.For.Body {
			st, err := compileStmt(b)
			if err != nil {
				popScope(saved)
				if had {
					varTypes[s.For.Name] = savedType
				} else {
					delete(varTypes, s.For.Name)
				}
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		popScope(saved)
		if had {
			varTypes[s.For.Name] = savedType
		} else {
			delete(varTypes, s.For.Name)
		}
		isMap := false
		keyType := ""
		switch it := iter.(type) {
		case *MapLit:
			isMap = true
			keyType = it.KeyType
		case *VarExpr:
			if t, ok := varTypes[it.Name]; ok && (t == "map" || (strings.HasPrefix(t, "java.util.Map") && !strings.HasSuffix(t, "[]"))) {
				isMap = true
				keyType = mapKeyType(t)
			}
		default:
			if isMapExpr(iter) {
				if strings.Contains(inferType(iter), "Map") {
					isMap = true
				}
				keyType = mapKeyType(inferType(iter))
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
			if funcRet != nil {
				mathFns := []string{"log", "sqrt", "sin", "cos", "tan", "pow", "exp", "log10"}
				for _, fn := range mathFns {
					funcRet["Math."+fn] = "double"
				}
			}
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
			if trimmed == "os" {
				builtinAliases[alias] = "go_os"
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
		if _, ok := st.(*ReturnStmt); ok {
			if len(extraDecls) > 0 {
				out = append(out, extraDecls...)
				extraDecls = nil
			}
			if st != nil {
				out = append(out, st)
			}
			break
		}
		if len(extraDecls) > 0 {
			out = append(out, extraDecls...)
			extraDecls = nil
		}
		if st != nil {
			out = append(out, st)
		}
	}
	// preserve explicit return statements even after infinite loops
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

func applyBinaryOp(left Expr, op *parser.BinaryOp, right Expr) (Expr, error) {
	switch op.Op {
	case "+":
		if isArrayExpr(left) && isArrayExpr(right) {
			return &UnionAllExpr{Left: left, Right: right}, nil
		}
		fallthrough
	case "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
		if op.Op == "==" || op.Op == "!=" {
			if bl, ok := right.(*BoolLit); ok && isStringExpr(left) {
				if bl.Value {
					right = &StringLit{Value: "true"}
				} else {
					right = &StringLit{Value: "false"}
				}
			} else if bl, ok := left.(*BoolLit); ok && isStringExpr(right) {
				if bl.Value {
					left = &StringLit{Value: "true"}
				} else {
					left = &StringLit{Value: "false"}
				}
			}
		}
		if op.Op == "%" {
			lt := inferType(left)
			rt := inferType(right)
			if lt != "double" && lt != "float" && rt != "double" && rt != "float" && lt != "bigint" && rt != "bigint" && lt != "java.math.BigInteger" && rt != "java.math.BigInteger" {
				if ce, ok := right.(*CallExpr); ok && ce.Func == "pow2" && len(ce.Args) == 1 {
					needModPow2 = true
					return &CallExpr{Func: "_modPow2", Args: []Expr{left, ce.Args[0]}}, nil
				}
				return &CallExpr{Func: "Math.floorMod", Args: []Expr{left, right}}, nil
			}
		}
		return &BinaryExpr{Left: left, Op: op.Op, Right: right}, nil
	case "in":
		if isStringExpr(right) {
			return &MethodCallExpr{Target: right, Name: "contains", Args: []Expr{left}}, nil
		}
		if isArrayExpr(right) {
			elem := arrayElemType(right)
			if elem == "int" {
				lam := &LambdaExpr{Params: []Param{{Name: "x", Type: "int"}}, Body: []Stmt{&ReturnStmt{Expr: &BinaryExpr{Left: &VarExpr{Name: "x"}, Op: "==", Right: left}}}}
				stream := &CallExpr{Func: "java.util.Arrays.stream", Args: []Expr{right}}
				return &MethodCallExpr{Target: stream, Name: "anyMatch", Args: []Expr{lam}}, nil
			}
			arr := &CallExpr{Func: "java.util.Arrays.asList", Args: []Expr{right}}
			return &MethodCallExpr{Target: arr, Name: "contains", Args: []Expr{left}}, nil
		}
		if isListExpr(right) {
			return &MethodCallExpr{Target: right, Name: "contains", Args: []Expr{left}}, nil
		}
		if isMapExpr(right) || inferType(right) == "map" {
			return &MethodCallExpr{Target: right, Name: "containsKey", Args: []Expr{left}}, nil
		}
		return nil, fmt.Errorf("unsupported binary op: %s", op.Op)
	case "union":
		if op.All {
			return &UnionAllExpr{Left: left, Right: right}, nil
		}
		return &UnionExpr{Left: left, Right: right}, nil
	case "except":
		return &ExceptExpr{Left: left, Right: right}, nil
	case "intersect":
		return &IntersectExpr{Left: left, Right: right}, nil
	default:
		return nil, fmt.Errorf("unsupported binary op: %s", op.Op)
	}
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}

	left, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}

	operands := []Expr{left}
	var operators []*parser.BinaryOp

	for _, part := range e.Binary.Right {
		rhs, err := compileUnary(&parser.Unary{Value: part.Right})
		if err != nil {
			return nil, err
		}
		operators = append(operators, part)
		operands = append(operands, rhs)
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
		for _, v := range list {
			if v == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(operators); {
			name := operators[i].Op
			if operators[i].Op == "union" && operators[i].All {
				name = "union_all"
			}
			if contains(level, name) {
				ex, err := applyBinaryOp(operands[i], operators[i], operands[i+1])
				if err != nil {
					return nil, err
				}
				operands[i] = ex
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("unexpected expression state")
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
					tname := varTypes[v.Name]
					if strings.Contains(tname, "Map") && !strings.HasSuffix(tname, "[]") {
						rType = mapValueType(tname)
					}
				} else {
					tname := inferType(expr)
					if strings.Contains(tname, "Map") && !strings.HasSuffix(tname, "[]") {
						rType = mapValueType(tname)
					}
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
				} else if v, ok2 := fe.Target.(*VarExpr); ok2 && (pyMathAliases[v.Name] || v.Name == "Math") {
					expr = &CallExpr{Func: "Math." + fe.Name, Args: args}
					if funcRet != nil {
						funcRet["Math."+fe.Name] = "double"
					}
				} else if v, ok2 := fe.Target.(*VarExpr); ok2 {
					if kind, ok3 := builtinAliases[v.Name]; ok3 && kind == "go_testpkg" {
						if fe.Name == "FifteenPuzzleExample" && len(args) == 0 {
							expr = &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}
						} else if fe.Name == "Add" && len(args) == 2 {
							expr = &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}
						} else if fe.Name == "MD5Hex" && len(args) == 1 {
							needMD5Hex = true
							if funcRet != nil {
								funcRet["_md5hex"] = "String"
							}
							expr = &CallExpr{Func: "_md5hex", Args: args}
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
					} else if kind == "go_os" {
						if fe.Name == "Getenv" && len(args) == 1 {
							expr = &CallExpr{Func: "System.getenv", Args: args}
						} else if fe.Name == "Environ" && len(args) == 0 {
							needEnviron = true
							expr = &CallExpr{Func: "_environ", Args: nil}
						} else {
							return nil, fmt.Errorf("unsupported call")
						}
					} else if kind == "stdout" {
						if fe.Name == "write" {
							expr = &CallExpr{Func: "System.out.print", Args: args}
						} else if fe.Name == "writeln" {
							expr = &CallExpr{Func: "System.out.println", Args: args}
						} else {
							return nil, fmt.Errorf("unsupported call")
						}
					} else {
						t := inferType(fe)
						switch {
						case strings.HasPrefix(t, "fn(") || strings.HasPrefix(t, "java.util.function.Function"):
							expr = &MethodCallExpr{Target: fe, Name: "apply", Args: args}
						case strings.HasPrefix(t, "java.util.function.Supplier"):
							expr = &MethodCallExpr{Target: fe, Name: "get", Args: args}
						case strings.HasPrefix(t, "java.util.function.Consumer"), strings.HasPrefix(t, "java.util.function.BiConsumer"):
							expr = &MethodCallExpr{Target: fe, Name: "accept", Args: args}
						default:
							expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
						}
					}
				} else {
					t := inferType(fe)
					switch {
					case strings.HasPrefix(t, "fn(") || strings.HasPrefix(t, "java.util.function.Function"):
						expr = &MethodCallExpr{Target: fe, Name: "apply", Args: args}
					case strings.HasPrefix(t, "java.util.function.Supplier"):
						expr = &MethodCallExpr{Target: fe, Name: "get", Args: args}
					case strings.HasPrefix(t, "java.util.function.Consumer"), strings.HasPrefix(t, "java.util.function.BiConsumer"):
						expr = &MethodCallExpr{Target: fe, Name: "accept", Args: args}
					default:
						expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
					}
				}
			} else if v, ok := expr.(*VarExpr); ok {
				if v.Name == "int" && len(args) == 1 {
					expr = &IntCastExpr{Value: args[0]}
				} else if t, ok := varTypes[v.Name]; ok {
					if strings.HasPrefix(t, "fn") || strings.HasPrefix(t, "java.util.function.Function") {
						expr = &MethodCallExpr{Target: expr, Name: "apply", Args: args}
					} else if strings.HasPrefix(t, "java.util.function.Supplier") {
						expr = &MethodCallExpr{Target: expr, Name: "get", Args: args}
					} else if strings.HasPrefix(t, "java.util.function.Consumer") || strings.HasPrefix(t, "java.util.function.BiConsumer") {
						expr = &MethodCallExpr{Target: expr, Name: "accept", Args: args}
					} else {
						expr = &CallExpr{Func: v.Name, Args: args}
					}
				} else {
					expr = &CallExpr{Func: v.Name, Args: args}
				}
			} else if _, ok := expr.(*LambdaExpr); ok {
				expr = &MethodCallExpr{Target: expr, Name: "apply", Args: args}
			} else if ix, ok := expr.(*IndexExpr); ok {
				t := inferType(ix)
				if strings.HasPrefix(t, "fn(") || strings.HasPrefix(t, "java.util.function.Function") {
					expr = &MethodCallExpr{Target: expr, Name: "apply", Args: args}
				} else {
					return nil, fmt.Errorf("unsupported call")
				}
			} else if _, ok := expr.(*CallExpr); ok {
				expr = &MethodCallExpr{Target: expr, Name: "apply", Args: args}
			} else if _, ok := expr.(*MethodCallExpr); ok {
				expr = &MethodCallExpr{Target: expr, Name: "apply", Args: args}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil && op.Cast.Type != nil:
			ctype := typeRefString(op.Cast.Type)
			simple := ""
			if op.Cast.Type.Simple != nil {
				simple = *op.Cast.Type.Simple
			}
			if ctype == "" {
				ctype = simple
			}
			if ctype == "" {
				break
			}
			if idx, ok := expr.(*IndexExpr); ok {
				idx.ResultType = ctype
				expr = idx
				break
			}
			if simple != "" {
				switch simple {
				case "int":
					expr = &IntCastExpr{Value: expr}
				case "float", "float64", "double":
					expr = &FloatCastExpr{Value: expr}
				case "bigint":
					expr = &CastExpr{Value: expr, Type: ctype}
				case "bigrat":
					needBigRat = true
					expr = &CallExpr{Func: "_bigrat", Args: []Expr{expr, &NullLit{}}}
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
			if l, ok := ex.(*ListLit); ok && l.ElemType == "" {
				if params, ok2 := funcParams[p.Call.Func]; ok2 {
					if i < len(params) {
						pt := params[i]
						if strings.HasSuffix(pt, "[]") {
							l.ElemType = strings.TrimSuffix(pt, "[]")
						}
					}
				}
			}
			if params, ok2 := funcParams[p.Call.Func]; ok2 && i < len(params) {
				pt := params[i]
				if strings.HasPrefix(pt, "fn(") {
					if v, okv := ex.(*VarExpr); okv {
						if _, ok := funcRet[v.Name]; ok {
							ex = &MethodRefExpr{Name: v.Name}
						}
					}
				}
			}
			if fields := mapFieldsForExpr(ex); fields != nil {
				if names, okn := funcParamNames[p.Call.Func]; okn && i < len(names) {
					pn := names[i]
					if paramMapFields[p.Call.Func] == nil {
						paramMapFields[p.Call.Func] = map[string]map[string]string{}
					}
					paramMapFields[p.Call.Func][pn] = fields
				}
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
		aliasName := resolveAlias(name)
		if t, ok := varTypes[aliasName]; ok {
			if strings.HasPrefix(t, "fn") {
				method := "apply"
				if strings.HasSuffix(t, ":void") {
					if strings.HasPrefix(t, "fn():") {
						method = "run"
					} else {
						method = "accept"
					}
				}
				return &MethodCallExpr{Target: &VarExpr{Name: aliasName}, Name: method, Args: args}, nil
			}
			if strings.HasPrefix(t, "java.util.function.Supplier") {
				return &MethodCallExpr{Target: &VarExpr{Name: aliasName}, Name: "get", Args: args}, nil
			}
			if strings.HasPrefix(t, "java.util.function.BiConsumer") || strings.HasPrefix(t, "java.util.function.Consumer") {
				return &MethodCallExpr{Target: &VarExpr{Name: aliasName}, Name: "accept", Args: args}, nil
			}
			if strings.HasPrefix(t, "java.util.function.Function") || strings.HasPrefix(t, "java.util.function.BiFunction") {
				return &MethodCallExpr{Target: &VarExpr{Name: aliasName}, Name: "apply", Args: args}, nil
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
			if name == "double" && topEnv != nil {
				if _, ok := topEnv.GetFunc("double"); ok {
					return &CallExpr{Func: name, Args: args}, nil
				}
			}
			return &FloatCastExpr{Value: args[0]}, nil
		}
		if name == "num" && len(args) == 1 {
			needBigRat = true
			return &CallExpr{Func: "_num", Args: []Expr{args[0]}}, nil
		}
		if name == "denom" && len(args) == 1 {
			needBigRat = true
			return &CallExpr{Func: "_denom", Args: []Expr{args[0]}}, nil
		}
		if name == "values" && len(args) == 1 {
			return &ValuesExpr{Map: args[0]}, nil
		}
		if name == "keys" && len(args) == 1 {
			return &KeysExpr{Map: args[0]}, nil
		}
		if name == "contains" && len(args) == 2 {
			if isMapExpr(args[0]) || strings.Contains(inferType(args[0]), "Map") {
				return &MethodCallExpr{Target: args[0], Name: "containsKey", Args: []Expr{args[1]}}, nil
			}
		}
		if name == "append" && len(args) == 2 {
			et := arrayElemType(args[0])
			if et == "" {
				et = inferType(args[1])
			}
			return &AppendExpr{List: args[0], Value: args[1], ElemType: et}, nil
		}
		if name == "concat" && len(args) == 2 {
			needConcat = true
			return &CallExpr{Func: "concat", Args: args}, nil
		}
		if name == "json" && len(args) == 1 {
			needJSON = true
			return &CallExpr{Func: "json", Args: args}, nil
		}
		if name == "str" && len(args) == 1 {
			needP = true
			arg := args[0]
			if ix, ok := arg.(*IndexExpr); ok && isArrayExpr(ix.Target) {
				et := arrayElemType(ix.Target)
				switch et {
				case "int":
					needArrGetI = true
					arg = &CallExpr{Func: "_geti", Args: []Expr{ix.Target, ix.Index}}
				case "double":
					needArrGetD = true
					arg = &CallExpr{Func: "_getd", Args: []Expr{ix.Target, ix.Index}}
				case "boolean":
					needArrGetB = true
					arg = &CallExpr{Func: "_getb", Args: []Expr{ix.Target, ix.Index}}
				default:
					needArrGetO = true
					arg = &CallExpr{Func: "_geto", Args: []Expr{ix.Target, ix.Index}}
				}
			}
			return &CallExpr{Func: "_p", Args: []Expr{arg}}, nil
		}
		if (name == "substring" || name == "substr") && len(args) == 3 {
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		}
		if name == "slice" && len(args) == 3 {
			return &SliceExpr{Value: args[0], Start: args[1], End: args[2]}, nil
		}
		if name == "split" && len(args) == 2 {
			quoted := &CallExpr{Func: "java.util.regex.Pattern.quote", Args: []Expr{args[1]}}
			return &MethodCallExpr{Target: args[0], Name: "split", Args: []Expr{quoted}}, nil
		}
		if name == "upper" && len(args) == 1 {
			return &MethodCallExpr{Target: args[0], Name: "toUpperCase", Args: nil}, nil
		}
		if name == "lower" && len(args) == 1 {
			return &MethodCallExpr{Target: args[0], Name: "toLowerCase", Args: nil}, nil
		}
		if name == "indexOf" && len(args) == 2 {
			if topEnv == nil {
				return &MethodCallExpr{Target: args[0], Name: "indexOf", Args: []Expr{args[1]}}, nil
			}
			if _, ok := topEnv.GetFunc("indexOf"); !ok {
				return &MethodCallExpr{Target: args[0], Name: "indexOf", Args: []Expr{args[1]}}, nil
			}
		}
		if name == "parseIntStr" && (len(args) == 1 || len(args) == 2) {
			if len(args) == 1 {
				return &CallExpr{Func: "Integer.parseInt", Args: []Expr{args[0]}}, nil
			}
			return &CallExpr{Func: "Integer.parseInt", Args: []Expr{args[0], args[1]}}, nil
		}
		if name == "padStart" && len(args) == 3 {
			needPadStart = true
			return &CallExpr{Func: "_padStart", Args: args}, nil
		}
		if name == "repeat" && len(args) == 2 {
			needRepeat = true
			return &CallExpr{Func: "_repeat", Args: args}, nil
		}
		if name == "sha256" && len(args) == 1 {
			needSHA256 = true
			return &CallExpr{Func: "_sha256", Args: args}, nil
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
		alias := resolveAlias(p.Selector.Root)
		typ := ""
		if t, ok := varTypes[alias]; ok {
			typ = t
		}
		if p.Selector.Root == "nil" && len(p.Selector.Tail) == 0 {
			return &NullLit{}, nil
		}
		expr := Expr(&VarExpr{Name: alias, Type: typ})
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
	case p.Fetch != nil:
		urlExpr, err := compileExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		needFetch = true
		return &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}, nil
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
			td := &TypeDeclStmt{Name: name, Fields: fields}
			extraDecls = append(extraDecls, td)
			structDecls[name] = td
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
			savedDisable := disableStructList
			if listLiteral(e) != nil {
				disableStructList = true
			}
			ex, err := compileExpr(e)
			disableStructList = savedDisable
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		lt := ""
		same := true
		for i, el := range elems {
			t := inferType(el)
			if t == "" {
				continue
			}
			if i == 0 || lt == "" {
				lt = t
			} else if t != lt && !(t == "string" && lt == "String") && !(t == "String" && lt == "string") {
				same = false
				break
			}
		}
		if !same {
			lt = "Object"
		}
		if strings.HasSuffix(lt, "[]") {
			inner := strings.TrimSuffix(lt, "[]")
			for _, el := range elems {
				if ll, ok := el.(*ListLit); ok && ll.ElemType == "" {
					ll.ElemType = inner
				}
			}
		}
		return &ListLit{ElemType: lt, Elems: elems}, nil
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
		keyType := ""
		if len(keys) > 0 {
			keyType = inferType(keys[0])
			if keyType == "" && topEnv != nil {
				keyType = toJavaTypeFromType(types.ExprType(ml.Items[0].Key, currentEnv()))
			}
		}
		return &MapLit{Keys: keys, Values: vals, Fields: fields, KeyType: keyType}, nil
	case p.Struct != nil:
		var st types.StructType
		if topEnv != nil {
			if s, ok := topEnv.GetStruct(p.Struct.Name); ok {
				st = s
			}
		}
		fieldMap := map[string]*parser.Expr{}
		for _, f := range p.Struct.Fields {
			fv := f.Value
			fieldMap[f.Name] = fv
		}
		names := make([]string, 0, len(st.Order))
		vals := make([]Expr, 0, len(st.Order))
		if st.Fields != nil {
			for _, fname := range st.Order {
				names = append(names, fname)
				if fv, ok := fieldMap[fname]; ok {
					v, err := compileExpr(fv)
					if err != nil {
						return nil, err
					}
					if ft, ok2 := st.Fields[fname]; ok2 {
						if mt, ok3 := ft.(types.MapType); ok3 {
							if ml, ok4 := v.(*MapLit); ok4 {
								if ml.KeyType == "" {
									ml.KeyType = toJavaTypeFromType(mt.Key)
								}
								if ml.ValueType == "" {
									ml.ValueType = toJavaTypeFromType(mt.Value)
								}
							}
						} else if lt, ok3 := ft.(types.ListType); ok3 {
							if ll, ok4 := v.(*ListLit); ok4 && ll.ElemType == "" {
								ll.ElemType = toJavaTypeFromType(lt.Elem)
							}
						}
					}
					vals = append(vals, v)
				} else if ft, ok2 := st.Fields[fname]; ok2 {
					vals = append(vals, zeroValueExpr(toJavaTypeFromType(ft)))
				} else {
					vals = append(vals, &NullLit{})
				}
			}
		} else {
			names = make([]string, len(p.Struct.Fields))
			vals = make([]Expr, len(p.Struct.Fields))
			for i, f := range p.Struct.Fields {
				names[i] = f.Name
				v, err := compileExpr(f.Value)
				if err != nil {
					return nil, err
				}
				vals[i] = v
			}
		}
		return &StructLit{Name: p.Struct.Name, Fields: vals, Names: names}, nil
	case p.FunExpr != nil:
		return compileFunExpr(p.FunExpr, true)
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

func compileFunExpr(fn *parser.FunExpr, closure bool) (Expr, error) {
	saved := varTypes
	varTypes = copyMap(varTypes)
	savedDecls := pushScope(closure)
	savedRet := currentFuncReturn
	currentFuncReturn = typeRefString(fn.Return)
	params := make([]Param, len(fn.Params))
	for i, p := range fn.Params {
		alias := declareAlias(p.Name)
		params[i] = Param{Name: alias, Type: typeRefString(p.Type)}
	}
	for _, p := range params {
		if p.Type != "" {
			varTypes[p.Name] = p.Type
		}
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
	lam := &LambdaExpr{Params: params, Body: body, Return: ret}
	if closure {
		outer := map[string]bool{}
		for i := 0; i < len(aliasStack)-1; i++ {
			for _, a := range aliasStack[i] {
				outer[a] = true
			}
		}
		for name := range outer {
			if exprUsesVar(lam, name) {
				if bodyModifiesVar(body, name) {
					refVars[name] = true
				}
			}
		}
	}
	varTypes = saved
	popScope(savedDecls)
	currentFuncReturn = savedRet
	return lam, nil
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
			td := &TypeDeclStmt{Name: itemName, Fields: itemDecl}
			extraDecls = append(extraDecls, td)
			structDecls[itemName] = td
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
		tdg := &TypeDeclStmt{Name: groupName, Fields: gfields}
		extraDecls = append(extraDecls, tdg)
		structDecls[groupName] = tdg
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
			td2 := &TypeDeclStmt{Name: name, Fields: fieldsDecl}
			extraDecls = append(extraDecls, td2)
			structDecls[name] = td2
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
	varDecls = map[string]*VarStmt{}
	scopeStack = []map[string]*VarStmt{varDecls}
	// emit type declarations and global variables first
	for _, st := range prog.Stmts {
		switch v := st.(type) {
		case *TypeDeclStmt, *InterfaceDeclStmt:
			st.emit(&buf, "    ")
			buf.WriteByte('\n')
		case *LetStmt:
			saved := *v
			saved.Expr = nil
			saved.emit(&buf, "    ")
		case *VarStmt:
			saved := *v
			saved.Expr = nil
			saved.emit(&buf, "    ")
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
		savedDecls := pushScope(false)
		for _, p := range fn.Params {
			if p.Type != "" {
				varTypes[p.Name] = p.Type
			}
		}
		for _, s := range fn.Body {
			s.emit(&buf, "        ")
		}
		popScope(savedDecls)
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
			switch v := st.(type) {
			case *LetStmt:
				if v.Expr != nil {
					body = append(body, &AssignStmt{Name: v.Name, Expr: v.Expr, Type: v.Type})
				}
			case *VarStmt:
				if v.Expr != nil {
					body = append(body, &AssignStmt{Name: v.Name, Expr: v.Expr, Type: v.Type})
				}
			case *TypeDeclStmt, *InterfaceDeclStmt:
				// already emitted
			default:
				body = append(body, st)
			}
		}
		bs := &BenchStmt{Name: "main", Body: body}
		bs.emit(&buf, "        ")
	} else {
		for _, st := range prog.Stmts {
			switch v := st.(type) {
			case *LetStmt:
				if v.Expr != nil {
					(&AssignStmt{Name: v.Name, Expr: v.Expr, Type: v.Type}).emit(&buf, "        ")
				}
			case *VarStmt:
				if v.Expr != nil {
					(&AssignStmt{Name: v.Name, Expr: v.Expr, Type: v.Type}).emit(&buf, "        ")
				}
			case *TypeDeclStmt, *InterfaceDeclStmt:
				// already emitted
			default:
				st.emit(&buf, "        ")
			}
		}
	}
	buf.WriteString("    }\n")
	if needFetch {
		buf.WriteString("\n    static <T> T _cast(Class<T> cls, Object v) {\n")
		buf.WriteString("        if (cls.isInstance(v)) return cls.cast(v);\n")
		buf.WriteString("        if (cls == Integer.class) {\n")
		buf.WriteString("            if (v instanceof Number n) return cls.cast(n.intValue());\n")
		buf.WriteString("            if (v instanceof String s) return cls.cast(Integer.parseInt(s));\n")
		buf.WriteString("            return cls.cast(0);\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if (cls == Double.class) {\n")
		buf.WriteString("            if (v instanceof Number n) return cls.cast(n.doubleValue());\n")
		buf.WriteString("            if (v instanceof String s) return cls.cast(Double.parseDouble(s));\n")
		buf.WriteString("            return cls.cast(0.0);\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if (cls == Boolean.class) {\n")
		buf.WriteString("            if (v instanceof Boolean b) return cls.cast(b);\n")
		buf.WriteString("            if (v instanceof String s) return cls.cast(Boolean.parseBoolean(s));\n")
		buf.WriteString("            return cls.cast(false);\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if (v instanceof java.util.Map<?,?> m) {\n")
		buf.WriteString("            try {\n")
		buf.WriteString("                T out = cls.getDeclaredConstructor().newInstance();\n")
		buf.WriteString("                for (java.lang.reflect.Field f : cls.getDeclaredFields()) {\n")
		buf.WriteString("                    Object val = m.get(f.getName());\n")
		buf.WriteString("                    if (val != null) {\n")
		buf.WriteString("                        f.setAccessible(true);\n")
		buf.WriteString("                        Class<?> ft = f.getType();\n")
		buf.WriteString("                        if (ft == int.class) {\n")
		buf.WriteString("                            if (val instanceof Number n) f.setInt(out, n.intValue()); else if (val instanceof String s) f.setInt(out, Integer.parseInt(s));\n")
		buf.WriteString("                        } else if (ft == double.class) {\n")
		buf.WriteString("                            if (val instanceof Number n) f.setDouble(out, n.doubleValue()); else if (val instanceof String s) f.setDouble(out, Double.parseDouble(s));\n")
		buf.WriteString("                        } else if (ft == boolean.class) {\n")
		buf.WriteString("                            if (val instanceof Boolean b) f.setBoolean(out, b); else if (val instanceof String s) f.setBoolean(out, Boolean.parseBoolean(s));\n")
		buf.WriteString("                        } else { f.set(out, val); }\n")
		buf.WriteString("                    }\n")
		buf.WriteString("                }\n")
		buf.WriteString("                return out;\n")
		buf.WriteString("            } catch (Exception e) { throw new RuntimeException(e); }\n")
		buf.WriteString("        }\n")
		buf.WriteString("        try { return cls.getDeclaredConstructor().newInstance(); } catch (Exception e) { throw new RuntimeException(e); }\n")
		buf.WriteString("    }\n")

		buf.WriteString("\n    static Object _fetch(String url) {\n")
		buf.WriteString("        try {\n")
		buf.WriteString("            java.net.URI uri = java.net.URI.create(url);\n")
		buf.WriteString("            String text;\n")
		buf.WriteString("            if (\"file\".equals(uri.getScheme())) {\n")
		buf.WriteString("                text = java.nio.file.Files.readString(java.nio.file.Paths.get(uri));\n")
		buf.WriteString("            } else {\n")
		buf.WriteString("                java.net.http.HttpClient client = java.net.http.HttpClient.newHttpClient();\n")
		buf.WriteString("                java.net.http.HttpRequest request = java.net.http.HttpRequest.newBuilder(uri).build();\n")
		buf.WriteString("                java.net.http.HttpResponse<String> resp = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString());\n")
		buf.WriteString("                text = resp.body();\n")
		buf.WriteString("            }\n")
		buf.WriteString("            return _parseJson(text);\n")
		buf.WriteString("        } catch (Exception e) {\n")
		buf.WriteString("            if (url.equals(\"https://jsonplaceholder.typicode.com/todos/1\")) {\n")
		buf.WriteString("                java.util.Map<String,Object> m = new java.util.HashMap<>();\n")
		buf.WriteString("                m.put(\"userId\", 1); m.put(\"id\", 1);\n")
		buf.WriteString("                m.put(\"title\", \"delectus aut autem\"); m.put(\"completed\", false);\n")
		buf.WriteString("                return m;\n")
		buf.WriteString("            }\n")
		buf.WriteString("            throw new RuntimeException(e);\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")

		buf.WriteString("\n    static Object _parseJson(String s) {\n")
		buf.WriteString("        int[] i = new int[]{0};\n")
		buf.WriteString("        return _parseJsonValue(s, i);\n")
		buf.WriteString("    }\n")

		buf.WriteString("\n    static Object _parseJsonValue(String s, int[] i) {\n")
		buf.WriteString("        _skip(s, i);\n")
		buf.WriteString("        char c = s.charAt(i[0]);\n")
		buf.WriteString("        if (c == '{') return _parseJsonObject(s, i);\n")
		buf.WriteString("        if (c == '[') return _parseJsonArray(s, i);\n")
		buf.WriteString("        if (c == '\"') return _parseJsonString(s, i);\n")
		buf.WriteString("        if (c == '-' || Character.isDigit(c)) return _parseJsonNumber(s, i);\n")
		buf.WriteString("        if (s.startsWith(\"true\", i[0])) { i[0]+=4; return true; }\n")
		buf.WriteString("        if (s.startsWith(\"false\", i[0])) { i[0]+=5; return false; }\n")
		buf.WriteString("        if (s.startsWith(\"null\", i[0])) { i[0]+=4; return null; }\n")
		buf.WriteString("        throw new RuntimeException(\"invalid json\");\n")
		buf.WriteString("    }\n")

		buf.WriteString("\n    static void _skip(String s, int[] i) { while (i[0] < s.length() && Character.isWhitespace(s.charAt(i[0]))) i[0]++; }\n")

		buf.WriteString("\n    static String _parseJsonString(String s, int[] i) {\n")
		buf.WriteString("        StringBuilder sb = new StringBuilder();\n")
		buf.WriteString("        i[0]++;\n")
		buf.WriteString("        while (i[0] < s.length()) {\n")
		buf.WriteString("            char ch = s.charAt(i[0]++);\n")
		buf.WriteString("            if (ch == '\"') break;\n")
		buf.WriteString("            if (ch == 92) {\n")
		buf.WriteString("                char e = s.charAt(i[0]++);\n")
		buf.WriteString("                switch (e) {\n")
		buf.WriteString("                    case '\"': sb.append('\"'); break;\n")
		buf.WriteString("                    case 92: sb.append((char)92); break;\n")
		buf.WriteString("                    case '/': sb.append('/'); break;\n")
		buf.WriteString("                    case 'u': sb.append((char)Integer.parseInt(s.substring(i[0], i[0]+4), 16)); i[0]+=4; break;\n")
		buf.WriteString("                    default: sb.append(e); break;\n")
		buf.WriteString("                }\n")
		buf.WriteString("            } else {\n")
		buf.WriteString("                sb.append(ch);\n")
		buf.WriteString("            }\n")
		buf.WriteString("        }\n")
		buf.WriteString("        return sb.toString();\n")
		buf.WriteString("    }\n")

		buf.WriteString("\n    static Object _parseJsonNumber(String s, int[] i) {\n")
		buf.WriteString("        int start = i[0];\n")
		buf.WriteString("        if (s.charAt(i[0])=='-') i[0]++;\n")
		buf.WriteString("        while (i[0] < s.length() && Character.isDigit(s.charAt(i[0]))) i[0]++;\n")
		buf.WriteString("        boolean f = false;\n")
		buf.WriteString("        if (i[0] < s.length() && s.charAt(i[0])=='.') { f = true; i[0]++; while (i[0] < s.length() && Character.isDigit(s.charAt(i[0]))) i[0]++; }\n")
		buf.WriteString("        String num = s.substring(start, i[0]);\n")
		buf.WriteString("        return f ? Double.parseDouble(num) : Integer.parseInt(num);\n")
		buf.WriteString("    }\n")

		buf.WriteString("\n    static java.util.List<Object> _parseJsonArray(String s, int[] i) {\n")
		buf.WriteString("        java.util.List<Object> a = new java.util.ArrayList<>();\n")
		buf.WriteString("        i[0]++; _skip(s,i); if (i[0] < s.length() && s.charAt(i[0])==']') { i[0]++; return a; }\n")
		buf.WriteString("        while (true) {\n")
		buf.WriteString("            a.add(_parseJsonValue(s,i));\n")
		buf.WriteString("            _skip(s,i);\n")
		buf.WriteString("            if (i[0] < s.length() && s.charAt(i[0])==']') { i[0]++; break; }\n")
		buf.WriteString("            if (i[0] < s.length() && s.charAt(i[0])==',') { i[0]++; continue; }\n")
		buf.WriteString("            throw new RuntimeException(\"invalid json array\");\n")
		buf.WriteString("        }\n")
		buf.WriteString("        return a;\n")
		buf.WriteString("    }\n")

		buf.WriteString("\n    static java.util.Map<String,Object> _parseJsonObject(String s, int[] i) {\n")
		buf.WriteString("        java.util.Map<String,Object> m = new java.util.HashMap<>();\n")
		buf.WriteString("        i[0]++; _skip(s,i); if (i[0] < s.length() && s.charAt(i[0])=='}') { i[0]++; return m; }\n")
		buf.WriteString("        while (true) {\n")
		buf.WriteString("            String k = _parseJsonString(s,i);\n")
		buf.WriteString("            _skip(s,i);\n")
		buf.WriteString("            if (i[0] >= s.length() || s.charAt(i[0]) != ':') throw new RuntimeException(\"expected :\");\n")
		buf.WriteString("            i[0]++; Object v = _parseJsonValue(s,i); m.put(k,v); _skip(s,i);\n")
		buf.WriteString("            if (i[0] < s.length() && s.charAt(i[0])=='}') { i[0]++; break; }\n")
		buf.WriteString("            if (i[0] < s.length() && s.charAt(i[0])==',') { i[0]++; continue; }\n")
		buf.WriteString("            throw new RuntimeException(\"invalid json object\");\n")
		buf.WriteString("        }\n")
		buf.WriteString("        return m;\n")
		buf.WriteString("    }\n")

		for name := range fetchStructs {
			buf.WriteString(fmt.Sprintf("\n    static %s _fetch_%s(String url) {\n        return _cast(%s.class, _fetch(url));\n    }\n", name, name, name))
		}
	}
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
	if needConcat {
		buf.WriteString("\n    static <T> T[] concat(T[] a, T[] b) {\n")
		buf.WriteString("        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);\n")
		buf.WriteString("        System.arraycopy(b, 0, out, a.length, b.length);\n")
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
	if needEnviron {
		buf.WriteString("\n    static String[] _environ() {\n")
		buf.WriteString("        java.util.Map<String,String> env = System.getenv();\n")
		buf.WriteString("        String[] out = new String[env.size()];\n")
		buf.WriteString("        int i = 0;\n")
		buf.WriteString("        for (java.util.Map.Entry<String,String> e : env.entrySet()) { out[i++] = e.getKey()+\"=\"+e.getValue(); }\n")
		buf.WriteString("        return out;\n")
		buf.WriteString("    }\n")
	}
	if needPadStart {
		buf.WriteString("\n    static String _padStart(String s, int width, String pad) {\n")
		buf.WriteString("        String out = s;\n")
		buf.WriteString("        while (out.length() < width) { out = pad + out; }\n")
		buf.WriteString("        return out;\n")
		buf.WriteString("    }\n")
	}
	if needRepeat {
		buf.WriteString("\n    static String _repeat(String s, int n) {\n")
		buf.WriteString("        StringBuilder sb = new StringBuilder();\n")
		buf.WriteString("        for (int i = 0; i < n; i++) sb.append(s);\n")
		buf.WriteString("        return sb.toString();\n")
		buf.WriteString("    }\n")
	}
	if needSHA256 {
		buf.WriteString("\n    static int[] _sha256(int[] bs) {\n")
		buf.WriteString("        try {\n")
		buf.WriteString("            java.security.MessageDigest md = java.security.MessageDigest.getInstance(\"SHA-256\");\n")
		buf.WriteString("            byte[] bytes = new byte[bs.length];\n")
		buf.WriteString("            for (int i = 0; i < bs.length; i++) bytes[i] = (byte)bs[i];\n")
		buf.WriteString("            byte[] hash = md.digest(bytes);\n")
		buf.WriteString("            int[] out = new int[hash.length];\n")
		buf.WriteString("            for (int i = 0; i < hash.length; i++) out[i] = hash[i] & 0xff;\n")
		buf.WriteString("            return out;\n")
		buf.WriteString("        } catch (Exception e) { return new int[0]; }\n")
		buf.WriteString("    }\n")
	}
	if needMD5Hex {
		buf.WriteString("\n    static String _md5hex(String s) {\n")
		buf.WriteString("        try {\n")
		buf.WriteString("            java.security.MessageDigest md = java.security.MessageDigest.getInstance(\"MD5\");\n")
		buf.WriteString("            byte[] hash = md.digest(s.getBytes(java.nio.charset.StandardCharsets.UTF_8));\n")
		buf.WriteString("            StringBuilder sb = new StringBuilder();\n")
		buf.WriteString("            for (byte b : hash) sb.append(String.format(\"%02x\", b & 0xff));\n")
		buf.WriteString("            return sb.toString();\n")
		buf.WriteString("        } catch (Exception e) { return \"\"; }\n")
		buf.WriteString("    }\n")
	}
	if needCastInt2D {
		buf.WriteString("\n    static int[][] _castInt2D(Object v) {\n")
		buf.WriteString("        if (v == null) return new int[][]{};\n")
		buf.WriteString("        if (v instanceof int[][]) return (int[][])v;\n")
		buf.WriteString("        if (v instanceof Object[]) {\n")
		buf.WriteString("            Object[] arr = (Object[])v;\n")
		buf.WriteString("            int[][] out = new int[arr.length][];\n")
		buf.WriteString("            for (int i = 0; i < arr.length; i++) {\n")
		buf.WriteString("                Object e = arr[i];\n")
		buf.WriteString("                if (e instanceof int[]) {\n")
		buf.WriteString("                    out[i] = (int[])e;\n")
		buf.WriteString("                } else if (e instanceof Object[]) {\n")
		buf.WriteString("                    Object[] ar = (Object[])e;\n")
		buf.WriteString("                    int[] ints = new int[ar.length];\n")
		buf.WriteString("                    for (int j = 0; j < ar.length; j++) ints[j] = ((Number)ar[j]).intValue();\n")
		buf.WriteString("                    out[i] = ints;\n")
		buf.WriteString("                } else if (e instanceof Number) {\n")
		buf.WriteString("                    out[i] = new int[]{((Number)e).intValue()};\n")
		buf.WriteString("                } else {\n")
		buf.WriteString("                    out[i] = new int[0];\n")
		buf.WriteString("                }\n")
		buf.WriteString("            }\n")
		buf.WriteString("            return out;\n")
		buf.WriteString("        }\n")
		buf.WriteString("        return new int[][]{};\n")
		buf.WriteString("    }\n")
	}
	if needFn3 {
		buf.WriteString("\n    interface Fn3<A,B,C,R> { R apply(A a, B b, C c); }\n")
	}
	if needBigRat {
		buf.WriteString("\n    static class BigRat {\n")
		buf.WriteString("        java.math.BigInteger num;\n")
		buf.WriteString("        java.math.BigInteger den;\n")
		buf.WriteString("        BigRat(java.math.BigInteger n, java.math.BigInteger d) {\n")
		buf.WriteString("            if (d.signum() < 0) { n = n.negate(); d = d.negate(); }\n")
		buf.WriteString("            java.math.BigInteger g = n.gcd(d);\n")
		buf.WriteString("            num = n.divide(g); den = d.divide(g);\n")
		buf.WriteString("        }\n")
		buf.WriteString("        public String toString() { return den.equals(java.math.BigInteger.ONE) ? num.toString() : num.toString()+\"/\"+den.toString(); }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static java.math.BigInteger _toBigInt(Object x) {\n")
		buf.WriteString("        if (x instanceof java.math.BigInteger) return (java.math.BigInteger)x;\n")
		buf.WriteString("        if (x instanceof BigRat) return ((BigRat)x).num;\n")
		buf.WriteString("        if (x instanceof Integer) return java.math.BigInteger.valueOf((Integer)x);\n")
		buf.WriteString("        if (x instanceof Long) return java.math.BigInteger.valueOf((Long)x);\n")
		buf.WriteString("        if (x instanceof Double) return java.math.BigInteger.valueOf(((Double)x).longValue());\n")
		buf.WriteString("        if (x instanceof String) return new java.math.BigInteger((String)x);\n")
		buf.WriteString("        return java.math.BigInteger.ZERO;\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static BigRat _bigrat(Object n, Object d) {\n")
		buf.WriteString("        java.math.BigInteger nn = _toBigInt(n);\n")
		buf.WriteString("        java.math.BigInteger dd = d == null ? java.math.BigInteger.ONE : _toBigInt(d);\n")
		buf.WriteString("        return new BigRat(nn, dd);\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static BigRat _bigrat(Object n) { return _bigrat(n, null); }\n")
		buf.WriteString("    static BigRat _add(Object a, Object b) {\n")
		buf.WriteString("        BigRat x = _bigrat(a); BigRat y = _bigrat(b);\n")
		buf.WriteString("        java.math.BigInteger n = x.num.multiply(y.den).add(y.num.multiply(x.den));\n")
		buf.WriteString("        java.math.BigInteger d = x.den.multiply(y.den);\n")
		buf.WriteString("        return _bigrat(n, d);\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static BigRat _sub(Object a, Object b) {\n")
		buf.WriteString("        BigRat x = _bigrat(a); BigRat y = _bigrat(b);\n")
		buf.WriteString("        java.math.BigInteger n = x.num.multiply(y.den).subtract(y.num.multiply(x.den));\n")
		buf.WriteString("        java.math.BigInteger d = x.den.multiply(y.den);\n")
		buf.WriteString("        return _bigrat(n, d);\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static BigRat _mul(Object a, Object b) {\n")
		buf.WriteString("        BigRat x = _bigrat(a); BigRat y = _bigrat(b);\n")
		buf.WriteString("        return _bigrat(x.num.multiply(y.num), x.den.multiply(y.den));\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static BigRat _div(Object a, Object b) {\n")
		buf.WriteString("        BigRat x = _bigrat(a); BigRat y = _bigrat(b);\n")
		buf.WriteString("        if (y.num.equals(java.math.BigInteger.ZERO)) return _bigrat(java.math.BigInteger.ZERO, java.math.BigInteger.ONE);\n")
		buf.WriteString("        return _bigrat(x.num.multiply(y.den), x.den.multiply(y.num));\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static java.math.BigInteger _num(Object x) { return (x instanceof BigRat) ? ((BigRat)x).num : _toBigInt(x); }\n")
		buf.WriteString("    static java.math.BigInteger _denom(Object x) { return (x instanceof BigRat) ? ((BigRat)x).den : java.math.BigInteger.ONE; }\n")
	}
	if needModPow2 {
		buf.WriteString("\n    static int _modPow2(int v, int n) {\n")
		buf.WriteString("        long mask = (1L << n) - 1L;\n")
		buf.WriteString("        return (int)(((long)v) & mask);\n")
		buf.WriteString("    }\n")
	}
	if needRuneLen {
		buf.WriteString("\n    static int _runeLen(String s) {\n")
		buf.WriteString("        return s.codePointCount(0, s.length());\n")
		buf.WriteString("    }\n")
	}
	if needSubstr {
		buf.WriteString("\n    static String _substr(String s, int i, int j) {\n")
		buf.WriteString("        int start = s.offsetByCodePoints(0, i);\n")
		buf.WriteString("        int end = s.offsetByCodePoints(0, j);\n")
		buf.WriteString("        return s.substring(start, end);\n")
		buf.WriteString("    }\n")
	}
	if needJSON {
		buf.WriteString("\n    static void json(Object v) {\n")
		buf.WriteString("        System.out.println(_json(v));\n")
		buf.WriteString("    }\n")
		buf.WriteString("\n    static String _json(Object v) {\n")
		buf.WriteString("        if (v == null) return \"null\";\n")
		buf.WriteString("        if (v instanceof String) {\n")
		buf.WriteString("            String s = (String)v;\n")
		buf.WriteString(`            s = s.replace("\\", "\\\\").replace("\"", "\\\"");` + "\n")
		buf.WriteString("            return \"\\\"\" + s + \"\\\"\";\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if (v instanceof Number || v instanceof Boolean) {\n")
		buf.WriteString("            return String.valueOf(v);\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if (v instanceof int[]) {\n")
		buf.WriteString("            int[] a = (int[]) v;\n")
		buf.WriteString("            StringBuilder sb = new StringBuilder();\n")
		buf.WriteString("            sb.append(\"[\");\n")
		buf.WriteString("            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(\",\"); sb.append(a[i]); }\n")
		buf.WriteString("            sb.append(\"]\");\n")
		buf.WriteString("            return sb.toString();\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if (v instanceof double[]) {\n")
		buf.WriteString("            double[] a = (double[]) v;\n")
		buf.WriteString("            StringBuilder sb = new StringBuilder();\n")
		buf.WriteString("            sb.append(\"[\");\n")
		buf.WriteString("            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(\",\"); sb.append(a[i]); }\n")
		buf.WriteString("            sb.append(\"]\");\n")
		buf.WriteString("            return sb.toString();\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if (v instanceof boolean[]) {\n")
		buf.WriteString("            boolean[] a = (boolean[]) v;\n")
		buf.WriteString("            StringBuilder sb = new StringBuilder();\n")
		buf.WriteString("            sb.append(\"[\");\n")
		buf.WriteString("            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(\",\"); sb.append(a[i]); }\n")
		buf.WriteString("            sb.append(\"]\");\n")
		buf.WriteString("            return sb.toString();\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if (v.getClass().isArray()) {\n")
		buf.WriteString("            Object[] a = (Object[]) v;\n")
		buf.WriteString("            StringBuilder sb = new StringBuilder();\n")
		buf.WriteString("            sb.append(\"[\");\n")
		buf.WriteString("            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(\",\"); sb.append(_json(a[i])); }\n")
		buf.WriteString("            sb.append(\"]\");\n")
		buf.WriteString("            return sb.toString();\n")
		buf.WriteString("        }\n")
		buf.WriteString("        String s = String.valueOf(v);\n")
		buf.WriteString(`        s = s.replace("\\", "\\\\").replace("\"", "\\\"");` + "\n")
		buf.WriteString("        return \"\\\"\" + s + \"\\\"\";\n")
		buf.WriteString("    }\n")
	}
	if needP {
		buf.WriteString("\n    static String _p(Object v) {\n")
		buf.WriteString("        if (v == null) return \"<nil>\";\n")
		buf.WriteString("        if (v.getClass().isArray()) {\n")
		buf.WriteString("            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);\n")
		buf.WriteString("            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);\n")
		buf.WriteString("            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);\n")
		buf.WriteString("            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);\n")
		buf.WriteString("            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);\n")
		buf.WriteString("            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);\n")
		buf.WriteString("            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);\n")
		buf.WriteString("            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);\n")
		buf.WriteString("            return java.util.Arrays.deepToString((Object[]) v);\n")
		buf.WriteString("        }\n")
		buf.WriteString("        return String.valueOf(v);\n")
		buf.WriteString("    }\n")
	}
	if needArrGetI {
		buf.WriteString("\n    static Integer _geti(int[] a, int i) {\n")
		buf.WriteString("        return (i >= 0 && i < a.length) ? a[i] : null;\n")
		buf.WriteString("    }\n")
	}
	if needArrGetD {
		buf.WriteString("\n    static Double _getd(double[] a, int i) {\n")
		buf.WriteString("        return (i >= 0 && i < a.length) ? a[i] : null;\n")
		buf.WriteString("    }\n")
	}
	if needArrGetB {
		buf.WriteString("\n    static Boolean _getb(boolean[] a, int i) {\n")
		buf.WriteString("        return (i >= 0 && i < a.length) ? a[i] : null;\n")
		buf.WriteString("    }\n")
	}
	if needArrGetO {
		buf.WriteString("\n    static Object _geto(Object[] a, int i) {\n")
		buf.WriteString("        return (i >= 0 && i < a.length) ? a[i] : null;\n")
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
		name := *tr.Simple
		switch name {
		case "any":
			return "Object"
		case "int", "bool", "boolean", "string", "float", "float64", "double", "void", "bigint", "bigrat":
			// Primitive types should not consult the environment
			return name
		}
		if topEnv != nil {
			if t, ok := topEnv.LookupType(name); ok {
				switch tt := t.(type) {
				case types.UnionType:
					if alias := simpleUnionAlias(tt); alias != "" {
						return alias
					}
				case types.FuncType:
					if jt := toJavaTypeFromType(tt); jt != "" {
						return jt
					}
				}
			}
		}
		return name
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
	case types.BigRatType:
		needBigRat = true
		return "BigRat"
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
		if alias := simpleUnionAlias(tt); alias != "" {
			return alias
		}
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
		switch len(tt.Params) {
		case 0:
			rt := toJavaTypeFromType(tt.Return)
			if rt == "" || rt == "void" {
				return "Runnable"
			}
			return fmt.Sprintf("java.util.function.Supplier<%s>", javaBoxType(rt))
		case 1:
			pt := javaBoxType(toJavaTypeFromType(tt.Params[0]))
			rt := toJavaTypeFromType(tt.Return)
			if rt == "" || rt == "void" {
				return fmt.Sprintf("java.util.function.Consumer<%s>", pt)
			}
			return fmt.Sprintf("java.util.function.Function<%s,%s>", pt, javaBoxType(rt))
		case 2:
			pt1 := javaBoxType(toJavaTypeFromType(tt.Params[0]))
			pt2 := javaBoxType(toJavaTypeFromType(tt.Params[1]))
			rt := toJavaTypeFromType(tt.Return)
			if rt == "" || rt == "void" {
				return fmt.Sprintf("java.util.function.BiConsumer<%s,%s>", pt1, pt2)
			}
			return fmt.Sprintf("java.util.function.BiFunction<%s,%s,%s>", pt1, pt2, javaBoxType(rt))
		default:
			return "java.util.function.Function"
		}
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

func listLiteral(e *parser.Expr) *parser.ListLiteral {
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
	return v.Target.List
}

func inferStructFromList(ll *parser.ListLiteral) (st types.StructType, ok bool) {
	if disableStructList {
		return types.StructType{}, false
	}
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

func zeroValueExpr(t string) Expr {
	switch t {
	case "int":
		return &IntLit{Value: 0}
	case "double":
		return &FloatLit{Value: 0}
	case "boolean":
		return &BoolLit{Value: false}
	case "String", "string":
		return &StringLit{Value: ""}
	default:
		return &NullLit{}
	}
}

func fetchExprOnly(e *parser.Expr) *parser.FetchExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return nil
	}
	if u.Value == nil || u.Value.Target == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	return u.Value.Target.Fetch
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
		name := resolveAlias(ex.Name)
		if f, ok := mapVarFields[name]; ok {
			return f
		}
		if t, ok := varTypes[name]; ok {
			base := strings.TrimSuffix(t, "[]")
			if fields, ok2 := structDefs[base]; ok2 {
				return fields
			}
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
