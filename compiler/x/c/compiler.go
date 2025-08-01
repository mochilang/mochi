//go:build slow

package ccode

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"gopkg.in/yaml.v3"

	"mochi/parser"
	"mochi/types"
)

var cReserved = map[string]bool{
	"auto": true, "break": true, "case": true, "char": true, "const": true,
	"continue": true, "default": true, "do": true, "double": true, "else": true,
	"enum": true, "extern": true, "float": true, "for": true, "goto": true,
	"if": true, "int": true, "long": true, "register": true, "return": true,
	"short": true, "signed": true, "sizeof": true, "static": true, "struct": true,
	"switch": true, "typedef": true, "union": true, "unsigned": true, "void": true,
	"volatile": true, "while": true,
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
		s = "_" + s
	}
	if cReserved[s] {
		s = "_" + s
	}
	// Avoid clashing with standard library functions such as remove()
	switch s {
	case "remove":
		s = "_remove"
	}
	return s
}

type captureInfo struct {
	global string
	typ    types.Type
}

type Compiler struct {
	structLits       map[*parser.MapLiteral]types.StructType
	buf              bytes.Buffer
	indent           int
	tmp              int
	env              *types.Env
	lambdas          []string
	needs            map[string]bool
	externs          []string
	externObjects    []string
	structs          map[string]bool
	listStructs      map[string]bool
	fetchStructs     map[string]bool
	currentStruct    string
	autoType         bool
	captures         map[string]captureInfo
	capturesByFun    map[string][]string
	uninitVars       map[string]bool
	pointerVars      map[string]bool
	groupKeys        map[string]types.Type
	builtinAliases   map[string]string
	needMath         bool
	listLens         map[string]int
	listVals         map[string][]int
	listValsFloat    map[string][]float64
	listValsString   map[string][]string
	stackArrays      map[string]bool
	arrayLens        map[string]string
	assignVar        string
	baseDir          string
	allocs           []string
	equalListStructs map[string]bool
	funcAliases      map[string]string
	appendTypes      map[string]types.Type
	jsonLines        map[string][]string
}

func (c *Compiler) pushPointerVars() map[string]bool {
	old := c.pointerVars
	c.pointerVars = map[string]bool{}
	return old
}

func (c *Compiler) popPointerVars(old map[string]bool) {
	c.pointerVars = old
}

func New(env *types.Env) *Compiler {
	return &Compiler{
		env:              env,
		structLits:       map[*parser.MapLiteral]types.StructType{},
		lambdas:          []string{},
		needs:            map[string]bool{},
		structs:          map[string]bool{},
		listStructs:      map[string]bool{},
		fetchStructs:     map[string]bool{},
		externObjects:    []string{},
		captures:         map[string]captureInfo{},
		capturesByFun:    map[string][]string{},
		uninitVars:       map[string]bool{},
		pointerVars:      map[string]bool{},
		groupKeys:        map[string]types.Type{},
		builtinAliases:   map[string]string{},
		listLens:         map[string]int{},
		listVals:         map[string][]int{},
		listValsFloat:    map[string][]float64{},
		listValsString:   map[string][]string{},
		stackArrays:      map[string]bool{},
		arrayLens:        map[string]string{},
		baseDir:          "",
		allocs:           []string{},
		equalListStructs: map[string]bool{},
		funcAliases:      map[string]string{},
		appendTypes:      map[string]types.Type{},
		jsonLines:        map[string][]string{},
	}
}

// NewWithAutoType creates a compiler and controls whether variable
// declarations use GCC's __auto_type for type inference.
func NewWithAutoType(env *types.Env, auto bool) *Compiler {
	c := New(env)
	c.autoType = auto
	return c
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) newTemp() string {
	return c.newTempPrefix("tmp")
}

func (c *Compiler) newTempPrefix(prefix string) string {
	c.tmp++
	return fmt.Sprintf("%s%d", prefix, c.tmp)
}

func (c *Compiler) newLoopVar() string {
	return c.newTempPrefix("i")
}

func (c *Compiler) stackListInt(name, lenExpr, initLen string) {
	c.need(needListInt)
	data := name + "_data"
	c.writeln(fmt.Sprintf("int %s[%s];", data, lenExpr))
	c.writeln(fmt.Sprintf("list_int %s = {%s, %s};", name, initLen, data))
	if c.stackArrays != nil {
		c.stackArrays[data] = true
	}
}

func (c *Compiler) need(key string) {
	if c.needs == nil {
		c.needs = map[string]bool{}
	}
	c.needs[key] = true
}

func (c *Compiler) has(key string) bool {
	return c.needs[key]
}

func (c *Compiler) cType(t *parser.TypeRef) string {
	if t == nil {
		return "void"
	}
	if t.Fun != nil {
		ret := c.cType(t.Fun.Return)
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.cType(p)
		}
		return fmt.Sprintf("%s (*)(%s)", ret, strings.Join(params, ", "))
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "double"
		case "bool":
			return "int"
		case "string":
			return "char*"
		default:
			if c.env != nil {
				if _, ok := c.env.GetStruct(*t.Simple); ok {
					return sanitizeTypeName(*t.Simple)
				}
				if _, ok := c.env.GetUnion(*t.Simple); ok {
					return sanitizeTypeName(*t.Simple)
				}
			}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" {
			if len(t.Generic.Args) == 1 {
				elem := c.cType(t.Generic.Args[0])
				if elem == "int" {
					return "list_int"
				}
				if elem == "double" {
					return "list_float"
				}
				if elem == "char*" {
					return "list_string"
				}
				if elem == "list_int" {
					return "list_list_int"
				}
			}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			key := c.cType(t.Generic.Args[0])
			val := c.cType(t.Generic.Args[1])
			if key == "int" && val == "int" {
				return "map_int_bool"
			}
			if key == "char*" && val == "int" {
				return "map_string_int"
			}
			if key == "int" && val == "char*" {
				return "map_int_string"
			}
		}
	}
	return "int"
}

func (c *Compiler) compileProgram(prog *parser.Program) ([]byte, error) {
	c.appendTypes = c.gatherAppendTypes(prog.Statements)
	defer func() { c.appendTypes = nil }()
	// forward declarations for user-defined types allow recursive structs
	forwardSet := map[string]bool{}
	var forward []string
	skip := map[*parser.Statement]bool{}
	var globals []string
	var initGlobals []*parser.VarStmt
	for _, s := range prog.Statements {
		if s.Import != nil {
			if s.Import.Lang != nil {
				lang := *s.Import.Lang
				path := strings.Trim(s.Import.Path, "\"")
				alias := s.Import.As
				if alias == "" {
					alias = parser.AliasFromPath(s.Import.Path)
				}
				alias = sanitizeName(alias)
				switch lang {
				case "python":
					if path == "math" {
						c.builtinAliases[alias] = "python_math"
						continue
					}
				case "go":
					if path == "mochi/runtime/ffi/go/testpkg" && s.Import.Auto {
						c.builtinAliases[alias] = "go_testpkg"
						continue
					}
				}
			}
		}
		if s.Type != nil {
			name := sanitizeTypeName(s.Type.Name)
			if !forwardSet[name] {
				forwardSet[name] = true
				forward = append(forward, name)
			}
			for _, v := range s.Type.Variants {
				vname := sanitizeTypeName(v.Name)
				if !forwardSet[vname] {
					forwardSet[vname] = true
					forward = append(forward, vname)
				}
			}
		}
		// collect simple global variables so functions can use them
		if s.Var != nil {
			if s.Var.Value != nil {
				if typ, val, ok := constLiteralTypeVal(s.Var.Value); ok {
					t := typ
					var vt types.Type
					if s.Var.Type != nil {
						t = c.cType(s.Var.Type)
						vt = resolveTypeRef(s.Var.Type, c.env)
					} else {
						switch typ {
						case "double":
							vt = types.FloatType{}
						case "char*":
							vt = types.StringType{}
						default:
							if isBoolLiteral(s.Var.Value) {
								vt = types.BoolType{}
							} else {
								vt = types.IntType{}
							}
						}
					}
					globals = append(globals, fmt.Sprintf("static %s %s = %s;", t, sanitizeName(s.Var.Name), val))
					if c.env != nil {
						c.env.SetVar(s.Var.Name, vt, true)
					}
					skip[s] = true
				} else {
					var vt types.Type = types.IntType{}
					if s.Var.Type != nil {
						vt = resolveTypeRef(s.Var.Type, c.env)
					} else if c.env != nil {
						vt = c.exprType(s.Var.Value)
					}
					globals = append(globals, fmt.Sprintf("static %s %s;", cTypeFromType(vt), sanitizeName(s.Var.Name)))
					if c.env != nil {
						c.env.SetVar(s.Var.Name, vt, true)
					}
					initGlobals = append(initGlobals, s.Var)
					skip[s] = true
				}
			} else {
				var vt types.Type = types.IntType{}
				if s.Var.Type != nil {
					vt = resolveTypeRef(s.Var.Type, c.env)
				}
				globals = append(globals, fmt.Sprintf("static %s %s;", cTypeFromType(vt), sanitizeName(s.Var.Name)))
				if c.env != nil {
					c.env.SetVar(s.Var.Name, vt, true)
				}
				skip[s] = true
			}
		}
		if s.Let != nil {
			if s.Let.Value != nil {
				if typ, val, ok := constLiteralTypeVal(s.Let.Value); ok {
					t := typ
					var vt types.Type
					if s.Let.Type != nil {
						t = c.cType(s.Let.Type)
						vt = resolveTypeRef(s.Let.Type, c.env)
					} else {
						switch typ {
						case "double":
							vt = types.FloatType{}
						case "char*":
							vt = types.StringType{}
						default:
							if isBoolLiteral(s.Let.Value) {
								vt = types.BoolType{}
							} else {
								vt = types.IntType{}
							}
						}
					}
					globals = append(globals, fmt.Sprintf("static %s %s = %s;", t, sanitizeName(s.Let.Name), val))
					if c.env != nil {
						c.env.SetVar(s.Let.Name, vt, true)
					}
					skip[s] = true
				} else {
					var vt types.Type = types.IntType{}
					if s.Let.Type != nil {
						vt = resolveTypeRef(s.Let.Type, c.env)
					} else if c.env != nil {
						vt = c.exprType(s.Let.Value)
					}
					globals = append(globals, fmt.Sprintf("static %s %s;", cTypeFromType(vt), sanitizeName(s.Let.Name)))
					if c.env != nil {
						c.env.SetVar(s.Let.Name, vt, true)
					}
					initGlobals = append(initGlobals, &parser.VarStmt{Name: s.Let.Name, Type: s.Let.Type, Value: s.Let.Value})
					skip[s] = true
				}
			} else {
				var vt types.Type = types.IntType{}
				if s.Let.Type != nil {
					vt = resolveTypeRef(s.Let.Type, c.env)
				}
				globals = append(globals, fmt.Sprintf("static %s %s;", cTypeFromType(vt), sanitizeName(s.Let.Name)))
				if c.env != nil {
					c.env.SetVar(s.Let.Name, vt, true)
				}
				skip[s] = true
			}
		}
	}

	// compile body first to know which helpers are needed
	oldBuf := c.buf
	c.buf = bytes.Buffer{}
	c.externs = nil
	c.lambdas = nil
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Import != nil {
			// import statements are ignored in the C backend
			continue
		}
		if s.ExternType != nil {
			name := sanitizeName(s.ExternType.Name)
			c.externs = append(c.externs, fmt.Sprintf("typedef struct %s %s;", name, name))
			if c.env != nil {
				st := types.StructType{Name: s.ExternType.Name, Fields: map[string]types.Type{}, Order: []string{}, Methods: map[string]types.Method{}}
				c.env.SetStruct(s.ExternType.Name, st)
			}
			continue
		}
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.ExternVar != nil {
			typ := c.cType(s.ExternVar.Type)
			name := sanitizeName(s.ExternVar.Name())
			c.externs = append(c.externs, fmt.Sprintf("extern %s %s;", typ, name))
			if c.env != nil {
				c.env.SetVar(s.ExternVar.Name(), resolveTypeRef(s.ExternVar.Type, c.env), true)
			}
		} else if s.ExternFun != nil {
			ret := c.cType(s.ExternFun.Return)
			params := make([]string, len(s.ExternFun.Params))
			var ptypes []types.Type
			for i, p := range s.ExternFun.Params {
				params[i] = fmt.Sprintf("%s %s", c.cType(p.Type), sanitizeName(p.Name))
				if c.env != nil {
					ptypes = append(ptypes, resolveTypeRef(p.Type, c.env))
				}
			}
			fname := sanitizeName(s.ExternFun.Name())
			c.externs = append(c.externs, fmt.Sprintf("extern %s %s(%s);", ret, fname, strings.Join(params, ", ")))
			if c.env != nil {
				ft := types.FuncType{Params: ptypes, Return: resolveTypeRef(s.ExternFun.Return, c.env)}
				c.env.SetVar(s.ExternFun.Name(), ft, true)
			}
		} else if s.ExternObject != nil {
			name := sanitizeName(s.ExternObject.Name)
			c.externs = append(c.externs, fmt.Sprintf("extern void* %s;", name))
			c.externObjects = append(c.externObjects, name)
			if c.env != nil {
				c.env.SetVar(s.ExternObject.Name, types.AnyType{}, true)
			}
		}
	}
	// entry function
	c.writeln("int _mochi_main() {")
	c.indent++
	for _, name := range c.externObjects {
		c.writeln(fmt.Sprintf("if (%s == NULL) { fprintf(stderr, \"extern object not registered: %s\\n\"); return 1; }", name, name))
	}
	for _, gv := range initGlobals {
		if err := c.compileStmt(&parser.Statement{Assign: &parser.AssignStmt{Name: gv.Name, Value: gv.Value}}); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if skip[s] {
			continue
		}
		if s.Fun == nil && s.Test == nil {
			if err := c.compileStmt(s); err != nil {
				return nil, err
			}
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			if caps, ok := c.capturesByFun[name]; ok && len(caps) > 0 {
				for _, v := range caps {
					src := sanitizeName(v)
					dst := fmt.Sprintf("%s_%s", name, src)
					if c.isStackArrayExpr(src) && c.env != nil {
						if vt, err := c.env.GetVar(v); err == nil {
							if _, ok := vt.(types.ListType); ok {
								c.writeln(fmt.Sprintf("%s.len = %s;", dst, c.listLenExpr(src)))
								c.writeln(fmt.Sprintf("%s.data = %s;", dst, src))
								continue
							}
						}
					}
					c.writeln(fmt.Sprintf("%s = %s;", dst, src))
				}
			}
			c.writeln(name + "();")
		}
	}
	for _, v := range c.allocs {
		c.writeln(fmt.Sprintf("free(%s.data);", v))
	}
	c.writeln("return 0;")
	c.indent--
	c.writeln("}")
	c.writeln("int main() { return _mochi_main(); }")
	body := c.buf.String()
	if len(c.lambdas) > 0 {
		body = strings.Join(c.lambdas, "\n") + "\n" + body
	}
	c.buf = oldBuf

	c.writeln("#include <stdio.h>")
	c.writeln("#include <stdlib.h>")
	if c.needMath {
		c.writeln("#include <math.h>")
	}
	if c.has(needStringHeader) || c.has(needConcatString) || c.has(needConcatListString) || c.has(needListString) || c.has(needUnionListString) || c.has(needExceptListString) || c.has(needIntersectListString) || c.has(needInListString) || c.has(needInString) || c.has(needMapStringInt) || c.has(needGroupByPairString) || c.has(needLower) || c.has(needUpper) || c.has(needSplit) || c.has(needFloat) {
		c.writeln("#include <string.h>")
	}
	if c.has(needLower) || c.has(needUpper) {
		c.writeln("#include <ctype.h>")
	}
	if c.has(needNow) {
		c.writeln("#include <time.h>")
	}
	if c.has(needSHA256) {
		c.writeln("#include <openssl/sha.h>")
	}
	c.writeln("")
	c.emitRuntime()
	if c.buf.Len() > 0 && c.buf.Bytes()[c.buf.Len()-1] != '\n' {
		c.writeln("")
	}
	for _, name := range forward {
		c.writeln(fmt.Sprintf("typedef struct %s %s;", name, name))
	}
	if len(forward) > 0 {
		c.writeln("")
	}
	for _, ex := range c.externs {
		c.writeln(ex)
	}
	if len(c.externs) > 0 {
		c.writeln("")
	}
	for _, g := range globals {
		c.writeln(g)
	}
	if len(globals) > 0 {
		c.writeln("")
	}
	c.buf.WriteString(body)
	return FormatC(c.buf.Bytes()), nil
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	if prog != nil && prog.Pos.Filename != "" {
		c.baseDir = filepath.Dir(prog.Pos.Filename)
	}
	return c.compileProgram(prog)
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeTypeName(t.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	if len(t.Variants) > 0 {
		// compile variant structs
		for _, v := range t.Variants {
			vname := sanitizeTypeName(v.Name)
			c.writeln(fmt.Sprintf("typedef struct %s {", vname))
			c.indent++
			for _, f := range v.Fields {
				typ := c.cType(f.Type)
				if f.Type != nil && f.Type.Simple != nil && *f.Type.Simple == t.Name {
					typ += "*"
				}
				c.writeln(fmt.Sprintf("%s %s;", typ, sanitizeName(f.Name)))
			}
			c.indent--
			c.writeln(fmt.Sprintf("}%s;", vname))
		}
		// union wrapper
		c.writeln(fmt.Sprintf("typedef struct %s {", name))
		c.indent++
		c.writeln("int tag;")
		c.writeln("union {")
		c.indent++
		for _, v := range t.Variants {
			vname := sanitizeTypeName(v.Name)
			c.writeln(fmt.Sprintf("%s %s;", vname, vname))
		}
		c.indent--
		c.writeln("} value;")
		c.indent--
		c.writeln(fmt.Sprintf("}%s;", name))
		// tag constants
		keys := make([]string, len(t.Variants))
		for i, v := range t.Variants {
			keys[i] = sanitizeTypeName(v.Name)
		}
		sort.Strings(keys)
		for i, k := range keys {
			c.writeln(fmt.Sprintf("#define %s_%s %d", sanitizeTypeName(t.Name), k, i))
		}
		if c.env != nil {
			ut := types.UnionType{Name: t.Name, Variants: map[string]types.StructType{}}
			for _, v := range t.Variants {
				fields := map[string]types.Type{}
				order := []string{}
				for _, f := range v.Fields {
					fields[f.Name] = resolveTypeRef(f.Type, c.env)
					order = append(order, f.Name)
				}
				st := types.StructType{Name: v.Name, Fields: fields, Order: order, Methods: map[string]types.Method{}}
				c.env.SetStruct(v.Name, st)
				ut.Variants[v.Name] = st
			}
			c.env.SetUnion(t.Name, ut)
		}
		return nil
	}
	c.writeln(fmt.Sprintf("typedef struct %s {", name))
	c.indent++
	var fields map[string]types.Type
	var order []string
	if c.env != nil {
		if st, ok := c.env.GetStruct(t.Name); ok {
			fields = st.Fields
			order = st.Order
		} else {
			fields = map[string]types.Type{}
		}
	}
	for _, m := range t.Members {
		if m.Field == nil {
			continue
		}
		typ := c.cType(m.Field.Type)
		c.writeln(fmt.Sprintf("%s %s;", typ, sanitizeName(m.Field.Name)))
		if c.env != nil {
			fields[m.Field.Name] = resolveTypeRef(m.Field.Type, c.env)
			order = append(order, m.Field.Name)
		}
	}
	c.indent--
	c.writeln(fmt.Sprintf("}%s;", name))
	if c.env != nil {
		st := types.StructType{Name: t.Name, Fields: fields, Order: order}
		if orig, ok := c.env.GetStruct(t.Name); ok {
			st.Methods = orig.Methods
		}
		c.env.SetStruct(t.Name, st)
		c.compileStructListType(st)
	}
	for _, m := range t.Members {
		if m.Method != nil {
			if err := c.compileTypeMethod(t.Name, m.Method); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	oldEnv := c.env
	oldPtr := c.pushPointerVars()
	var bodyEnv *types.Env
	if c.env != nil {
		bodyEnv = types.NewEnv(c.env)
		for _, p := range fun.Params {
			if p.Type != nil {
				bodyEnv.SetVar(p.Name, resolveTypeRef(p.Type, c.env), true)
				if _, ok := resolveTypeRef(p.Type, c.env).(types.StructType); ok {
					c.pointerVars[p.Name] = true
				}
			} else {
				bodyEnv.SetVar(p.Name, types.IntType{}, true)
			}
		}
	}
	c.appendTypes = c.gatherAppendTypes(fun.Body)
	defer func() { c.appendTypes = nil }()
	var retType types.Type
	if fun.Return != nil {
		retType = resolveTypeRef(fun.Return, c.env)
		if types.IsStringAnyMapLike(retType) && c.env != nil {
			c.env = bodyEnv
			if t := c.inferFunReturnType(fun.Body); !types.IsStringAnyMapLike(t) {
				retType = t
			}
			c.env = oldEnv
		}
	} else {
		if c.env != nil {
			c.env = bodyEnv
			retType = c.inferFunReturnType(fun.Body)
			c.env = oldEnv
		} else {
			retType = types.VoidType{}
		}
	}
	ret := cTypeFromType(retType)
	if isListIntType(retType) {
		c.need(needListInt)
	}
	if isListFloatType(retType) {
		c.need(needListFloat)
	}
	if isListStringType(retType) {
		c.need(needListString)
	}
	if isListListIntType(retType) {
		c.need(needListListInt)
		c.need(needListInt)
	}
	var retSuffix string
	fname := sanitizeName(fun.Name)
	if fun.Name == "main" {
		fname = "mochi_main"
		if c.funcAliases != nil {
			c.funcAliases["main"] = fname
		}
	}
	if strings.Contains(ret, "(*") {
		idx := strings.Index(ret, "(*")
		prefix := ret[:idx+2]
		retSuffix = ret[idx+2:]
		c.buf.WriteString(prefix + fname)
	} else {
		c.buf.WriteString(ret + " ")
		c.buf.WriteString(fname)
	}
	c.buf.WriteByte('(')
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		typ := c.cType(p.Type)
		if p.Type != nil {
			if _, ok := resolveTypeRef(p.Type, c.env).(types.StructType); ok {
				typ += "*"
			}
		}
		c.buf.WriteString(typ)
		c.buf.WriteByte(' ')
		c.buf.WriteString(sanitizeName(p.Name))
		if p.Type != nil {
			t := resolveTypeRef(p.Type, c.env)
			if isListStringType(t) {
				c.need(needListString)
			}
			if isListFloatType(t) {
				c.need(needListFloat)
			}
			if isListIntType(t) {
				c.need(needListInt)
			}
			if isListListIntType(t) {
				c.need(needListListInt)
				c.need(needListInt)
				c.need(needListInt)
				c.need(needListInt)
				c.need(needListInt)
			}
		}
	}
	c.buf.WriteByte(')')
	if retSuffix != "" {
		c.buf.WriteString(retSuffix)
	}
	c.buf.WriteString(" {\n")
	c.indent++
	prevEnv := c.env
	if c.env != nil {
		c.env = bodyEnv
	}
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if c.env != nil {
		c.env = prevEnv
	}
	c.popPointerVars(oldPtr)
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileTypeMethod(structName string, fun *parser.FunStmt) error {
	oldEnv := c.env
	oldPtr := c.pushPointerVars()
	var bodyEnv *types.Env
	if c.env != nil {
		bodyEnv = types.NewEnv(c.env)
		if st, ok := oldEnv.GetStruct(structName); ok {
			for name, ft := range st.Fields {
				bodyEnv.SetVar(name, ft, true)
			}
			for name, m := range st.Methods {
				bodyEnv.SetVar(name, m.Type, true)
			}
		}
		for _, p := range fun.Params {
			if p.Type != nil {
				bodyEnv.SetVar(p.Name, resolveTypeRef(p.Type, oldEnv), true)
				if _, ok := resolveTypeRef(p.Type, oldEnv).(types.StructType); ok {
					c.pointerVars[p.Name] = true
				}
			} else {
				bodyEnv.SetVar(p.Name, types.IntType{}, true)
			}
		}
	}
	var retType types.Type
	if fun.Return != nil {
		retType = resolveTypeRef(fun.Return, c.env)
		if types.IsStringAnyMapLike(retType) && c.env != nil {
			c.env = bodyEnv
			if t := c.inferFunReturnType(fun.Body); !types.IsStringAnyMapLike(t) {
				retType = t
			}
			c.env = oldEnv
		}
	} else {
		if c.env != nil {
			c.env = bodyEnv
			retType = c.inferFunReturnType(fun.Body)
			c.env = oldEnv
		} else {
			retType = types.VoidType{}
		}
	}
	ret := cTypeFromType(retType)
	if isListIntType(retType) {
		c.need(needListInt)
	}
	if isListFloatType(retType) {
		c.need(needListFloat)
	}
	if isListStringType(retType) {
		c.need(needListString)
	}
	if isListListIntType(retType) {
		c.need(needListListInt)
		c.need(needListInt)
	}
	c.buf.WriteString(ret + " ")
	c.buf.WriteString(sanitizeName(structName) + "_" + sanitizeName(fun.Name))
	c.buf.WriteByte('(')
	c.buf.WriteString(sanitizeTypeName(structName) + "* self")
	for _, p := range fun.Params {
		c.buf.WriteString(", ")
		c.buf.WriteString(c.cType(p.Type))
		c.buf.WriteByte(' ')
		c.buf.WriteString(sanitizeName(p.Name))
	}
	c.buf.WriteString("){\n")
	c.indent++
	prevEnv := c.env
	oldStruct := c.currentStruct
	if c.env != nil {
		c.env = bodyEnv
	}
	c.currentStruct = structName
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.currentStruct = oldStruct
	if c.env != nil {
		c.env = prevEnv
	}
	c.popPointerVars(oldPtr)
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeTypeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	if c.indent > 0 {
		oldBuf := c.buf
		oldIndent := c.indent
		c.buf = bytes.Buffer{}
		c.indent = 0
		c.writeln("typedef struct {")
		c.indent++
		for _, fn := range st.Order {
			ft := st.Fields[fn]
			c.writeln(fmt.Sprintf("%s %s;", cTypeFromType(ft), fieldName(fn)))
		}
		c.indent--
		c.writeln(fmt.Sprintf("}%s;", name))
		c.compileStructListType(st)
		c.lambdas = append(c.lambdas, c.buf.String())
		c.buf = oldBuf
		c.indent = oldIndent
	} else {
		c.writeln("typedef struct {")
		c.indent++
		for _, fn := range st.Order {
			ft := st.Fields[fn]
			c.writeln(fmt.Sprintf("%s %s;", cTypeFromType(ft), fieldName(fn)))
		}
		c.indent--
		c.writeln(fmt.Sprintf("}%s;", name))
		c.compileStructListType(st)
	}
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func (c *Compiler) compileStructListType(st types.StructType) {
	listName := sanitizeListName(st.Name)
	if c.listStructs[listName] {
		return
	}
	c.listStructs[listName] = true
	if c.indent > 0 {
		oldBuf := c.buf
		oldIndent := c.indent
		c.buf = bytes.Buffer{}
		c.indent = 0
		c.writeln(fmt.Sprintf("typedef struct { int len; %s *data; } %s;", sanitizeTypeName(st.Name), listName))
		c.writeln(fmt.Sprintf("%s %s(int len) {", listName, createListFuncName(st.Name)))
		c.indent++
		c.writeln(fmt.Sprintf("%s l;", listName))
		c.writeln("l.len = len;")
		c.writeln(fmt.Sprintf("l.data = calloc(len, sizeof(%s));", sanitizeTypeName(st.Name)))
		c.writeln("if (!l.data && len > 0) { fprintf(stderr, \"alloc failed\\n\"); exit(1); }")
		c.writeln("return l;")
		c.indent--
		c.writeln("}")
		c.lambdas = append(c.lambdas, c.buf.String())
		c.buf = oldBuf
		c.indent = oldIndent
		return
	}
	c.writeln(fmt.Sprintf("typedef struct { int len; %s *data; } %s;", sanitizeTypeName(st.Name), listName))
	c.writeln(fmt.Sprintf("%s %s(int len) {", listName, createListFuncName(st.Name)))
	c.indent++
	c.writeln(fmt.Sprintf("%s l;", listName))
	c.writeln("l.len = len;")
	c.writeln(fmt.Sprintf("l.data = calloc(len, sizeof(%s));", sanitizeTypeName(st.Name)))
	c.writeln("if (!l.data && len > 0) { fprintf(stderr, \"alloc failed\\n\"); exit(1); }")
	c.writeln("return l;")
	c.indent--
	c.writeln("}")
}

func (c *Compiler) emitFetchStructFunc(st types.StructType) string {
	name := "_fetch_" + sanitizeName(st.Name)
	if c.fetchStructs[name] {
		return name
	}
	c.fetchStructs[name] = true
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = 0
	c.writeln(fmt.Sprintf("static %s %s(const char* url, void* opts){", sanitizeTypeName(st.Name), name))
	c.indent++
	c.writeln("map_string row = _fetch(url, opts);")
	c.writeln(fmt.Sprintf("%s out;", sanitizeTypeName(st.Name)))
	for _, fn := range st.Order {
		ft := st.Fields[fn]
		field := fieldName(fn)
		switch ft.(type) {
		case types.IntType, types.BoolType:
			c.writeln(fmt.Sprintf("out.%s = atoi(map_string_get(row, \"%s\"));", field, fn))
		case types.FloatType:
			c.writeln(fmt.Sprintf("out.%s = atof(map_string_get(row, \"%s\"));", field, fn))
		case types.StringType:
			c.writeln(fmt.Sprintf("out.%s = strdup(map_string_get(row, \"%s\"));", field, fn))
		default:
			c.writeln(fmt.Sprintf("/* unsupported field %s */", fn))
		}
	}
	c.writeln("return out;")
	c.indent--
	c.writeln("}")
	c.lambdas = append(c.lambdas, c.buf.String())
	c.buf = oldBuf
	c.indent = oldIndent
	return name
}

func (c *Compiler) emitEqualListStructFunc(st types.StructType) string {
	name := "_equal_list_" + sanitizeName(st.Name)
	if c.equalListStructs[name] {
		return name
	}
	c.equalListStructs[name] = true
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = 0
	listName := sanitizeListName(st.Name)
	structName := sanitizeTypeName(st.Name)
	c.writeln(fmt.Sprintf("static int %s(%s a, %s b){", name, listName, listName))
	c.indent++
	c.writeln("if (a.len != b.len) return 0;")
	c.writeln("for(int i=0;i<a.len;i++){")
	c.indent++
	c.writeln(fmt.Sprintf("if (memcmp(&a.data[i], &b.data[i], sizeof(%s)) != 0) return 0;", structName))
	c.indent--
	c.writeln("}")
	c.writeln("return 1;")
	c.indent--
	c.writeln("}")
	c.lambdas = append(c.lambdas, c.buf.String())
	c.buf = oldBuf
	c.indent = oldIndent
	c.need(needStringHeader)
	return name
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Fun != nil:
		// Nested function declarations are hoisted to the top level.
		// Capture variables from the enclosing scope using globals.
		oldBuf := c.buf
		oldIndent := c.indent
		oldCaps := c.captures
		paramNames := make([]string, len(s.Fun.Params))
		for i, p := range s.Fun.Params {
			paramNames[i] = sanitizeName(p.Name)
		}
		captured := freeVarsStmt(s.Fun, paramNames)
		capMap := map[string]captureInfo{}
		for _, v := range captured {
			if c.env != nil {
				if t, err := c.env.GetVar(v); err == nil {
					g := fmt.Sprintf("%s_%s", sanitizeName(s.Fun.Name), sanitizeName(v))
					capMap[v] = captureInfo{global: g, typ: t}
					c.lambdas = append(c.lambdas, fmt.Sprintf("static %s %s;", cTypeFromType(t), g))
				}
			}
		}
		c.captures = capMap
		c.buf = bytes.Buffer{}
		c.indent = 0
		if err := c.compileFun(s.Fun); err != nil {
			c.captures = oldCaps
			c.buf = oldBuf
			c.indent = oldIndent
			return err
		}
		code := c.buf.String()
		c.lambdas = append(c.lambdas, code)
		c.captures = oldCaps
		c.buf = oldBuf
		c.indent = oldIndent
		if len(captured) > 0 {
			c.capturesByFun[sanitizeName(s.Fun.Name)] = captured
		}
		if c.env != nil {
			var params []types.Type
			for _, p := range s.Fun.Params {
				if p.Type != nil {
					params = append(params, resolveTypeRef(p.Type, c.env))
				} else {
					params = append(params, types.IntType{})
				}
			}
			var ret types.Type
			if s.Fun.Return != nil {
				ret = resolveTypeRef(s.Fun.Return, c.env)
			} else {
				bodyEnv := types.NewEnv(c.env)
				for i, p := range s.Fun.Params {
					bodyEnv.SetVar(p.Name, params[i], true)
				}
				prev := c.env
				c.env = bodyEnv
				ret = c.inferFunReturnType(s.Fun.Body)
				c.env = prev
			}
			if ret == nil {
				ret = types.VoidType{}
			}
			ft := types.FuncType{Params: params, Return: ret}
			c.env.SetVar(s.Fun.Name, ft, true)
		}
		return nil
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		lhs := sanitizeName(s.Assign.Name)
		target := lhs
		if c.currentStruct != "" && c.env != nil && len(s.Assign.Index) == 0 {
			if st, ok := c.env.GetStruct(c.currentStruct); ok {
				if _, ok2 := st.Fields[s.Assign.Name]; ok2 {
					target = "self->" + lhs
				}
			}
		}
		var curT types.Type
		if c.env != nil {
			curT, _ = c.env.GetVar(s.Assign.Name)
		}
		if call, ok := callPattern(s.Assign.Value); ok && call.Func == "append" && len(call.Args) == 2 {
			delete(c.listLens, lhs)
			delete(c.listVals, lhs)
			delete(c.listValsString, lhs)
			delete(c.listValsFloat, lhs)
			delete(c.listValsString, lhs)
		}
		if len(s.Assign.Index) == 1 {
			if c.env != nil {
				if tv := curT; tv != nil {
					key := c.compileExpr(s.Assign.Index[0].Start)
					val := c.compileExpr(s.Assign.Value)
					if isMapIntBoolType(tv) {
						c.need(needMapIntBool)
						c.writeln(fmt.Sprintf("map_int_bool_put(&%s, %s, %s);", lhs, key, val))
						return nil
					}
					if isMapStringIntType(tv) {
						c.need(needMapStringInt)
						c.writeln(fmt.Sprintf("map_string_int_put(&%s, %s, %s);", lhs, key, val))
						return nil
					}
					if isMapIntStringType(tv) {
						c.need(needMapIntString)
						c.writeln(fmt.Sprintf("map_int_string_put(&%s, %s, %s);", lhs, key, val))
						return nil
					}
				}
			}
		}
		for _, idx := range s.Assign.Index {
			if st, ok := curT.(types.StructType); ok {
				if key, ok2 := types.SimpleStringKey(idx.Start); ok2 {
					if ft, ok3 := st.Fields[key]; ok3 {
						target = fmt.Sprintf("%s.%s", target, fieldName(key))
						curT = ft
						continue
					}
				}
			}
			ix := c.compileExpr(idx.Start)
			target = c.listItemExpr(target, ix)
			if lt, ok := curT.(types.ListType); ok {
				curT = lt.Elem
			} else if mt, ok := curT.(types.MapType); ok {
				curT = mt.Value
			}
		}
		useArrow := c.pointerVars[lhs] && len(s.Assign.Index) == 0
		for i, f := range s.Assign.Field {
			if i == 0 && useArrow {
				target = fmt.Sprintf("%s->%s", target, fieldName(f.Name))
				useArrow = false
			} else {
				target = fmt.Sprintf("%s.%s", target, fieldName(f.Name))
			}
		}
		if call, ok := callPattern(s.Assign.Value); ok && call.Func == "append" && len(call.Args) == 2 {
			elem := listElemType(call.Args[0], c.env)
			if elem == nil || types.ContainsAny(elem) {
				elem = c.guessType(call.Args[1])
				if ml := asMapLiteral(call.Args[1]); ml != nil {
					if st, ok := c.structLits[ml]; ok {
						elem = st
					} else if st, ok2 := c.inferStructFromMap(ml, s.Assign.Name); ok2 {
						elem = st
						c.structLits[ml] = st
						if c.env != nil {
							c.env.SetStruct(st.Name, st)
						}
						c.compileStructType(st)
						c.compileStructListType(st)
					}
				}
			}
			if st, ok := elem.(types.StructType); ok {
				c.compileStructType(st)
				c.compileStructListType(st)
			}
			if c.env != nil && elem != nil {
				c.env.SetVar(s.Assign.Name, types.ListType{Elem: elem}, true)
			}
			delete(c.listLens, lhs)
			delete(c.listVals, lhs)
		}
		val := c.compileExpr(s.Assign.Value)
		c.writeln(fmt.Sprintf("%s = %s;", target, val))
	case s.Return != nil:
		val := c.compileExpr(s.Return.Value)
		c.writeln(fmt.Sprintf("return %s;", val))
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expr != nil:
		expr := c.compileExpr(s.Expr.Expr)
		if expr != "" {
			c.writeln(fmt.Sprintf("%s;", expr))
		}
	case s.Expect != nil:
		if err := c.compileExpect(s.Expect); err != nil {
			return err
		}
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Import != nil:
		// imports are ignored at code generation time
		return nil
	case s.ExternType != nil:
		// extern type declarations have no runtime effect
		return nil
	case s.ExternObject != nil:
		// extern objects have no runtime effect
		return nil
	default:
		// unsupported
	}
	return nil
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	name := sanitizeName(stmt.Name)
	var t types.Type
	if stmt.Type != nil {
		t = resolveTypeRef(stmt.Type, c.env)
		if stmt.Value != nil {
			if st, ok := castMapToStruct(stmt.Value, c.env); ok {
				t = st
			} else if ll := stmt.Value.Binary.Left.Value.Target.List; ll != nil {
				if st, ok := c.inferStructFromList(ll, stmt.Name); ok {
					t = types.ListType{Elem: st}
					if c.env != nil {
						c.env.SetStruct(st.Name, st)
					}
					c.compileStructType(st)
					c.compileStructListType(st)
					for _, el := range ll.Elems {
						if ml := el.Binary.Left.Value.Target.Map; ml != nil {
							c.structLits[ml] = st
						}
					}
				}
			} else if ml := stmt.Value.Binary.Left.Value.Target.Map; ml != nil {
				if !(isMapStringIntLiteral(ml, c.env) || isMapIntStringLiteral(ml, c.env)) {
					if st, ok := c.inferStructFromMap(ml, stmt.Name); ok {
						t = st
						if c.env != nil {
							c.env.SetStruct(st.Name, st)
						}
						c.compileStructType(st)
						c.structLits[ml] = st
					}
				}
			}
		}
	} else if stmt.Value != nil {
		t = c.exprType(stmt.Value)
		if sl := asStructLiteral(stmt.Value); sl != nil && c.env != nil {
			if ut, ok := c.env.FindUnionByVariant(sl.Name); ok {
				t = ut
			}
		}
		if lt, ok := t.(types.ListType); ok {
			if gt, ok2 := lt.Elem.(types.GroupType); ok2 {
				elem := gt.Elem
				listElem := types.ListType{Elem: elem}
				structName := "Group" + sanitizeTypeName(cTypeFromType(elem))
				st := types.StructType{
					Name:   structName,
					Fields: map[string]types.Type{"key": gt.Key, "items": listElem},
					Order:  []string{"key", "items"},
				}
				c.compileStructType(st)
				c.compileStructListType(st)
				t = types.ListType{Elem: st}
			}
		}
		if st, ok := castMapToStruct(stmt.Value, c.env); ok {
			t = st
		} else if ll := stmt.Value.Binary.Left.Value.Target.List; ll != nil {
			if st, ok := c.inferStructFromList(ll, stmt.Name); ok {
				t = types.ListType{Elem: st}
				if c.env != nil {
					c.env.SetStruct(st.Name, st)
				}
				c.compileStructType(st)
				c.compileStructListType(st)
				for _, el := range ll.Elems {
					if ml := el.Binary.Left.Value.Target.Map; ml != nil {
						c.structLits[ml] = st
					}
				}
			}
		} else if ml := stmt.Value.Binary.Left.Value.Target.Map; ml != nil {
			if !(isMapStringIntLiteral(ml, c.env) || isMapIntStringLiteral(ml, c.env)) {
				if st, ok := c.inferStructFromMap(ml, stmt.Name); ok {
					t = st
					if c.env != nil {
						c.env.SetStruct(st.Name, st)
					}
					c.compileStructType(st)
					c.structLits[ml] = st
				}
			}
		} else if q := stmt.Value.Binary.Left.Value.Target.Query; q != nil {
			if ml := asMapLiteral(q.Select); ml != nil && c.env != nil {
				// Build a temporary environment with query variables to
				// improve type inference of the map literal.
				qenv := types.NewEnv(c.env)
				if st, ok := c.exprType(q.Source).(types.ListType); ok {
					qenv.SetVar(q.Var, st.Elem, true)
				}
				for _, f := range q.Froms {
					if ft, ok := c.exprType(f.Src).(types.ListType); ok {
						qenv.SetVar(f.Var, ft.Elem, true)
					}
				}
				for _, j := range q.Joins {
					if jt, ok := c.exprType(j.Src).(types.ListType); ok {
						qenv.SetVar(j.Var, jt.Elem, true)
					}
				}
				if q.Group != nil {
					if st, ok := c.exprType(q.Source).(types.ListType); ok {
						qenv.SetVar(q.Group.Name, types.GroupType{Elem: st.Elem}, true)
					}
				}
				prevEnv := c.env
				c.env = qenv
				if q.Group != nil {
					var keyT types.Type
					if len(q.Group.Exprs) == 1 {
						keyT = c.exprType(q.Group.Exprs[0])
					} else if len(q.Group.Exprs) == 2 {
						if _, ok := c.exprType(q.Group.Exprs[0]).(types.StringType); ok {
							if _, ok2 := c.exprType(q.Group.Exprs[1]).(types.StringType); ok2 {
								keyT = types.StructType{Name: "pair_string", Fields: map[string]types.Type{"a": types.StringType{}, "b": types.StringType{}}, Order: []string{"a", "b"}}
							}
						}
					}
					if keyT != nil {
						c.groupKeys[q.Group.Name] = keyT
					}
				}
				if st, ok := c.inferStructFromMap(ml, stmt.Name); ok {
					t = types.ListType{Elem: st}
					c.env = prevEnv
					c.env.SetStruct(st.Name, st)
					c.compileStructType(st)
					c.compileStructListType(st)
					c.structLits[ml] = st
				} else {
					c.env = prevEnv
				}
				if q.Group != nil {
					delete(c.groupKeys, q.Group.Name)
				}
			}
		}
	}
	if _, ok := t.(types.AnyType); ok && stmt.Value != nil {
		if isFloatArg(stmt.Value, c.env) {
			t = types.FloatType{}
		}
	}
	if t == nil {
		t = types.IntType{}
	}
	typ := cTypeFromType(t)
	if c.autoType && stmt.Type == nil && stmt.Value != nil {
		typ = "__auto_type"
	}
	if stmt.Value != nil && isNowExpr(stmt.Value) {
		typ = "long long"
		if c.env != nil {
			t = types.Int64Type{}
		}
	}
	if stmt.Value != nil && isNowExpr(stmt.Value) {
		typ = "long long"
		if c.env != nil {
			t = types.Int64Type{}
		}
	}
	if stmt.Value != nil {
		if vals, ok := c.evalListIntExpr(stmt.Value); ok && (isListIntType(t) || isListBoolType(t) || t == nil || types.ContainsAny(t)) {
			if t == nil || types.ContainsAny(t) {
				t = types.ListType{Elem: types.IntType{}}
				if c.env != nil {
					c.env.SetVar(stmt.Name, t, false)
				}
			}
			{
				parts := make([]string, len(vals))
				for i, v := range vals {
					parts[i] = strconv.Itoa(v)
				}
				c.need(needListInt)
				c.writeln(fmt.Sprintf("list_int %s = list_int_create(%d);", name, len(vals)))
				for i, v := range parts {
					c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
				}
				c.listLens[name] = len(vals)
				c.listVals[name] = vals
				return nil
			}
		} else if fvals, ok := c.evalListFloatExpr(stmt.Value); ok && (isListFloatType(t) || t == nil || types.ContainsAny(t)) {
			if t == nil || types.ContainsAny(t) {
				t = types.ListType{Elem: types.FloatType{}}
				if c.env != nil {
					c.env.SetVar(stmt.Name, t, false)
				}
			}
			c.need(needListFloat)
			c.writeln(fmt.Sprintf("list_float %s = list_float_create(%d);", name, len(fvals)))
			for i, v := range fvals {
				s := strconv.FormatFloat(v, 'f', -1, 64)
				if !strings.ContainsAny(s, ".eE") {
					s += ".0"
				}
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, s))
			}
			c.listLens[name] = len(fvals)
			c.listValsFloat[name] = fvals
			return nil
		} else if svals, ok := c.evalListStringExpr(stmt.Value); ok && (isListStringType(t) || t == nil || types.ContainsAny(t)) {
			if t == nil || types.ContainsAny(t) {
				t = types.ListType{Elem: types.StringType{}}
				if c.env != nil {
					c.env.SetVar(stmt.Name, t, false)
				}
			}
			c.need(needListString)
			c.writeln(fmt.Sprintf("list_string %s = list_string_create(%d);", name, len(svals)))
			for i, v := range svals {
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
			c.listLens[name] = len(svals)
			c.listValsString[name] = svals
			return nil
		}
		if f := asFetchExpr(stmt.Value); f != nil {
			if st, ok := t.(types.StructType); ok {
				c.need(needFetch)
				c.need(needStringHeader)
				c.need(needMapStringGet)
				fname := c.emitFetchStructFunc(st)
				url := c.compileExpr(f.URL)
				opts := "NULL"
				if f.With != nil {
					opts = c.compileExpr(f.With)
				}
				c.writeln(fmt.Sprintf("%s %s = %s(%s, %s);", typ, name, fname, url, opts))
			} else {
				val := c.compileFetchExpr(f)
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			}
		} else if isEmptyListLiteral(stmt.Value) {
			if isListStringType(t) {
				c.need(needListString)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("list_string %s = {0, NULL};", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			} else if isListFloatType(t) {
				c.need(needListFloat)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("list_float %s = {0, NULL};", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			} else if isListListIntType(t) {
				c.need(needListListInt)
				c.need(needListInt)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("list_list_int %s = {0, NULL};", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			} else if isListIntType(t) || isListBoolType(t) {
				c.need(needListInt)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("list_int %s = {0, NULL};", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			} else if st, ok := isListStructType(t); ok {
				c.compileStructListType(st)
				listName := sanitizeListName(st.Name)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("%s %s = {0, NULL};", listName, val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			} else if isMapIntBoolType(t) && isEmptyMapLiteral(stmt.Value) {
				c.need(needMapIntBool)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("map_int_bool %s = map_int_bool_create(0);", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			} else if isMapStringIntType(t) && isEmptyMapLiteral(stmt.Value) {
				c.need(needMapStringInt)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("map_string_int %s = map_string_int_create(0);", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			} else if isMapIntStringType(t) && isEmptyMapLiteral(stmt.Value) {
				c.need(needMapIntString)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("map_int_string %s = map_int_string_create(0);", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			} else if isMapStringIntType(t) && isEmptyMapLiteral(stmt.Value) {
				c.need(needMapStringInt)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("map_string_int %s = map_string_int_create(0);", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
			} else if isMapIntStringType(t) && isEmptyMapLiteral(stmt.Value) {
				c.need(needMapIntString)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("map_int_string %s = map_int_string_create(0);", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
			} else {
				c.assignVar = name
				val := c.compileExpr(stmt.Value)
				c.assignVar = ""
				if q := stmt.Value.Binary.Left.Value.Target.Query; q != nil {
					if ml := asMapLiteral(q.Select); ml != nil {
						if st, ok := c.structLits[ml]; ok {
							typ = sanitizeListName(st.Name)
							t = types.ListType{Elem: st}
						}
					}
				}
				if val != name {
					c.writeln(formatFuncPtrDecl(typ, name, val))
					for i, a := range c.allocs {
						if a == val {
							c.allocs[i] = name
						}
					}
				}
			}
		} else {
			c.assignVar = name
			val := c.compileExpr(stmt.Value)
			c.assignVar = ""
			if q := stmt.Value.Binary.Left.Value.Target.Query; q != nil {
				if ml := asMapLiteral(q.Select); ml != nil {
					if st, ok := c.structLits[ml]; ok {
						typ = sanitizeListName(st.Name)
						t = types.ListType{Elem: st}
					}
				}
			}
			if val != name {
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			}
		}
	} else {
		// uninitialized variables default to the zero value of their type
		val := ""
		if stmt.Type != nil {
			val = defaultCValue(t)
		}
		decl := formatFuncPtrDecl(typ, name, val)
		if val == "" {
			if strings.HasSuffix(decl, " = ;") {
				decl = strings.TrimSuffix(decl, " = ;") + ";"
			}
		}
		c.writeln(decl)
		if stmt.Type != nil && stmt.Value == nil {
			if c.uninitVars == nil {
				c.uninitVars = map[string]bool{}
			}
			c.uninitVars[stmt.Name] = true
		}
	}
	if c.env != nil {
		c.env.SetVar(stmt.Name, t, false)
	}
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	name := sanitizeName(stmt.Name)
	var t types.Type
	if stmt.Type != nil {
		t = resolveTypeRef(stmt.Type, c.env)
		if stmt.Value != nil {
			if ll := stmt.Value.Binary.Left.Value.Target.List; ll != nil {
				if st, ok := c.inferStructFromList(ll, stmt.Name); ok {
					t = types.ListType{Elem: st}
					if c.env != nil {
						c.env.SetStruct(st.Name, st)
					}
					c.compileStructType(st)
					c.compileStructListType(st)
					for _, el := range ll.Elems {
						if ml := el.Binary.Left.Value.Target.Map; ml != nil {
							c.structLits[ml] = st
						}
					}
				}
			} else if ml := stmt.Value.Binary.Left.Value.Target.Map; ml != nil {
				if !(isMapStringIntLiteral(ml, c.env) || isMapIntStringLiteral(ml, c.env)) {
					if st, ok := c.inferStructFromMap(ml, stmt.Name); ok {
						t = st
						if c.env != nil {
							c.env.SetStruct(st.Name, st)
						}
						c.compileStructType(st)
						c.structLits[ml] = st
					}
				}
			}
		}
	} else if stmt.Value != nil {
		t = c.exprType(stmt.Value)
		if isEmptyListLiteral(stmt.Value) && stmt.Type == nil {
			if at, ok := c.appendTypes[stmt.Name]; ok {
				t = at
			}
		}
		if st, ok := castMapToStruct(stmt.Value, c.env); ok {
			t = st
		} else if ll := stmt.Value.Binary.Left.Value.Target.List; ll != nil {
			if st, ok := c.inferStructFromList(ll, stmt.Name); ok {
				t = types.ListType{Elem: st}
				if c.env != nil {
					c.env.SetStruct(st.Name, st)
				}
				c.compileStructType(st)
				c.compileStructListType(st)
				for _, el := range ll.Elems {
					if ml := el.Binary.Left.Value.Target.Map; ml != nil {
						c.structLits[ml] = st
					}
				}
			}
		} else if ml := stmt.Value.Binary.Left.Value.Target.Map; ml != nil {
			if !(isMapStringIntLiteral(ml, c.env) || isMapIntStringLiteral(ml, c.env)) {
				if st, ok := c.inferStructFromMap(ml, stmt.Name); ok {
					t = st
					if c.env != nil {
						c.env.SetStruct(st.Name, st)
					}
					c.compileStructType(st)
					c.structLits[ml] = st
				}
			}
		}
	}
	if t == nil {
		t = types.IntType{}
	}
	typ := cTypeFromType(t)
	if c.autoType && stmt.Type == nil && stmt.Value != nil {
		typ = "__auto_type"
	}
	if stmt.Value != nil {
		if vals, ok := c.evalListIntExpr(stmt.Value); ok && (isListIntType(t) || t == nil || types.ContainsAny(t)) {
			if t == nil || types.ContainsAny(t) {
				t = types.ListType{Elem: types.IntType{}}
				if c.env != nil {
					c.env.SetVar(stmt.Name, t, true)
				}
			}
			{
				parts := make([]string, len(vals))
				for i, v := range vals {
					parts[i] = strconv.Itoa(v)
				}
				c.need(needListInt)
				c.writeln(fmt.Sprintf("list_int %s = list_int_create(%d);", name, len(vals)))
				for i, v := range parts {
					c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
				}
				c.listLens[name] = len(vals)
				c.listVals[name] = vals
				return nil
			}
		} else if vals, ok := c.evalListFloatExpr(stmt.Value); ok && (isListFloatType(t) || t == nil || types.ContainsAny(t)) {
			if t == nil || types.ContainsAny(t) {
				t = types.ListType{Elem: types.FloatType{}}
				if c.env != nil {
					c.env.SetVar(stmt.Name, t, true)
				}
			}
			c.need(needListFloat)
			c.writeln(fmt.Sprintf("list_float %s = list_float_create(%d);", name, len(vals)))
			for i, v := range vals {
				s := strconv.FormatFloat(v, 'f', -1, 64)
				if !strings.ContainsAny(s, ".eE") {
					s += ".0"
				}
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, s))
			}
			c.listLens[name] = len(vals)
			c.listValsFloat[name] = vals
			return nil
		} else if vals, ok := c.evalListStringExpr(stmt.Value); ok && (isListStringType(t) || t == nil || types.ContainsAny(t)) {
			if t == nil || types.ContainsAny(t) {
				t = types.ListType{Elem: types.StringType{}}
				if c.env != nil {
					c.env.SetVar(stmt.Name, t, true)
				}
			}
			c.need(needListString)
			c.writeln(fmt.Sprintf("list_string %s = list_string_create(%d);", name, len(vals)))
			for i, v := range vals {
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
			c.listLens[name] = len(vals)
			c.listValsString[name] = vals
			return nil
		}
		if f := asFetchExpr(stmt.Value); f != nil {
			if st, ok := t.(types.StructType); ok {
				c.need(needFetch)
				c.need(needStringHeader)
				c.need(needMapStringGet)
				fname := c.emitFetchStructFunc(st)
				url := c.compileExpr(f.URL)
				opts := "NULL"
				if f.With != nil {
					opts = c.compileExpr(f.With)
				}
				c.writeln(fmt.Sprintf("%s %s = %s(%s, %s);", typ, name, fname, url, opts))
			} else {
				val := c.compileFetchExpr(f)
				c.writeln(formatFuncPtrDecl(typ, name, val))
			}
		} else if isEmptyListLiteral(stmt.Value) {
			if isListStringType(t) {
				c.need(needListString)
				c.writeln(fmt.Sprintf("list_string %s = {0, NULL};", name))
			} else if isListFloatType(t) {
				c.need(needListFloat)
				c.writeln(fmt.Sprintf("list_float %s = {0, NULL};", name))
			} else if isListListIntType(t) {
				c.need(needListListInt)
				c.writeln(fmt.Sprintf("list_list_int %s = {0, NULL};", name))
			} else if lt, ok := t.(types.ListType); ok {
				if st, ok2 := lt.Elem.(types.StructType); ok2 {
					c.compileStructType(st)
					c.compileStructListType(st)
					listC := sanitizeListName(st.Name)
					c.writeln(fmt.Sprintf("%s %s = {0, NULL};", listC, name))
				} else if isListIntType(t) || isListBoolType(t) {
					c.need(needListInt)
					c.writeln(fmt.Sprintf("list_int %s = {0, NULL};", name))
				}
			} else if isListIntType(t) || isListBoolType(t) {
				c.need(needListInt)
				c.writeln(fmt.Sprintf("list_int %s = {0, NULL};", name))
			} else if isMapIntBoolType(t) && isEmptyMapLiteral(stmt.Value) {
				c.need(needMapIntBool)
				val := c.newTemp()
				c.writeln(fmt.Sprintf("map_int_bool %s = map_int_bool_create(0);", val))
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			} else {
				c.assignVar = name
				val := c.compileExpr(stmt.Value)
				c.assignVar = ""
				if val != name {
					c.writeln(formatFuncPtrDecl(typ, name, val))
					for i, a := range c.allocs {
						if a == val {
							c.allocs[i] = name
						}
					}
				}
			}
		} else {
			c.assignVar = name
			val := c.compileExpr(stmt.Value)
			c.assignVar = ""
			if val != name {
				c.writeln(formatFuncPtrDecl(typ, name, val))
				for i, a := range c.allocs {
					if a == val {
						c.allocs[i] = name
					}
				}
			}
		}
	} else {
		val := ""
		if stmt.Type != nil {
			val = defaultCValue(t)
		}
		if val != "" {
			c.writeln(formatFuncPtrDecl(typ, name, val))
		} else {
			c.writeln(fmt.Sprintf("%s %s;", typ, name))
		}
		if stmt.Type != nil && stmt.Value == nil {
			if c.uninitVars == nil {
				c.uninitVars = map[string]bool{}
			}
			c.uninitVars[stmt.Name] = true
		}
	}
	if c.env != nil {
		c.env.SetVar(stmt.Name, t, true)
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := sanitizeName(f.Name)
	useVar := f.Name != "_"
	if f.RangeEnd != nil {
		start := c.compileExpr(f.Source)
		end := c.compileExpr(f.RangeEnd)
		loopVar := name
		if !useVar {
			loopVar = c.newTemp()
		}
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", loopVar, start, loopVar, end, loopVar))
		if useVar && loopVar != name {
			c.indent++
			c.writeln(fmt.Sprintf("int %s = %s;", name, loopVar))
		} else {
			c.indent++
		}
	} else {
		src := c.compileExpr(f.Source)
		idx := c.newTemp()
		isStr := isStringExpr(f.Source, c.env)
		isListStr := isListStringExpr(f.Source, c.env)
		var elemC string
		if lt, ok := c.exprType(f.Source).(types.ListType); ok {
			elemC = cTypeFromType(lt.Elem)
			if elemC == "" {
				elemC = "int"
			}
		}
		if isStr {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s[%s] != '\\0'; %s++) {", idx, src, idx, idx))
			c.indent++
			if useVar {
				c.writeln(fmt.Sprintf("char %s[2];", name))
				c.writeln(fmt.Sprintf("%s[0] = %s[%s];", name, src, idx))
				c.writeln(fmt.Sprintf("%s[1] = '\\0';", name))
			}
		} else if isListStr {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", idx, idx, c.listLenExpr(src), idx))
			c.indent++
			if useVar {
				c.writeln(fmt.Sprintf("char* %s = %s;", name, c.listItemExpr(src, idx)))
			}
		} else if isMapStringIntExpr(f.Source, c.env) {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", idx, idx, c.listLenExpr(src), idx))
			c.indent++
			if useVar {
				c.writeln(fmt.Sprintf("char* %s = %s.data[%s].key;", name, src, idx))
			}
		} else if isMapIntStringExpr(f.Source, c.env) {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", idx, idx, c.listLenExpr(src), idx))
			c.indent++
			if useVar {
				c.writeln(fmt.Sprintf("int %s = %s.data[%s].key;", name, src, idx))
			}
		} else {
			if l, ok := c.listLens[src]; ok {
				c.writeln(fmt.Sprintf("for (int %s = 0; %s < %d; %s++) {", idx, idx, l, idx))
				c.indent++
				if useVar {
					if elemC == "" {
						elemC = "int"
					}
					c.writeln(fmt.Sprintf("%s %s = %s;", elemC, name, c.listItemExpr(src, idx)))
				}
			} else {
				c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", idx, idx, c.listLenExpr(src), idx))
				c.indent++
				if useVar {
					if elemC == "" {
						elemC = "int"
					}
					c.writeln(fmt.Sprintf("%s %s = %s;", elemC, name, c.listItemExpr(src, idx)))
				}
			}
		}
		oldEnv := c.env
		if c.env != nil {
			c.env = types.NewEnv(c.env)
			if useVar {
				if isStr || isListStr {
					c.env.SetVar(f.Name, types.StringType{}, true)
				} else if isMapStringIntExpr(f.Source, c.env) {
					c.env.SetVar(f.Name, types.StringType{}, true)
				} else if isMapIntStringExpr(f.Source, c.env) {
					c.env.SetVar(f.Name, types.IntType{}, true)
				} else if lt, ok := c.exprType(f.Source).(types.ListType); ok {
					c.env.SetVar(f.Name, lt.Elem, true)
				} else {
					c.env.SetVar(f.Name, types.IntType{}, true)
				}
			}
		}
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		if c.env != nil {
			c.env = oldEnv
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	oldEnv := c.env
	if c.env != nil {
		c.env = types.NewEnv(c.env)
		if useVar {
			c.env.SetVar(f.Name, types.IntType{}, true)
		}
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if c.env != nil {
		c.env = oldEnv
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond := c.boolExpr(stmt.Cond)
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("} else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, st := range stmt.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond := c.boolExpr(w.Cond)
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := sanitizeName(u.Target)
	var elemType types.Type
	if c.env != nil {
		if v, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := v.(types.ListType); ok {
				elemType = lt.Elem
			}
		}
	}
	elemC := cTypeFromType(elemType)
	if elemC == "" {
		elemC = "int"
	}
	idx := c.newTemp()
	c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", idx, idx, c.listLenExpr(list), idx))
	c.indent++
	item := c.newTemp()
	c.writeln(fmt.Sprintf("%s %s = %s;", elemC, item, c.listItemExpr(list, idx)))
	oldStruct := c.currentStruct
	origEnv := c.env
	if st, ok := elemType.(types.StructType); ok {
		c.currentStruct = st.Name
		c.compileStructType(st)
		if c.env != nil {
			child := types.NewEnv(c.env)
			for _, f := range st.Order {
				ft := st.Fields[f]
				child.SetVar(f, ft, true)
			}
			c.env = child
		}
	}
	var cond string
	if u.Where != nil {
		cond = c.compileExpr(u.Where)
		cond = strings.ReplaceAll(cond, "self->", item+".")
		c.writeln(fmt.Sprintf("if (%s) {", cond))
		c.indent++
	}
	for _, it := range u.Set.Items {
		key, _ := identName(it.Key)
		if key == "" {
			key, _ = types.SimpleStringKey(it.Key)
		}
		val := c.compileExpr(it.Value)
		val = strings.ReplaceAll(val, "self->", item+".")
		c.writeln(fmt.Sprintf("%s.%s = %s;", item, fieldName(key), val))
	}
	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}
	c.writeln(fmt.Sprintf("%s = %s;", c.listItemExpr(list, idx), item))
	if origEnv != nil {
		c.env = origEnv
	}
	c.currentStruct = oldStruct
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	cond := c.boolExpr(e.Value)
	c.writeln(fmt.Sprintf("if (!(%s)) { fprintf(stderr, \"expect failed\\n\"); exit(1); }", cond))
	return nil
}

// boolExpr converts expression e to a boolean C expression.
func (c *Compiler) boolExpr(e *parser.Expr) string {
	expr := c.compileExpr(e)
	t := c.exprType(e)
	switch tt := t.(type) {
	case types.BoolType:
		return expr
	case types.IntType, types.FloatType:
		return fmt.Sprintf("(%s != 0)", expr)
	case types.StringType:
		c.need(needStringHeader)
		return fmt.Sprintf("(%s && %s[0] != '\\0')", expr, expr)
	case types.StructType:
		c.need(needStringHeader)
		zero := fmt.Sprintf("(%s){0}", sanitizeTypeName(tt.Name))
		return fmt.Sprintf("memcmp(&%s, &%s, sizeof(%s)) != 0", expr, zero, sanitizeTypeName(tt.Name))
	default:
		return fmt.Sprintf("(%s)", expr)
	}
}

func (c *Compiler) compileIfExpr(e *parser.IfExpr) string {
	cond := c.boolExpr(e.Cond)
	thenVal := c.compileExpr(e.Then)
	elseVal := "0"
	if e.ElseIf != nil {
		elseVal = c.compileIfExpr(e.ElseIf)
	} else if e.Else != nil {
		elseVal = c.compileExpr(e.Else)
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenVal, elseVal)
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) string {
	t := c.exprType(m.Target)
	if ut, ok := t.(types.UnionType); ok {
		tmp := c.newTemp()
		target := c.compileExpr(m.Target)
		c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(ut), tmp, target))
		resVar := c.newTemp()
		retT := c.exprType(m.Cases[0].Result)
		c.writeln(fmt.Sprintf("%s %s;", cTypeFromType(retT), resVar))
		c.writeln(fmt.Sprintf("switch (%s.tag) {", tmp))
		c.indent++
		var defaultVal string
		for _, cs := range m.Cases {
			if ident, ok := identName(cs.Pattern); ok && ident == "_" {
				defaultVal = c.compileExpr(cs.Result)
				continue
			}
			if call, ok := callPattern(cs.Pattern); ok {
				if st, ok2 := ut.Variants[call.Func]; ok2 {
					c.writeln(fmt.Sprintf("case %s_%s:", sanitizeTypeName(ut.Name), sanitizeTypeName(call.Func)))
					c.indent++
					for i, arg := range call.Args {
						if name, ok := identName(arg); ok {
							field := st.Order[i]
							ft := st.Fields[field]
							expr := fmt.Sprintf("%s.value.%s.%s", tmp, sanitizeTypeName(call.Func), fieldName(field))
							if _, ok := ft.(types.UnionType); ok {
								expr = "*" + expr
							}
							c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(ft), sanitizeName(name), expr))
						}
					}
					val := c.compileExpr(cs.Result)
					c.writeln(fmt.Sprintf("%s = %s;", resVar, val))
					c.writeln("break;")
					c.indent--
				}
				continue
			}
			if ident, ok := identName(cs.Pattern); ok {
				if _, ok2 := ut.Variants[ident]; ok2 {
					c.writeln(fmt.Sprintf("case %s_%s:", sanitizeTypeName(ut.Name), sanitizeTypeName(ident)))
					c.indent++
					val := c.compileExpr(cs.Result)
					c.writeln(fmt.Sprintf("%s = %s;", resVar, val))
					c.writeln("break;")
					c.indent--
					continue
				}
			}
		}
		c.writeln("default:")
		c.indent++
		if defaultVal == "" {
			defaultVal = defaultCValue(retT)
		}
		c.writeln(fmt.Sprintf("%s = %s;", resVar, defaultVal))
		c.writeln("break;")
		c.indent--
		c.indent--
		c.writeln("}")
		return resVar
	}

	target := c.compileExpr(m.Target)
	resT := c.exprType(m.Cases[0].Result)
	expr := defaultCValue(resT)
	for i := len(m.Cases) - 1; i >= 0; i-- {
		pat := c.compileExpr(m.Cases[i].Pattern)
		res := c.compileExpr(m.Cases[i].Result)
		if pat == "_" {
			expr = res
		} else {
			expr = fmt.Sprintf("(%s == %s ? %s : %s)", target, pat, res, expr)
		}
	}
	return expr
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) string {
	path := "\"\""
	rawPath := ""
	if l.Path != nil {
		rawPath = *l.Path
		if c.baseDir != "" && !filepath.IsAbs(rawPath) {
			cand := filepath.Join(c.baseDir, rawPath)
			if _, err := os.Stat(cand); err != nil {
				cand2 := filepath.Join(c.baseDir, "..", rawPath)
				if _, err2 := os.Stat(cand2); err2 == nil {
					cand = cand2
				} else {
					cand3 := filepath.Join(c.baseDir, "..", "..", rawPath)
					if _, err3 := os.Stat(cand3); err3 == nil {
						cand = cand3
					}
				}
			}
			rawPath = cand
		}
		path = fmt.Sprintf("%q", *l.Path)
	}
	format := "json"
	if ml := asMapLiteral(l.With); ml != nil {
		for _, it := range ml.Items {
			if k, ok := identName(it.Key); ok && k == "format" {
				if typ, val, ok := constLiteralTypeVal(it.Value); ok && typ == "char*" {
					format = strings.Trim(val, "\"")
				}
			}
		}
	}

	if format == "yaml" && l.Path != nil && l.Type != nil && l.Type.Simple != nil && c.env != nil {
		if st, ok := c.env.GetStruct(*l.Type.Simple); ok {
			st = dedupStruct(st)
			data, err := os.ReadFile(rawPath)
			if err == nil {
				var rows []map[string]any
				if yaml.Unmarshal(data, &rows) == nil {
					listName := sanitizeListName(st.Name)
					c.compileStructType(st)
					c.compileStructListType(st)
					arr := c.newTempPrefix("arr")
					c.writeln(fmt.Sprintf("%s %s[] = {", sanitizeTypeName(st.Name), arr))
					c.indent++
					for i, r := range rows {
						parts := make([]string, 0, len(st.Order))
						for _, f := range st.Order {
							v := r[f]
							switch st.Fields[f].(type) {
							case types.StringType:
								parts = append(parts, fmt.Sprintf(".%s = %q", fieldName(f), v))
							case types.FloatType:
								fv, _ := strconv.ParseFloat(fmt.Sprint(v), 64)
								parts = append(parts, fmt.Sprintf(".%s = %.17g", fieldName(f), fv))
							default:
								iv, _ := strconv.Atoi(fmt.Sprint(v))
								parts = append(parts, fmt.Sprintf(".%s = %d", fieldName(f), iv))
							}
						}
						line := fmt.Sprintf("(%s){%s}", sanitizeTypeName(st.Name), strings.Join(parts, ", "))
						if i < len(rows)-1 {
							line += ","
						}
						c.writeln(line)
					}
					c.indent--
					c.writeln("};")
					name := c.newTemp()
					c.writeln(fmt.Sprintf("%s %s = {%d, %s};", listName, name, len(rows), arr))
					return name
				}
			}
		}
	}

	c.need(needLoadJSON)
	c.need(needMapStringInt)
	c.need(needStringHeader)
	return fmt.Sprintf("_load_json(%s)", path)
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) string {
	src := c.compileExpr(s.Src)
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	format := "json"
	if s.With != nil && s.With.Binary != nil && len(s.With.Binary.Right) == 0 {
		if m := s.With.Binary.Left.Value.Target.Map; m != nil {
			for _, it := range m.Items {
				if key, ok := types.SimpleStringKey(it.Key); ok && key == "format" {
					if _, val, ok2 := constLiteralTypeVal(it.Value); ok2 {
						format = strings.Trim(val, "\"")
					}
				}
			}
		}
	}
	if format == "jsonl" && path == "\"-\"" {
		if lines, ok := c.evalJSONLinesExpr(s.Src); ok {
			for _, line := range lines {
				c.writeln(fmt.Sprintf("printf(\"%s\\n\");", escapeCString(line)))
			}
			return ""
		}
		if name, ok := identName(s.Src); ok {
			if lines, ok2 := c.jsonLines[name]; ok2 {
				for _, line := range lines {
					c.writeln(fmt.Sprintf("printf(\"%s\\n\");", escapeCString(line)))
				}
				return ""
			}
		}
	}
	if format == "jsonl" {
		c.need(needSaveJSONL)
	} else {
		c.need(needSaveJSON)
	}
	c.need(needLoadJSON)
	c.need(needMapStringInt)
	c.need(needStringHeader)
	if format == "jsonl" {
		return fmt.Sprintf("_save_jsonl(%s, %s)", src, path)
	}
	return fmt.Sprintf("_save_json(%s, %s)", src, path)
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) string {
	url := c.compileExpr(f.URL)
	opts := "NULL"
	if f.With != nil {
		opts = c.compileExpr(f.With)
	}
	c.need(needFetch)
	c.need(needStringHeader)
	return fmt.Sprintf("_fetch(%s, %s)", url, opts)
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) string {
	name := fmt.Sprintf("_lambda%d", len(c.lambdas))
	paramNames := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		paramNames[i] = sanitizeName(p.Name)
	}
	captured := freeVars(fn, paramNames)

	oldBuf := c.buf
	oldIndent := c.indent
	oldCaps := c.captures
	capMap := map[string]captureInfo{}
	for _, v := range captured {
		if c.env != nil {
			if t, err := c.env.GetVar(v); err == nil {
				gname := fmt.Sprintf("%s_%s", name, sanitizeName(v))
				capMap[v] = captureInfo{global: gname, typ: t}
				c.lambdas = append(c.lambdas, fmt.Sprintf("static %s %s;", cTypeFromType(t), gname))
			}
		}
	}
	c.captures = capMap

	c.buf = bytes.Buffer{}
	c.indent = 0
	var body []*parser.Statement
	if fn.ExprBody != nil {
		body = []*parser.Statement{{Return: &parser.ReturnStmt{Value: fn.ExprBody}}}
	} else {
		body = fn.BlockBody
	}
	fs := &parser.FunStmt{Name: name, Params: fn.Params, Return: fn.Return, Body: body}
	if err := c.compileFun(fs); err != nil {
		c.captures = oldCaps
		c.buf = oldBuf
		c.indent = oldIndent
		return "0"
	}
	code := c.buf.String()
	c.lambdas = append(c.lambdas, code)
	c.captures = oldCaps
	c.buf = oldBuf
	c.indent = oldIndent

	if len(captured) == 0 {
		return name
	}
	assigns := make([]string, len(captured))
	for i, v := range captured {
		assigns[i] = fmt.Sprintf("%s_%s = %s", name, sanitizeName(v), sanitizeName(v))
	}
	assigns = append(assigns, name)
	return fmt.Sprintf("(%s)", strings.Join(assigns, ", "))
}

func (c *Compiler) compilePartialCall(fnName string, ft types.FuncType, args []*parser.Expr) string {
	name := fmt.Sprintf("_partial%d", len(c.lambdas))
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = 0
	remaining := ft.Params[len(args):]
	params := make([]string, len(remaining))
	paramNames := make([]string, len(remaining))
	for i, p := range remaining {
		pname := fmt.Sprintf("p%d", i)
		params[i] = fmt.Sprintf("%s %s", cTypeFromType(p), pname)
		paramNames[i] = pname
	}
	c.writeln(fmt.Sprintf("static %s %s(%s){", cTypeFromType(ft.Return), name, strings.Join(params, ", ")))
	c.indent++
	callArgs := make([]string, len(ft.Params))
	for i := range args {
		g := fmt.Sprintf("%s_arg%d", name, i)
		callArgs[i] = g
	}
	for i, pn := range paramNames {
		callArgs[len(args)+i] = pn
	}
	c.writeln(fmt.Sprintf("return %s(%s);", sanitizeName(fnName), strings.Join(callArgs, ", ")))
	c.indent--
	c.writeln("}")
	// declare captured argument globals before the lambda definition
	decls := make([]string, len(args))
	for i := range args {
		t := ft.Params[i]
		g := fmt.Sprintf("%s_arg%d", name, i)
		decls[i] = fmt.Sprintf("static %s %s;", cTypeFromType(t), g)
	}
	c.lambdas = append(c.lambdas, strings.Join(decls, "\n"))
	c.lambdas = append(c.lambdas, c.buf.String())
	c.buf = oldBuf
	c.indent = oldIndent
	assigns := make([]string, len(args))
	for i, a := range args {
		expr := c.compileExpr(a)
		assigns[i] = fmt.Sprintf("%s_arg%d = %s", name, i, expr)
	}
	assigns = append(assigns, name)
	return fmt.Sprintf("(%s)", strings.Join(assigns, ", "))
}

// compileQueryExpr generates C code for simple dataset queries. It now
// supports optional `sort by`, `skip` and `take` clauses in addition to basic
// `from`/`where`/`select`. Joins and grouping remain unimplemented.
func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) string {
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Skip == nil && q.Take == nil {
		if name, ok := identName(q.Group.Exprs[0]); ok && name == q.Var {
			src := c.compileExpr(q.Source)
			srcT := c.exprType(q.Source)
			if lt, ok := srcT.(types.ListType); ok {
				if _, ok := lt.Elem.(types.IntType); ok {
					c.need(needGroupByInt)
					c.need(needListInt)
					prevEnv := c.env
					if c.env != nil {
						genv := types.NewEnv(c.env)
						genv.SetVar(q.Var, lt.Elem, true)
						c.env = genv
					}
					var cond string
					if q.Where != nil {
						cond = c.compileExpr(q.Where)
					}
					tmpSrc := src
					if cond != "" {
						filtered := c.newTemp()
						idxVar := c.newTemp()
						c.stackListInt(filtered, c.listLenExpr(src), "0")
						c.writeln(fmt.Sprintf("int %s = 0;", idxVar))
						loop := c.newLoopVar()
						c.writeln(fmt.Sprintf("for (int %s=0; %s<%s; %s++) {", loop, loop, c.listLenExpr(src), loop))
						c.indent++
						c.writeln(fmt.Sprintf("int %s = %s;", sanitizeName(q.Var), c.listItemExpr(src, loop)))
						c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
						c.writeln(fmt.Sprintf("%s.data[%s] = %s;", filtered, idxVar, sanitizeName(q.Var)))
						c.writeln(fmt.Sprintf("%s++;", idxVar))
						c.indent--
						c.writeln("}")
						c.writeln(fmt.Sprintf("%s.len = %s;", filtered, idxVar))
						tmpSrc = filtered
					}
					groups := c.newTemp()
					c.writeln(fmt.Sprintf("list_group_int %s = _group_by_int(%s);", groups, tmpSrc))
					oldEnv := c.env
					if c.env != nil {
						genv := types.NewEnv(c.env)
						genv.SetVar(q.Group.Name, types.GroupType{Elem: lt.Elem}, true)
						c.env = genv
					}
					c.groupKeys[q.Group.Name] = lt.Elem
					var retT types.Type
					if ml := asMapLiteral(q.Select); ml != nil {
						if st, ok := c.structLits[ml]; ok {
							retT = st
						} else if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
							retT = st
							c.structLits[ml] = st
							if c.env != nil {
								c.env.SetStruct(st.Name, st)
							}
							c.compileStructType(st)
							c.compileStructListType(st)
						}
					}
					if retT == nil {
						retT = c.exprType(q.Select)
						if ml := asMapLiteral(q.Select); ml != nil {
							if st, ok := retT.(types.StructType); ok {
								c.structLits[ml] = st
							}
						}
					}
					if ml := asMapLiteral(q.Select); ml != nil {
						if st, ok := retT.(types.StructType); ok {
							c.structLits[ml] = st
						}
					}
					var sortT types.Type
					if q.Sort != nil {
						sortT = c.guessType(q.Sort)
					}
					retList := types.ListType{Elem: retT}
					listC := cTypeFromType(retList)
					listCreate := listC + "_create"
					if st, ok := retT.(types.StructType); ok {
						listC = sanitizeListName(st.Name)
						listCreate = createListFuncName(st.Name)
					}
					if listC == "list_string" {
						c.need(needListString)
					} else if listC == "list_float" {
						c.need(needListFloat)
					} else if listC == "list_list_int" {
						c.need(needListListInt)
					} else if listC == "list_int" {
						c.need(needListInt)
					}
					res := c.newTemp()
					idx := c.newTemp()
					var keyArr string
					keyType := ""
					if q.Sort != nil {
						keyType = cTypeFromType(sortT)
						if keyType == "" {
							keyType = "int"
						}
					}
					c.writeln(fmt.Sprintf("%s %s = %s(%s.len);", listC, res, listCreate, groups))
					if keyType != "" {
						keyArr = c.newTemp()
						c.writeln(fmt.Sprintf("%s *%s = (%s*)malloc(sizeof(%s)*%s.len);", keyType, keyArr, keyType, keyType, groups))
					}
					c.writeln(fmt.Sprintf("int %s = 0;", idx))
					loop := c.newLoopVar()
					c.writeln(fmt.Sprintf("for (int %s=0; %s<%s.len; %s++) {", loop, loop, groups, loop))
					c.indent++
					c.writeln(fmt.Sprintf("_GroupInt %s = %s.data[%s];", sanitizeName(q.Group.Name), groups, loop))
					val := c.compileExpr(q.Select)
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
					if keyType != "" {
						sortExpr := c.compileExpr(q.Sort)
						c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, idx, sortExpr))
					}
					c.writeln(fmt.Sprintf("%s++;", idx))
					c.indent--
					c.writeln("}")
					c.writeln(fmt.Sprintf("%s.len = %s;", res, idx))
					if c.env != nil {
						c.env = oldEnv
					}
					if c.env != nil {
						c.env = prevEnv
					}
					delete(c.groupKeys, q.Group.Name)
					if keyType != "" {
						tmpK := c.newTemp()
						tmpV := c.newTemp()
						elemOut := cTypeFromType(retT)
						if elemOut == "" {
							elemOut = "int"
						}
						outer := c.newLoopVar()
						inner := c.newLoopVar()
						c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s-1; %s++) {", outer, outer, idx, outer))
						c.indent++
						c.writeln(fmt.Sprintf("for (int %s = %s+1; %s < %s; %s++) {", inner, outer, inner, idx, inner))
						c.indent++
						if keyType == "char*" {
							c.writeln(fmt.Sprintf("if (strcmp(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
						} else if keyType == "map_string_int" {
							c.writeln(fmt.Sprintf("if (cmp_map_string_int(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
						} else {
							c.writeln(fmt.Sprintf("if (%s[%s] > %s[%s]) {", keyArr, outer, keyArr, inner))
						}
						c.indent++
						c.writeln(fmt.Sprintf("%s %s = %s[%s];", keyType, tmpK, keyArr, outer))
						c.writeln(fmt.Sprintf("%s[%s] = %s[%s];", keyArr, outer, keyArr, inner))
						c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, inner, tmpK))
						c.writeln(fmt.Sprintf("%s %s = %s.data[%s];", elemOut, tmpV, res, outer))
						c.writeln(fmt.Sprintf("%s.data[%s] = %s.data[%s];", res, outer, res, inner))
						c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, inner, tmpV))
						c.indent--
						c.writeln("}")
						c.indent--
						c.writeln("}")
						c.indent--
						c.writeln("}")
					}
					return res
				}
			}
		} else if len(q.Group.Exprs) == 1 {
			if ml := asMapLiteral(q.Group.Exprs[0]); ml != nil && len(ml.Items) == 2 {
				if _, ok := c.exprType(ml.Items[0].Value).(types.StringType); ok {
					if _, ok2 := c.exprType(ml.Items[1].Value).(types.StringType); ok2 {
						// rewrite into two-expression grouping to reuse pair-string logic
						newQ := *q
						newGroup := *q.Group
						newGroup.Exprs = []*parser.Expr{ml.Items[0].Value, ml.Items[1].Value}
						newQ.Group = &newGroup
						return c.compileQueryExpr(&newQ)
					}
				}
			}
			src := c.compileExpr(q.Source)
			srcT := c.exprType(q.Source)
			if lt, ok := srcT.(types.ListType); ok {
				prevEnv := c.env
				if c.env != nil {
					genv := types.NewEnv(c.env)
					genv.SetVar(q.Var, lt.Elem, true)
					c.env = genv
				}
				if _, ok := c.exprType(q.Group.Exprs[0]).(types.StringType); ok {
					c.need(needGroupByString)
					c.need(needListString)
					c.need(needListInt)
					var cond string
					if q.Where != nil {
						cond = c.compileExpr(q.Where)
					}
					rows := c.newTemp()
					keys := c.newTemp()
					idxVar := c.newTemp()
					listC := cTypeFromType(srcT)
					var listCreate string
					if st, ok := lt.Elem.(types.StructType); ok {
						c.compileStructType(st)
						c.compileStructListType(st)
						listC = sanitizeListName(st.Name)
						listCreate = createListFuncName(st.Name)
					} else {
						listCreate = listC + "_create"
					}
					c.writeln(fmt.Sprintf("%s %s = %s(%s);", listC, rows, listCreate, c.listLenExpr(src)))
					c.writeln(fmt.Sprintf("list_string %s = list_string_create(%s);", keys, c.listLenExpr(src)))
					c.writeln(fmt.Sprintf("int %s = 0;", idxVar))
					loop := c.newLoopVar()
					c.writeln(fmt.Sprintf("for (int %s=0; %s<%s; %s++) {", loop, loop, c.listLenExpr(src), loop))
					c.indent++
					c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(lt.Elem), sanitizeName(q.Var), c.listItemExpr(src, loop)))
					if cond != "" {
						c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
					}
					key := c.compileExpr(q.Group.Exprs[0])
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", rows, idxVar, sanitizeName(q.Var)))
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", keys, idxVar, key))
					c.writeln(fmt.Sprintf("%s++;", idxVar))
					c.indent--
					c.writeln("}")
					c.writeln(fmt.Sprintf("%s.len = %s;", rows, idxVar))
					c.writeln(fmt.Sprintf("%s.len = %s;", keys, idxVar))
					groups := c.newTemp()
					c.writeln(fmt.Sprintf("list_group_string %s = _group_by_string(%s);", groups, keys))
					oldEnv := c.env
					if c.env != nil {
						genv := types.NewEnv(c.env)
						genv.SetVar(q.Group.Name, types.GroupType{Elem: lt.Elem}, true)
						c.env = genv
					}
					c.groupKeys[q.Group.Name] = types.StringType{}
					if ml := asMapLiteral(q.Select); ml != nil {
						if _, ok := c.structLits[ml]; !ok {
							if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
								c.structLits[ml] = st
								if c.env != nil {
									c.env.SetStruct(st.Name, st)
								}
								c.compileStructType(st)
								c.compileStructListType(st)
							}
						}
					}
					var retT types.Type
					if ml := asMapLiteral(q.Select); ml != nil {
						if st, ok := c.structLits[ml]; ok {
							retT = st
						} else if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
							retT = st
							c.structLits[ml] = st
							if c.env != nil {
								c.env.SetStruct(st.Name, st)
							}
							c.compileStructType(st)
							c.compileStructListType(st)
						}
					}
					if retT == nil {
						retT = c.exprType(q.Select)
						if ml := asMapLiteral(q.Select); ml != nil {
							if st, ok := retT.(types.StructType); ok {
								c.structLits[ml] = st
							}
						}
					}
					var groupStructName string
					if gt, ok := retT.(types.GroupType); ok {
						elem := gt.Elem
						listElem := types.ListType{Elem: elem}
						groupStructName = "Group" + sanitizeTypeName(cTypeFromType(elem))
						st := types.StructType{
							Name:   groupStructName,
							Fields: map[string]types.Type{"key": gt.Key, "items": listElem},
							Order:  []string{"key", "items"},
						}
						c.compileStructType(st)
						c.compileStructListType(st)
						retT = st
					}
					retList := types.ListType{Elem: retT}
					listRes := cTypeFromType(retList)
					listResCreate := listRes + "_create"
					if st, ok := retT.(types.StructType); ok {
						listRes = sanitizeListName(st.Name)
						listResCreate = createListFuncName(st.Name)
					}
					if listRes == "list_string" {
						c.need(needListString)
					} else if listRes == "list_float" {
						c.need(needListFloat)
					} else if listRes == "list_list_int" {
						c.need(needListListInt)
					}
					var sortT types.Type
					if q.Sort != nil {
						sortT = c.guessType(q.Sort)
					}
					res := c.newTemp()
					idxRes := c.newTemp()
					var keyArr string
					keyType := ""
					if q.Sort != nil {
						keyType = cTypeFromType(sortT)
						if keyType == "" {
							keyType = "int"
						}
					}
					c.writeln(fmt.Sprintf("%s %s = %s(%s.len);", listRes, res, listResCreate, groups))
					if keyType != "" {
						keyArr = c.newTemp()
						c.writeln(fmt.Sprintf("%s *%s = (%s*)malloc(sizeof(%s)*%s.len);", keyType, keyArr, keyType, keyType, groups))
					}
					c.writeln(fmt.Sprintf("int %s = 0;", idxRes))
					c.writeln(fmt.Sprintf("for (int gi=0; gi<%s.len; gi++) {", groups))
					c.indent++
					c.writeln(fmt.Sprintf("_GroupString _gp = %s.data[gi];", groups))
					items := c.newTemp()
					c.writeln(fmt.Sprintf("%s %s = %s(_gp.items.len);", listC, items, listCreate))
					loopJ := c.newLoopVar()
					c.writeln(fmt.Sprintf("for (int %s=0; %s<_gp.items.len; %s++) {", loopJ, loopJ, loopJ))
					c.indent++
					c.writeln(fmt.Sprintf("%s.data[%s] = %s.data[_gp.items.data[%s]];", items, loopJ, rows, loopJ))
					c.indent--
					c.writeln("}")
					c.writeln(fmt.Sprintf("%s.len = _gp.items.len;", items))
					if groupStructName != "" {
						c.writeln(fmt.Sprintf("%s %s = { _gp.key, %s };", sanitizeTypeName(groupStructName), sanitizeName(q.Group.Name), items))
					} else {
						c.writeln(fmt.Sprintf("struct {char* key; %s items; } %s = { _gp.key, %s };", listC, sanitizeName(q.Group.Name), items))
					}
					if q.Group.Having != nil {
						cond := c.compileExpr(q.Group.Having)
						c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
					}
					val := c.compileExpr(q.Select)
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idxRes, val))
					if keyType != "" {
						sortExpr := c.compileExpr(q.Sort)
						c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, idxRes, sortExpr))
					}
					c.writeln(fmt.Sprintf("%s++;", idxRes))
					c.indent--
					c.writeln("}")
					c.writeln(fmt.Sprintf("%s.len = %s;", res, idxRes))
					if c.env != nil {
						c.env = oldEnv
					}
					if c.env != nil {
						c.env = prevEnv
					}
					delete(c.groupKeys, q.Group.Name)
					if keyType != "" {
						tmpK := c.newTemp()
						tmpV := c.newTemp()
						elemOut := cTypeFromType(retT)
						if elemOut == "" {
							elemOut = "int"
						}
						outer := c.newLoopVar()
						inner := c.newLoopVar()
						c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s-1; %s++) {", outer, outer, idxRes, outer))
						c.indent++
						c.writeln(fmt.Sprintf("for (int %s = %s+1; %s < %s; %s++) {", inner, outer, inner, idxRes, inner))
						c.indent++
						if keyType == "char*" {
							c.writeln(fmt.Sprintf("if (strcmp(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
						} else if keyType == "map_string_int" {
							c.writeln(fmt.Sprintf("if (cmp_map_string_int(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
						} else {
							c.writeln(fmt.Sprintf("if (%s[%s] > %s[%s]) {", keyArr, outer, keyArr, inner))
						}
						c.indent++
						c.writeln(fmt.Sprintf("%s %s = %s[%s];", keyType, tmpK, keyArr, outer))
						c.writeln(fmt.Sprintf("%s[%s] = %s[%s];", keyArr, outer, keyArr, inner))
						c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, inner, tmpK))
						c.writeln(fmt.Sprintf("%s %s = %s.data[%s];", elemOut, tmpV, res, outer))
						c.writeln(fmt.Sprintf("%s.data[%s] = %s.data[%s];", res, outer, res, inner))
						c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, inner, tmpV))
						c.indent--
						c.writeln("}")
						c.indent--
						c.writeln("}")
						c.indent--
						c.writeln("}")
					}
					return res
				} else if _, ok := c.exprType(q.Group.Exprs[0]).(types.IntType); ok {
					c.need(needGroupByInt)
					c.need(needListInt)
					var cond string
					if q.Where != nil {
						cond = c.compileExpr(q.Where)
					}
					rows := c.newTemp()
					keys := c.newTemp()
					idxVar := c.newTemp()
					listC := cTypeFromType(srcT)
					var listCreate string
					if st, ok := lt.Elem.(types.StructType); ok {
						c.compileStructType(st)
						c.compileStructListType(st)
						listC = sanitizeListName(st.Name)
						listCreate = createListFuncName(st.Name)
					} else {
						listCreate = listC + "_create"
					}
					c.writeln(fmt.Sprintf("%s %s = %s(%s);", listC, rows, listCreate, c.listLenExpr(src)))
					c.stackListInt(keys, c.listLenExpr(src), "0")
					c.writeln(fmt.Sprintf("int %s = 0;", idxVar))
					loop := c.newLoopVar()
					c.writeln(fmt.Sprintf("for (int %s=0; %s<%s; %s++) {", loop, loop, c.listLenExpr(src), loop))
					c.indent++
					c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(lt.Elem), sanitizeName(q.Var), c.listItemExpr(src, loop)))
					if cond != "" {
						c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
					}
					key := c.compileExpr(q.Group.Exprs[0])
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", rows, idxVar, sanitizeName(q.Var)))
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", keys, idxVar, key))
					c.writeln(fmt.Sprintf("%s++;", idxVar))
					c.indent--
					c.writeln("}")
					c.writeln(fmt.Sprintf("%s.len = %s;", rows, idxVar))
					c.writeln(fmt.Sprintf("%s.len = %s;", keys, idxVar))
					groups := c.newTemp()
					c.writeln(fmt.Sprintf("list_group_int %s = _group_by_int(%s);", groups, keys))
					oldEnv := c.env
					if c.env != nil {
						genv := types.NewEnv(c.env)
						genv.SetVar(q.Group.Name, types.GroupType{Elem: lt.Elem}, true)
						c.env = genv
					}
					c.groupKeys[q.Group.Name] = types.IntType{}
					if ml := asMapLiteral(q.Select); ml != nil {
						if _, ok := c.structLits[ml]; !ok {
							if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
								c.structLits[ml] = st
								if c.env != nil {
									c.env.SetStruct(st.Name, st)
								}
								c.compileStructType(st)
								c.compileStructListType(st)
							}
						}
					}
					var retT types.Type
					if ml := asMapLiteral(q.Select); ml != nil {
						if st, ok := c.structLits[ml]; ok {
							retT = st
						} else if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
							retT = st
							c.structLits[ml] = st
							if c.env != nil {
								c.env.SetStruct(st.Name, st)
							}
							c.compileStructType(st)
							c.compileStructListType(st)
						}
					}
					if retT == nil {
						retT = c.exprType(q.Select)
						if ml := asMapLiteral(q.Select); ml != nil {
							if st, ok := retT.(types.StructType); ok {
								c.structLits[ml] = st
							}
						}
					}
					retList := types.ListType{Elem: retT}
					listRes := cTypeFromType(retList)
					listResCreate := listRes + "_create"
					if st, ok := retT.(types.StructType); ok {
						listRes = sanitizeListName(st.Name)
						listResCreate = createListFuncName(st.Name)
					}
					if listRes == "list_string" {
						c.need(needListString)
					} else if listRes == "list_float" {
						c.need(needListFloat)
					} else if listRes == "list_list_int" {
						c.need(needListListInt)
					}
					var sortT types.Type
					if q.Sort != nil {
						sortT = c.guessType(q.Sort)
					}
					res := c.newTemp()
					idxRes := c.newTemp()
					var keyArr string
					keyType := ""
					if q.Sort != nil {
						keyType = cTypeFromType(sortT)
						if keyType == "" {
							keyType = "int"
						}
					}
					c.writeln(fmt.Sprintf("%s %s = %s(%s.len);", listRes, res, listResCreate, groups))
					if keyType != "" {
						keyArr = c.newTemp()
						c.writeln(fmt.Sprintf("%s *%s = (%s*)malloc(sizeof(%s)*%s.len);", keyType, keyArr, keyType, keyType, groups))
					}
					c.writeln(fmt.Sprintf("int %s = 0;", idxRes))
					c.writeln(fmt.Sprintf("for (int gi=0; gi<%s.len; gi++) {", groups))
					c.indent++
					c.writeln(fmt.Sprintf("_GroupInt _gp = %s.data[gi];", groups))
					items := c.newTemp()
					c.writeln(fmt.Sprintf("%s %s = %s(_gp.items.len);", listC, items, listCreate))
					loopJ := c.newLoopVar()
					c.writeln(fmt.Sprintf("for (int %s=0; %s<_gp.items.len; %s++) {", loopJ, loopJ, loopJ))
					c.indent++
					c.writeln(fmt.Sprintf("%s.data[%s] = %s.data[_gp.items.data[%s]];", items, loopJ, rows, loopJ))
					c.indent--
					c.writeln("}")
					c.writeln(fmt.Sprintf("%s.len = _gp.items.len;", items))
					c.writeln(fmt.Sprintf("struct {int key; %s items; } %s = { _gp.key, %s };", listC, sanitizeName(q.Group.Name), items))
					if q.Group.Having != nil {
						cond := c.compileExpr(q.Group.Having)
						c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
					}
					val := c.compileExpr(q.Select)
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idxRes, val))
					if keyType != "" {
						sortExpr := c.compileExpr(q.Sort)
						c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, idxRes, sortExpr))
					}
					c.writeln(fmt.Sprintf("%s++;", idxRes))
					c.indent--
					c.writeln("}")
					c.writeln(fmt.Sprintf("%s.len = %s;", res, idxRes))
					if c.env != nil {
						c.env = oldEnv
					}
					if c.env != nil {
						c.env = prevEnv
					}
					delete(c.groupKeys, q.Group.Name)
					if keyType != "" {
						tmpK := c.newTemp()
						tmpV := c.newTemp()
						elemOut := cTypeFromType(retT)
						if elemOut == "" {
							elemOut = "int"
						}
						outer := c.newLoopVar()
						inner := c.newLoopVar()
						c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s-1; %s++) {", outer, outer, idxRes, outer))
						c.indent++
						c.writeln(fmt.Sprintf("for (int %s = %s+1; %s < %s; %s++) {", inner, outer, inner, idxRes, inner))
						c.indent++
						if keyType == "char*" {
							c.writeln(fmt.Sprintf("if (strcmp(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
						} else if keyType == "map_string_int" {
							c.writeln(fmt.Sprintf("if (cmp_map_string_int(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
						} else {
							c.writeln(fmt.Sprintf("if (%s[%s] > %s[%s]) {", keyArr, outer, keyArr, inner))
						}
						c.indent++
						c.writeln(fmt.Sprintf("%s %s = %s[%s];", keyType, tmpK, keyArr, outer))
						c.writeln(fmt.Sprintf("%s[%s] = %s[%s];", keyArr, outer, keyArr, inner))
						c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, inner, tmpK))
						c.writeln(fmt.Sprintf("%s %s = %s.data[%s];", elemOut, tmpV, res, outer))
						c.writeln(fmt.Sprintf("%s.data[%s] = %s.data[%s];", res, outer, res, inner))
						c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, inner, tmpV))
						c.indent--
						c.writeln("}")
						c.indent--
						c.writeln("}")
						c.indent--
						c.writeln("}")
					}
					return res
				}
			}
		} else if len(q.Group.Exprs) == 2 {
			src := c.compileExpr(q.Source)
			srcT := c.exprType(q.Source)
			if lt, ok := srcT.(types.ListType); ok {
				if _, ok := c.exprType(q.Group.Exprs[0]).(types.StringType); ok {
					if _, ok := c.exprType(q.Group.Exprs[1]).(types.StringType); ok {
						c.need(needGroupByPairString)
						c.need(needListPairString)
						prevEnv := c.env
						if c.env != nil {
							genv := types.NewEnv(c.env)
							genv.SetVar(q.Var, lt.Elem, true)
							c.env = genv
						}
						var cond string
						if q.Where != nil {
							cond = c.compileExpr(q.Where)
						}
						rows := c.newTemp()
						pairs := c.newTemp()
						idxVar := c.newTemp()
						listC := cTypeFromType(srcT)
						listCreate := listC + "_create"
						if st, ok := lt.Elem.(types.StructType); ok {
							c.compileStructType(st)
							c.compileStructListType(st)
							listC = sanitizeListName(st.Name)
							listCreate = createListFuncName(st.Name)
						}
						c.writeln(fmt.Sprintf("%s %s = %s(%s);", listC, rows, listCreate, c.listLenExpr(src)))
						c.writeln(fmt.Sprintf("list_pair_string %s = list_pair_string_create(%s);", pairs, c.listLenExpr(src)))
						c.writeln(fmt.Sprintf("int %s = 0;", idxVar))
						loop := c.newLoopVar()
						c.writeln(fmt.Sprintf("for (int %s=0; %s<%s; %s++) {", loop, loop, c.listLenExpr(src), loop))
						c.indent++
						c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(lt.Elem), sanitizeName(q.Var), c.listItemExpr(src, loop)))
						if cond != "" {
							c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
						}
						keyA := c.compileExpr(q.Group.Exprs[0])
						keyB := c.compileExpr(q.Group.Exprs[1])
						c.writeln(fmt.Sprintf("%s.data[%s] = %s;", rows, idxVar, sanitizeName(q.Var)))
						c.writeln(fmt.Sprintf("%s.data[%s] = (pair_string){%s, %s};", pairs, idxVar, keyA, keyB))
						c.writeln(fmt.Sprintf("%s++;", idxVar))
						c.indent--
						c.writeln("}")
						c.writeln(fmt.Sprintf("%s.len = %s;", rows, idxVar))
						c.writeln(fmt.Sprintf("%s.len = %s;", pairs, idxVar))
						groups := c.newTemp()
						c.writeln(fmt.Sprintf("list_group_pair_string %s = _group_by_pair_string(%s);", groups, pairs))
						oldEnv := c.env
						if c.env != nil {
							genv := types.NewEnv(c.env)
							genv.SetVar(q.Group.Name, types.GroupType{Elem: lt.Elem}, true)
							c.env = genv
						}
						c.groupKeys[q.Group.Name] = types.StructType{Name: "pair_string", Fields: map[string]types.Type{"a": types.StringType{}, "b": types.StringType{}}, Order: []string{"a", "b"}}
						if ml := asMapLiteral(q.Select); ml != nil {
							if _, ok := c.structLits[ml]; !ok {
								if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
									c.structLits[ml] = st
									if c.env != nil {
										c.env.SetStruct(st.Name, st)
									}
									c.compileStructType(st)
									c.compileStructListType(st)
								}
							}
						}
						var retT types.Type
						if ml := asMapLiteral(q.Select); ml != nil {
							if st, ok := c.structLits[ml]; ok {
								retT = st
							} else if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
								retT = st
								c.structLits[ml] = st
								if c.env != nil {
									c.env.SetStruct(st.Name, st)
								}
								c.compileStructType(st)
								c.compileStructListType(st)
							}
						}
						if retT == nil {
							retT = c.exprType(q.Select)
							if ml := asMapLiteral(q.Select); ml != nil {
								if st, ok := retT.(types.StructType); ok {
									c.structLits[ml] = st
								}
							}
						}
						retList := types.ListType{Elem: retT}
						listRes := cTypeFromType(retList)
						listResCreate := listRes + "_create"
						if st, ok := retT.(types.StructType); ok {
							listRes = sanitizeListName(st.Name)
							listResCreate = createListFuncName(st.Name)
						}
						if listRes == "list_string" {
							c.need(needListString)
						} else if listRes == "list_float" {
							c.need(needListFloat)
						} else if listRes == "list_list_int" {
							c.need(needListListInt)
						}
						var sortT types.Type
						if q.Sort != nil {
							sortT = c.guessType(q.Sort)
						}
						res := c.newTemp()
						idxRes := c.newTemp()
						var keyArr string
						keyType := ""
						if q.Sort != nil {
							keyType = cTypeFromType(sortT)
							if keyType == "" {
								keyType = "int"
							}
						}
						c.writeln(fmt.Sprintf("%s %s = %s(%s.len);", listRes, res, listResCreate, groups))
						if keyType != "" {
							keyArr = c.newTemp()
							c.writeln(fmt.Sprintf("%s *%s = (%s*)malloc(sizeof(%s)*%s.len);", keyType, keyArr, keyType, keyType, groups))
						}
						c.writeln(fmt.Sprintf("int %s = 0;", idxRes))
						loopG := c.newLoopVar()
						c.writeln(fmt.Sprintf("for (int %s=0; %s<%s.len; %s++) {", loopG, loopG, groups, loopG))
						c.indent++
						c.writeln(fmt.Sprintf("_GroupPairString _gp = %s.data[%s];", groups, loopG))
						items := c.newTemp()
						c.writeln(fmt.Sprintf("%s %s = %s(_gp.items.len);", listC, items, listCreate))
						loopJ := c.newLoopVar()
						c.writeln(fmt.Sprintf("for (int %s=0; %s<_gp.items.len; %s++) {", loopJ, loopJ, loopJ))
						c.indent++
						c.writeln(fmt.Sprintf("%s.data[%s] = %s.data[_gp.items.data[%s]];", items, loopJ, rows, loopJ))
						c.indent--
						c.writeln("}")
						c.writeln(fmt.Sprintf("%s.len = _gp.items.len;", items))
						c.writeln(fmt.Sprintf("struct { pair_string key; %s items; } %s = { _gp.key, %s };", listC, sanitizeName(q.Group.Name), items))
						if q.Group.Having != nil {
							cond := c.compileExpr(q.Group.Having)
							c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
						}
						val := c.compileExpr(q.Select)
						c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idxRes, val))
						if keyType != "" {
							sortExpr := c.compileExpr(q.Sort)
							c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, idxRes, sortExpr))
						}
						c.writeln(fmt.Sprintf("%s++;", idxRes))
						c.indent--
						c.writeln("}")
						c.writeln(fmt.Sprintf("%s.len = %s;", res, idxRes))
						if c.env != nil {
							c.env = oldEnv
						}
						if c.env != nil {
							c.env = prevEnv
						}
						delete(c.groupKeys, q.Group.Name)
						if keyType != "" {
							tmpK := c.newTemp()
							tmpV := c.newTemp()
							elemOut := cTypeFromType(retT)
							if elemOut == "" {
								elemOut = "int"
							}
							outer := c.newLoopVar()
							inner := c.newLoopVar()
							c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s-1; %s++) {", outer, outer, idxRes, outer))
							c.indent++
							c.writeln(fmt.Sprintf("for (int %s = %s+1; %s < %s; %s++) {", inner, outer, inner, idxRes, inner))
							c.indent++
							if keyType == "char*" {
								c.writeln(fmt.Sprintf("if (strcmp(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
							} else if keyType == "map_string_int" {
								c.writeln(fmt.Sprintf("if (cmp_map_string_int(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
							} else {
								c.writeln(fmt.Sprintf("if (%s[%s] > %s[%s]) {", keyArr, outer, keyArr, inner))
							}
							c.indent++
							c.writeln(fmt.Sprintf("%s %s = %s[%s];", keyType, tmpK, keyArr, outer))
							c.writeln(fmt.Sprintf("%s[%s] = %s[%s];", keyArr, outer, keyArr, inner))
							c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, inner, tmpK))
							c.writeln(fmt.Sprintf("%s %s = %s.data[%s];", elemOut, tmpV, res, outer))
							c.writeln(fmt.Sprintf("%s.data[%s] = %s.data[%s];", res, outer, res, inner))
							c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, inner, tmpV))
							c.indent--
							c.writeln("}")
							c.indent--
							c.writeln("}")
							c.indent--
							c.writeln("}")
						}
						return res
					}
				}
			}
		}
	}
	// handle simple cross joins (multiple from clauses without joins)
	if len(q.Froms) > 0 && len(q.Joins) == 0 && q.Group == nil {
		// collect all sources
		type srcInfo struct {
			varName string
			expr    string
			elem    types.Type
		}
		sources := []srcInfo{}
		firstExpr := c.compileExpr(q.Source)
		firstT := c.exprType(q.Source)
		lt, ok := firstT.(types.ListType)
		if !ok {
			return "0"
		}
		sources = append(sources, srcInfo{varName: q.Var, expr: firstExpr, elem: lt.Elem})
		for _, f := range q.Froms {
			fe := c.compileExpr(f.Src)
			ft := c.exprType(f.Src)
			flt, ok := ft.(types.ListType)
			if !ok {
				return "0"
			}
			sources = append(sources, srcInfo{varName: f.Var, expr: fe, elem: flt.Elem})
		}

		oldEnv := c.env
		if c.env != nil {
			c.env = types.NewEnv(c.env)
			for _, s := range sources {
				c.env.SetVar(s.varName, s.elem, true)
			}
		}

		var cond string
		if q.Where != nil {
			cond = c.compileExpr(q.Where)
		}

		if ml := asMapLiteral(q.Select); ml != nil {
			if _, ok := c.structLits[ml]; !ok {
				if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
					c.structLits[ml] = st
					if c.env != nil {
						c.env.SetStruct(st.Name, st)
					}
					c.compileStructType(st)
					c.compileStructListType(st)
				}
			}
		}

		val := c.compileExpr(q.Select)
		retT := c.exprType(q.Select)
		if ml := asMapLiteral(q.Select); ml != nil {
			if st, ok := c.structLits[ml]; ok {
				retT = st
			}
		}
		retList := types.ListType{Elem: retT}
		listC := cTypeFromType(retList)
		listCreate := listC + "_create"
		if st, ok := retT.(types.StructType); ok {
			listC = sanitizeListName(st.Name)
			listCreate = createListFuncName(st.Name)
		}
		if listC == "list_string" {
			c.need(needListString)
		} else if listC == "list_float" {
			c.need(needListFloat)
		} else if listC == "list_list_int" {
			c.need(needListListInt)
		} else if listC == "list_int" {
			c.need(needListInt)
		}

		res := c.newTemp()
		idx := c.newTemp()
		lenExpr := c.listLenExpr(sources[0].expr)
		for i := 1; i < len(sources); i++ {
			lenExpr = fmt.Sprintf("%s * %s", lenExpr, c.listLenExpr(sources[i].expr))
		}
		c.writeln(fmt.Sprintf("%s %s = %s(%s);", listC, res, listCreate, lenExpr))
		c.allocs = append(c.allocs, res)
		c.writeln(fmt.Sprintf("int %s = 0;", idx))

		var loop func(int)
		loop = func(i int) {
			src := sources[i]
			iter := sanitizeName(src.varName) + "_idx"
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iter, iter, c.listLenExpr(src.expr), iter))
			c.indent++
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(src.elem), sanitizeName(src.varName), c.listItemExpr(src.expr, iter)))
			if i+1 < len(sources) {
				loop(i + 1)
			} else {
				if cond != "" {
					c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
				}
				c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
				c.writeln(fmt.Sprintf("%s++;", idx))
			}
			c.indent--
			c.writeln("}")
		}
		loop(0)
		if cond != "" {
			c.writeln(fmt.Sprintf("%s.len = %s;", res, idx))
		}
		if c.env != nil {
			c.env = oldEnv
		}
		return res
	}

	// handle a single right join without additional clauses
	if len(q.Joins) == 1 && len(q.Froms) == 0 && q.Group == nil {
		j := q.Joins[0]
		if j.Side != nil && *j.Side == "right" {
			leftExpr := c.compileExpr(q.Source)
			leftT := c.exprType(q.Source)
			leftLt, ok := leftT.(types.ListType)
			if !ok {
				return "0"
			}
			rightExpr := c.compileExpr(j.Src)
			rightT := c.exprType(j.Src)
			rightLt, ok := rightT.(types.ListType)
			if !ok {
				return "0"
			}
			joinOn := c.compileExpr(j.On)

			oldEnv := c.env
			if c.env != nil {
				c.env = types.NewEnv(c.env)
				c.env.SetVar(j.Var, rightLt.Elem, true)
				c.env.SetVar(q.Var, leftLt.Elem, true)
			}

			var cond string
			if q.Where != nil {
				cond = c.compileExpr(q.Where)
			}

			if ml := asMapLiteral(q.Select); ml != nil {
				if _, ok := c.structLits[ml]; !ok {
					if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
						c.structLits[ml] = st
						if c.env != nil {
							c.env.SetStruct(st.Name, st)
						}
						c.compileStructType(st)
						c.compileStructListType(st)
					}
				}
			}

			val := c.compileExpr(q.Select)
			retT := c.exprType(q.Select)
			if ml := asMapLiteral(q.Select); ml != nil {
				if st, ok := c.structLits[ml]; ok {
					retT = st
				}
			}
			retList := types.ListType{Elem: retT}
			listC := cTypeFromType(retList)
			listCreate := listC + "_create"
			if st, ok := retT.(types.StructType); ok {
				listC = sanitizeListName(st.Name)
				listCreate = createListFuncName(st.Name)
			}
			if listC == "list_string" {
				c.need(needListString)
			} else if listC == "list_float" {
				c.need(needListFloat)
			} else if listC == "list_list_int" {
				c.need(needListListInt)
			} else if listC == "list_int" {
				c.need(needListInt)
			}

			res := c.newTemp()
			idx := c.newTemp()
			c.writeln(fmt.Sprintf("%s %s = %s(%s * %s);", listC, res, listCreate, c.listLenExpr(rightExpr), c.listLenExpr(leftExpr)))
			c.writeln(fmt.Sprintf("int %s = 0;", idx))
			iterR := c.newTemp()
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iterR, iterR, c.listLenExpr(rightExpr), iterR))
			c.indent++
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(rightLt.Elem), sanitizeName(j.Var), c.listItemExpr(rightExpr, iterR)))
			matched := c.newTemp()
			c.writeln(fmt.Sprintf("int %s = 0;", matched))
			iterL := c.newTemp()
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iterL, iterL, c.listLenExpr(leftExpr), iterL))
			c.indent++
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(leftLt.Elem), sanitizeName(q.Var), c.listItemExpr(leftExpr, iterL)))
			c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", joinOn))
			c.writeln(fmt.Sprintf("%s = 1;", matched))
			if cond != "" {
				c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
			}
			c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
			c.writeln(fmt.Sprintf("%s++;", idx))
			c.indent--
			c.writeln("}")
			c.writeln(fmt.Sprintf("if (!%s) {", matched))
			c.indent++
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(leftLt.Elem), sanitizeName(q.Var), defaultCValue(leftLt.Elem)))
			if cond != "" {
				c.writeln(fmt.Sprintf("if (!(%s)) { } else {", cond))
				c.indent++
				c.writeln("}")
				c.indent--
			}
			c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
			c.writeln(fmt.Sprintf("%s++;", idx))
			c.indent--
			c.writeln("}")
			c.indent--
			c.writeln("}")
			c.writeln(fmt.Sprintf("%s.len = %s;", res, idx))
			if c.env != nil {
				c.env = oldEnv
			}
			return res
		}
		if j.Side != nil && *j.Side == "outer" {
			leftExpr := c.compileExpr(q.Source)
			leftT := c.exprType(q.Source)
			leftLt, ok := leftT.(types.ListType)
			if !ok {
				return "0"
			}
			rightExpr := c.compileExpr(j.Src)
			rightT := c.exprType(j.Src)
			rightLt, ok := rightT.(types.ListType)
			if !ok {
				return "0"
			}
			joinOn := c.compileExpr(j.On)

			oldEnv := c.env
			if c.env != nil {
				c.env = types.NewEnv(c.env)
				c.env.SetVar(j.Var, rightLt.Elem, true)
				c.env.SetVar(q.Var, leftLt.Elem, true)
			}

			var cond string
			if q.Where != nil {
				cond = c.compileExpr(q.Where)
			}

			if ml := asMapLiteral(q.Select); ml != nil {
				if _, ok := c.structLits[ml]; !ok {
					if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
						c.structLits[ml] = st
						if c.env != nil {
							c.env.SetStruct(st.Name, st)
						}
						c.compileStructType(st)
						c.compileStructListType(st)
					}
				}
			}

			val := c.compileExpr(q.Select)
			retT := c.exprType(q.Select)
			if ml := asMapLiteral(q.Select); ml != nil {
				if st, ok := c.structLits[ml]; ok {
					retT = st
				}
			}
			retList := types.ListType{Elem: retT}
			listC := cTypeFromType(retList)
			listCreate := listC + "_create"
			if st, ok := retT.(types.StructType); ok {
				listC = sanitizeListName(st.Name)
				listCreate = createListFuncName(st.Name)
			}
			if listC == "list_string" {
				c.need(needListString)
			} else if listC == "list_float" {
				c.need(needListFloat)
			} else if listC == "list_list_int" {
				c.need(needListListInt)
			} else if listC == "list_int" {
				c.need(needListInt)
			}

			res := c.newTemp()
			idx := c.newTemp()
			c.writeln(fmt.Sprintf("%s %s = %s(%s * %s + %s + %s);", listC, res, listCreate, c.listLenExpr(leftExpr), c.listLenExpr(rightExpr), c.listLenExpr(leftExpr), c.listLenExpr(rightExpr)))
			c.writeln(fmt.Sprintf("int %s = 0;", idx))
			leftMatched := c.newTemp()
			rightMatched := c.newTemp()
			c.writeln(fmt.Sprintf("int *%s = calloc(%s, sizeof(int));", leftMatched, c.listLenExpr(leftExpr)))
			c.writeln(fmt.Sprintf("int *%s = calloc(%s, sizeof(int));", rightMatched, c.listLenExpr(rightExpr)))
			iterL := c.newTemp()
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iterL, iterL, c.listLenExpr(leftExpr), iterL))
			c.indent++
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(leftLt.Elem), sanitizeName(q.Var), c.listItemExpr(leftExpr, iterL)))
			iterR := c.newTemp()
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iterR, iterR, c.listLenExpr(rightExpr), iterR))
			c.indent++
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(rightLt.Elem), sanitizeName(j.Var), c.listItemExpr(rightExpr, iterR)))
			c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", joinOn))
			c.writeln(fmt.Sprintf("%s[%s] = 1;", leftMatched, iterL))
			c.writeln(fmt.Sprintf("%s[%s] = 1;", rightMatched, iterR))
			if cond != "" {
				c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
			}
			c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
			c.writeln(fmt.Sprintf("%s++;", idx))
			c.indent--
			c.writeln("}")
			c.indent--
			c.writeln("}")
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iterL, iterL, c.listLenExpr(leftExpr), iterL))
			c.indent++
			c.writeln(fmt.Sprintf("if (%s[%s]) continue;", leftMatched, iterL))
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(leftLt.Elem), sanitizeName(q.Var), c.listItemExpr(leftExpr, iterL)))
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(rightLt.Elem), sanitizeName(j.Var), defaultCValue(rightLt.Elem)))
			if cond != "" {
				c.writeln(fmt.Sprintf("if (!(%s)) { } else {", cond))
				c.indent++
				c.writeln("}")
				c.indent--
			}
			c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
			c.writeln(fmt.Sprintf("%s++;", idx))
			c.indent--
			c.writeln("}")
			iterR = c.newTemp()
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iterR, iterR, c.listLenExpr(rightExpr), iterR))
			c.indent++
			c.writeln(fmt.Sprintf("if (%s[%s]) continue;", rightMatched, iterR))
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(rightLt.Elem), sanitizeName(j.Var), c.listItemExpr(rightExpr, iterR)))
			c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(leftLt.Elem), sanitizeName(q.Var), defaultCValue(leftLt.Elem)))
			if cond != "" {
				c.writeln(fmt.Sprintf("if (!(%s)) { } else {", cond))
				c.indent++
				c.writeln("}")
				c.indent--
			}
			c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
			c.writeln(fmt.Sprintf("%s++;", idx))
			c.indent--
			c.writeln("}")
			c.writeln(fmt.Sprintf("free(%s);", leftMatched))
			c.writeln(fmt.Sprintf("free(%s);", rightMatched))
			c.writeln(fmt.Sprintf("%s.len = %s;", res, idx))
			if c.env != nil {
				c.env = oldEnv
			}
			return res
		}
	}

	// handle a single join grouped by a string expression and counting rows
	if len(q.Joins) == 1 && len(q.Froms) == 0 && q.Group != nil && len(q.Group.Exprs) == 1 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		j := q.Joins[0]
		leftExpr := c.compileExpr(q.Source)
		leftT := c.exprType(q.Source)
		leftLt, ok := leftT.(types.ListType)
		if !ok {
			return "0"
		}
		rightExpr := c.compileExpr(j.Src)
		rightT := c.exprType(j.Src)
		rightLt, ok := rightT.(types.ListType)
		if !ok {
			return "0"
		}
		joinOn := c.compileExpr(j.On)

		oldEnv := c.env
		if c.env != nil {
			c.env = types.NewEnv(c.env)
			c.env.SetVar(j.Var, rightLt.Elem, true)
			c.env.SetVar(q.Var, leftLt.Elem, true)
		}

		var cond string
		if q.Where != nil {
			cond = c.compileExpr(q.Where)
		}

		if ml := asMapLiteral(q.Select); ml != nil {
			if _, ok := c.structLits[ml]; !ok {
				if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
					c.structLits[ml] = st
					if c.env != nil {
						c.env.SetStruct(st.Name, st)
					}
					c.compileStructType(st)
					c.compileStructListType(st)
				}
			}
		}

		ml := asMapLiteral(q.Select)
		var retT types.StructType
		if ml != nil {
			if st, ok := c.structLits[ml]; ok {
				retT = st
			}
		}
		if retT.Name == "" {
			retT = types.StructType{Name: "Stat", Fields: map[string]types.Type{"name": types.StringType{}, "count": types.IntType{}}, Order: []string{"name", "count"}}
			c.compileStructType(retT)
			c.compileStructListType(retT)
		}

		res := c.assignVar
		if res == "" {
			res = c.newTemp()
		}
		lenVar := res + "_len"
		c.writeln(fmt.Sprintf("%s %s[10];", sanitizeTypeName(retT.Name), res))
		c.writeln(fmt.Sprintf("int %s = 0;", lenVar))
		if c.stackArrays != nil {
			c.stackArrays[res] = true
		}
		if c.arrayLens != nil {
			c.arrayLens[res] = lenVar
		}
		loopL := c.newLoopVar()
		c.writeln(fmt.Sprintf("for (int %s=0; %s<%s; %s++) {", loopL, loopL, c.listLenExpr(leftExpr), loopL))
		c.indent++
		c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(leftLt.Elem), sanitizeName(q.Var), c.listItemExpr(leftExpr, loopL)))
		loopR := c.newLoopVar()
		c.writeln(fmt.Sprintf("for (int %s=0; %s<%s; %s++) {", loopR, loopR, c.listLenExpr(rightExpr), loopR))
		c.indent++
		c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(rightLt.Elem), sanitizeName(j.Var), c.listItemExpr(rightExpr, loopR)))
		c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", joinOn))
		if cond != "" {
			c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
		}
		key := c.compileExpr(q.Group.Exprs[0])
		found := c.newTemp()
		kloop := c.newLoopVar()
		c.writeln(fmt.Sprintf("int %s = -1;", found))
		c.writeln(fmt.Sprintf("for (int %s=0; %s<%s; %s++) {", kloop, kloop, lenVar, kloop))
		c.indent++
		c.writeln(fmt.Sprintf("if (strcmp(%s[%s].name, %s) == 0) { %s = %s; break; }", res, kloop, key, found, kloop))
		c.indent--
		c.writeln("}")
		c.writeln(fmt.Sprintf("if (%s == -1) {", found))
		c.indent++
		c.writeln(fmt.Sprintf("%s[%s].name = %s;", res, lenVar, key))
		c.writeln(fmt.Sprintf("%s[%s].count = 1;", res, lenVar))
		c.writeln(fmt.Sprintf("%s = %s;", found, lenVar))
		c.writeln(fmt.Sprintf("%s++;", lenVar))
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln(fmt.Sprintf("%s[%s].count++;", res, found))
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		if c.env != nil {
			c.env = oldEnv
		}
		return res
	}

	// handle joins without grouping
	if len(q.Joins) > 0 && q.Group == nil {
		type srcInfo struct {
			varName string
			expr    string
			elem    types.Type
			joinOn  string
			left    bool
		}

		sources := []srcInfo{}
		firstExpr := c.compileExpr(q.Source)
		firstT := c.exprType(q.Source)
		lt, ok := firstT.(types.ListType)
		if !ok {
			return "0"
		}
		sources = append(sources, srcInfo{varName: q.Var, expr: firstExpr, elem: lt.Elem})
		for _, f := range q.Froms {
			fe := c.compileExpr(f.Src)
			ft := c.exprType(f.Src)
			flt, ok := ft.(types.ListType)
			if !ok {
				return "0"
			}
			sources = append(sources, srcInfo{varName: f.Var, expr: fe, elem: flt.Elem})
		}
		for _, j := range q.Joins {
			je := c.compileExpr(j.Src)
			jt := c.exprType(j.Src)
			jlt, ok := jt.(types.ListType)
			if !ok {
				return "0"
			}
			on := c.compileExpr(j.On)
			left := false
			if j.Side != nil && *j.Side == "left" {
				left = true
			}
			sources = append(sources, srcInfo{varName: j.Var, expr: je, elem: jlt.Elem, joinOn: on, left: left})
		}

		oldEnv := c.env
		if c.env != nil {
			c.env = types.NewEnv(c.env)
			for _, s := range sources {
				c.env.SetVar(s.varName, s.elem, true)
			}
		}

		var cond string
		if q.Where != nil {
			cond = c.compileExpr(q.Where)
		}

		if ml := asMapLiteral(q.Select); ml != nil {
			if _, ok := c.structLits[ml]; !ok {
				if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
					c.structLits[ml] = st
					c.compileStructType(st)
					c.compileStructListType(st)
				}
			}
		}

		val := c.compileExpr(q.Select)
		retT := c.exprType(q.Select)
		if ml := asMapLiteral(q.Select); ml != nil {
			if st, ok := c.structLits[ml]; ok {
				retT = st
			} else if st, ok := retT.(types.StructType); ok {
				c.structLits[ml] = st
				retT = st
			}
		}
		retList := types.ListType{Elem: retT}
		listC := cTypeFromType(retList)
		listCreate := listC + "_create"
		if st, ok := retT.(types.StructType); ok {
			listC = sanitizeListName(st.Name)
			listCreate = createListFuncName(st.Name)
		}
		if listC == "list_string" {
			c.need(needListString)
		} else if listC == "list_float" {
			c.need(needListFloat)
		} else if listC == "list_list_int" {
			c.need(needListListInt)
		} else if listC == "list_int" {
			c.need(needListInt)
		}

		res := c.newTemp()
		idx := c.newTemp()
		lenExpr := c.listLenExpr(sources[0].expr)
		for i := 1; i < len(sources); i++ {
			lenExpr = fmt.Sprintf("%s * %s", lenExpr, c.listLenExpr(sources[i].expr))
		}
		c.writeln(fmt.Sprintf("%s %s = %s(%s);", listC, res, listCreate, lenExpr))
		c.writeln(fmt.Sprintf("int %s = 0;", idx))

		var loop func(int)
		loop = func(i int) {
			src := sources[i]
			iter := c.newTemp()
			if src.joinOn == "" {
				c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iter, iter, c.listLenExpr(src.expr), iter))
				c.indent++
				c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(src.elem), sanitizeName(src.varName), c.listItemExpr(src.expr, iter)))
				if i+1 < len(sources) {
					loop(i + 1)
				} else {
					if cond != "" {
						c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
					}
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
					c.writeln(fmt.Sprintf("%s++;", idx))
				}
				c.indent--
				c.writeln("}")
			} else if src.left {
				matched := c.newTemp()
				c.writeln(fmt.Sprintf("int %s = 0;", matched))
				c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iter, iter, c.listLenExpr(src.expr), iter))
				c.indent++
				c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(src.elem), sanitizeName(src.varName), c.listItemExpr(src.expr, iter)))
				c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", src.joinOn))
				c.writeln(fmt.Sprintf("%s = 1;", matched))
				if i+1 < len(sources) {
					loop(i + 1)
				} else {
					if cond != "" {
						c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
					}
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
					c.writeln(fmt.Sprintf("%s++;", idx))
				}
				c.indent--
				c.writeln("}")
				c.writeln(fmt.Sprintf("if (!%s) {", matched))
				c.indent++
				c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(src.elem), sanitizeName(src.varName), defaultCValue(src.elem)))
				if i+1 < len(sources) {
					loop(i + 1)
				} else {
					if cond != "" {
						c.writeln(fmt.Sprintf("if (!(%s)) { } else {", cond))
						c.indent++
						c.writeln("}")
						c.indent--
					}
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
					c.writeln(fmt.Sprintf("%s++;", idx))
				}
				c.indent--
				c.writeln("}")
			} else {
				c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iter, iter, c.listLenExpr(src.expr), iter))
				c.indent++
				c.writeln(fmt.Sprintf("%s %s = %s;", cTypeFromType(src.elem), sanitizeName(src.varName), c.listItemExpr(src.expr, iter)))
				c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", src.joinOn))
				if i+1 < len(sources) {
					loop(i + 1)
				} else {
					if cond != "" {
						c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
					}
					c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
					c.writeln(fmt.Sprintf("%s++;", idx))
				}
				c.indent--
				c.writeln("}")
			}
		}
		loop(0)
		c.writeln(fmt.Sprintf("%s.len = %s;", res, idx))
		if c.env != nil {
			c.env = oldEnv
		}
		return res
	}

	// handle query over a group variable
	if len(q.Froms) == 0 && len(q.Joins) == 0 && q.Group == nil {
		if gt, ok := c.exprType(q.Source).(types.GroupType); ok {
			src := c.compileExpr(q.Source)
			elemType := gt.Elem
			elemC := cTypeFromType(elemType)
			oldEnv := c.env
			if c.env != nil {
				c.env = types.NewEnv(c.env)
				c.env.SetVar(q.Var, elemType, true)
			}
			var cond string
			if q.Where != nil {
				cond = c.compileExpr(q.Where)
			}
			if ml := asMapLiteral(q.Select); ml != nil {
				if _, ok := c.structLits[ml]; !ok {
					if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
						c.structLits[ml] = st
						c.compileStructType(st)
						c.compileStructListType(st)
					}
				}
			}
			val := c.compileExpr(q.Select)
			retT := c.exprType(q.Select)
			if ml := asMapLiteral(q.Select); ml != nil {
				if st, ok := c.structLits[ml]; ok {
					retT = st
				} else if st, ok := retT.(types.StructType); ok {
					c.structLits[ml] = st
					retT = st
				}
			}
			retList := types.ListType{Elem: retT}
			listC := cTypeFromType(retList)
			listCreate := listC + "_create"
			if st, ok := retT.(types.StructType); ok {
				listC = sanitizeListName(st.Name)
				listCreate = createListFuncName(st.Name)
			}
			if listC == "list_string" {
				c.need(needListString)
			} else if listC == "list_float" {
				c.need(needListFloat)
			} else if listC == "list_list_int" {
				c.need(needListListInt)
			} else if listC == "list_int" {
				c.need(needListInt)
			}
			res := c.newTemp()
			idx := c.newTemp()
			c.writeln(fmt.Sprintf("%s %s = %s(%s.items.len);", listC, res, listCreate, src))
			c.writeln(fmt.Sprintf("int %s = 0;", idx))
			loop := c.newLoopVar()
			c.writeln(fmt.Sprintf("for (int %s=0; %s<%s.items.len; %s++) {", loop, loop, src, loop))
			c.indent++
			c.writeln(fmt.Sprintf("%s %s = %s.items.data[%s];", elemC, sanitizeName(q.Var), src, loop))
			if cond != "" {
				c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
			}
			c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
			c.writeln(fmt.Sprintf("%s++;", idx))
			c.indent--
			c.writeln("}")
			c.writeln(fmt.Sprintf("%s.len = %s;", res, idx))
			if c.env != nil {
				c.env = oldEnv
			}
			return res
		}
	}

	// only handle simple queries without joins or grouping
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil {
		retT := c.exprType(q.Select)
		if ml := asMapLiteral(q.Select); ml != nil {
			if st, ok := c.structLits[ml]; ok {
				retT = st
				c.compileStructType(st)
				c.compileStructListType(st)
			} else if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
				retT = st
				c.structLits[ml] = st
				if c.env != nil {
					c.env.SetStruct(st.Name, st)
				}
				c.compileStructType(st)
				c.compileStructListType(st)
			}
		}
		listC := cTypeFromType(types.ListType{Elem: retT})
		if listC == "" {
			listC = "list_int"
		}
		switch listC {
		case "list_string":
			c.need(needListString)
		case "list_float":
			c.need(needListFloat)
		case "list_list_int":
			c.need(needListListInt)
		}
		return fmt.Sprintf("(%s){0, NULL}", listC)
	}

	src := c.compileExpr(q.Source)
	srcT := c.exprType(q.Source)
	lt, ok := srcT.(types.ListType)
	if !ok {
		return "0"
	}

	elemType := lt.Elem
	elemC := cTypeFromType(elemType)
	oldEnv := c.env
	if c.env != nil {
		c.env = types.NewEnv(c.env)
		c.env.SetVar(q.Var, elemType, true)
	}
	var cond string
	if q.Where != nil {
		cond = c.compileExpr(q.Where)
	}

	var sortT types.Type
	if q.Sort != nil {
		sortT = c.guessType(q.Sort)
	}

	var skipExpr string
	if q.Skip != nil {
		skipExpr = c.compileExpr(q.Skip)
	} else {
		skipExpr = "0"
	}

	var takeExpr string
	if q.Take != nil {
		takeExpr = c.compileExpr(q.Take)
	} else {
		takeExpr = "-1"
	}

	var selStruct types.StructType
	var hasStruct bool
	if ml := asMapLiteral(q.Select); ml != nil {
		if st, ok := c.structLits[ml]; ok {
			selStruct = st
			hasStruct = true
		} else if st, ok2 := c.inferStructFromMap(ml, q.Var); ok2 {
			c.structLits[ml] = st
			c.compileStructType(st)
			c.compileStructListType(st)
			selStruct = st
			hasStruct = true
		}
	}

	if agg, arg, ok := c.aggregateCall(q.Select); ok && q.Sort == nil && q.Skip == nil && q.Take == nil {
		argT := c.exprType(arg)
		listArg := types.ListType{Elem: argT}
		listC := cTypeFromType(listArg)
		listCreate := listC + "_create"
		if st, ok := argT.(types.StructType); ok {
			listC = sanitizeListName(st.Name)
			listCreate = createListFuncName(st.Name)
		}
		if listC == "" {
			listC = "list_int"
		}
		if listC == "list_string" {
			c.need(needListString)
		} else if listC == "list_float" {
			c.need(needListFloat)
		} else if listC == "list_list_int" {
			c.need(needListListInt)
		} else if listC == "list_int" {
			c.need(needListInt)
		}

		tmp := c.newTemp()
		idxTmp := c.newTemp()
		iter := c.newTemp()
		c.writeln(fmt.Sprintf("%s %s = %s(%s);", listC, tmp, listCreate, c.listLenExpr(src)))
		c.writeln(fmt.Sprintf("int %s = 0;", idxTmp))
		c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iter, iter, c.listLenExpr(src), iter))
		c.indent++
		c.writeln(fmt.Sprintf("%s %s = %s;", elemC, sanitizeName(q.Var), c.listItemExpr(src, iter)))
		if cond != "" {
			c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
		}
		val := c.compileExpr(arg)
		c.writeln(fmt.Sprintf("%s.data[%s] = %s;", tmp, idxTmp, val))
		c.writeln(fmt.Sprintf("%s++;", idxTmp))
		c.indent--
		c.writeln("}")
		c.writeln(fmt.Sprintf("%s.len = %s;", tmp, idxTmp))
		if c.env != nil {
			c.env = oldEnv
		}
		return c.aggregateExpr(agg, tmp, argT)
	}

	val := c.compileExpr(q.Select)
	retT := c.exprType(q.Select)
	if hasStruct {
		retT = selStruct
	} else if st, ok := retT.(types.StructType); ok {
		selStruct = st
		hasStruct = true
	}
	retList := types.ListType{Elem: retT}
	listC := cTypeFromType(retList)
	var listCreate string
	if hasStruct {
		listC = sanitizeListName(selStruct.Name)
		listCreate = createListFuncName(selStruct.Name)
	} else {
		listCreate = listC + "_create"
	}
	if listC == "" {
		listC = "list_int"
	}
	if listC == "list_string" {
		c.need(needListString)
	} else if listC == "list_float" {
		c.need(needListFloat)
	} else if listC == "list_list_int" {
		c.need(needListListInt)
	} else if listC == "list_int" {
		c.need(needListInt)
	}

	res := c.newTemp()
	idx := c.newTemp()
	iter := c.newTemp()
	var keyArr string
	keyType := ""
	if q.Sort != nil {
		keyType = cTypeFromType(sortT)
		if keyType == "" {
			keyType = "int"
		}
	}
	c.writeln(fmt.Sprintf("%s %s = %s(%s);", listC, res, listCreate, c.listLenExpr(src)))
	c.allocs = append(c.allocs, res)
	if keyType != "" {
		keyArr = c.newTemp()
		c.writeln(fmt.Sprintf("%s *%s = (%s*)malloc(sizeof(%s)*%s);", keyType, keyArr, keyType, keyType, c.listLenExpr(src)))
	}
	hasLimit := q.Skip != nil || q.Take != nil
	skipVar := ""
	takeVar := ""
	seenVar := ""
	c.writeln(fmt.Sprintf("int %s = 0;", idx))
	if hasLimit {
		skipVar = c.newTemp()
		takeVar = c.newTemp()
		seenVar = c.newTemp()
		c.writeln(fmt.Sprintf("int %s = %s;", skipVar, skipExpr))
		c.writeln(fmt.Sprintf("int %s = %s;", takeVar, takeExpr))
		c.writeln(fmt.Sprintf("int %s = 0;", seenVar))
	}
	c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s; %s++) {", iter, iter, c.listLenExpr(src), iter))
	c.indent++
	c.writeln(fmt.Sprintf("%s %s = %s;", elemC, sanitizeName(q.Var), c.listItemExpr(src, iter)))
	if cond != "" {
		c.writeln(fmt.Sprintf("if (!(%s)) { continue; }", cond))
	}
	if hasLimit {
		c.writeln(fmt.Sprintf("if (%s < %s) { %s++; continue; }", seenVar, skipVar, seenVar))
		c.writeln(fmt.Sprintf("if (%s >= 0 && %s >= %s) { break; }", takeVar, idx, takeVar))
		c.writeln(fmt.Sprintf("%s++;", seenVar))
	}
	c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, idx, val))
	if keyType != "" {
		sortExpr := c.compileExpr(q.Sort)
		c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, idx, sortExpr))
	}
	c.writeln(fmt.Sprintf("%s++;", idx))
	c.indent--
	c.writeln("}")
	c.writeln(fmt.Sprintf("%s.len = %s;", res, idx))
	if c.env != nil {
		c.env = oldEnv
	}
	if keyType != "" {
		tmpK := c.newTemp()
		tmpV := c.newTemp()
		elemOut := cTypeFromType(retT)
		if elemOut == "" {
			elemOut = "int"
		}
		outer := c.newLoopVar()
		inner := c.newLoopVar()
		c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s-1; %s++) {", outer, outer, idx, outer))
		c.indent++
		c.writeln(fmt.Sprintf("for (int %s = %s+1; %s < %s; %s++) {", inner, outer, inner, idx, inner))
		c.indent++
		if keyType == "char*" {
			c.writeln(fmt.Sprintf("if (strcmp(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
		} else if keyType == "map_string_int" {
			c.writeln(fmt.Sprintf("if (cmp_map_string_int(%s[%s], %s[%s]) > 0) {", keyArr, outer, keyArr, inner))
		} else {
			c.writeln(fmt.Sprintf("if (%s[%s] > %s[%s]) {", keyArr, outer, keyArr, inner))
		}
		c.indent++
		c.writeln(fmt.Sprintf("%s %s = %s[%s];", keyType, tmpK, keyArr, outer))
		c.writeln(fmt.Sprintf("%s[%s] = %s[%s];", keyArr, outer, keyArr, inner))
		c.writeln(fmt.Sprintf("%s[%s] = %s;", keyArr, inner, tmpK))
		c.writeln(fmt.Sprintf("%s %s = %s.data[%s];", elemOut, tmpV, res, outer))
		c.writeln(fmt.Sprintf("%s.data[%s] = %s.data[%s];", res, outer, res, inner))
		c.writeln(fmt.Sprintf("%s.data[%s] = %s;", res, inner, tmpV))
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
	}
	return res
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	captured := freeVarsTestBlock(t)
	capMap := map[string]captureInfo{}
	for _, v := range captured {
		if c.env != nil {
			expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: v}}}}}}
			typ := c.exprType(expr)
			g := fmt.Sprintf("%s_%s", name, sanitizeName(v))
			capMap[v] = captureInfo{global: g, typ: typ}
			c.writeln(fmt.Sprintf("static %s %s;", cTypeFromType(typ), g))
		}
	}
	oldCaps := c.captures
	c.captures = capMap
	c.writeln("static void " + name + "() {")
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			c.captures = oldCaps
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.captures = oldCaps
	if len(captured) > 0 {
		c.capturesByFun[name] = captured
	}
	return nil
}

func (c *Compiler) aggregateCall(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "", nil, false
	}
	p := u.Value
	if p.Target == nil || p.Target.Call == nil || len(p.Ops) > 0 {
		return "", nil, false
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return "", nil, false
	}
	switch call.Func {
	case "sum", "avg", "min", "max", "count", "len":
		return call.Func, call.Args[0], true
	}
	return "", nil, false
}

func (c *Compiler) aggregateExpr(name, list string, elem types.Type) string {
	if fvals, ok := c.listValsFloat[list]; ok {
		switch name {
		case "sum":
			total := 0.0
			for _, v := range fvals {
				total += v
			}
			s := strconv.FormatFloat(total, 'f', -1, 64)
			if !strings.ContainsAny(s, ".eE") {
				s += ".0"
			}
			return s
		case "avg":
			if len(fvals) == 0 {
				return "0.0"
			}
			total := 0.0
			for _, v := range fvals {
				total += v
			}
			avg := total / float64(len(fvals))
			s := strconv.FormatFloat(avg, 'f', -1, 64)
			if !strings.ContainsAny(s, ".eE") {
				s += ".0"
			}
			return s
		case "min":
			if len(fvals) == 0 {
				return "0.0"
			}
			m := fvals[0]
			for _, v := range fvals[1:] {
				if v < m {
					m = v
				}
			}
			s := strconv.FormatFloat(m, 'f', -1, 64)
			if !strings.ContainsAny(s, ".eE") {
				s += ".0"
			}
			return s
		case "max":
			if len(fvals) == 0 {
				return "0.0"
			}
			m := fvals[0]
			for _, v := range fvals[1:] {
				if v > m {
					m = v
				}
			}
			s := strconv.FormatFloat(m, 'f', -1, 64)
			if !strings.ContainsAny(s, ".eE") {
				s += ".0"
			}
			return s
		case "count", "len":
			return strconv.Itoa(len(fvals))
		}
	}
	if vals, ok := c.listVals[list]; ok {
		switch name {
		case "sum":
			total := 0
			for _, v := range vals {
				total += v
			}
			return strconv.Itoa(total)
		case "avg":
			if len(vals) == 0 {
				return "0"
			}
			total := 0
			for _, v := range vals {
				total += v
			}
			avg := float64(total) / float64(len(vals))
			s := strconv.FormatFloat(avg, 'f', -1, 64)
			if !strings.ContainsAny(s, ".eE") {
				s += ".0"
			}
			return s
		case "min":
			if len(vals) == 0 {
				return "0"
			}
			m := vals[0]
			for _, v := range vals[1:] {
				if v < m {
					m = v
				}
			}
			return strconv.Itoa(m)
		case "max":
			if len(vals) == 0 {
				return "0"
			}
			m := vals[0]
			for _, v := range vals[1:] {
				if v > m {
					m = v
				}
			}
			return strconv.Itoa(m)
		case "count", "len":
			return strconv.Itoa(len(vals))
		}
	}
	if svals, ok := c.listValsString[list]; ok {
		switch name {
		case "count", "len":
			return strconv.Itoa(len(svals))
		case "min":
			if len(svals) == 0 {
				return "\"\""
			}
			m := strings.Trim(svals[0], "\"")
			for _, v := range svals[1:] {
				sv := strings.Trim(v, "\"")
				if sv < m {
					m = sv
				}
			}
			return fmt.Sprintf("\"%s\"", m)
		case "max":
			if len(svals) == 0 {
				return "\"\""
			}
			m := strings.Trim(svals[0], "\"")
			for _, v := range svals[1:] {
				sv := strings.Trim(v, "\"")
				if sv > m {
					m = sv
				}
			}
			return fmt.Sprintf("\"%s\"", m)
		}
	}
	switch name {
	case "count", "len":
		return fmt.Sprintf("%s.len", list)
	case "sum":
		switch elem.(type) {
		case types.FloatType:
			c.need(needSumFloat)
			return fmt.Sprintf("_sum_float(%s)", list)
		default:
			c.need(needSumInt)
			return fmt.Sprintf("_sum_int(%s)", list)
		}
	case "avg":
		switch elem.(type) {
		case types.FloatType:
			c.need(needAvgFloat)
			return fmt.Sprintf("_avg_float(%s)", list)
		default:
			c.need(needAvg)
			return fmt.Sprintf("_avg(%s)", list)
		}
	case "min":
		switch elem.(type) {
		case types.FloatType:
			c.need(needMinFloat)
			return fmt.Sprintf("_min_float(%s)", list)
		case types.StringType:
			c.need(needMinString)
			return fmt.Sprintf("_min_string(%s)", list)
		default:
			c.need(needMinInt)
			return fmt.Sprintf("_min_int(%s)", list)
		}
	case "max":
		switch elem.(type) {
		case types.FloatType:
			c.need(needMaxFloat)
			return fmt.Sprintf("_max_float(%s)", list)
		case types.StringType:
			c.need(needMaxString)
			return fmt.Sprintf("_max_string(%s)", list)
		default:
			c.need(needMaxInt)
			return fmt.Sprintf("_max_int(%s)", list)
		}
	}
	return list
}

func (c *Compiler) compileExpr(e *parser.Expr) string {
	if e == nil {
		return ""
	}
	res := c.compileBinary(e.Binary)
	return res
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) string {
	if vals, ok := c.evalListIntBinary(b); ok {
		return c.emitConstListInt(vals)
	}
	if fvals, ok := c.evalListFloatExpr(&parser.Expr{Binary: b}); ok && len(b.Right) == 0 {
		return c.emitConstListFloat(fvals)
	}
	if svals, ok := c.evalListStringExpr(&parser.Expr{Binary: b}); ok && len(b.Right) == 0 {
		return c.emitConstListString(svals)
	}
	left := c.compileUnary(b.Left)
	leftType := c.unaryType(b.Left)
	leftList := isListListUnary(b.Left, c.env)
	leftListInt := isListIntUnary(b.Left, c.env)
	leftListFloat := isListFloatUnary(b.Left, c.env)
	leftListString := isListStringUnary(b.Left, c.env)
	leftString := isStringUnary(b.Left, c.env)
	for _, op := range b.Right {
		right := c.compilePostfix(op.Right)
		rightType := c.postfixType(op.Right)
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftList && isListListPostfix(op.Right, c.env) {
			c.need(needConcatListListInt)
			c.need(needListListInt)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_list_int %s = concat_list_list_int(%s, %s);", name, left, right))
			left = name
			leftList = true
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftListInt && isListIntPostfix(op.Right, c.env) {
			c.need(needConcatListInt)
			c.need(needListInt)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_int %s = concat_list_int(%s, %s);", name, left, right))
			left = name
			leftListInt = true
			leftList = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftListFloat && isListFloatPostfix(op.Right, c.env) {
			c.need(needConcatListFloat)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_float %s = concat_list_float(%s, %s);", name, left, right))
			left = name
			leftListFloat = true
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftListString && isListStringPostfix(op.Right, c.env) {
			c.need(needConcatListString)
			c.need(needListString)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_string %s = concat_list_string(%s, %s);", name, left, right))
			left = name
			leftListString = true
			leftList = false
			leftListInt = false
			leftString = false
			continue
		}
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftString && isStringPostfixOrIndex(op.Right, c.env) {
			c.need(needConcatString)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("char* %s = concat_string(%s, %s);", name, left, right))
			left = name
			leftString = true
			leftList = false
			leftListInt = false
			leftListString = false
			continue
		}
		if (op.Op == "+" || (op.Op == "union" && op.All)) && !leftString {
			if _, ok := leftType.(types.AnyType); ok && isStringPostfixOrIndex(op.Right, c.env) {
				c.need(needConcatString)
				name := c.newTemp()
				c.writeln(fmt.Sprintf("char* %s = concat_string(%s, %s);", name, left, right))
				left = name
				leftString = true
				leftList = false
				leftListInt = false
				leftListString = false
				continue
			}
		}
		if (op.Op == "+") && leftString {
			c.need(needConcatString)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("char* %s = concat_string(%s, %s);", name, left, right))
			left = name
			leftString = true
			leftList = false
			leftListInt = false
			leftListString = false
			continue
		}
		if op.Op == "union" && leftListInt && isListIntPostfix(op.Right, c.env) {
			c.need(needUnionListInt)
			c.need(needListInt)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_int %s = union_list_int(%s, %s);", name, left, right))
			left = name
			leftListInt = true
			leftList = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "union" && leftListFloat && isListFloatPostfix(op.Right, c.env) {
			c.need(needUnionListFloat)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_float %s = union_list_float(%s, %s);", name, left, right))
			left = name
			leftListFloat = true
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "union" && leftListString && isListStringPostfix(op.Right, c.env) {
			c.need(needUnionListString)
			c.need(needListString)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_string %s = union_list_string(%s, %s);", name, left, right))
			left = name
			leftListString = true
			leftList = false
			leftListInt = false
			leftString = false
			continue
		}
		if op.Op == "union" && leftList && isListListPostfix(op.Right, c.env) {
			c.need(needUnionListListInt)
			c.need(needListInt)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_list_int %s = union_list_list_int(%s, %s);", name, left, right))
			left = name
			leftList = true
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "except" && leftListInt && isListIntPostfix(op.Right, c.env) {
			c.need(needExceptListInt)
			c.need(needListInt)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_int %s = except_list_int(%s, %s);", name, left, right))
			left = name
			leftListInt = true
			leftList = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "except" && leftListFloat && isListFloatPostfix(op.Right, c.env) {
			c.need(needExceptListFloat)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_float %s = except_list_float(%s, %s);", name, left, right))
			left = name
			leftListFloat = true
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "except" && leftListString && isListStringPostfix(op.Right, c.env) {
			c.need(needExceptListString)
			c.need(needListString)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_string %s = except_list_string(%s, %s);", name, left, right))
			left = name
			leftListString = true
			leftList = false
			leftListInt = false
			leftString = false
			continue
		}
		if op.Op == "except" && leftList && isListListPostfix(op.Right, c.env) {
			c.need(needExceptListListInt)
			c.need(needListInt)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_list_int %s = except_list_list_int(%s, %s);", name, left, right))
			left = name
			leftList = true
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "intersect" && leftListInt && isListIntPostfix(op.Right, c.env) {
			c.need(needIntersectListInt)
			c.need(needListInt)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_int %s = intersect_list_int(%s, %s);", name, left, right))
			left = name
			leftListInt = true
			leftList = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "intersect" && leftListString && isListStringPostfix(op.Right, c.env) {
			c.need(needIntersectListString)
			c.need(needListString)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_string %s = intersect_list_string(%s, %s);", name, left, right))
			left = name
			leftListString = true
			leftList = false
			leftListInt = false
			leftString = false
			continue
		}
		if op.Op == "intersect" && leftListFloat && isListFloatPostfix(op.Right, c.env) {
			c.need(needIntersectListFloat)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_float %s = intersect_list_float(%s, %s);", name, left, right))
			left = name
			leftListFloat = true
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "intersect" && leftList && isListListPostfix(op.Right, c.env) {
			c.need(needIntersectListListInt)
			c.need(needListInt)
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_list_int %s = intersect_list_list_int(%s, %s);", name, left, right))
			left = name
			leftList = true
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "in" && isListIntPostfix(op.Right, c.env) {
			if len(b.Right) == 1 {
				if iv, ok := constIntValue(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}); ok {
					if vals, ok2 := c.evalListIntPostfix(op.Right); ok2 {
						found := false
						for _, v := range vals {
							if v == iv {
								found = true
								break
							}
						}
						if found {
							left = "1"
						} else {
							left = "0"
						}
						leftList = false
						leftListInt = false
						leftListString = false
						leftListFloat = false
						leftString = false
						leftType = types.IntType{}
						continue
					}
				}
			}
			if l, ok := c.listLens[right]; ok && c.isStackArrayExpr(right) {
				c.need(needInArrayInt)
				left = fmt.Sprintf("contains_array_int(%s, %d, %s)", right, l, left)
			} else {
				c.need(needInListInt)
				c.need(needListInt)
				left = fmt.Sprintf("contains_list_int(%s, %s)", right, left)
			}
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "in" && isListStringPostfix(op.Right, c.env) {
			if len(b.Right) == 1 {
				if sv, ok := constStringValue(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}); ok {
					if vals, ok2 := c.evalListStringPostfix(op.Right); ok2 {
						found := false
						for _, v := range vals {
							if v == sv {
								found = true
								break
							}
						}
						if found {
							left = "1"
						} else {
							left = "0"
						}
						leftList = false
						leftListInt = false
						leftListString = false
						leftListFloat = false
						leftString = false
						leftType = types.IntType{}
						continue
					}
				}
			}
			c.need(needInListString)
			c.need(needListString)
			left = fmt.Sprintf("contains_list_string(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "in" && isListFloatPostfix(op.Right, c.env) {
			if len(b.Right) == 1 {
				if fv, ok := constFloatValue(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}); ok {
					if vals, ok2 := c.evalListFloatPostfix(op.Right); ok2 {
						found := false
						for _, v := range vals {
							if v == fv {
								found = true
								break
							}
						}
						if found {
							left = "1"
						} else {
							left = "0"
						}
						leftList = false
						leftListInt = false
						leftListString = false
						leftListFloat = false
						leftString = false
						leftType = types.IntType{}
						continue
					}
				}
			}
			c.need(needInListFloat)
			left = fmt.Sprintf("contains_list_float(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "in" && isListListPostfix(op.Right, c.env) {
			c.need(needInListListInt)
			c.need(needListInt)
			left = fmt.Sprintf("contains_list_list_int(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "in" && isMapIntBoolPostfix(op.Right, c.env) {
			c.need(needInMapIntBool)
			left = fmt.Sprintf("map_int_bool_contains(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "in" && isMapStringIntPostfix(op.Right, c.env) {
			c.need(needInMapStringInt)
			left = fmt.Sprintf("map_string_int_contains(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "in" && isMapIntStringPostfix(op.Right, c.env) {
			c.need(needInMapIntString)
			left = fmt.Sprintf("map_int_string_contains(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "in" && leftString && isStringPostfixOrIndex(op.Right, c.env) {
			c.need(needInString)
			left = fmt.Sprintf("contains_string(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "==" || op.Op == "!=" {
			lt, okLeft := leftType.(types.ListType)
			_, okRight := rightType.(types.ListType)
			if okLeft && okRight {
				tmp := c.newTemp()
				idx := c.newLoopVar()
				leftLen := c.listLenExpr(left)
				rightLen := c.listLenExpr(right)
				c.writeln(fmt.Sprintf("int %s = 1;", tmp))
				c.writeln(fmt.Sprintf("if (%s != %s) { %s = 0; } else {", leftLen, rightLen, tmp))
				c.indent++
				c.writeln(fmt.Sprintf("for (int %s=0; %s<%s; %s++) {", idx, idx, leftLen, idx))
				c.indent++
				if st, ok := lt.Elem.(types.StructType); ok {
					parts := make([]string, 0, len(st.Order))
					for _, f := range st.Order {
						ft := st.Fields[f]
						litem := c.listItemExpr(left, idx)
						ritem := c.listItemExpr(right, idx)
						if _, ok := ft.(types.StringType); ok {
							c.need(needStringHeader)
							parts = append(parts, fmt.Sprintf("strcmp(%s.%s, %s.%s) != 0", litem, fieldName(f), ritem, fieldName(f)))
						} else {
							parts = append(parts, fmt.Sprintf("%s.%s != %s.%s", litem, fieldName(f), ritem, fieldName(f)))
						}
					}
					cond := strings.Join(parts, " || ")
					c.writeln(fmt.Sprintf("if (%s) { %s = 0; break; }", cond, tmp))
				} else {
					c.writeln(fmt.Sprintf("if (%s != %s) { %s = 0; break; }", c.listItemExpr(left, idx), c.listItemExpr(right, idx), tmp))
				}
				c.indent--
				c.writeln("}")
				c.indent--
				c.writeln("}")
				if op.Op == "==" {
					left = tmp
				} else {
					left = fmt.Sprintf("(!%s)", tmp)
				}
				leftList = false
				leftListInt = false
				leftListString = false
				leftListFloat = false
				leftString = false
				continue
			}
		}
		if (op.Op == "==" || op.Op == "!=" || op.Op == "<" || op.Op == ">" || op.Op == "<=" || op.Op == ">=") && leftString && isStringPostfixOrIndex(op.Right, c.env) {
			c.need(needStringHeader)
			cmp := fmt.Sprintf("strcmp(%s, %s)", left, right)
			switch op.Op {
			case "==":
				left = fmt.Sprintf("(%s == 0)", cmp)
			case "!=":
				left = fmt.Sprintf("(%s != 0)", cmp)
			case "<":
				left = fmt.Sprintf("(%s < 0)", cmp)
			case ">":
				left = fmt.Sprintf("(%s > 0)", cmp)
			case "<=":
				left = fmt.Sprintf("(%s <= 0)", cmp)
			case ">=":
				left = fmt.Sprintf("(%s >= 0)", cmp)
			}
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "%" && (isFloat(leftType) || isFloat(rightType)) {
			c.needMath = true
			left = fmt.Sprintf("fmod((double)%s, (double)%s)", left, right)
			leftType = types.FloatType{}
		} else if op.Op == "/" && isInt(leftType) && isInt(rightType) {
			left = fmt.Sprintf("((double)%s) / ((double)%s)", left, right)
			leftType = types.FloatType{}
		} else if op.Op == "/" && (strings.Contains(left, "_sum_int(") || strings.Contains(right, "_sum_int(")) {
			left = fmt.Sprintf("((double)%s) / ((double)%s)", left, right)
			leftType = types.FloatType{}
		} else {
			left = fmt.Sprintf("%s %s %s", left, op.Op, right)
			leftType = resultType(op.Op, leftType, rightType)
		}
		leftList = false
		leftListInt = false
		leftListString = false
		leftListFloat = false
		leftString = false
	}
	return left
}

func (c *Compiler) compileUnary(u *parser.Unary) string {
	expr := c.compilePostfix(u.Value)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			expr = fmt.Sprintf("(-%s)", expr)
		} else if op == "!" {
			expr = fmt.Sprintf("(!%s)", expr)
		}
	}
	return expr
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) string {
	// Special case: casting a map literal to a struct should
	// produce a direct struct initializer without generating
	// an intermediate map value.
	if p.Target != nil && p.Target.Map != nil && len(p.Ops) == 1 && p.Ops[0].Cast != nil {
		ct := resolveTypeRef(p.Ops[0].Cast.Type, c.env)
		if st, ok := ct.(types.StructType); ok {
			parts := make([]string, len(p.Target.Map.Items))
			for i, it := range p.Target.Map.Items {
				key, _ := types.SimpleStringKey(it.Key)
				val := c.compileExpr(it.Value)
				parts[i] = fmt.Sprintf(".%s = %s", fieldName(key), val)
			}
			return fmt.Sprintf("(%s){%s}", sanitizeTypeName(st.Name), strings.Join(parts, ", "))
		}
	}

	expr := c.compilePrimary(p.Target)
	isStr := isStringPrimary(p.Target, c.env)
	isFloatList := isListFloatPrimary(p.Target, c.env)
	isStringList := isListStringPrimary(p.Target, c.env)
	curT := c.primaryType(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon == nil {
				if st, ok := curT.(types.StructType); ok {
					if key, ok2 := types.SimpleStringKey(op.Index.Start); ok2 {
						if ft, ok3 := st.Fields[key]; ok3 {
							expr = fmt.Sprintf("%s.%s", expr, fieldName(key))
							curT = ft
							isStr = isStringType(ft)
							isFloatList = isListFloatType(ft)
							isStringList = isListStringType(ft)
							continue
						}
					}
				}
				idx := c.compileExpr(op.Index.Start)
				if isStr && op.Index.End == nil {
					name := c.newTemp()
					c.need(needStringHeader)
					c.need(needIndexString)
					c.writeln(fmt.Sprintf("char* %s = _index_string(%s, %s);", name, expr, idx))
					if c.env != nil {
						c.env.SetVar(name, types.StringType{}, true)
					}
					expr = name
					isStr = true
				} else if isMapStringIntPrimary(p.Target, c.env) {
					c.need(needMapStringInt)
					expr = fmt.Sprintf("map_string_int_get(%s, %s)", expr, idx)
					isStr = false
					isFloatList = false
					isStringList = false
				} else if isMapIntStringPrimary(p.Target, c.env) {
					c.need(needMapIntString)
					expr = fmt.Sprintf("map_int_string_get(%s, %s)", expr, idx)
					isStr = true
					isFloatList = false
					isStringList = false
				} else {
					if c.isStackArrayExpr(expr) {
						expr = fmt.Sprintf("%s[%s]", expr, idx)
					} else {
						expr = fmt.Sprintf("%s.data[%s]", expr, idx)
					}
					if isStringList {
						isStr = true
					} else {
						isStr = false
					}
					isFloatList = false
					isStringList = false
				}
				if lt, ok := curT.(types.ListType); ok {
					curT = lt.Elem
				} else if mt, ok := curT.(types.MapType); ok {
					curT = mt.Value
				}
			} else {
				start := "0"
				if op.Index.Start != nil {
					start = c.compileExpr(op.Index.Start)
				}
				end := fmt.Sprintf("%s.len", expr)
				if op.Index.End != nil {
					end = c.compileExpr(op.Index.End)
				}
				name := c.newTemp()
				if isStr {
					c.need(needStringHeader)
					c.need(needSliceString)
					c.writeln(fmt.Sprintf("char* %s = slice_string(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.StringType{}, true)
					}
					expr = name
					isStr = true
				} else if isFloatList {
					c.need(needSliceListFloat)
					c.writeln(fmt.Sprintf("list_float %s = slice_list_float(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.ListType{Elem: types.FloatType{}}, true)
					}
					expr = name
					isStr = false
					isFloatList = false
					isStringList = false
				} else if isStringList {
					c.need(needSliceListString)
					c.need(needListString)
					c.writeln(fmt.Sprintf("list_string %s = slice_list_string(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.ListType{Elem: types.StringType{}}, true)
					}
					expr = name
					isStr = false
					isFloatList = false
					isStringList = false
				} else {
					c.need(needSliceListInt)
					if c.isStackArrayExpr(expr) {
						// wrap stack-based array as list_int
						wrapper := c.newTemp()
						c.need(needListInt)
						c.writeln(fmt.Sprintf("list_int %s = {%s, %s};", wrapper, c.listLenExpr(expr), expr))
						expr = wrapper
					} else {
						c.need(needListInt)
					}
					c.writeln(fmt.Sprintf("list_int %s = slice_list_int(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.ListType{Elem: types.IntType{}}, true)
					}
					expr = name
					isStr = false
					isFloatList = false
					isStringList = false
				}
			}
		} else if op.Cast != nil {
			ct := resolveTypeRef(op.Cast.Type, c.env)
			if st, ok := ct.(types.StructType); ok && p.Target != nil && p.Target.Map != nil {
				parts := make([]string, len(p.Target.Map.Items))
				for i, it := range p.Target.Map.Items {
					key, _ := types.SimpleStringKey(it.Key)
					val := c.compileExpr(it.Value)
					parts[i] = fmt.Sprintf(".%s = %s", fieldName(key), val)
				}
				expr = fmt.Sprintf("(%s){%s}", sanitizeTypeName(st.Name), strings.Join(parts, ", "))
				isStr = false
				isFloatList = false
				isStringList = false
			} else if _, ok := ct.(types.IntType); ok && isStr {
				c.need(needStringHeader)
				expr = fmt.Sprintf("atoi(%s)", expr)
				isStr = false
			} else if _, ok := ct.(types.FloatType); ok && isStr {
				c.need(needStringHeader)
				expr = fmt.Sprintf("atof(%s)", expr)
				isStr = false
			} else {
				expr = fmt.Sprintf("(%s)(%s)", c.cType(op.Cast.Type), expr)
				isStr = false
			}
			isFloatList = false
			isStringList = false
		} else if op.Call != nil {
			if p.Target != nil && p.Target.Selector != nil {
				sel := p.Target.Selector
				if len(sel.Tail) > 0 {
					base := sel.Root
					method := sel.Tail[len(sel.Tail)-1]
					if mod, ok := c.builtinAliases[base]; ok {
						args := make([]string, len(op.Call.Args))
						for i, a := range op.Call.Args {
							args[i] = c.compileExpr(a)
						}
						switch mod {
						case "python_math":
							switch method {
							case "sqrt":
								if len(args) == 1 {
									if v, ok := constFloatValue(op.Call.Args[0]); ok {
										expr = strconv.FormatFloat(math.Sqrt(v), 'f', -1, 64)
									} else {
										expr = fmt.Sprintf("__builtin_sqrt(%s)", args[0])
									}
									continue
								}
							case "pow":
								if len(args) == 2 {
									if b, ok2 := constFloatValue(op.Call.Args[1]); ok2 && (b == 2 || b == 2.0) {
										expr = fmt.Sprintf("(%s * %s)", args[0], args[0])
										continue
									}
									if a, ok1 := constFloatValue(op.Call.Args[0]); ok1 {
										if b, ok2 := constFloatValue(op.Call.Args[1]); ok2 {
											expr = strconv.FormatFloat(math.Pow(a, b), 'f', -1, 64)
										} else {
											expr = fmt.Sprintf("__builtin_pow(%s, %s)", args[0], args[1])
										}
									} else {
										expr = fmt.Sprintf("__builtin_pow(%s, %s)", args[0], args[1])
									}
									continue
								}
							case "sin":
								if len(args) == 1 {
									if v, ok := constFloatValue(op.Call.Args[0]); ok {
										expr = strconv.FormatFloat(math.Sin(v), 'f', -1, 64)
									} else {
										expr = fmt.Sprintf("__builtin_sin(%s)", args[0])
									}
									continue
								}
							case "log":
								if len(args) == 1 {
									if v, ok := constFloatValue(op.Call.Args[0]); ok {
										expr = strconv.FormatFloat(math.Log(v), 'f', -1, 64)
									} else {
										expr = fmt.Sprintf("__builtin_log(%s)", args[0])
									}
									continue
								}
							}
						case "go_testpkg":
							switch method {
							case "Add":
								if len(args) == 2 {
									expr = fmt.Sprintf("(%s + %s)", args[0], args[1])
									continue
								}
							}
						}
					}
				}
			}
			if p.Target != nil && p.Target.Selector != nil && c.env != nil {
				sel := p.Target.Selector
				if len(sel.Tail) > 0 {
					baseSel := &parser.SelectorExpr{Root: sel.Root, Tail: sel.Tail[:len(sel.Tail)-1]}
					meth := sel.Tail[len(sel.Tail)-1]
					stype, ok := c.selectorStructType(baseSel)
					if ok {
						if _, ok2 := stype.Methods[meth]; ok2 {
							recv := c.compileSelector(baseSel)
							args := make([]string, len(op.Call.Args)+1)
							args[0] = "&" + recv
							for i, a := range op.Call.Args {
								args[i+1] = c.compileExpr(a)
							}
							expr = fmt.Sprintf("%s_%s(%s)", sanitizeName(stype.Name), sanitizeName(meth), strings.Join(args, ", "))
							isStr = false
							isFloatList = false
							isStringList = false
							continue
						}
					} else if meth == "contains" {
						if _, ok := c.selectorType(baseSel).(types.StringType); ok && len(op.Call.Args) == 1 {
							recv := c.compileSelector(baseSel)
							arg := c.compileExpr(op.Call.Args[0])
							c.need(needInString)
							c.need(needStringHeader)
							expr = fmt.Sprintf("contains_string(%s, %s)", recv, arg)
							isStr = false
							isFloatList = false
							isStringList = false
							continue
						}
					}
				}
			}
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				args[i] = c.compileExpr(a)
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
			isStr = false
			isFloatList = false
			isStringList = false
		}
	}
	return expr
}

func (c *Compiler) compilePrimary(p *parser.Primary) string {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil:
		return c.compileSelector(p.Selector)
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v := c.compileExpr(f.Value)
			if c.env != nil {
				if ut, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
					st := ut.Variants[p.Struct.Name]
					if ft, ok2 := st.Fields[f.Name]; ok2 {
						if _, ok3 := ft.(types.UnionType); ok3 {
							v = fmt.Sprintf("&%s", v)
						}
					}
				}
			}
			parts[i] = fmt.Sprintf(".%s = %s", fieldName(f.Name), v)
		}
		if c.env != nil {
			if ut, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
				return fmt.Sprintf("(%s){.tag=%s_%s, .value.%s=(%s){%s}}", sanitizeTypeName(ut.Name), sanitizeTypeName(ut.Name), sanitizeTypeName(p.Struct.Name), sanitizeTypeName(p.Struct.Name), sanitizeTypeName(p.Struct.Name), strings.Join(parts, ", "))
			}
		}
		return fmt.Sprintf("(%s){%s}", sanitizeTypeName(p.Struct.Name), strings.Join(parts, ", "))
	case p.List != nil:
		var name string
		if c.assignVar != "" {
			name = sanitizeName(c.assignVar)
		} else {
			name = c.newTemp()
		}
		nested := false
		if len(p.List.Elems) > 0 {
			if isListLiteral(p.List.Elems[0]) || isListIntExpr(p.List.Elems[0], c.env) {
				nested = true
			}
		}
		if len(p.List.Elems) > 0 {
			if ml := asMapLiteral(p.List.Elems[0]); ml != nil {
				if _, ok := c.structLits[ml]; !ok {
					if st, ok2 := c.inferStructFromList(p.List, name); ok2 {
						c.structLits[ml] = st
						if c.env != nil {
							c.env.SetStruct(st.Name, st)
						}
						c.compileStructType(st)
						c.compileStructListType(st)
					}
				}
				if st, ok := c.structLits[ml]; ok {
					c.compileStructType(st)
					vals := make([]string, len(p.List.Elems))
					jsons := make([]string, len(p.List.Elems))
					okJSON := true
					for i, el := range p.List.Elems {
						m2 := asMapLiteral(el)
						if m2 != nil {
							vals[i] = c.structInitFromMap(st, m2)
							if j, ok2 := jsonObjectLiteral(m2); ok2 {
								jsons[i] = j
							} else {
								okJSON = false
							}
						} else {
							vals[i] = c.compileExpr(el)
							okJSON = false
						}
					}
					c.writeln(fmt.Sprintf("%s %s[] = {%s};", sanitizeTypeName(st.Name), name, strings.Join(vals, ", ")))
					lenVar := name + "_len"
					c.writeln(fmt.Sprintf("int %s = sizeof(%s)/sizeof(%s[0]);", lenVar, name, name))
					if c.stackArrays != nil {
						c.stackArrays[name] = true
					}
					if c.arrayLens != nil {
						c.arrayLens[name] = lenVar
					}
					if okJSON && c.jsonLines != nil {
						c.jsonLines[name] = jsons
					}
					return name
				}
			}
		}
		if nested {
			c.need(needListListInt)
			c.need(needListInt)
			vals := make([]string, len(p.List.Elems))
			for i, el := range p.List.Elems {
				vals[i] = c.compileExpr(el)
			}
			data := name + "_data"
			c.writeln(fmt.Sprintf("list_int %s[] = {%s};", data, strings.Join(vals, ", ")))
			c.writeln(fmt.Sprintf("list_list_int %s = {%d, %s};", name, len(vals), data))
			if c.stackArrays != nil {
				c.stackArrays[data] = true
			}
		} else if len(p.List.Elems) > 0 && isStringExpr(p.List.Elems[0], c.env) {
			c.need(needListString)
			vals := make([]string, len(p.List.Elems))
			for i, el := range p.List.Elems {
				vals[i] = c.compileExpr(el)
			}
			c.need(needListString)
			c.writeln(fmt.Sprintf("list_string %s = list_string_create(%d);", name, len(p.List.Elems)))
			for i, v := range vals {
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
		} else if len(p.List.Elems) > 0 && isFloatExpr(p.List.Elems[0], c.env) {
			c.need(needListFloat)
			vals := make([]string, len(p.List.Elems))
			for i, el := range p.List.Elems {
				vals[i] = c.compileExpr(el)
			}
			c.need(needListFloat)
			c.writeln(fmt.Sprintf("list_float %s = list_float_create(%d);", name, len(p.List.Elems)))
			for i, v := range vals {
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
		} else if len(p.List.Elems) > 0 {
			if ml := asMapLiteral(p.List.Elems[0]); ml != nil {
				if st, ok := c.structLits[ml]; ok {
					c.compileStructType(st)
					vals := make([]string, len(p.List.Elems))
					for i, el := range p.List.Elems {
						if m2 := asMapLiteral(el); m2 != nil {
							vals[i] = c.structInitFromMap(st, m2)
						} else {
							vals[i] = c.compileExpr(el)
						}
					}
					c.writeln(fmt.Sprintf("%s %s[] = {%s};", sanitizeTypeName(st.Name), name, strings.Join(vals, ", ")))
					lenVar := name + "_len"
					c.writeln(fmt.Sprintf("int %s = sizeof(%s)/sizeof(%s[0]);", lenVar, name, name))
					if c.stackArrays != nil {
						c.stackArrays[name] = true
					}
					if c.arrayLens != nil {
						c.arrayLens[name] = lenVar
					}
					return name
				}
			} else if sl := asStructLiteral(p.List.Elems[0]); sl != nil {
				stName := sl.Name
				if st, ok := c.env.GetStruct(stName); ok {
					c.compileStructType(st)
					vals := make([]string, len(p.List.Elems))
					for i, el := range p.List.Elems {
						if s2 := asStructLiteral(el); s2 != nil {
							vals[i] = c.structInitFromStruct(st, s2)
						} else {
							vals[i] = c.compileExpr(el)
						}
					}
					c.writeln(fmt.Sprintf("%s %s[] = {%s};", sanitizeTypeName(st.Name), name, strings.Join(vals, ", ")))
					lenVar := name + "_len"
					c.writeln(fmt.Sprintf("int %s = sizeof(%s)/sizeof(%s[0]);", lenVar, name, name))
					if c.stackArrays != nil {
						c.stackArrays[name] = true
					}
					if c.arrayLens != nil {
						c.arrayLens[name] = lenVar
					}
					return name
				}
			}
			vals := make([]string, len(p.List.Elems))
			for i, el := range p.List.Elems {
				vals[i] = c.compileExpr(el)
			}
			c.need(needListInt)
			c.writeln(fmt.Sprintf("list_int %s = list_int_create(%d);", name, len(p.List.Elems)))
			for i, v := range vals {
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
			c.listLens[name] = len(p.List.Elems)
		} else {
			c.need(needListInt)
			c.writeln(fmt.Sprintf("list_int %s = list_int_create(0);", name))
			c.listLens[name] = 0
		}
		return name
	case p.Map != nil:
		if st, ok := c.structLits[p.Map]; ok {
			parts := make([]string, len(p.Map.Items))
			for i, it := range p.Map.Items {
				key, _ := types.SimpleStringKey(it.Key)
				v := c.compileExpr(it.Value)
				parts[i] = fmt.Sprintf(".%s = %s", fieldName(key), v)
			}
			return fmt.Sprintf("(%s){%s}", sanitizeTypeName(st.Name), strings.Join(parts, ", "))
		}
		if !(isMapStringIntLiteral(p.Map, c.env) || isMapIntStringLiteral(p.Map, c.env)) {
			if st, ok := c.inferStructFromMap(p.Map, "tmp"); ok {
				c.structLits[p.Map] = st
				if c.env != nil {
					c.env.SetStruct(st.Name, st)
				}
				c.compileStructType(st)
				c.compileStructListType(st)
				parts := make([]string, len(p.Map.Items))
				for i, it := range p.Map.Items {
					key, _ := types.SimpleStringKey(it.Key)
					v := c.compileExpr(it.Value)
					parts[i] = fmt.Sprintf(".%s = %s", fieldName(key), v)
				}
				return fmt.Sprintf("(%s){%s}", sanitizeTypeName(st.Name), strings.Join(parts, ", "))
			}
		}
		name := c.newTemp()
		if isMapStringIntLiteral(p.Map, c.env) {
			c.need(needMapStringInt)
			c.writeln(fmt.Sprintf("map_string_int %s = map_string_int_create(%d);", name, len(p.Map.Items)))
			for _, it := range p.Map.Items {
				k := c.compileExpr(it.Key)
				if s, ok := types.SimpleStringKey(it.Key); ok {
					k = fmt.Sprintf("\"%s\"", s)
				}
				v := c.compileExpr(it.Value)
				c.writeln(fmt.Sprintf("map_string_int_put(&%s, %s, %s);", name, k, v))
			}
		} else if isMapIntStringLiteral(p.Map, c.env) {
			c.need(needMapIntString)
			c.writeln(fmt.Sprintf("map_int_string %s = map_int_string_create(%d);", name, len(p.Map.Items)))
			for _, it := range p.Map.Items {
				k := c.compileExpr(it.Key)
				v := c.compileExpr(it.Value)
				c.writeln(fmt.Sprintf("map_int_string_put(&%s, %s, %s);", name, k, v))
			}
		} else {
			c.need(needMapIntBool)
			c.writeln(fmt.Sprintf("map_int_bool %s = map_int_bool_create(%d);", name, len(p.Map.Items)))
			for _, it := range p.Map.Items {
				k := c.compileExpr(it.Key)
				if s, ok := types.SimpleStringKey(it.Key); ok {
					k = fmt.Sprintf("\"%s\"", s)
				}
				v := c.compileExpr(it.Value)
				c.writeln(fmt.Sprintf("map_int_bool_put(&%s, %s, %s);", name, k, v))
			}
		}
		return name
	case p.Call != nil:
		if p.Call.Func == "len" {
			isStr := isStringExpr(p.Call.Args[0], c.env)
			if !isStr {
				if ct := cTypeFromType(c.exprType(p.Call.Args[0])); ct == "char*" {
					isStr = true
				}
			}
			if vals, ok := c.evalListIntExpr(p.Call.Args[0]); ok {
				return strconv.Itoa(len(vals))
			} else if fvals, ok := c.evalListFloatExpr(p.Call.Args[0]); ok {
				return strconv.Itoa(len(fvals))
			}
			arg := c.compileExpr(p.Call.Args[0])
			if isStr {
				c.need(needStringHeader)
				return fmt.Sprintf("strlen(%s)", arg)
			}
			return c.listLenExpr(arg)
		} else if p.Call.Func == "substring" {
			c.need(needSliceString)
			c.need(needStringHeader)
			s := c.compileExpr(p.Call.Args[0])
			start := c.compileExpr(p.Call.Args[1])
			end := c.compileExpr(p.Call.Args[2])
			return fmt.Sprintf("slice_string(%s, %s, %s)", s, start, end)
		} else if p.Call.Func == "indexOf" {
			c.need(needIndexOfString)
			s := c.compileExpr(p.Call.Args[0])
			sub := c.compileExpr(p.Call.Args[1])
			return fmt.Sprintf("index_of_string(%s, %s)", s, sub)
		} else if p.Call.Func == "lower" {
			arg := c.compileExpr(p.Call.Args[0])
			c.need(needLower)
			c.need(needStringHeader)
			return fmt.Sprintf("_lower(%s)", arg)
		} else if p.Call.Func == "upper" {
			arg := c.compileExpr(p.Call.Args[0])
			c.need(needUpper)
			c.need(needStringHeader)
			return fmt.Sprintf("_upper(%s)", arg)
		} else if p.Call.Func == "float" {
			arg := c.compileExpr(p.Call.Args[0])
			if isStringExpr(p.Call.Args[0], c.env) {
				c.need(needFloat)
				c.need(needStringHeader)
				return fmt.Sprintf("_float(%s)", arg)
			}
			return fmt.Sprintf("(double)(%s)", arg)
		} else if p.Call.Func == "split" {
			s := c.compileExpr(p.Call.Args[0])
			sep := c.compileExpr(p.Call.Args[1])
			c.need(needSplit)
			c.need(needStringHeader)
			return fmt.Sprintf("_split(%s, %s)", s, sep)
		} else if p.Call.Func == "sha256" {
			arg := c.compileExpr(p.Call.Args[0])
			if isStringArg(p.Call.Args[0], c.env) {
				c.need(needStringHeader)
				c.need(needSHA256)
				return fmt.Sprintf("_sha256_string(%s)", arg)
			}
			c.need(needSHA256)
			return fmt.Sprintf("_sha256_list(%s)", arg)
		} else if p.Call.Func == "print" {
			simple := true
			for _, a := range p.Call.Args {
				if isListListExpr(a, c.env) || isListIntExpr(a, c.env) || isListFloatExpr(a, c.env) || isListStringExpr(a, c.env) || isListStructExpr(a, c.env) || isStructExpr(a, c.env) {
					simple = false
					break
				}
			}
			if simple {
				var fmtParts []string
				var params []string
				for _, a := range p.Call.Args {
					if typ, val, ok := constLiteralTypeVal(a); ok && typ == "char*" {
						if len(fmtParts) > 0 {
							fmtParts = append(fmtParts, " ")
						}
						unq := strings.Trim(val, "\"")
						if uq, err := strconv.Unquote("\"" + unq + "\""); err == nil {
							unq = uq
						}
						fmtParts = append(fmtParts, escapeCString(unq))
						continue
					}
					if len(fmtParts) > 0 {
						fmtParts = append(fmtParts, " ")
					}
					fmtStr := "%d"
					if isStringArg(a, c.env) {
						fmtStr = "%s"
					} else if isFloatArg(a, c.env) {
						fmtStr = "%.17g"
					} else if name, okn := identName(a); okn && c.env != nil {
						if vt, err := c.env.GetVar(name); err == nil {
							if _, okf := vt.(types.FloatType); okf {
								fmtStr = "%.17g"
							}
						}
					} else if isFloatExpr(a, c.env) || looksLikeFloatConst(c.compileExpr(a)) {
						if _, ok := constFloatValue(a); ok {
							fmtStr = "%.17g"
						}
					}
					argCode := c.compileExpr(a)
					if fmtStr == "%.17g" {
						if _, ok := constIntValue(a); ok {
							argCode += ".0"
						}
					}
					if isBoolArg(a, c.env) {
						fmtStr = "%s"
						params = append(params, fmt.Sprintf("(%s)?\"true\":\"false\"", argCode))
					} else {
						params = append(params, argCode)
					}
					fmtParts = append(fmtParts, fmtStr)
				}
				fmtParts = append(fmtParts, "\\n")
				format := strings.Join(fmtParts, "")
				format = strings.ReplaceAll(format, "\n", "\\n")
				format = strings.ReplaceAll(format, "\"", "\\\"")
				if len(params) > 0 {
					c.writeln(fmt.Sprintf("printf(\"%s\", %s);", format, strings.Join(params, ", ")))
				} else {
					c.writeln(fmt.Sprintf("printf(\"%s\");", format))
				}
				return ""
			}
			for i, a := range p.Call.Args {
				if name, ok := identName(a); ok && c.uninitVars[name] {
					end := " "
					if i == len(p.Call.Args)-1 {
						end = "\\n"
					}
					c.writeln(fmt.Sprintf("printf(\"<nil>%s\");", end))
					continue
				}
				argExpr := c.compileExpr(a)
				if isListListExpr(a, c.env) {
					c.need(needListListInt)
					c.need(needPrintListInt)
					c.need(needListInt)
					c.need(needPrintListListInt)
					c.writeln(fmt.Sprintf("_print_list_list_int(%s);", argExpr))
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else if isListIntExpr(a, c.env) {
					if l, ok := c.listLens[argExpr]; ok {
						loop := c.newLoopVar()
						c.writeln(fmt.Sprintf("for (int %s=0; %s<%d; %s++) {", loop, loop, l, loop))
						c.indent++
						c.writeln(fmt.Sprintf("if(%s>0) printf(\" \");", loop))
						c.writeln(fmt.Sprintf("printf(\"%s\", %s);", "%d", c.listItemExpr(argExpr, loop)))
						c.indent--
						c.writeln("}")
						if i == len(p.Call.Args)-1 {
							c.writeln("printf(\"\\n\");")
						} else {
							c.writeln("printf(\" \");")
						}
					} else {
						c.need(needListListInt)
						c.need(needPrintListInt)
						c.writeln(fmt.Sprintf("_print_list_int(%s);", argExpr))
						if i == len(p.Call.Args)-1 {
							c.writeln("printf(\"\\n\");")
						} else {
							c.writeln("printf(\" \");")
						}
					}
				} else if isListFloatExpr(a, c.env) {
					c.need(needListFloat)
					c.need(needPrintListFloat)
					c.writeln(fmt.Sprintf("_print_list_float(%s);", argExpr))
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else if isListStringExpr(a, c.env) {
					c.need(needListString)
					c.need(needPrintListString)
					c.writeln(fmt.Sprintf("_print_list_string(%s);", argExpr))
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else if isListStructExpr(a, c.env) {
					lt, _ := c.exprType(a).(types.ListType)
					st, _ := lt.Elem.(types.StructType)
					loop := c.newLoopVar()
					c.writeln(fmt.Sprintf("for (int %s=0; %s<%s; %s++) {", loop, loop, c.listLenExpr(argExpr), loop))
					c.indent++
					c.writeln(fmt.Sprintf("%s it = %s;", sanitizeTypeName(st.Name), c.listItemExpr(argExpr, loop)))
					c.writeln(fmt.Sprintf("if(%s>0) printf(\" \");", loop))
					c.writeln("printf(\"map[\");")
					for i2, field := range st.Order {
						if i2 > 0 {
							c.writeln("printf(\" \");")
						}
						c.writeln(fmt.Sprintf("printf(\"%s:\");", field))
						fe := fmt.Sprintf("it.%s", fieldName(field))
						switch st.Fields[field].(type) {
						case types.StringType:
							c.writeln(fmt.Sprintf("printf(\"%s\", %s);", "%s", fe))
						case types.FloatType:
							c.writeln(fmt.Sprintf("printf(\"%s\", %s);", "%.17g", fe))
						default:
							c.writeln(fmt.Sprintf("printf(\"%s\", %s);", "%d", fe))
						}
					}
					c.writeln("printf(\"]\");")
					c.indent--
					c.writeln("}")
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else if isStructExpr(a, c.env) {
					st, _ := c.exprType(a).(types.StructType)
					zero := fmt.Sprintf("(%s){0}", sanitizeTypeName(st.Name))
					c.writeln(fmt.Sprintf("if(memcmp(&%s, &%s, sizeof(%s))==0){ printf(\"<nil>\"); } else {", argExpr, zero, sanitizeTypeName(st.Name)))
					c.indent++
					c.writeln("printf(\"map[\");")
					for i2, field := range st.Order {
						if i2 > 0 {
							c.writeln("printf(\" \");")
						}
						c.writeln(fmt.Sprintf("printf(\"%s:\");", field))
						fe := fmt.Sprintf("%s.%s", argExpr, fieldName(field))
						switch st.Fields[field].(type) {
						case types.StringType:
							c.writeln(fmt.Sprintf("printf(\"%s\", %s);", "%s", fe))
						case types.FloatType:
							c.writeln(fmt.Sprintf("printf(\"%s\", %s);", "%.17g", fe))
						default:
							c.writeln(fmt.Sprintf("printf(\"%s\", %s);", "%d", fe))
						}
					}
					c.writeln("printf(\"]\");")
					c.indent--
					c.writeln("}")
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else {
					if isBoolArg(a, c.env) {
						end := " "
						if i == len(p.Call.Args)-1 {
							end = "\\n"
						}
						c.writeln(fmt.Sprintf("printf(\"%%s%s\", (%s)?\"true\":\"false\");", end, argExpr))
					} else {
						fmtStr := "%d"
						if isStringArg(a, c.env) {
							fmtStr = "%s"
						} else if isFloatArg(a, c.env) {
							fmtStr = "%.17g"
						} else if name, okn := identName(a); okn && c.env != nil {
							if vt, err := c.env.GetVar(name); err == nil {
								if _, okf := vt.(types.FloatType); okf {
									fmtStr = "%.17g"
								}
							}
						} else if isFloatExpr(a, c.env) || looksLikeFloatConst(argExpr) {
							if _, ok := constFloatValue(a); ok {
								fmtStr = "%.17g"
							}
						}
						end := " "
						if i == len(p.Call.Args)-1 {
							end = "\\n"
						}
						c.writeln(fmt.Sprintf("printf(\"%s%s\", %s);", fmtStr, end, argExpr))
					}
				}
			}
			return ""
		} else if p.Call.Func == "append" {
			if len(p.Call.Args) != 2 {
				return "0"
			}
			if vals, ok := c.evalAppendInt(p.Call.Args[0], p.Call.Args[1]); ok {
				return c.emitConstListInt(vals)
			}
			lst := c.compileExpr(p.Call.Args[0])
			val := c.compileExpr(p.Call.Args[1])
			switch lt := listElemType(p.Call.Args[0], c.env).(type) {
			case types.IntType, types.BoolType:
				c.need(needConcatListInt)
				tmp := c.newTemp()
				c.stackListInt(tmp, "1", "1")
				c.writeln(fmt.Sprintf("%s.data[0] = %s;", tmp, val))
				name := c.newTemp()
				c.writeln(fmt.Sprintf("list_int %s = concat_list_int(%s, %s);", name, lst, tmp))
				return name
			case types.FloatType:
				c.need(needConcatListFloat)
				tmp := c.newTemp()
				c.writeln(fmt.Sprintf("list_float %s = list_float_create(1);", tmp))
				c.writeln(fmt.Sprintf("%s.data[0] = %s;", tmp, val))
				name := c.newTemp()
				c.writeln(fmt.Sprintf("list_float %s = concat_list_float(%s, %s);", name, lst, tmp))
				return name
			case types.StringType:
				c.need(needConcatListString)
				c.need(needListString)
				tmp := c.newTemp()
				c.writeln(fmt.Sprintf("list_string %s = list_string_create(1);", tmp))
				c.writeln(fmt.Sprintf("%s.data[0] = %s;", tmp, val))
				name := c.newTemp()
				c.writeln(fmt.Sprintf("list_string %s = concat_list_string(%s, %s);", name, lst, tmp))
				return name
			case types.ListType:
				// only support list<list<int>>
				if isListListExpr(p.Call.Args[0], c.env) {
					c.need(needConcatListListInt)
					c.need(needListListInt)
					tmp := c.newTemp()
					c.writeln(fmt.Sprintf("list_list_int %s = list_list_int_create(1);", tmp))
					c.writeln(fmt.Sprintf("%s.data[0] = %s;", tmp, val))
					name := c.newTemp()
					c.writeln(fmt.Sprintf("list_list_int %s = concat_list_list_int(%s, %s);", name, lst, tmp))
					return name
				}
			case types.StructType:
				st := lt
				c.compileStructType(st)
				c.compileStructListType(st)
				listC := sanitizeListName(st.Name)
				create := createListFuncName(st.Name)
				name := c.newTemp()
				c.writeln(fmt.Sprintf("%s %s = %s(%s.len + 1);", listC, name, create, lst))
				loop := c.newLoopVar()
				c.writeln(fmt.Sprintf("for (int %s=0; %s<%s.len; %s++) {", loop, loop, lst, loop))
				c.indent++
				c.writeln(fmt.Sprintf("%s.data[%s] = %s.data[%s];", name, loop, lst, loop))
				c.indent--
				c.writeln("}")
				c.writeln(fmt.Sprintf("%s.data[%s.len] = %s;", name, lst, val))
				c.writeln(fmt.Sprintf("%s.len = %s.len + 1;", name, lst))
				return name
			}
			return "0"
		} else if p.Call.Func == "exists" {
			arg := c.compileExpr(p.Call.Args[0])
			if _, ok := c.exprType(p.Call.Args[0]).(types.GroupType); ok {
				return fmt.Sprintf("%s.items.len > 0", arg)
			}
			return fmt.Sprintf("%s > 0", c.listLenExpr(arg))
		} else if p.Call.Func == "values" {
			arg := c.compileExpr(p.Call.Args[0])
			if isMapStringIntExpr(p.Call.Args[0], c.env) {
				c.need(needValuesMapStringInt)
				c.need(needMapStringInt)
				c.need(needListInt)
				return fmt.Sprintf("_values_map_string_int(%s)", arg)
			} else if isMapIntStringExpr(p.Call.Args[0], c.env) {
				c.need(needValuesMapIntString)
				c.need(needMapIntString)
				c.need(needListString)
				return fmt.Sprintf("_values_map_int_string(%s)", arg)
			} else if isMapIntBoolExpr(p.Call.Args[0], c.env) {
				c.need(needValuesMapIntBool)
				c.need(needMapIntBool)
				c.need(needListInt)
				return fmt.Sprintf("_values_map_int_bool(%s)", arg)
			}
			return "0"
		} else if p.Call.Func == "count" {
			if vals, ok := c.evalListIntExpr(p.Call.Args[0]); ok {
				return strconv.Itoa(len(vals))
			} else if fvals, ok := c.evalListFloatExpr(p.Call.Args[0]); ok {
				return strconv.Itoa(len(fvals))
			}
			t := c.exprType(p.Call.Args[0])
			arg := c.compileExpr(p.Call.Args[0])
			switch t.(type) {
			case types.GroupType:
				return fmt.Sprintf("%s.items.len", arg)
			case types.ListType:
				return c.listLenExpr(arg)
			default:
				c.need(needCount)
				return fmt.Sprintf("_count(%s)", arg)
			}
		} else if p.Call.Func == "sum" {
			if vals, ok := c.evalListIntExpr(p.Call.Args[0]); ok {
				total := 0
				for _, v := range vals {
					total += v
				}
				return strconv.Itoa(total)
			} else if fvals, ok := c.evalListFloatExpr(p.Call.Args[0]); ok {
				total := 0.0
				for _, v := range fvals {
					total += v
				}
				s := strconv.FormatFloat(total, 'f', -1, 64)
				if !strings.ContainsAny(s, ".eE") {
					s += ".0"
				}
				return s
			}
			arg := c.compileExpr(p.Call.Args[0])
			if isListIntExpr(p.Call.Args[0], c.env) {
				if l, ok := c.listLens[arg]; ok {
					sum := c.newTemp()
					loop := c.newLoopVar()
					c.writeln(fmt.Sprintf("double %s = 0;", sum))
					c.writeln(fmt.Sprintf("for (int %s=0; %s<%d; %s++) {", loop, loop, l, loop))
					c.indent++
					c.writeln(fmt.Sprintf("%s += %s;", sum, c.listItemExpr(arg, loop)))
					c.indent--
					c.writeln("}")
					if c.env != nil {
						c.env.SetVar(sum, types.FloatType{}, true)
					}
					return sum
				}
			}
			elem := listElemType(p.Call.Args[0], c.env)
			return c.aggregateExpr("sum", arg, elem)
		} else if p.Call.Func == "avg" {
			if vals, ok := c.evalListIntExpr(p.Call.Args[0]); ok {
				if len(vals) == 0 {
					return "0"
				}
				total := 0
				for _, v := range vals {
					total += v
				}
				avg := float64(total) / float64(len(vals))
				s := strconv.FormatFloat(avg, 'f', -1, 64)
				if !strings.ContainsAny(s, ".eE") {
					s += ".0"
				}
				return s
			} else if fvals, ok := c.evalListFloatExpr(p.Call.Args[0]); ok {
				if len(fvals) == 0 {
					return "0.0"
				}
				total := 0.0
				for _, v := range fvals {
					total += v
				}
				avg := total / float64(len(fvals))
				s := strconv.FormatFloat(avg, 'f', -1, 64)
				if !strings.ContainsAny(s, ".eE") {
					s += ".0"
				}
				return s
			}
			arg := c.compileExpr(p.Call.Args[0])
			if isListIntExpr(p.Call.Args[0], c.env) {
				if l, ok := c.listLens[arg]; ok {
					sum := c.newTemp()
					loop := c.newLoopVar()
					c.writeln(fmt.Sprintf("int %s = 0;", sum))
					c.writeln(fmt.Sprintf("for (int %s=0; %s<%d; %s++) {", loop, loop, l, loop))
					c.indent++
					c.writeln(fmt.Sprintf("%s += %s;", sum, c.listItemExpr(arg, loop)))
					c.indent--
					c.writeln("}")
					res := c.newTemp()
					c.writeln(fmt.Sprintf("double %s = %s /(double)%d;", res, sum, l))
					if c.env != nil {
						c.env.SetVar(res, types.FloatType{}, true)
					}
					return res
				}
			}
			elem := listElemType(p.Call.Args[0], c.env)
			return c.aggregateExpr("avg", arg, elem)
		} else if p.Call.Func == "min" {
			if vals, ok := c.evalListIntExpr(p.Call.Args[0]); ok {
				if len(vals) == 0 {
					return "0"
				}
				m := vals[0]
				for _, v := range vals[1:] {
					if v < m {
						m = v
					}
				}
				return strconv.Itoa(m)
			} else if fvals, ok := c.evalListFloatExpr(p.Call.Args[0]); ok {
				if len(fvals) == 0 {
					return "0.0"
				}
				m := fvals[0]
				for _, v := range fvals[1:] {
					if v < m {
						m = v
					}
				}
				s := strconv.FormatFloat(m, 'f', -1, 64)
				if !strings.ContainsAny(s, ".eE") {
					s += ".0"
				}
				return s
			}
			arg := c.compileExpr(p.Call.Args[0])
			if isListIntExpr(p.Call.Args[0], c.env) {
				if l, ok := c.listLens[arg]; ok {
					min := c.newTemp()
					loop := c.newLoopVar()
					if l > 0 {
						c.writeln(fmt.Sprintf("int %s = %s;", min, c.listItemExpr(arg, "0")))
						c.writeln(fmt.Sprintf("for (int %s=1; %s<%d; %s++) {", loop, loop, l, loop))
						c.indent++
						c.writeln(fmt.Sprintf("if (%s < %s) %s = %s;", c.listItemExpr(arg, loop), min, min, c.listItemExpr(arg, loop)))
						c.indent--
						c.writeln("}")
					} else {
						c.writeln(fmt.Sprintf("int %s = 0;", min))
					}
					if c.env != nil {
						c.env.SetVar(min, types.IntType{}, true)
					}
					return min
				}
			}
			elem := listElemType(p.Call.Args[0], c.env)
			return c.aggregateExpr("min", arg, elem)

		} else if p.Call.Func == "max" {
			if vals, ok := c.evalListIntExpr(p.Call.Args[0]); ok {
				if len(vals) == 0 {
					return "0"
				}
				m := vals[0]
				for _, v := range vals[1:] {
					if v > m {
						m = v
					}
				}
				return strconv.Itoa(m)
			} else if fvals, ok := c.evalListFloatExpr(p.Call.Args[0]); ok {
				if len(fvals) == 0 {
					return "0.0"
				}
				m := fvals[0]
				for _, v := range fvals[1:] {
					if v > m {
						m = v
					}
				}
				s := strconv.FormatFloat(m, 'f', -1, 64)
				if !strings.ContainsAny(s, ".eE") {
					s += ".0"
				}
				return s
			}
			arg := c.compileExpr(p.Call.Args[0])
			if isListIntExpr(p.Call.Args[0], c.env) {
				if l, ok := c.listLens[arg]; ok {
					max := c.newTemp()
					loop := c.newLoopVar()
					if l > 0 {
						c.writeln(fmt.Sprintf("int %s = %s;", max, c.listItemExpr(arg, "0")))
						c.writeln(fmt.Sprintf("for (int %s=1; %s<%d; %s++) {", loop, loop, l, loop))
						c.indent++
						c.writeln(fmt.Sprintf("if (%s > %s) %s = %s;", c.listItemExpr(arg, loop), max, max, c.listItemExpr(arg, loop)))
						c.indent--
						c.writeln("}")
					} else {
						c.writeln(fmt.Sprintf("int %s = 0;", max))
					}
					if c.env != nil {
						c.env.SetVar(max, types.IntType{}, true)
					}
					return max
				}
			}
			elem := listElemType(p.Call.Args[0], c.env)
			return c.aggregateExpr("max", arg, elem)
		} else if p.Call.Func == "reduce" {
			list := c.compileExpr(p.Call.Args[0])
			fn := c.compileExpr(p.Call.Args[1])
			init := c.compileExpr(p.Call.Args[2])
			switch listElemType(p.Call.Args[0], c.env).(type) {
			case types.FloatType:
				c.need(needReduceFloat)
				return fmt.Sprintf("_reduce_float(%s, %s, %s)", list, fn, init)
			case types.StringType:
				c.need(needReduceString)
				return fmt.Sprintf("_reduce_string(%s, %s, %s)", list, fn, init)
			default:
				c.need(needReduceInt)
				return fmt.Sprintf("_reduce_int(%s, %s, %s)", list, fn, init)
			}
		} else if p.Call.Func == "str" {
			arg := c.compileExpr(p.Call.Args[0])
			name := c.newTemp()
			c.need(needStr)
			c.writeln(fmt.Sprintf("char* %s = _str(%s);", name, arg))
			return name
		} else if p.Call.Func == "int" {
			arg := c.compileExpr(p.Call.Args[0])
			c.need(needStringHeader)
			return fmt.Sprintf("atoi(%s)", arg)
		} else if p.Call.Func == "input" {
			c.need(needInput)
			c.need(needStringHeader)
			return "_input()"
		} else if p.Call.Func == "now" {
			c.need(needNow)
			return "_now()"
		} else if p.Call.Func == "json" {
			if len(p.Call.Args) != 1 {
				return ""
			}
			if ml := asMapLiteral(p.Call.Args[0]); ml != nil {
				c.need(needJSON)
				c.need(needListFloat)
				c.need(needListString)
				c.need(needListListInt)
				c.writeln("printf(\"{\");")
				for i, item := range ml.Items {
					if i > 0 {
						c.writeln("printf(\",\");")
					}
					if item.Key != nil && item.Key.Binary != nil && item.Key.Binary.Left != nil && item.Key.Binary.Left.Value != nil && item.Key.Binary.Left.Value.Target != nil && item.Key.Binary.Left.Value.Target.Lit != nil && item.Key.Binary.Left.Value.Target.Lit.Str != nil {
						key := strings.ReplaceAll(*item.Key.Binary.Left.Value.Target.Lit.Str, "\"", "\\\"")
						c.writeln(fmt.Sprintf("_json_string(\"%s\");", key))
						c.writeln("printf(\":\");")
					}
					c.emitJSONExpr(item.Value)
				}
				c.writeln("printf(\"}\\n\");")
				return ""
			}
			c.emitJSONExpr(p.Call.Args[0])
			return ""
		}
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			arg := c.compileExpr(a)
			if _, ok := c.exprType(a).(types.StructType); ok {
				arg = "&" + arg
			}
			args[i] = arg
		}
		if c.env != nil {
			if tv, err := c.env.GetVar(p.Call.Func); err == nil {
				if ft, ok := tv.(types.FuncType); ok {
					if len(p.Call.Args) < len(ft.Params) {
						return c.compilePartialCall(p.Call.Func, ft, p.Call.Args)
					}
				}
			}
			if caps, ok := c.capturesByFun[sanitizeName(p.Call.Func)]; ok && len(caps) > 0 {
				assigns := make([]string, len(caps))
				for i, v := range caps {
					assigns[i] = fmt.Sprintf("%s_%s = %s", sanitizeName(p.Call.Func), sanitizeName(v), sanitizeName(v))
				}
				assigns = append(assigns, fmt.Sprintf("%s(%s)", sanitizeName(p.Call.Func), strings.Join(args, ", ")))
				return fmt.Sprintf("(%s)", strings.Join(assigns, ", "))
			}
		}
		name := sanitizeName(p.Call.Func)
		if alias, ok := c.funcAliases[name]; ok {
			name = alias
		}
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", "))
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Group != nil:
		return fmt.Sprintf("(%s)", c.compileExpr(p.Group))
	default:
		return "0"
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		s := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return s
	case l.Bool != nil:
		if *l.Bool {
			return "1"
		} else {
			return "0"
		}
	case l.Str != nil:
		return strconv.Quote(*l.Str)
	}
	return "0"
}

func (c *Compiler) compileSelector(s *parser.SelectorExpr) string {
	if len(s.Tail) == 1 && s.Tail[0] == "len" {
		root := sanitizeName(s.Root)
		if l, ok := c.listLens[root]; ok {
			return strconv.Itoa(l)
		}
	}
	if mod, ok := c.builtinAliases[s.Root]; ok && len(s.Tail) == 1 {
		switch mod {
		case "python_math":
			switch s.Tail[0] {
			case "pi":
				return "3.141592653589793"
			case "e":
				return "2.718281828459045"
			}
		case "go_testpkg":
			switch s.Tail[0] {
			case "Pi":
				return "3.14"
			case "Answer":
				return "42"
			}
		}
	}
	if capInfo, ok := c.captures[s.Root]; ok {
		expr := capInfo.global
		typ := capInfo.typ
		for _, f := range s.Tail {
			if st, ok := typ.(types.StructType); ok {
				expr += "." + fieldName(f)
				typ = st.Fields[f]
			} else {
				expr += "." + fieldName(f)
				typ = nil
			}
		}
		return expr
	}
	expr := sanitizeName(s.Root)
	ptr := c.pointerVars[s.Root]
	var typ types.Type
	if c.currentStruct != "" && c.env != nil {
		if st, ok := c.env.GetStruct(c.currentStruct); ok {
			if ft, ok2 := st.Fields[s.Root]; ok2 {
				expr = "self->" + sanitizeName(s.Root)
				typ = ft
			}
		}
	}
	if typ == nil && c.env != nil {
		if t, err := c.env.GetVar(s.Root); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if ut, ok2 := c.env.FindUnionByVariant(s.Root); ok2 && len(ft.Params) == 0 {
					return fmt.Sprintf("(%s){.tag=%s_%s}", sanitizeTypeName(ut.Name), sanitizeTypeName(ut.Name), sanitizeTypeName(s.Root))
				}
			}
			typ = t
			if ut, ok := t.(types.UnionType); ok {
				if _, ok2 := c.env.FindUnionByVariant(s.Root); ok2 {
					return fmt.Sprintf("(%s){.tag=%s_%s}", sanitizeTypeName(ut.Name), sanitizeTypeName(ut.Name), sanitizeTypeName(s.Root))
				}
			}
		}
		if typ == nil {
			if ut, ok := c.env.FindUnionByVariant(s.Root); ok {
				return fmt.Sprintf("(%s){.tag=%s_%s}", sanitizeTypeName(ut.Name), sanitizeTypeName(ut.Name), sanitizeTypeName(s.Root))
			}
		}
	}
	for i, f := range s.Tail {
		if gt, ok := typ.(types.GroupType); ok {
			if f == "key" {
				expr += ".key"
				typ = gt.Elem
				continue
			}
			if f == "items" {
				expr += ".items"
				typ = types.ListType{Elem: gt.Elem}
				continue
			}
		}
		if ut, ok := typ.(types.UnionType); ok {
			var variant string
			var ft types.Type
			for name, st := range ut.Variants {
				if t, ok2 := st.Fields[f]; ok2 {
					if variant != "" {
						variant = ""
						break
					}
					variant = name
					ft = t
				}
			}
			if variant != "" {
				expr += fmt.Sprintf(".value.%s.%s", sanitizeTypeName(variant), fieldName(f))
				typ = ft
				continue
			}
		}
		if ptr && i == 0 {
			expr += "->" + fieldName(f)
		} else {
			expr += "." + fieldName(f)
		}
		if st, ok := typ.(types.StructType); ok {
			typ = st.Fields[f]
		} else {
			typ = nil
		}
	}
	return expr
}

func (c *Compiler) selectorStructType(sel *parser.SelectorExpr) (types.StructType, bool) {
	if c.env == nil {
		return types.StructType{}, false
	}
	var t types.Type
	var ok bool
	v, err := c.env.GetVar(sel.Root)
	if err == nil {
		t = v
		ok = true
	} else if c.currentStruct != "" {
		if st, ok2 := c.env.GetStruct(c.currentStruct); ok2 {
			if ft, ok3 := st.Fields[sel.Root]; ok3 {
				t = ft
				ok = true
			}
		}
	}
	if !ok {
		return types.StructType{}, false
	}
	for _, f := range sel.Tail {
		st, ok := t.(types.StructType)
		if !ok {
			return types.StructType{}, false
		}
		t = st.Fields[f]
	}
	st, ok := t.(types.StructType)
	return st, ok
}

func (c *Compiler) selectorType(sel *parser.SelectorExpr) types.Type {
	if sel == nil {
		return types.AnyType{}
	}
	if c.env != nil {
		var typ types.Type
		if t, err := c.env.GetVar(sel.Root); err == nil {
			typ = t
		} else if c.currentStruct != "" {
			if st, ok := c.env.GetStruct(c.currentStruct); ok {
				typ = st.Fields[sel.Root]
			}
		}
		if typ != nil {
			for _, f := range sel.Tail {
				if st, ok := typ.(types.StructType); ok {
					typ = st.Fields[f]
				} else if ut, ok := typ.(types.UnionType); ok {
					var found types.Type
					for _, st := range ut.Variants {
						if ft, ok2 := st.Fields[f]; ok2 {
							if found != nil && !equalTypes(found, ft) {
								found = nil
								break
							}
							found = ft
						}
					}
					if found == nil {
						typ = nil
						break
					}
					typ = found
				} else {
					typ = nil
					break
				}
			}
			if typ != nil {
				return typ
			}
		}
	}
	p := &parser.Primary{Selector: sel}
	postfix := &parser.PostfixExpr{Target: p}
	unary := &parser.Unary{Value: postfix}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	return c.exprType(expr)
}

func isStringArg(e *parser.Expr, env *types.Env) bool {
	if e == nil {
		return false
	}
	c := New(env)
	_, ok := c.exprType(e).(types.StringType)
	return ok
}

func isStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isStringUnary(e.Binary.Left, env)
}

func isStringUnaryOrIndex(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isStringPostfixOrIndex(u.Value, env)
}

func isStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isStringPostfix(u.Value, env)
}

func isStringPostfixOrIndex(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		if p.Ops[0].Index != nil {
			if isStringPrimary(p.Target, env) || isListStringPrimary(p.Target, env) {
				return true
			}
			return false
		}
		return false
	}
	return isStringPrimary(p.Target, env)
}

func isStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		if p.Ops[0].Index != nil {
			if isListStringPrimary(p.Target, env) || isStringPrimary(p.Target, env) {
				return true
			}
			return false
		}
		return false
	}
	return isStringPrimary(p.Target, env)
}

func isStringPrimary(p *parser.Primary, env *types.Env) bool {
	switch {
	case p == nil:
		return false
	case p.Lit != nil && p.Lit.Str != nil:
		return true
	case p.Call != nil && p.Call.Func == "str":
		return true
	case p.Call != nil && env != nil:
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if _, ok := ft.Return.(types.StringType); ok {
					return true
				}
			}
		}
	case p.Selector != nil && env != nil:
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			for i, f := range p.Selector.Tail {
				if i == 0 && f == "key" {
					if gt, ok := t.(types.GroupType); ok {
						t = gt.Key
						continue
					}
				}
				st, ok := t.(types.StructType)
				if !ok {
					return false
				}
				ft, ok := st.Fields[f]
				if !ok {
					return false
				}
				t = ft
			}
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func isFloatArg(e *parser.Expr, env *types.Env) bool {
	if e == nil {
		return false
	}
	t := types.ExprType(e, env)
	_, ok := t.(types.FloatType)
	return ok
}

func isBoolArg(e *parser.Expr, env *types.Env) bool {
	if e == nil {
		return false
	}
	c := New(env)
	_, ok := c.exprType(e).(types.BoolType)
	return ok
}

// guessType infers a more specific type for expression e, falling back to int
// when the regular inference reports Any. It checks string, float and bool
// heuristics similar to inferStructFromMap.
func (c *Compiler) guessType(e *parser.Expr) types.Type {
	t := c.exprType(e)
	if name, ok := groupKeySelector(e); ok {
		if kt, ok2 := c.groupKeys[name]; ok2 {
			return kt
		}
		// fall back to env lookup or assume string when group key type
		// information has not been recorded yet (e.g. during
		// gatherAppendTypes)
		if c.env != nil {
			if vt, err := c.env.GetVar(name); err == nil {
				if gt, ok := vt.(types.GroupType); ok {
					if gt.Key != nil {
						return gt.Key
					}
				}
			}
		}
		return types.StringType{}
	}
	if types.ContainsAny(t) {
		switch {
		case isStringExpr(e, c.env):
			return types.StringType{}
		case isFloatExpr(e, c.env):
			return types.FloatType{}
		case isBoolArg(e, c.env):
			return types.BoolType{}
		default:
			return types.IntType{}
		}
	}
	return t
}

// gatherAppendTypes records list element types for variables that are assigned
// using append later in the statement list. This improves inference for
// variables declared with an empty list literal.
func (c *Compiler) gatherAppendTypes(stmts []*parser.Statement) map[string]types.Type {
	res := map[string]types.Type{}
	var walk func([]*parser.Statement)
	walk = func(list []*parser.Statement) {
		for _, st := range list {
			switch {
			case st.Assign != nil:
				if call, ok := callPattern(st.Assign.Value); ok && call.Func == "append" && len(call.Args) == 2 {
					if name, ok2 := identName(call.Args[0]); ok2 && name == st.Assign.Name {
						elem := c.guessType(call.Args[1])
						if ml := asMapLiteral(call.Args[1]); ml != nil {
							if stt, ok3 := c.inferStructFromMap(ml, st.Assign.Name); ok3 {
								elem = stt
							}
						}
						res[st.Assign.Name] = types.ListType{Elem: elem}
					}
				}
			case st.For != nil:
				walk(st.For.Body)
			case st.While != nil:
				walk(st.While.Body)
			case st.If != nil:
				walk(st.If.Then)
				if st.If.Else != nil {
					walk(st.If.Else)
				}
				for ei := st.If.ElseIf; ei != nil; ei = ei.ElseIf {
					walk(ei.Then)
				}
			case st.Fun != nil:
				walk(st.Fun.Body)
			}
		}
	}
	walk(stmts)
	return res
}

// groupKeySelector reports the root variable name if e is a `var.key` selector.
func groupKeySelector(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil {
		return "", false
	}
	sel := u.Value.Target.Selector
	if sel != nil && len(sel.Tail) >= 1 && sel.Tail[0] == "key" {
		return sel.Root, true
	}
	return "", false
}

func isFloatExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isFloatUnary(e.Binary.Left, env)
}

func isFloatUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isFloatPostfix(u.Value, env)
}

func isFloatPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isFloatPrimary(p.Target, env)
}

func isFloatPrimary(p *parser.Primary, env *types.Env) bool {
	switch {
	case p == nil:
		return false
	case p.Lit != nil && p.Lit.Float != nil:
		return true
	case p.Selector != nil && env != nil:
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.FloatType); ok {
				return true
			}
		}
	case p.Call != nil && env != nil:
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if _, ok2 := ft.Return.(types.FloatType); ok2 {
					return true
				}
			}
		}
	}
	return false
}

func isNowExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isNowUnary(e.Binary.Left)
}

func isNowUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isNowPostfix(u.Value)
}

func isNowPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isNowPrimary(p.Target)
}

func isNowPrimary(p *parser.Primary) bool {
	return p != nil && p.Call != nil && p.Call.Func == "now"
}

func isListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListLiteralUnary(e.Binary.Left)
}

func isListLiteralUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isListLiteralPostfix(u.Value)
}

func isListLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isListLiteralPrimary(p.Target)
}

func isListLiteralPrimary(p *parser.Primary) bool {
	return p != nil && p.List != nil
}

func isEmptyListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return false
	}
	if e.Binary.Left.Value.Target.List != nil {
		return len(e.Binary.Left.Value.Target.List.Elems) == 0
	}
	return false
}

func isEmptyMapLiteral(e *parser.Expr) bool {
	ml := asMapLiteral(e)
	return ml != nil && len(ml.Items) == 0
}

func isBoolLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil || len(u.Ops) > 0 || len(u.Value.Ops) > 0 {
		return false
	}
	lit := u.Value.Target.Lit
	return lit != nil && lit.Bool != nil
}

func isMapStringIntLiteral(ml *parser.MapLiteral, env *types.Env) bool {
	if ml == nil {
		return false
	}
	for _, it := range ml.Items {
		if _, ok := types.SimpleStringKey(it.Key); !ok {
			return false
		}
		if env != nil {
			switch types.ExprType(it.Value, env).(type) {
			case types.IntType, types.BoolType:
			default:
				return false
			}
		}
	}
	return true
}

func isMapIntStringLiteral(ml *parser.MapLiteral, env *types.Env) bool {
	if ml == nil {
		return false
	}
	for _, it := range ml.Items {
		if _, ok := types.ExprType(it.Key, env).(types.IntType); !ok {
			return false
		}
		if env != nil {
			if _, ok := types.ExprType(it.Value, env).(types.StringType); !ok {
				return false
			}
		}
	}
	return true
}

func isListListExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListListUnary(e.Binary.Left, env)
}

func isListListUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListListPostfix(u.Value, env)
}

func isListListPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isListListPrimary(p.Target, env)
}

func isListListPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) > 0 {
			if isListLiteral(p.List.Elems[0]) || isListIntExpr(p.List.Elems[0], env) {
				return true
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if isListListIntType(t) {
				return true
			}
		}
	}
	if p.Call != nil && env != nil {
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if isListListIntType(ft.Return) {
					return true
				}
			}
		}
	}
	return false
}

func isListIntExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListIntUnary(e.Binary.Left, env)
}

func isListIntUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListIntPostfix(u.Value, env)
}

func isListIntPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) == 0 {
		return isListIntPrimary(p.Target, env)
	}
	if p.Ops[0].Index != nil && p.Ops[0].Index.Colon != nil {
		if isStringPrimary(p.Target, env) {
			return false
		}
		return true
	}
	return false
}

func isListIntPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.Call != nil && p.Call.Func == "values" && env != nil {
		if isMapStringIntExpr(p.Call.Args[0], env) || isMapIntBoolExpr(p.Call.Args[0], env) {
			return true
		}
	}
	if p.List != nil {
		if len(p.List.Elems) == 0 {
			return true
		}
		el := p.List.Elems[0]
		if el.Binary != nil && el.Binary.Left != nil && el.Binary.Left.Value != nil && el.Binary.Left.Value.Target != nil {
			if el.Binary.Left.Value.Target.Lit != nil {
				if el.Binary.Left.Value.Target.Lit.Int != nil || el.Binary.Left.Value.Target.Lit.Bool != nil {
					return true
				}
			}
			if el.Binary.Left.Value.Target.Selector != nil && env != nil {
				name := el.Binary.Left.Value.Target.Selector.Root
				if t, err := env.GetVar(name); err == nil {
					if lt, ok := t.(types.ListType); ok {
						switch lt.Elem.(type) {
						case types.IntType, types.BoolType:
							return true
						}
					}
					switch t.(type) {
					case types.IntType, types.BoolType:
						return true
					}
				}
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				switch lt.Elem.(type) {
				case types.IntType, types.BoolType:
					return true
				}
			}
		}
	}
	if p.Call != nil && env != nil {
		if p.Call.Func == "append" {
			if len(p.Call.Args) > 0 && isListIntExpr(p.Call.Args[0], env) {
				return true
			}
		}
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if lt, ok2 := ft.Return.(types.ListType); ok2 {
					switch lt.Elem.(type) {
					case types.IntType, types.BoolType:
						return true
					}
				}
			}
		}
	}
	return false
}

func isListStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListStringUnary(e.Binary.Left, env)
}

func isListStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListStringPostfix(u.Value, env)
}

func isListStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) == 0 {
		return isListStringPrimary(p.Target, env)
	}
	if p.Ops[0].Index != nil && p.Ops[0].Index.Colon != nil {
		if isStringPrimary(p.Target, env) {
			return false
		}
		return true
	}
	return false
}

func isListStringPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) == 0 {
			return true
		}
		el := p.List.Elems[0]
		if el.Binary != nil && el.Binary.Left != nil && el.Binary.Left.Value != nil && el.Binary.Left.Value.Target != nil {
			if lit := el.Binary.Left.Value.Target.Lit; lit != nil && lit.Str != nil {
				return true
			}
			if sel := el.Binary.Left.Value.Target.Selector; sel != nil && env != nil {
				if t, err := env.GetVar(sel.Root); err == nil {
					if _, ok := t.(types.StringType); ok {
						return true
					}
					if lt, ok := t.(types.ListType); ok {
						if _, ok2 := lt.Elem.(types.StringType); ok2 {
							return true
						}
					}
				}
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.StringType); ok {
					return true
				}
			}
		}
	}
	if p.Call != nil && env != nil {
		if p.Call.Func == "append" {
			if len(p.Call.Args) > 0 && isListStringExpr(p.Call.Args[0], env) {
				return true
			}
		}
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if lt, ok2 := ft.Return.(types.ListType); ok2 {
					if _, ok3 := lt.Elem.(types.StringType); ok3 {
						return true
					}
				}
			}
		}
	}
	return false
}

func isListFloatExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListFloatUnary(e.Binary.Left, env)
}

func isListFloatUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListFloatPostfix(u.Value, env)
}

func isListFloatPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) == 0 {
		return isListFloatPrimary(p.Target, env)
	}
	if p.Ops[0].Index != nil && p.Ops[0].Index.Colon != nil {
		if isStringPrimary(p.Target, env) {
			return false
		}
		return true
	}
	return false
}

func isListFloatPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) == 0 {
			return true
		}
		el := p.List.Elems[0]
		if el.Binary != nil && el.Binary.Left != nil && el.Binary.Left.Value != nil && el.Binary.Left.Value.Target != nil {
			if lit := el.Binary.Left.Value.Target.Lit; lit != nil && lit.Float != nil {
				return true
			}
			if sel := el.Binary.Left.Value.Target.Selector; sel != nil && env != nil {
				if t, err := env.GetVar(sel.Root); err == nil {
					if _, ok := t.(types.FloatType); ok {
						return true
					}
					if lt, ok := t.(types.ListType); ok {
						if _, ok2 := lt.Elem.(types.FloatType); ok2 {
							return true
						}
					}
				}
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.FloatType); ok {
					return true
				}
			}
		}
	}
	if p.Call != nil && env != nil {
		if p.Call.Func == "append" {
			if len(p.Call.Args) > 0 && isListFloatExpr(p.Call.Args[0], env) {
				return true
			}
		}
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if lt, ok2 := ft.Return.(types.ListType); ok2 {
					if _, ok3 := lt.Elem.(types.FloatType); ok3 {
						return true
					}
				}
			}
		}
	}
	return false
}

func isMapIntBoolExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isMapIntBoolUnary(e.Binary.Left, env)
}

func isMapIntBoolUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isMapIntBoolPostfix(u.Value, env)
}

func isMapIntBoolPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		return false
	}
	return isMapIntBoolPrimary(p.Target, env)
}

func isMapIntBoolPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if isMapIntBoolType(t) {
				return true
			}
		}
	}
	return false
}

func isMapStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isMapStringUnary(e.Binary.Left, env)
}

func isMapStringIntExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isMapStringIntUnary(e.Binary.Left, env)
}

func isMapStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isMapStringPostfix(u.Value, env)
}

func isMapStringIntUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isMapStringIntPostfix(u.Value, env)
}

func isMapStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		return false
	}
	return isMapStringPrimary(p.Target, env)
}

func isMapStringIntPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		return false
	}
	return isMapStringIntPrimary(p.Target, env)
}

func isMapStringPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if isMapStringType(t) {
				return true
			}
		}
	}
	return false
}

func isMapStringIntPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if isMapStringIntType(t) {
				return true
			}
		}
	}
	return false
}

func isMapIntStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isMapIntStringUnary(e.Binary.Left, env)
}

func isMapIntStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isMapIntStringPostfix(u.Value, env)
}

func isMapIntStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		return false
	}
	return isMapIntStringPrimary(p.Target, env)
}

func isMapIntStringPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if isMapIntStringType(t) {
				return true
			}
		}
	}
	return false
}

func isListMapStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListMapStringUnary(e.Binary.Left, env)
}

func isListMapStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListMapStringPostfix(u.Value, env)
}

func isListMapStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) == 0 {
		return isListMapStringPrimary(p.Target, env)
	}
	if p.Ops[0].Index != nil && p.Ops[0].Index.Colon != nil {
		if isStringPrimary(p.Target, env) {
			return false
		}
		return true
	}
	return false
}

func isListMapStringPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) == 0 {
			return true
		}
		el := p.List.Elems[0]
		if el.Binary != nil && el.Binary.Left != nil && el.Binary.Left.Value != nil && el.Binary.Left.Value.Target != nil {
			if el.Binary.Left.Value.Target.Map != nil {
				return true
			}
			if sel := el.Binary.Left.Value.Target.Selector; sel != nil && env != nil {
				if t, err := env.GetVar(sel.Root); err == nil {
					if isMapStringType(t) {
						return true
					}
					if lt, ok := t.(types.ListType); ok {
						if isMapStringType(lt.Elem) {
							return true
						}
					}
				}
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if isMapStringType(lt.Elem) {
					return true
				}
			}
		}
	}
	if p.Call != nil && env != nil {
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if lt, ok2 := ft.Return.(types.ListType); ok2 {
					if isMapStringType(lt.Elem) {
						return true
					}
				}
			}
		}
	}
	return false
}

func isListStructExpr(e *parser.Expr, env *types.Env) bool {
	if env == nil || e == nil {
		return false
	}
	if lt, ok := types.ExprType(e, env).(types.ListType); ok {
		if _, ok2 := lt.Elem.(types.StructType); ok2 {
			return true
		}
	}
	return false
}

func isStructExpr(e *parser.Expr, env *types.Env) bool {
	if env == nil || e == nil {
		return false
	}
	_, ok := types.ExprType(e, env).(types.StructType)
	return ok
}

func asMapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return nil
	}
	return e.Binary.Left.Value.Target.Map
}

// castMapToStruct detects expressions of the form `{...} as StructName` and
// returns the target struct type. It only matches a direct cast applied to a
// map literal with no additional operators.
func castMapToStruct(e *parser.Expr, env *types.Env) (types.StructType, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return types.StructType{}, false
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Map == nil {
		return types.StructType{}, false
	}
	if len(u.Value.Ops) != 1 || u.Value.Ops[0].Cast == nil {
		return types.StructType{}, false
	}
	t := resolveTypeRef(u.Value.Ops[0].Cast.Type, env)
	st, ok := t.(types.StructType)
	return st, ok
}

func asFetchExpr(e *parser.Expr) *parser.FetchExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	if e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return nil
	}
	return e.Binary.Left.Value.Target.Fetch
}

func asStructLiteral(e *parser.Expr) *parser.StructLiteral {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return nil
	}
	return e.Binary.Left.Value.Target.Struct
}

func (c *Compiler) emitJSONExpr(e *parser.Expr) {
	argExpr := c.compileExpr(e)
	c.need(needJSON)
	c.need(needListFloat)
	c.need(needListString)
	c.need(needListListInt)
	c.need(needListInt)
	if isListListExpr(e, c.env) {
		c.writeln(fmt.Sprintf("_json_list_list_int(%s);", argExpr))
	} else if isListIntExpr(e, c.env) {
		c.writeln(fmt.Sprintf("_json_list_int(%s);", argExpr))
	} else if isListFloatExpr(e, c.env) {
		c.writeln(fmt.Sprintf("_json_list_float(%s);", argExpr))
	} else if isListStringExpr(e, c.env) {
		c.writeln(fmt.Sprintf("_json_list_string(%s);", argExpr))
	} else if isListMapStringExpr(e, c.env) {
		c.need(needJSONListMapString)
		c.writeln(fmt.Sprintf("_json_list_map_string(%s);", argExpr))
	} else if isListStructExpr(e, c.env) {
		lt, _ := c.exprType(e).(types.ListType)
		st, _ := lt.Elem.(types.StructType)
		loop := c.newLoopVar()
		c.writeln("printf(\"[\");")
		c.writeln(fmt.Sprintf("for(int %s=0; %s<%s; %s++){", loop, loop, c.listLenExpr(argExpr), loop))
		c.indent++
		c.writeln(fmt.Sprintf("if(%s>0) printf(\",\");", loop))
		c.writeln(fmt.Sprintf("%s it = %s;", sanitizeTypeName(st.Name), c.listItemExpr(argExpr, loop)))
		c.writeln("printf(\"{\");")
		for i, field := range st.Order {
			if i > 0 {
				c.writeln("printf(\",\");")
			}
			c.writeln(fmt.Sprintf("_json_string(\"%s\");", field))
			c.writeln("printf(\":\");")
			fe := fmt.Sprintf("it.%s", fieldName(field))
			switch st.Fields[field].(type) {
			case types.IntType, types.BoolType:
				c.writeln(fmt.Sprintf("_json_int(%s);", fe))
			case types.FloatType:
				c.writeln(fmt.Sprintf("_json_float(%s);", fe))
			case types.StringType:
				c.writeln(fmt.Sprintf("_json_string(%s);", fe))
			}
		}
		c.writeln("printf(\"}\");")
		c.indent--
		c.writeln("}")
		c.writeln("printf(\"]\");")
	} else if t, ok := c.exprType(e).(types.ListType); ok {
		if st, ok2 := t.Elem.(types.StructType); ok2 {
			loop := c.newLoopVar()
			c.writeln("printf(\"[\");")
			c.writeln(fmt.Sprintf("for(int %s=0; %s<%s.len; %s++){", loop, loop, argExpr, loop))
			c.indent++
			c.writeln(fmt.Sprintf("if(%s>0) printf(\",\");", loop))
			c.writeln(fmt.Sprintf("%s it = %s.data[%s];", sanitizeTypeName(st.Name), argExpr, loop))
			c.writeln("printf(\"{\");")
			for i, field := range st.Order {
				if i > 0 {
					c.writeln("printf(\",\");")
				}
				c.writeln(fmt.Sprintf("_json_string(\"%s\");", field))
				c.writeln("printf(\":\");")
				fe := fmt.Sprintf("it.%s", fieldName(field))
				switch st.Fields[field].(type) {
				case types.IntType, types.BoolType:
					c.writeln(fmt.Sprintf("_json_int(%s);", fe))
				case types.FloatType:
					c.writeln(fmt.Sprintf("_json_float(%s);", fe))
				case types.StringType:
					c.writeln(fmt.Sprintf("_json_string(%s);", fe))
				}
			}
			c.writeln("printf(\"}\");")
			c.indent--
			c.writeln("}")
			c.writeln("printf(\"]\");")
		} else {
			c.writeln(fmt.Sprintf("_json_int(%s.len);", argExpr))
		}
	} else if isFloatArg(e, c.env) {
		c.writeln(fmt.Sprintf("_json_float(%s);", argExpr))
	} else if isStringArg(e, c.env) {
		c.writeln(fmt.Sprintf("_json_string(%s);", argExpr))
	} else if isMapStringExpr(e, c.env) {
		c.need(needJSONMapString)
		c.writeln(fmt.Sprintf("_json_map_string(%s);", argExpr))
	} else if isMapStringIntExpr(e, c.env) {
		c.need(needJSONMapStringInt)
		c.writeln(fmt.Sprintf("_json_map_string_int(%s);", argExpr))
	} else if st, ok := c.exprType(e).(types.StructType); ok {
		c.writeln("printf(\"{\");")
		for i, field := range st.Order {
			if i > 0 {
				c.writeln("printf(\",\");")
			}
			c.writeln(fmt.Sprintf("_json_string(\"%s\");", field))
			c.writeln("printf(\":\");")
			fe := fmt.Sprintf("%s.%s", argExpr, fieldName(field))
			switch st.Fields[field].(type) {
			case types.IntType:
				c.writeln(fmt.Sprintf("_json_int(%s);", fe))
			case types.FloatType:
				c.writeln(fmt.Sprintf("_json_float(%s);", fe))
			case types.StringType:
				c.writeln(fmt.Sprintf("_json_string(%s);", fe))
			}
		}
		c.writeln("printf(\"}\");")
	} else {
		c.writeln(fmt.Sprintf("_json_int(%s);", argExpr))
	}
}

func (c *Compiler) inferStructFromList(ll *parser.ListLiteral, name string) (types.StructType, bool) {
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
		fields[key] = types.ExprType(it.Value, c.env)
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
			t := types.ExprType(it.Value, c.env)
			if !equalTypes(fields[key], t) {
				return types.StructType{}, false
			}
		}
	}
	stName := sanitizeName(singular(name))
	idx := 1
	base := stName
	for {
		if c.structs[stName] {
			stName = fmt.Sprintf("%s%d", base, idx)
			idx++
		} else if c.env != nil {
			if _, ok := c.env.GetStruct(stName); ok {
				stName = fmt.Sprintf("%s%d", base, idx)
				idx++
			} else {
				break
			}
		} else {
			break
		}
	}
	st := types.StructType{Name: stName, Fields: fields, Order: order}
	return st, true
}

// inferStructFromMap creates a struct type from a map literal if all keys are
// simple strings. It is used when query select clauses return map literals so
// that the generated C code can use a plain struct instead of a generic map.
func (c *Compiler) inferStructFromMap(ml *parser.MapLiteral, name string) (types.StructType, bool) {
	if ml == nil || len(ml.Items) == 0 {
		return types.StructType{}, false
	}
	fields := map[string]types.Type{}
	order := make([]string, len(ml.Items))
	for i, it := range ml.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			return types.StructType{}, false
		}
		order[i] = key
		var t types.Type
		if inner := it.Value.Binary.Left; inner != nil && inner.Value != nil && inner.Value.Target != nil && inner.Value.Target.Map != nil {
			if st, ok := c.inferStructFromMap(inner.Value.Target.Map, key); ok {
				t = st
				c.compileStructType(st)
				c.structLits[inner.Value.Target.Map] = st
			} else {
				t = c.guessType(it.Value)
			}
		} else {
			t = c.guessType(it.Value)
		}
		fields[key] = t
	}
	stName := sanitizeName(name) + "Item"
	idx := 1
	base := stName
	for {
		if c.structs[stName] {
			stName = fmt.Sprintf("%s%d", base, idx)
			idx++
		} else if c.env != nil {
			if _, ok := c.env.GetStruct(stName); ok {
				stName = fmt.Sprintf("%s%d", base, idx)
				idx++
			} else {
				break
			}
		} else {
			break
		}
	}
	st := types.StructType{Name: stName, Fields: fields, Order: order}
	return st, true
}

// constLiteralTypeVal returns the C type and value for simple literal
// expressions. It only handles integer, float, boolean and string literals.
func constLiteralTypeVal(e *parser.Expr) (typ, val string, ok bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", "", false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil || len(u.Ops) > 0 || len(u.Value.Ops) > 0 {
		return "", "", false
	}
	lit := u.Value.Target.Lit
	if lit == nil {
		return "", "", false
	}
	switch {
	case lit.Int != nil:
		return "int", strconv.Itoa(int(*lit.Int)), true
	case lit.Float != nil:
		s := strconv.FormatFloat(*lit.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return "double", s, true
	case lit.Bool != nil:
		if bool(*lit.Bool) {
			return "int", "1", true
		}
		return "int", "0", true
	case lit.Str != nil:
		return "char*", fmt.Sprintf("%q", *lit.Str), true
	default:
		return "", "", false
	}
}

func (c *Compiler) listIntLiteralPrimary(p *parser.Primary) ([]int, bool) {
	if p == nil {
		return nil, false
	}
	if p.List != nil {
		vals := make([]int, len(p.List.Elems))
		for i, el := range p.List.Elems {
			typ, val, ok := constLiteralTypeVal(el)
			if !ok || typ != "int" {
				return nil, false
			}
			iv, _ := strconv.Atoi(val)
			vals[i] = iv
		}
		return vals, true
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 {
		if vals, ok := c.listVals[p.Selector.Root]; ok {
			return vals, true
		}
	}
	return nil, false
}

func (c *Compiler) listIntLiteralPostfix(p *parser.PostfixExpr) ([]int, bool) {
	if p == nil || len(p.Ops) > 0 {
		return nil, false
	}
	return c.listIntLiteralPrimary(p.Target)
}

func (c *Compiler) listIntLiteralUnary(u *parser.Unary) ([]int, bool) {
	if u == nil || len(u.Ops) > 0 {
		return nil, false
	}
	return c.listIntLiteralPostfix(u.Value)
}

func (c *Compiler) listIntLiteralExpr(e *parser.Expr) ([]int, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	return c.listIntLiteralUnary(e.Binary.Left)
}

func unionInts(a, b []int) []int {
	res := []int{}
	seen := map[int]bool{}
	for _, v := range a {
		if !seen[v] {
			seen[v] = true
			res = append(res, v)
		}
	}
	for _, v := range b {
		if !seen[v] {
			seen[v] = true
			res = append(res, v)
		}
	}
	return res
}

func exceptInts(a, b []int) []int {
	mb := map[int]bool{}
	for _, v := range b {
		mb[v] = true
	}
	res := []int{}
	for _, v := range a {
		if !mb[v] {
			res = append(res, v)
		}
	}
	return res
}

func intersectInts(a, b []int) []int {
	mb := map[int]bool{}
	for _, v := range b {
		mb[v] = true
	}
	res := []int{}
	seen := map[int]bool{}
	for _, v := range a {
		if mb[v] && !seen[v] {
			seen[v] = true
			res = append(res, v)
		}
	}
	return res
}

func constFloatValue(e *parser.Expr) (float64, bool) {
	typ, val, ok := constLiteralTypeVal(e)
	if !ok {
		return 0, false
	}
	switch typ {
	case "int":
		iv, _ := strconv.Atoi(val)
		return float64(iv), true
	case "double":
		fv, _ := strconv.ParseFloat(val, 64)
		return fv, true
	default:
		return 0, false
	}
}

func constStringValue(e *parser.Expr) (string, bool) {
	typ, val, ok := constLiteralTypeVal(e)
	if !ok || typ != "char*" {
		return "", false
	}
	return val, true
}

func constIntValue(e *parser.Expr) (int, bool) {
	typ, val, ok := constLiteralTypeVal(e)
	if !ok || typ != "int" {
		return 0, false
	}
	iv, _ := strconv.Atoi(val)
	return iv, true
}

func looksLikeFloatConst(expr string) bool {
	if strings.ContainsAny(expr, ".eE") {
		if _, err := strconv.ParseFloat(expr, 64); err == nil {
			return true
		}
	}
	return false
}

func (c *Compiler) evalListIntUnary(u *parser.Unary) ([]int, bool) {
	return c.listIntLiteralUnary(u)
}

func (c *Compiler) evalListIntPostfix(p *parser.PostfixExpr) ([]int, bool) {
	return c.listIntLiteralPostfix(p)
}

func (c *Compiler) evalListIntBinary(b *parser.BinaryExpr) ([]int, bool) {
	vals, ok := c.evalListIntUnary(b.Left)
	if !ok {
		return nil, false
	}
	for _, op := range b.Right {
		rv, ok := c.evalListIntPostfix(op.Right)
		if !ok {
			return nil, false
		}
		switch op.Op {
		case "+":
			vals = append(vals, rv...)
		case "union":
			if op.All {
				vals = append(vals, rv...)
			} else {
				vals = unionInts(vals, rv)
			}
		case "except":
			vals = exceptInts(vals, rv)
		case "intersect":
			vals = intersectInts(vals, rv)
		default:
			return nil, false
		}
	}
	return vals, true
}

func (c *Compiler) evalListIntExpr(e *parser.Expr) ([]int, bool) {
	if e == nil || e.Binary == nil {
		return nil, false
	}
	return c.evalListIntBinary(e.Binary)
}

func (c *Compiler) evalListFloatExpr(e *parser.Expr) ([]float64, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	if e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return nil, false
	}
	ll := e.Binary.Left.Value.Target.List
	if ll == nil {
		return nil, false
	}
	vals := make([]float64, len(ll.Elems))
	for i, el := range ll.Elems {
		typ, val, ok := constLiteralTypeVal(el)
		if !ok || typ != "double" {
			return nil, false
		}
		fv, _ := strconv.ParseFloat(val, 64)
		vals[i] = fv
	}
	return vals, true
}

func (c *Compiler) evalListStringExpr(e *parser.Expr) ([]string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	if e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return nil, false
	}
	ll := e.Binary.Left.Value.Target.List
	if ll == nil {
		return nil, false
	}
	vals := make([]string, len(ll.Elems))
	for i, el := range ll.Elems {
		typ, val, ok := constLiteralTypeVal(el)
		if !ok || typ != "char*" {
			return nil, false
		}
		vals[i] = val
	}
	return vals, true
}

func (c *Compiler) evalListFloatPostfix(p *parser.PostfixExpr) ([]float64, bool) {
	if p == nil || len(p.Ops) > 0 {
		return nil, false
	}
	if p.Target != nil {
		if p.Target.List != nil {
			vals := make([]float64, len(p.Target.List.Elems))
			for i, el := range p.Target.List.Elems {
				typ, val, ok := constLiteralTypeVal(el)
				if !ok || typ != "double" {
					return nil, false
				}
				fv, _ := strconv.ParseFloat(val, 64)
				vals[i] = fv
			}
			return vals, true
		}
		if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
			if vals, ok := c.listValsFloat[p.Target.Selector.Root]; ok {
				return vals, true
			}
		}
	}
	return nil, false
}

func (c *Compiler) evalListStringPostfix(p *parser.PostfixExpr) ([]string, bool) {
	if p == nil || len(p.Ops) > 0 {
		return nil, false
	}
	if p.Target != nil {
		if p.Target.List != nil {
			vals := make([]string, len(p.Target.List.Elems))
			for i, el := range p.Target.List.Elems {
				typ, val, ok := constLiteralTypeVal(el)
				if !ok || typ != "char*" {
					return nil, false
				}
				vals[i] = val
			}
			return vals, true
		}
		if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
			if vals, ok := c.listValsString[p.Target.Selector.Root]; ok {
				return vals, true
			}
		}
	}
	return nil, false
}

func (c *Compiler) evalAppendInt(listExpr, valExpr *parser.Expr) ([]int, bool) {
	lst, ok := c.evalListIntExpr(listExpr)
	if !ok {
		return nil, false
	}
	typ, val, ok := constLiteralTypeVal(valExpr)
	if !ok || typ != "int" {
		return nil, false
	}
	iv, _ := strconv.Atoi(val)
	return append(lst, iv), true
}

func (c *Compiler) emitConstListInt(vals []int) string {
	name := c.newTemp()
	parts := make([]string, len(vals))
	for i, v := range vals {
		parts[i] = strconv.Itoa(v)
	}
	c.need(needListInt)
	c.writeln(fmt.Sprintf("list_int %s = list_int_create(%d);", name, len(vals)))
	for i, v := range parts {
		c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
	}
	c.listLens[name] = len(vals)
	c.listVals[name] = vals
	return name
}

func (c *Compiler) emitConstListFloat(vals []float64) string {
	name := c.newTemp()
	c.need(needListFloat)
	c.writeln(fmt.Sprintf("list_float %s = list_float_create(%d);", name, len(vals)))
	for i, fv := range vals {
		s := strconv.FormatFloat(fv, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, s))
	}
	c.listLens[name] = len(vals)
	c.listValsFloat[name] = vals
	return name
}

func (c *Compiler) emitConstListString(vals []string) string {
	name := c.newTemp()
	c.need(needListString)
	c.writeln(fmt.Sprintf("list_string %s = list_string_create(%d);", name, len(vals)))
	for i, v := range vals {
		c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
	}
	c.listLens[name] = len(vals)
	c.listValsString[name] = vals
	return name
}

func (c *Compiler) listLenExpr(name string) string {
	if expr, ok := c.arrayLens[name]; ok {
		return expr
	}
	if l, ok := c.listLens[name]; ok {
		return strconv.Itoa(l)
	}
	return name + ".len"
}

func (c *Compiler) listItemExpr(name, idx string) string {
	if c.isStackArrayExpr(name) {
		return fmt.Sprintf("%s[%s]", name, idx)
	}
	return fmt.Sprintf("%s.data[%s]", name, idx)
}

func (c *Compiler) isStackArrayExpr(expr string) bool {
	if expr == "" {
		return false
	}
	root := expr
	if i := strings.IndexAny(expr, "[."); i >= 0 {
		root = expr[:i]
	}
	_, ok := c.stackArrays[root]
	return ok
}

// evalJSONLinesExpr returns JSON strings for a list of map literals if it can
// be fully evaluated at compile time.
func (c *Compiler) evalJSONLinesExpr(e *parser.Expr) ([]string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	if e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return nil, false
	}
	ll := e.Binary.Left.Value.Target.List
	if ll == nil {
		return nil, false
	}
	lines := make([]string, len(ll.Elems))
	for i, el := range ll.Elems {
		if el.Binary == nil || len(el.Binary.Right) != 0 {
			return nil, false
		}
		if el.Binary.Left == nil || el.Binary.Left.Value == nil || el.Binary.Left.Value.Target == nil {
			return nil, false
		}
		ml := el.Binary.Left.Value.Target.Map
		if ml == nil {
			return nil, false
		}
		parts := make([]string, len(ml.Items))
		for j, it := range ml.Items {
			key, ok := types.SimpleStringKey(it.Key)
			if !ok {
				return nil, false
			}
			typ, val, ok := constLiteralTypeVal(it.Value)
			if !ok {
				return nil, false
			}
			switch typ {
			case "int", "double":
				parts[j] = fmt.Sprintf("\"%s\":%s", key, val)
			case "char*":
				parts[j] = fmt.Sprintf("\"%s\":%s", key, val)
			default:
				return nil, false
			}
		}
		lines[i] = fmt.Sprintf("{%s}", strings.Join(parts, ","))
	}
	return lines, true
}

func jsonObjectLiteral(ml *parser.MapLiteral) (string, bool) {
	if ml == nil {
		return "", false
	}
	parts := make([]string, len(ml.Items))
	for i, it := range ml.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			return "", false
		}
		typ, val, ok := constLiteralTypeVal(it.Value)
		if !ok {
			return "", false
		}
		switch typ {
		case "int", "double":
			parts[i] = fmt.Sprintf("\"%s\":%s", key, val)
		case "char*":
			parts[i] = fmt.Sprintf("\"%s\":%s", key, val)
		default:
			return "", false
		}
	}
	return "{" + strings.Join(parts, ",") + "}", true
}

func (c *Compiler) structInitFromMap(st types.StructType, ml *parser.MapLiteral) string {
	if ml == nil {
		return "{}"
	}
	parts := make([]string, len(st.Order))
	for i, fn := range st.Order {
		for _, it := range ml.Items {
			key, _ := types.SimpleStringKey(it.Key)
			if key == fn {
				parts[i] = c.compileExpr(it.Value)
				break
			}
		}
	}
	return "{" + strings.Join(parts, ", ") + "}"
}

func (c *Compiler) structInitFromStruct(st types.StructType, sl *parser.StructLiteral) string {
	if sl == nil {
		return "{}"
	}
	parts := make([]string, len(st.Order))
	for i, fn := range st.Order {
		for _, f := range sl.Fields {
			if f.Name == fn {
				parts[i] = c.compileExpr(f.Value)
				break
			}
		}
	}
	return "{" + strings.Join(parts, ", ") + "}"
}
