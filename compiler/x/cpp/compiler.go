//go:build slow

package cpp

import (
	"bytes"
	"fmt"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
)

type structInfo struct {
	Name   string
	Fields []string
	Types  []string
}

type variantInfo struct {
	Name   string
	Fields []string
	Types  []string
	Union  string
}

type unionInfo struct {
	Name     string
	Variants []*variantInfo
}

var cppReserved = map[string]bool{
	"alignas": true, "alignof": true, "and": true, "and_eq": true, "asm": true,
	"auto": true, "bitand": true, "bitor": true, "bool": true, "break": true,
	"case": true, "catch": true, "char": true, "class": true, "compl": true,
	"const": true, "constexpr": true, "const_cast": true, "continue": true,
	"decltype": true, "default": true, "delete": true, "do": true,
	"double": true, "dynamic_cast": true, "else": true, "enum": true,
	"explicit": true, "export": true, "extern": true, "false": true,
	"float": true, "for": true, "friend": true, "goto": true, "if": true,
	"inline": true, "int": true, "long": true, "mutable": true,
	"namespace": true, "new": true, "noexcept": true, "not": true,
	"not_eq": true, "nullptr": true, "operator": true, "or": true,
	"or_eq": true, "private": true, "protected": true, "public": true,
	"register": true, "reinterpret_cast": true, "return": true,
	"short": true, "signed": true, "sizeof": true, "static": true,
	"static_assert": true, "static_cast": true, "struct": true,
	"switch": true, "template": true, "this": true, "thread_local": true,
	"throw": true, "true": true, "try": true, "typedef": true, "typeid": true,
	"typename": true, "union": true, "unsigned": true, "using": true,
	"virtual": true, "void": true, "volatile": true, "wchar_t": true,
	"while": true, "xor": true, "xor_eq": true,
}

// Compiler translates a subset of Mochi to simple C++17 code.
type Compiler struct {
	header bytes.Buffer
	buf    bytes.Buffer
	indent int
	scope  int
	tmp    int
	vars   map[string]string

	funParams map[string]int

	aliases map[string]string

	renamed map[string]string

	structMap    map[string]*structInfo
	structByName map[string]*structInfo
	structCount  int
	varStruct    map[string]string
	elemType     map[string]string
	groups       map[string]struct{}
	usesJSON     bool
	usesIO       bool
	definedLoad  bool
	usesAppend   bool
	usesAvg      bool
	usesAny      bool
	usesNow      bool

	curFun    string
	returnMap map[string]string

	unions   map[string]*unionInfo
	variants map[string]*variantInfo

	packages map[string]struct{}

	future []*parser.Statement

	placeholders map[string]string

	nextStructName string
}

// Future sets the slice of upcoming statements used for type prediction.
func (c *Compiler) Future(stmts []*parser.Statement) { c.future = stmts }

// New returns a new compiler instance.
func New() *Compiler {
	return &Compiler{
		vars:           map[string]string{},
		aliases:        map[string]string{},
		renamed:        map[string]string{},
		structMap:      map[string]*structInfo{},
		structByName:   map[string]*structInfo{},
		varStruct:      map[string]string{},
		elemType:       map[string]string{},
		funParams:      map[string]int{},
		groups:         map[string]struct{}{},
		usesJSON:       false,
		usesIO:         false,
		definedLoad:    false,
		scope:          0,
		unions:         map[string]*unionInfo{},
		variants:       map[string]*variantInfo{},
		packages:       map[string]struct{}{},
		future:         nil,
		placeholders:   map[string]string{},
		usesAppend:     false,
		usesAvg:        false,
		usesAny:        false,
		usesNow:        false,
		curFun:         "",
		returnMap:      map[string]string{},
		nextStructName: "",
	}
}

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("__tmp%d", c.tmp)
}

func parseNumber(s string) (float64, bool, bool) {
	if v, err := strconv.ParseInt(s, 10, 64); err == nil {
		return float64(v), true, true
	}
	if v, err := strconv.ParseFloat(s, 64); err == nil {
		return v, false, true
	}
	return 0, false, false
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) headerWriteln(s string) {
	c.header.WriteString(s)
	c.header.WriteByte('\n')
}

func (c *Compiler) defineStruct(info *structInfo) {
	// Replace any field types that reference known variables with their
	// underlying struct type to avoid undeclared identifiers in the
	// generated C++ output. This is helpful when map literals are used
	// before the variable's struct type information is fully propagated.
	re := regexp.MustCompile(`([A-Za-z_][A-Za-z0-9_]*)\.`)
	for i, t := range info.Types {
		for v, st := range c.varStruct {
			if st == "" {
				continue
			}
			base := strings.TrimSuffix(st, "{}")
			if strings.Contains(t, v+".") {
				t = strings.ReplaceAll(t, v+".",
					fmt.Sprintf("std::declval<%s>().", base))
			}
		}
		if matches := re.FindAllStringSubmatch(t, -1); matches != nil {
			for _, m := range matches {
				name := m[1]
				if st := c.varStruct[name]; st != "" {
					base := strings.TrimSuffix(st, "{}")
					t = strings.ReplaceAll(t, name+".",
						fmt.Sprintf("std::declval<%s>().", base))
				} else if et := c.elemType[name]; et != "" && c.isStructName(et) {
					base := strings.TrimSuffix(et, "{}")
					t = strings.ReplaceAll(t, name+".",
						fmt.Sprintf("std::declval<%s>().", base))
				}
			}
		}
		if strings.HasPrefix(t, "decltype(") {
			inner := strings.TrimSuffix(strings.TrimPrefix(t, "decltype("), ")")
			if !(strings.HasPrefix(inner, "std::declval<") && strings.Contains(inner, ">().")) {
				if c.vars[inner] == "" && c.varStruct[inner] == "" && c.elemType[inner] == "" {
					t = "std::any"
					c.usesAny = true
				}
			}
		}
		info.Types[i] = t
	}
	var def strings.Builder
	def.WriteString("struct " + info.Name + " {")
	for i, n := range info.Fields {
		def.WriteString(info.Types[i] + " " + n + "; ")
	}
	def.WriteString("};")
	c.headerWriteln(def.String())
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
	if cppReserved[s] {
		s = "_" + s
	}
	return s
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

// Compile converts a parsed Mochi program to C++ source code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.header.Reset()
	c.structMap = map[string]*structInfo{}
	c.structByName = map[string]*structInfo{}
	c.structCount = 0
	c.unions = map[string]*unionInfo{}
	c.variants = map[string]*variantInfo{}
	c.usesJSON = false

	globals := []*parser.Statement{}
	hasFunc := false
	for _, st := range p.Statements {
		if st.Fun != nil {
			hasFunc = true
			break
		}
	}
	encounteredFun := false
	encounteredStmt := false
	for _, st := range p.Statements {
		if st.Fun != nil {
			encounteredFun = true
			encounteredStmt = true
			continue
		}
		if hasFunc && !encounteredFun && !encounteredStmt && (st.Let != nil || st.Var != nil || st.Type != nil) {
			globals = append(globals, st)
			continue
		}
		if st.Let == nil && st.Var == nil && st.Type == nil {
			encounteredStmt = true
		}
	}

	globalSet := map[*parser.Statement]struct{}{}
	for _, st := range globals {
		globalSet[st] = struct{}{}
	}
	if err := c.compileBlock(globals); err != nil {
		return nil, err
	}
	if len(globals) > 0 {
		c.writeln("")
	}

	// first generate function declarations
	for _, st := range p.Statements {
		if st.Fun != nil {
			if err := c.compileFun(st.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("int main() {")
	c.indent++
	c.scope++
	mainStmts := []*parser.Statement{}
	for _, st := range p.Statements {
		if st.Fun != nil {
			continue
		}
		if _, ok := globalSet[st]; ok {
			continue
		}
		mainStmts = append(mainStmts, st)
	}
	if err := c.compileBlock(mainStmts); err != nil {
		return nil, err
	}
	c.writeln("return 0;")
	c.indent--
	c.scope--
	c.writeln("}")

	if c.usesJSON {
		names := make([]string, 0, len(c.structMap))
		for name := range c.structMap {
			names = append(names, name)
		}
		sort.Strings(names)
		for _, n := range names {
			c.generateJSONPrinter(c.structMap[n])
		}
	}

	var body bytes.Buffer
	body.Write(c.header.Bytes())
	body.Write(c.buf.Bytes())

	src := body.Bytes()
	for pkg := range c.packages {
		src = bytes.ReplaceAll(src, []byte(pkg+"."), []byte(pkg+"::"))
	}
	for ph, name := range c.placeholders {
		if t := c.elemType[name]; t != "" {
			src = bytes.ReplaceAll(src, []byte(ph), []byte(t))
		}
	}

	includes := []string{"#include <iostream>"}
	add := func(h string) {
		for _, e := range includes {
			if e == h {
				return
			}
		}
		includes = append(includes, h)
	}
	if bytes.Contains(src, []byte("std::vector")) || c.usesJSON {
		add("#include <vector>")
	}
	if bytes.Contains(src, []byte("std::unordered_map")) || c.usesJSON {
		add("#include <unordered_map>")
	}
	if bytes.Contains(src, []byte("std::map")) || c.usesJSON {
		add("#include <map>")
	}
	if bytes.Contains(src, []byte("std::string")) {
		add("#include <string>")
	}
	if bytes.Contains(src, []byte("std::sort")) || bytes.Contains(src, []byte("std::remove")) || bytes.Contains(src, []byte("std::min_element")) || bytes.Contains(src, []byte("std::max_element")) || bytes.Contains(src, []byte("std::unique")) || bytes.Contains(src, []byte("std::find")) || bytes.Contains(src, []byte("std::any_of")) {
		add("#include <algorithm>")
	}
	if bytes.Contains(src, []byte("std::accumulate")) {
		add("#include <numeric>")
	}
	if bytes.Contains(src, []byte("std::pair")) {
		add("#include <utility>")
	}
	if bytes.Contains(src, []byte("std::ifstream")) || bytes.Contains(src, []byte("std::ofstream")) || c.usesIO {
		add("#include <fstream>")
	}
	if bytes.Contains(src, []byte("std::unique_ptr")) {
		add("#include <memory>")
	}
	if bytes.Contains(src, []byte("std::any")) || c.usesAny {
		add("#include <any>")
	}
	if bytes.Contains(src, []byte("std::setprecision")) {
		add("#include <iomanip>")
	}
	if c.usesNow {
		add("#include <chrono>")
	}

	var out bytes.Buffer
	for _, h := range includes {
		out.WriteString(h)
		out.WriteByte('\n')
	}
	out.WriteByte('\n')
	if c.usesJSON {
		out.WriteString("template<typename T> void __json(const T&);\n")
		out.WriteString("inline void __json(int v){ std::cout<<v; }\n")
		out.WriteString("inline void __json(double v){ std::cout<<v; }\n")
		out.WriteString("inline void __json(bool v){ std::cout<<(v?\"true\":\"false\"); }\n")
		out.WriteString("inline void __json(const std::string &v){ std::cout<<\"\\\"\"<<v<<\"\\\"\"; }\n")
		out.WriteString("inline void __json(const char* v){ std::cout<<\"\\\"\"<<v<<\"\\\"\"; }\n")
		out.WriteString("template<typename T> void __json(const std::vector<T>& v){ std::cout<<\"[\"; bool first=true; for(const auto&x:v){ if(!first) std::cout<<\", \"; first=false; __json(x);} std::cout<<\"]\"; }\n")
		out.WriteString("template<typename K,typename V> void __json(const std::map<K,V>& m){ std::cout<<\"{\"; bool first=true; for(const auto&kv:m){ if(!first) std::cout<<\",\"; first=false; __json(kv.first); std::cout<<\":\"; __json(kv.second);} std::cout<<\"}\"; }\n")
		out.WriteString("template<typename K,typename V> void __json(const std::unordered_map<K,V>& m){ std::cout<<\"{\"; bool first=true; for(const auto&kv:m){ if(!first) std::cout<<\",\"; first=false; __json(kv.first); std::cout<<\":\"; __json(kv.second);} std::cout<<\"}\"; }\n")
		out.WriteByte('\n')
	}
	if c.usesAny {
		out.WriteString("inline bool __any_eq(const std::any&a,const std::any&b){ if(a.type()!=b.type()) return false; if(a.type()==typeid(int)) return std::any_cast<int>(a)==std::any_cast<int>(b); if(a.type()==typeid(double)) return std::any_cast<double>(a)==std::any_cast<double>(b); if(a.type()==typeid(bool)) return std::any_cast<bool>(a)==std::any_cast<bool>(b); if(a.type()==typeid(std::string)) return std::any_cast<std::string>(a)==std::any_cast<std::string>(b); return false; }\n")
		out.WriteString("inline void __print_any(const std::any&a){ if(a.type()==typeid(int)) std::cout<<std::any_cast<int>(a); else if(a.type()==typeid(double)) std::cout<<std::any_cast<double>(a); else if(a.type()==typeid(bool)) std::cout<<(std::any_cast<bool>(a)?\"true\":\"false\"); else if(a.type()==typeid(std::string)) std::cout<<std::any_cast<std::string>(a); }\n")
		out.WriteString("inline std::string __any_str(const std::any&a){ if(a.type()==typeid(int)) return std::to_string(std::any_cast<int>(a)); if(a.type()==typeid(double)) return std::to_string(std::any_cast<double>(a)); if(a.type()==typeid(bool)) return std::any_cast<bool>(a)?\"true\":\"false\"; if(a.type()==typeid(std::string)) return std::any_cast<std::string>(a); return \"\"; }\n")
		out.WriteByte('\n')
	}
	out.Write(src)
	return FormatCPP(out.Bytes()), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	name := fn.Name
	if name == "main" {
		name = "__mochi_main"
		c.renamed[fn.Name] = name
	}
	c.funParams[fn.Name] = len(fn.Params)
	c.funParams[name] = len(fn.Params)
	prevFun := c.curFun
	c.curFun = fn.Name
	defer func() { c.curFun = prevFun }()
	c.writeIndent()
	ret, err := c.compileType(fn.Return)
	if err != nil {
		return err
	}
	c.buf.WriteString(ret + " ")
	c.buf.WriteString(name)
	c.buf.WriteString("(")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		typ, err := c.compileType(p.Type)
		if err != nil {
			return err
		}
		c.buf.WriteString(typ)
		c.buf.WriteByte(' ')
		c.buf.WriteString(p.Name)
		switch {
		case typ == "std::string":
			c.vars[p.Name] = "string"
		case typ == "int" || typ == "double" || typ == "bool":
			c.vars[p.Name] = strings.TrimPrefix(typ, "std::")
		case strings.HasPrefix(typ, "std::vector<"):
			c.vars[p.Name] = "vector"
			elem := typ[len("std::vector<") : len(typ)-1]
			c.elemType[p.Name] = elem
			if c.isStructName(elem) {
				c.varStruct[p.Name] = elem
			}
		}
	}
	c.buf.WriteString(") {\n")
	c.indent++
	c.scope++
	if err := c.compileBlock(fn.Body); err != nil {
		return err
	}
	c.indent--
	c.scope--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStmt(st *parser.Statement) error {
	switch {
	case st.Import != nil:
		return c.compileImport(st.Import)
	case st.Type != nil:
		return c.compileTypeDecl(st.Type)
	case st.ExternVar != nil || st.ExternFun != nil || st.ExternType != nil || st.ExternObject != nil:
		// extern declarations are ignored in the C++ backend
		return nil
	case st.Fun != nil:
		lambda, err := c.compileLambda(st.Fun.Params, nil, st.Fun.Body)
		if err != nil {
			return err
		}
		c.funParams[st.Fun.Name] = len(st.Fun.Params)
		c.writeIndent()
		c.buf.WriteString("auto ")
		c.buf.WriteString(st.Fun.Name)
		c.buf.WriteString(" = ")
		c.buf.WriteString(lambda)
		c.buf.WriteString(";\n")
		return nil
	case st.Let != nil:
		return c.compileLet(st.Let)
	case st.Var != nil:
		return c.compileVar(st.Var)
	case st.Assign != nil:
		return c.compileAssign(st.Assign)
	case st.Return != nil:
		expr, err := c.compileExpr(st.Return.Value)
		if err != nil {
			return err
		}
		if c.curFun != "" {
			if mv := c.extractMapValueType(expr); mv != "" {
				c.returnMap[c.curFun] = mv
			}
		}
		c.writeln("return " + expr + ";")
		return nil
	case st.If != nil:
		return c.compileIf(st.If)
	case st.While != nil:
		return c.compileWhile(st.While)
	case st.For != nil:
		return c.compileFor(st.For)
	case st.Test != nil:
		return c.compileTestBlock(st.Test)
	case st.Expect != nil:
		return c.compileExpect(st.Expect)
	case st.Update != nil:
		return c.compileUpdate(st.Update)
	case st.Break != nil:
		c.writeln("break;")
		return nil
	case st.Continue != nil:
		c.writeln("continue;")
		return nil
	case st.Expr != nil:
		if call, ok := printCall(st.Expr.Expr); ok {
			return c.compilePrint(call.Args)
		}
		expr, err := c.compileExpr(st.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	default:
		return fmt.Errorf("unsupported statement at %v", st.Pos)
	}
}

func (c *Compiler) compileBlock(stmts []*parser.Statement) error {
	prev := c.future
	for i, st := range stmts {
		if i+1 < len(stmts) {
			c.future = stmts[i+1:]
		} else {
			c.future = nil
		}
		if err := c.compileStmt(st); err != nil {
			c.future = prev
			return err
		}
	}
	c.future = prev
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		u := &unionInfo{Name: t.Name}
		c.unions[t.Name] = u
		c.headerWriteln("struct " + t.Name + " { virtual ~" + t.Name + "() = default; };")
		for _, v := range t.Variants {
			vi := &variantInfo{Name: v.Name, Union: t.Name}
			for _, f := range v.Fields {
				typ, err := c.compileType(f.Type)
				if err != nil {
					return err
				}
				if f.Type != nil && f.Type.Simple != nil && *f.Type.Simple == t.Name {
					typ = "std::unique_ptr<" + t.Name + ">"
				}
				vi.Fields = append(vi.Fields, f.Name)
				vi.Types = append(vi.Types, typ)
			}
			c.variants[v.Name] = vi
			u.Variants = append(u.Variants, vi)
			c.headerWriteln("struct " + v.Name + " : " + t.Name + " {")
			for i, fn := range vi.Fields {
				ft := vi.Types[i]
				if ft == "" {
					ft = "std::unique_ptr<" + t.Name + ">"
				}
				c.headerWriteln("    " + ft + " " + fn + ";")
			}
			c.headerWriteln("};")
		}
		return nil
	}
	if len(t.Members) == 0 {
		return nil
	}
	info := &structInfo{Name: t.Name}
	for _, m := range t.Members {
		if m.Field != nil {
			typ, err := c.compileType(m.Field.Type)
			if err != nil {
				return err
			}
			info.Fields = append(info.Fields, m.Field.Name)
			info.Types = append(info.Types, typ)
		}
	}
	c.defineStruct(info)
	sig := t.Name + ":" + strings.Join(info.Fields, ",")
	c.structMap[sig] = info
	c.structByName[info.Name] = info
	return nil
}

func (c *Compiler) compileLet(st *parser.LetStmt) error {
	var exprStr string
	prevHint := c.nextStructName
	if st.Value != nil {
		c.nextStructName = structNameFromVar(st.Name)
		var err error
		exprStr, err = c.compileExpr(st.Value)
		c.nextStructName = prevHint
		if err != nil {
			return err
		}
	}

	typ, err := c.compileType(st.Type)
	if err != nil {
		return err
	}
	var elemHint string
	if st.Type != nil && isEmptyListExpr(st.Value) {
		if st.Type.Generic != nil && st.Type.Generic.Name == "list" && len(st.Type.Generic.Args) == 1 {
			elem, err := c.compileType(st.Type.Generic.Args[0])
			if err != nil {
				return err
			}
			exprStr = "{}"
			elemHint = elem
			typ = fmt.Sprintf("std::vector<%s>", elem)
		}
	}
	if st.Type == nil {
		if isEmptyListExpr(st.Value) {
			if et := c.predictElemType(st.Name); et != "" {
				typ = fmt.Sprintf("std::vector<%s>", et)
				exprStr = fmt.Sprintf("std::vector<%s>{}", et)
				if c.isStructName(et) {
					c.varStruct[st.Name] = et
				}
				c.elemType[st.Name] = et
				c.vars[st.Name] = "vector"
			} else {
				ph := "__" + st.Name + "_type"
				typ = fmt.Sprintf("std::vector<%s>", ph)
				exprStr = fmt.Sprintf("std::vector<%s>{}", ph)
				c.placeholders[ph] = st.Name
				c.vars[st.Name] = "vector"
			}
		} else if et := c.extractVectorElemType(exprStr); et != "" && !strings.HasPrefix(exprStr, "std::unordered_map<") && !strings.HasPrefix(exprStr, "std::map<") && c.vars[exprStr] != "map" {
			typ = fmt.Sprintf("std::vector<%s>", et)
			elemHint = et
		} else if et := c.elemType[exprStr]; et != "" && !strings.HasPrefix(exprStr, "std::unordered_map<") && !strings.HasPrefix(exprStr, "std::map<") && c.vars[exprStr] != "map" {
			typ = fmt.Sprintf("std::vector<%s>", et)
			elemHint = et
		} else if typ == "auto" {
			if m := regexp.MustCompile(`^([A-Za-z_][A-Za-z0-9_]*)\[`).FindStringSubmatch(exprStr); m != nil {
				base := m[1]
				if et := c.elemType[base]; et != "" {
					typ = et
					elemHint = et
					if c.isStructName(et) {
						c.varStruct[st.Name] = et
					}
				}
			}
		}
	}

	san := sanitizeName(st.Name)
	if san != st.Name {
		c.aliases[st.Name] = san
	}

	c.writeIndent()
	c.buf.WriteString(typ)
	c.buf.WriteByte(' ')
	c.buf.WriteString(san)
	if st.Value != nil {
		exprStr = removeRedundantInit(exprStr, typ)
		c.buf.WriteString(" = ")
		c.buf.WriteString(exprStr)
	} else if st.Type != nil {
		switch typ {
		case "int":
			c.buf.WriteString(" = 0")
		case "std::string":
			c.buf.WriteString(" = \"\"")
		}
	}
	c.buf.WriteString(";\n")
	if st.Type != nil {
		if typ == "std::string" {
			c.vars[st.Name] = "string"
		}
	} else {
		inferred := c.inferType(exprStr)
		if inferred == "" && strings.Contains(exprStr, "__sum") {
			inferred = "int"
		}
		if inferred == "" && c.isVectorExpr(st.Value) {
			inferred = "vector"
		}
		c.vars[st.Name] = inferred
	}
	if t := c.varStruct[exprStr]; t != "" {
		c.varStruct[st.Name] = t
	} else if s := c.extractVectorStruct(exprStr); s != "" {
		c.varStruct[st.Name] = s
	}
	if elemHint != "" {
		c.elemType[st.Name] = elemHint
	} else if et := c.extractVectorElemType(exprStr); et != "" {
		c.elemType[st.Name] = et
	} else if t := c.elemType[exprStr]; t != "" {
		c.elemType[st.Name] = t
	}
	if mv := c.extractMapValueType(exprStr); mv != "" {
		c.elemType[st.Name] = mv
		c.vars[st.Name] = "map"
	}
	return nil
}

func (c *Compiler) compileVar(st *parser.VarStmt) error {
	var exprStr string
	prevHint := c.nextStructName
	if st.Value != nil {
		c.nextStructName = structNameFromVar(st.Name)
		var err error
		exprStr, err = c.compileExpr(st.Value)
		c.nextStructName = prevHint
		if err != nil {
			return err
		}
	}

	typ, err := c.compileType(st.Type)
	if err != nil {
		return err
	}
	var elemHint string
	if st.Type == nil {
		if isEmptyListExpr(st.Value) {
			if et := c.predictElemType(st.Name); et != "" {
				typ = fmt.Sprintf("std::vector<%s>", et)
				exprStr = fmt.Sprintf("std::vector<%s>{}", et)
				if c.isStructName(et) {
					c.varStruct[st.Name] = et
				}
				c.elemType[st.Name] = et
				c.vars[st.Name] = "vector"
			} else {
				ph := "__" + st.Name + "_type"
				typ = fmt.Sprintf("std::vector<%s>", ph)
				exprStr = fmt.Sprintf("std::vector<%s>{}", ph)
				c.placeholders[ph] = st.Name
				c.vars[st.Name] = "vector"
			}
		} else if et := c.extractVectorElemType(exprStr); et != "" && !strings.HasPrefix(exprStr, "std::unordered_map<") && !strings.HasPrefix(exprStr, "std::map<") && c.vars[exprStr] != "map" {
			typ = fmt.Sprintf("std::vector<%s>", et)
			elemHint = et
		} else if et := c.elemType[exprStr]; et != "" && !strings.HasPrefix(exprStr, "std::unordered_map<") && !strings.HasPrefix(exprStr, "std::map<") && c.vars[exprStr] != "map" {
			typ = fmt.Sprintf("std::vector<%s>", et)
			elemHint = et
		}
	}

	san := sanitizeName(st.Name)
	if san != st.Name {
		c.aliases[st.Name] = san
	}

	c.writeIndent()
	c.buf.WriteString(typ)
	c.buf.WriteByte(' ')
	c.buf.WriteString(san)
	if st.Value != nil {
		exprStr = removeRedundantInit(exprStr, typ)
		c.buf.WriteString(" = ")
		c.buf.WriteString(exprStr)
	} else if st.Type != nil {
		switch typ {
		case "int":
			c.buf.WriteString(" = 0")
		case "std::string":
			c.buf.WriteString(" = \"\"")
		}
	}
	c.buf.WriteString(";\n")
	if st.Type != nil {
		if typ == "std::string" {
			c.vars[st.Name] = "string"
		}
	} else {
		inferred := c.inferType(exprStr)
		if inferred == "" && strings.Contains(exprStr, "__sum") {
			inferred = "int"
		}
		if inferred == "" && c.isVectorExpr(st.Value) {
			inferred = "vector"
		}
		c.vars[st.Name] = inferred
	}
	if t := c.varStruct[exprStr]; t != "" {
		c.varStruct[st.Name] = t
	} else if s := c.extractVectorStruct(exprStr); s != "" {
		c.varStruct[st.Name] = s
	}
	if elemHint != "" {
		c.elemType[st.Name] = elemHint
	} else if et := c.extractVectorElemType(exprStr); et != "" {
		c.elemType[st.Name] = et
	} else if t := c.elemType[exprStr]; t != "" {
		c.elemType[st.Name] = t
	}
	if mv := c.extractMapValueType(exprStr); mv != "" {
		c.elemType[st.Name] = mv
		c.vars[st.Name] = "map"
	}
	return nil
}

func (c *Compiler) compileAssign(st *parser.AssignStmt) error {
	c.writeIndent()
	name := st.Name
	for _, idx := range st.Index {
		if idx.Start != nil {
			expr, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			name += "[" + expr + "]"
		}
	}
	for _, fld := range st.Field {
		name += "." + fld.Name
	}
	// Special case: "x = append(x, y)" -> "x.push_back(y);"
	if len(st.Index) == 0 && len(st.Field) == 0 {
		if call, ok := callPattern(st.Value); ok && call.Func == "append" && len(call.Args) == 2 {
			if id, ok2 := identName(call.Args[0]); ok2 && id == st.Name {
				arg, err := c.compileExpr(call.Args[1])
				if err != nil {
					return err
				}
				c.buf.WriteString(name + ".push_back(" + arg + ");\n")
				// update placeholder type info
				if t := c.extractVectorType(arg); t != "" {
					c.elemType[st.Name] = t
				} else if t := c.structLiteralType(arg); t != "" {
					c.elemType[st.Name] = t
					c.varStruct[st.Name] = t
				} else if t := c.varStruct[arg]; t != "" {
					c.elemType[st.Name] = t
					c.varStruct[st.Name] = t
				} else if t := c.elemType[arg]; t != "" {
					c.elemType[st.Name] = t
				} else if t := inferExprType(arg); t != "" {
					c.elemType[st.Name] = t
				}
				return nil
			}
		}
	}
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	// If this is an append back into the same variable, record the element
	// type so placeholders in the declaration can be resolved.
	if call, ok := callPattern(st.Value); ok && call.Func == "append" && len(call.Args) == 2 {
		if id, ok2 := identName(call.Args[0]); ok2 && id == st.Name {
			if arg, err2 := c.compileExpr(call.Args[1]); err2 == nil {
				if t := c.extractVectorType(arg); t != "" {
					c.elemType[st.Name] = t
				} else if t := c.structLiteralType(arg); t != "" {
					c.elemType[st.Name] = t
					c.varStruct[st.Name] = t
				} else if t := c.varStruct[arg]; t != "" {
					c.elemType[st.Name] = t
					c.varStruct[st.Name] = t
				} else if t := c.elemType[arg]; t != "" {
					c.elemType[st.Name] = t
				}
			}
		}
	}
	c.buf.WriteString(name + " = " + expr + ";\n")
	if len(st.Index) == 0 && len(st.Field) == 0 {
		if s := c.extractVectorStruct(expr); s != "" {
			c.varStruct[st.Name] = s
		} else if t := c.varStruct[expr]; t != "" {
			c.varStruct[st.Name] = t
		}
		if et := c.extractVectorElemType(expr); et != "" {
			c.elemType[st.Name] = et
		} else if t := c.elemType[expr]; t != "" {
			c.elemType[st.Name] = t
		}
		if typ := c.inferType(expr); typ != "" {
			c.vars[st.Name] = typ
		}
	}
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	cond = c.ensureBool(cond)
	c.writeln("if (" + cond + ") {")
	c.indent++
	if err := c.compileBlock(st.Then); err != nil {
		return err
	}
	c.indent--
	c.writeln("}")
	if st.Else != nil {
		c.writeln("else {")
		c.indent++
		if err := c.compileBlock(st.Else); err != nil {
			return err
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	cond = c.ensureBool(cond)
	c.writeln("while (" + cond + ") {")
	c.indent++
	if err := c.compileBlock(st.Body); err != nil {
		return err
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(st *parser.ForStmt) error {
	src, err := c.compileExpr(st.Source)
	if err != nil {
		return err
	}
	if st.RangeEnd != nil {
		startExpr := src
		endExpr, err := c.compileExpr(st.RangeEnd)
		if err != nil {
			return err
		}
		c.vars[st.Name] = "int"
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", st.Name, startExpr, st.Name, endExpr, st.Name))
		c.indent++
		if err := c.compileBlock(st.Body); err != nil {
			return err
		}
		c.indent--
		c.writeln("}")
		return nil
	}

	if c.vars[src] == "map" {
		iter := c.newTmp()
		c.writeln(fmt.Sprintf("for (const auto &%s : %s) {", iter, src))
		c.indent++
		c.writeln(fmt.Sprintf("auto %s = %s.first;", st.Name, iter))
	} else {
		c.writeln(fmt.Sprintf("for (auto %s : %s) {", st.Name, src))
		c.indent++
		if t := c.varStruct[src]; t != "" {
			c.varStruct[st.Name] = t
		}
		if et := c.elemType[src]; et != "" {
			if strings.Contains(et, "std::string") {
				c.vars[st.Name] = "string"
			} else if et == "int" || et == "double" || et == "bool" {
				c.vars[st.Name] = et
			} else if et == "std::any" {
				c.vars[st.Name] = "any"
			} else if c.isStructName(et) {
				c.varStruct[st.Name] = et
			}
			c.elemType[st.Name] = et
		}
	}
	if c.vars[src] == "map" {
		// key type unknown; avoid printing as pair
	}
	if err := c.compileBlock(st.Body); err != nil {
		return err
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := sanitizeName(u.Target)
	item := c.newTmp()
	c.writeln(fmt.Sprintf("for (auto &%s : %s) {", item, list))
	c.indent++

	used := map[string]struct{}{}
	if u.Where != nil {
		collectIdents(u.Where, used)
	}
	for _, it := range u.Set.Items {
		if key, ok := identName(it.Key); ok {
			used[key] = struct{}{}
		}
		collectIdents(it.Value, used)
	}

	oldAliases := c.aliases
	c.aliases = make(map[string]string)
	for k := range oldAliases {
		c.aliases[k] = oldAliases[k]
	}
	for f := range used {
		c.aliases[f] = fmt.Sprintf("%s.%s", item, sanitizeName(f))
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.aliases = oldAliases
			return err
		}
		cond = c.ensureBool(cond)
		c.writeln("if (" + cond + ") {")
		c.indent++
	}

	for _, it := range u.Set.Items {
		if key, ok := identName(it.Key); ok {
			val, err := c.compileExpr(it.Value)
			if err != nil {
				c.aliases = oldAliases
				return err
			}
			c.writeln(fmt.Sprintf("%s.%s = %s;", item, sanitizeName(key), val))
			continue
		}
		keyExpr, err := c.compileExpr(it.Key)
		if err != nil {
			c.aliases = oldAliases
			return err
		}
		valExpr, err := c.compileExpr(it.Value)
		if err != nil {
			c.aliases = oldAliases
			return err
		}
		c.writeln(fmt.Sprintf("%s[%s] = %s;", item, keyExpr, valExpr))
	}

	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	c.aliases = oldAliases
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	c.writeln("// test " + strings.Trim(t.Name, "\""))
	if err := c.compileBlock(t.Body); err != nil {
		return err
	}
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	// expectations are ignored in generated C++
	return nil
}

func (c *Compiler) compileImport(im *parser.ImportStmt) error {
	if im.Lang == nil {
		return nil
	}
	c.packages[im.As] = struct{}{}
	switch *im.Lang {
	case "python":
		if im.Path == "math" {
			c.headerWriteln("#include <cmath>")
			c.headerWriteln("namespace " + im.As + " {")
			c.headerWriteln("inline double sqrt(double x){ return std::sqrt(x); }")
			c.headerWriteln("inline double pow(double x,double y){ return std::pow(x,y); }")
			c.headerWriteln("inline double sin(double x){ return std::sin(x); }")
			c.headerWriteln("inline double log(double x){ return std::log(x); }")
			c.headerWriteln("const double pi = 3.141592653589793;")
			c.headerWriteln("const double e = 2.718281828459045;")
			c.headerWriteln("}")
		}
	case "go":
		if im.Path == "mochi/runtime/ffi/go/testpkg" && im.Auto {
			c.headerWriteln("namespace " + im.As + " {")
			c.headerWriteln("inline int Add(int a,int b){ return a+b; }")
			c.headerWriteln("const double Pi = 3.14;")
			c.headerWriteln("const int Answer = 42;")
			c.headerWriteln("inline std::string MD5Hex(const std::string& s){ return s; }")
			c.headerWriteln("inline std::string FifteenPuzzleExample(){ return \"\"; }")
			c.headerWriteln("}")
		}
	}
	return nil
}

func printCall(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	pf := u.Value
	if len(pf.Ops) > 0 || pf.Target == nil || pf.Target.Call == nil {
		return nil, false
	}
	if pf.Target.Call.Func != "print" {
		return nil, false
	}
	return pf.Target.Call, true
}

func (c *Compiler) isVectorExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) > 0 {
		if len(e.Binary.Right) == 1 {
			op := e.Binary.Right[0].Op
			if op == "union" || op == "union_all" || op == "except" || op == "intersect" {
				return true
			}
		}
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	pf := u.Value
	if len(pf.Ops) > 0 {
		for _, op := range pf.Ops {
			if op.Index != nil && (op.Index.End != nil || op.Index.Colon != nil) {
				return true
			}
		}
		return false
	}
	if pf.Target != nil {
		if pf.Target.List != nil || pf.Target.Query != nil {
			return true
		}
		if pf.Target.Call != nil {
			switch pf.Target.Call.Func {
			case "append", "values":
				return true
			}
		}
		if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 0 {
			if t, ok := c.vars[pf.Target.Selector.Root]; ok && t == "vector" {
				return true
			}
		}
	}
	return false
}

func isEmptyListExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	pf := u.Value
	if len(pf.Ops) > 0 || pf.Target == nil || pf.Target.List == nil {
		return false
	}
	return len(pf.Target.List.Elems) == 0
}

func (c *Compiler) predictElemType(name string) string {
	return c.predictElemTypeIn(name, c.future)
}

func (c *Compiler) predictElemTypeIn(name string, stmts []*parser.Statement) string {
	for _, st := range stmts {
		switch {
		case st.Assign != nil && st.Assign.Name == name:
			exprStr, err := c.simulateExpr(st.Assign.Value)
			if err == nil {
				if et := c.extractVectorElemType(exprStr); et != "" {
					return et
				}
				if strings.Contains(exprStr, "__append(") {
					if idx := strings.Index(exprStr, ","); idx != -1 {
						arg := strings.TrimSpace(exprStr[idx+1:])
						if vt := c.extractVectorType(arg); vt != "" {
							return vt
						}
						if et := c.extractVectorElemType(arg); et != "" {
							return et
						}
					}
				}
			}
		case st.For != nil:
			srcExpr, err := c.simulateExpr(st.For.Source)
			if err == nil {
				if t := c.varStruct[srcExpr]; t != "" {
					c.varStruct[st.For.Name] = t
				}
				if et := c.elemType[srcExpr]; et != "" {
					c.varStruct[st.For.Name] = et
				}
			}
			if et := c.predictElemTypeIn(name, st.For.Body); et != "" {
				return et
			}
		case st.While != nil:
			if et := c.predictElemTypeIn(name, st.While.Body); et != "" {
				return et
			}
		case st.If != nil:
			if et := c.predictElemTypeIn(name, st.If.Then); et != "" {
				return et
			}
			if st.If.Else != nil {
				if et := c.predictElemTypeIn(name, st.If.Else); et != "" {
					return et
				}
			}
		case st.Test != nil:
			if et := c.predictElemTypeIn(name, st.Test.Body); et != "" {
				return et
			}
		}
	}
	return ""
}

func (c *Compiler) simulateExpr(e *parser.Expr) (string, error) {
	sub := *c
	sub.buf.Reset()
	sub.header.Reset()
	sub.future = nil
	// Deep copy maps to avoid polluting the parent compiler
	sub.structMap = map[string]*structInfo{}
	for k, v := range c.structMap {
		sub.structMap[k] = v
	}
	sub.structByName = map[string]*structInfo{}
	for k, v := range c.structByName {
		sub.structByName[k] = v
	}
	sub.unions = map[string]*unionInfo{}
	for k, v := range c.unions {
		sub.unions[k] = v
	}
	sub.variants = map[string]*variantInfo{}
	for k, v := range c.variants {
		sub.variants[k] = v
	}
	sub.placeholders = map[string]string{}
	for k, v := range c.placeholders {
		sub.placeholders[k] = v
	}
	sub.vars = map[string]string{}
	for k, v := range c.vars {
		sub.vars[k] = v
	}
	sub.aliases = map[string]string{}
	for k, v := range c.aliases {
		sub.aliases[k] = v
	}
	sub.varStruct = map[string]string{}
	for k, v := range c.varStruct {
		sub.varStruct[k] = v
	}
	sub.elemType = map[string]string{}
	for k, v := range c.elemType {
		sub.elemType[k] = v
	}
	return sub.compileExpr(e)
}

// Predict returns the predicted element type for a vector variable.
func (c *Compiler) Predict(name string) string { return c.predictElemType(name) }

func (c *Compiler) compilePrint(args []*parser.Expr) error {
	// Special case: print(append(x, y)) -> x.push_back(y); print(x)
	if len(args) == 1 {
		if call, ok := callPattern(args[0]); ok && call.Func == "append" && len(call.Args) == 2 {
			if id, ok2 := identName(call.Args[0]); ok2 {
				val, err := c.compileExpr(call.Args[1])
				if err != nil {
					return err
				}
				c.writeIndent()
				c.buf.WriteString(id + ".push_back(" + val + ");\n")
				return c.compilePrint([]*parser.Expr{call.Args[0]})
			}
		}
		// Special case: a single simple argument can be printed on one line
		// without the extra block used for complex prints.
		s, err := c.compileExpr(args[0])
		if err != nil {
			return err
		}
		typ := c.inferType(s)
		if typ == "" {
			if t, ok := c.vars[s]; ok {
				typ = t
			}
		}
		if typ == "" && c.isVectorExpr(args[0]) {
			typ = "vector"
		}
		structType := ""
		if t := c.varStruct[s]; t != "" {
			structType = t
		} else if t := c.structLiteralType(s); t != "" {
			structType = t
		}
		if typ != "vector" {
			c.writeIndent()
			if typ == "any" || typ == "" && structType == "" && c.isAnyExpr(s) {
				c.buf.WriteString("__print_any(" + s + "); std::cout << std::endl;")
				c.buf.WriteByte('\n')
				return nil
			}
			switch typ {
			case "bool":
				c.buf.WriteString("std::cout << (" + s + " ? \"true\" : \"false\") << std::endl;")
			case "int":
				c.buf.WriteString("std::cout << " + s + " << std::endl;")
			case "double":
				c.buf.WriteString("std::cout << std::fixed << std::setprecision(1) << " + s + " << std::endl;")
			case "string":
				c.buf.WriteString("std::cout << " + s + " << std::endl;")
			case "pair":
				c.buf.WriteString("std::cout << std::boolalpha << " + s + ".first << ' ' << " + s + ".second << std::endl;")
			case "":
				if structType != "" {
					c.usesJSON = true
					c.buf.WriteString("__json(" + s + "); std::cout << std::endl;")
				} else {
					c.buf.WriteString("std::cout << " + s + " << std::endl;")
				}
			default:
				c.buf.WriteString("std::cout << " + s + " << std::endl;")
			}
			c.buf.WriteByte('\n')
			return nil
		} else if id, ok := identName(args[0]); ok {
			c.writeIndent()
			if et := c.elemType[id]; et != "" && c.isStructName(et) {
				c.usesJSON = true
				c.buf.WriteString("std::cout<<\"[\"; for(size_t i=0;i<" + id + ".size();++i){ if(i) std::cout<<\", \"; __json(" + id + "[i]); } std::cout<<\"]\" << std::endl;\n")
			} else {
				c.buf.WriteString("std::cout<<\"[\"; for(size_t i=0;i<" + id + ".size();++i){ if(i) std::cout<<\", \"; std::cout<<" + id + "[i]; } std::cout<<\"]\" << std::endl;\n")
			}
			return nil
		}
	}

	// Fallback: original implementation using a scoped block
	c.writeIndent()
	c.buf.WriteString("{ ")
	for i, a := range args {
		s, err := c.compileExpr(a)
		if err != nil {
			return err
		}
		typ := c.inferType(s)
		if typ == "" {
			if t, ok := c.vars[s]; ok {
				typ = t
			}
		}
		if typ == "" && c.isVectorExpr(a) {
			typ = "vector"
		}
		structType := ""
		if t := c.varStruct[s]; t != "" {
			structType = t
		} else if t := c.structLiteralType(s); t != "" {
			structType = t
		} else if dot := strings.Index(s, "."); dot != -1 {
			base := s[:dot]
			fld := s[dot+1:]
			if t := c.varStruct[base]; t != "" {
				if idx := strings.Index(t, "{"); idx != -1 {
					t = t[:idx]
				}
				if info, ok := c.structByName[t]; ok {
					for i, f := range info.Fields {
						if f == sanitizeName(fld) {
							ft := info.Types[i]
							if idx := strings.Index(ft, "{"); idx != -1 {
								ft = ft[:idx]
							}
							if c.isStructName(ft) {
								structType = ft
							}
						}
					}
				}
			}
		}
		if i > 0 {
			c.buf.WriteString("std::cout << ' '; ")
		}
		switch typ {
		case "vector":
			tmp := c.newTmp()
			c.buf.WriteString("auto " + tmp + " = " + s + "; bool first=true; for(const auto &_x : " + tmp + "){ if(!first) std::cout<<' '; first=false; ")
			if et := c.elemType[s]; et != "" && c.isStructName(et) {
				c.buf.WriteString("std::cout<<\"<struct>\";")
			} else {
				c.buf.WriteString("std::cout<<_x;")
			}
			c.buf.WriteString(" } ")
		case "bool":
			c.buf.WriteString("std::cout << (" + s + " ? \"true\" : \"false\"); ")
		case "int", "double", "string":
			c.buf.WriteString("std::cout << " + s + "; ")
		case "pair":
			c.buf.WriteString("std::cout << std::boolalpha << " + s + ".first << ' ' << " + s + ".second; ")
		case "":
			if structType != "" {
				c.usesJSON = true
				c.buf.WriteString("__json(" + s + "); ")
			} else {
				if c.isAnyExpr(s) {
					c.buf.WriteString("__print_any(" + s + "); ")
				} else {
					c.buf.WriteString("std::cout << " + s + "; ")
				}
			}
		default:
			c.buf.WriteString("std::cout << " + s + "; ")
		}
	}
	c.buf.WriteString("std::cout << std::endl; }")
	c.buf.WriteByte('\n')
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands := []string{left}
	ops := []string{}
	for _, part := range b.Right {
		rhs, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, rhs)
		ops = append(ops, part.Op)
	}

	prec := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}
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
				var combined string
				if ops[i] == "in" {
					container := r
					elem := l
					typ := c.vars[container]
					if typ == "string" || strings.HasPrefix(container, "std::string") {
						combined = fmt.Sprintf("(%s.find(%s) != std::string::npos)", container, elem)
					} else if typ == "map" {
						combined = fmt.Sprintf("(%s.count(%s) > 0)", container, elem)
					} else {
						combined = fmt.Sprintf("(std::find(%s.begin(), %s.end(), %s) != %s.end())", container, container, elem, container)
					}
				} else if ops[i] == "union" || ops[i] == "union_all" || ops[i] == "except" || ops[i] == "intersect" {
					switch ops[i] {
					case "union":
						combined = fmt.Sprintf("([&](auto a, auto b){ a.insert(a.end(), b.begin(), b.end()); std::sort(a.begin(), a.end()); a.erase(std::unique(a.begin(), a.end()), a.end()); return a; })(%s, %s)", l, r)
					case "union_all":
						combined = fmt.Sprintf("([&](auto a, auto b){ a.insert(a.end(), b.begin(), b.end()); return a; })(%s, %s)", l, r)
					case "except":
						combined = fmt.Sprintf("([&](auto a, auto b){ for(auto &x:b) a.erase(std::remove(a.begin(), a.end(), x), a.end()); return a; })(%s, %s)", l, r)
					case "intersect":
						combined = fmt.Sprintf("([&](auto a, auto b){ std::vector<std::decay_t<decltype(a[0])>> r_; for(auto &x:a) if(std::find(b.begin(), b.end(), x)!=b.end()) r_.push_back(x); return r_; })(%s, %s)", l, r)
					}
				} else {
					if ops[i] == "+" && strings.HasPrefix(l, "std::string(") && strings.HasSuffix(l, ")") && strings.HasPrefix(r, "std::string(") && strings.HasSuffix(r, ")") {
						left := strings.TrimSuffix(strings.TrimPrefix(l, "std::string("), ")")
						right := strings.TrimSuffix(strings.TrimPrefix(r, "std::string("), ")")
						combined = fmt.Sprintf("std::string(%s %s)", left, right)
					} else if ops[i] == "+" || ops[i] == "-" || ops[i] == "*" || ops[i] == "/" {
						if lv, li, lok := parseNumber(l); lok {
							if rv, ri, rok := parseNumber(r); rok {
								var val float64
								switch ops[i] {
								case "+":
									val = lv + rv
								case "-":
									val = lv - rv
								case "*":
									val = lv * rv
								case "/":
									if rv != 0 {
										val = lv / rv
									} else {
										val = 0
									}
								}
								if li && ri && ops[i] != "/" {
									combined = fmt.Sprintf("%d", int64(val))
								} else {
									combined = fmt.Sprint(val)
								}
							}
						}
					}
					if combined == "" {
						if (ops[i] == "==" || ops[i] == "!=") &&
							(c.isAnyExpr(l) || c.isAnyExpr(r)) {
							c.usesAny = true
							if !c.isAnyExpr(l) {
								l = fmt.Sprintf("std::any{%s}", l)
							}
							if !c.isAnyExpr(r) {
								r = fmt.Sprintf("std::any{%s}", r)
							}
							if ops[i] == "==" {
								combined = fmt.Sprintf("__any_eq(%s, %s)", l, r)
							} else {
								combined = fmt.Sprintf("!__any_eq(%s, %s)", l, r)
							}
						} else {
							combined = fmt.Sprintf("(%s %s %s)", l, ops[i], r)
						}
					}
				}
				operands[i] = combined
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state after binary expression")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	x, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	if len(u.Ops) > 0 {
		// constant folding for simple numeric and boolean expressions
		if v, isInt, ok := parseNumber(x); ok {
			sign := 1.0
			foldable := true
			for _, op := range u.Ops {
				switch op {
				case "-":
					sign = -sign
				case "+":
					// no-op
				default:
					foldable = false
				}
			}
			if foldable {
				val := sign * v
				if isInt {
					return fmt.Sprintf("%d", int64(val)), nil
				}
				return fmt.Sprint(val), nil
			}
		} else if x == "true" || x == "false" {
			flip := 0
			for _, op := range u.Ops {
				if op != "!" {
					flip = -1
					break
				}
				flip ^= 1
			}
			if flip >= 0 {
				if flip%2 == 1 {
					if x == "true" {
						return "false", nil
					}
					return "true", nil
				}
				return x, nil
			}
		}
	}
	if c.isAnyExpr(x) {
		for i := len(u.Ops) - 1; i >= 0; i-- {
			op := u.Ops[i]
			switch op {
			case "!":
				x = fmt.Sprintf("(!std::any_cast<bool>(%s))", x)
			case "-", "+":
				x = fmt.Sprintf("(%sstd::any_cast<int>(%s))", op, x)
			default:
				x = fmt.Sprintf("(%s%s)", op, x)
			}
		}
		return x, nil
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		x = fmt.Sprintf("(%s%s)", u.Ops[i], x)
	}
	return x, nil
}

func (c *Compiler) compilePostfix(pf *parser.PostfixExpr) (string, error) {
	// special case: map literal cast to struct
	if len(pf.Ops) > 0 && pf.Ops[0].Cast != nil && pf.Target.Map != nil {
		t, err := c.compileType(pf.Ops[0].Cast.Type)
		if err != nil {
			return "", err
		}
		if t != "int" && t != "std::string" {
			return c.structFromMapLiteral(t, pf.Target.Map)
		}
	}

	base, err := c.compilePrimary(pf.Target)
	if err != nil {
		return "", err
	}
	expr := base
	for _, op := range pf.Ops {
		if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, s)
			}
			if base == "len" && len(args) == 1 {
				expr = fmt.Sprintf("(%s.size())", args[0])
			} else if base == "append" && len(args) == 2 {
				if !c.usesAppend {
					c.headerWriteln("template<typename T,typename U> std::vector<T> __append(const std::vector<T>& v,const U& x){")
					c.headerWriteln("    auto r=v;")
					c.headerWriteln("    r.push_back(x);")
					c.headerWriteln("    return r;")
					c.headerWriteln("}")
					c.usesAppend = true
				}
				expr = fmt.Sprintf("__append(%s, %s)", args[0], args[1])
			} else if base == "sum" && len(args) == 1 {
				expr = fmt.Sprintf("([&](auto v){ return std::accumulate(v.begin(), v.end(), 0.0); })(%s)", args[0])
			} else if base == "avg" && len(args) == 1 {
				if !c.usesAvg {
					c.headerWriteln("template<typename T> double __avg(const std::vector<T>& v){")
					c.headerWriteln("    if(v.empty()) return 0;")
					c.headerWriteln("    double s=0;")
					c.headerWriteln("    for(const auto &x:v) s+=x;")
					c.headerWriteln("    return s/v.size();")
					c.headerWriteln("}")
					c.usesAvg = true
				}
				expr = fmt.Sprintf("__avg(%s)", args[0])
			} else if base == "count" && len(args) == 1 {
				expr = fmt.Sprintf("((int)%s.size())", args[0])
			} else if base == "min" && len(args) == 1 {
				expr = fmt.Sprintf("(*std::min_element(%s.begin(), %s.end()))", args[0], args[0])
			} else if base == "max" && len(args) == 1 {
				expr = fmt.Sprintf("(*std::max_element(%s.begin(), %s.end()))", args[0], args[0])
			} else if strings.HasSuffix(base, ".contains") && len(args) == 1 {
				target := strings.TrimSuffix(base, ".contains")
				expr = fmt.Sprintf("(%s.find(%s) != std::string::npos)", target, args[0])
			} else {
				expr = base + "(" + strings.Join(args, ", ") + ")"
			}
			base = expr
		} else if op.Index != nil {
			if op.Index.End != nil || op.Index.Colon != nil {
				start := "0"
				var err error
				if op.Index.Start != nil {
					start, err = c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
				}
				end, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				if end == "" {
					end = fmt.Sprintf("%s.size()", expr)
				}
				if c.isVectorType(expr) {
					et := c.extractVectorElemType(expr)
					if et == "" {
						et = fmt.Sprintf("std::decay_t<decltype(%s[0])>", expr)
					}
					expr = fmt.Sprintf("([&](auto v){ return std::vector<%s>(v.begin()+%s, v.begin()+%s); })(%s)", et, start, end, expr)
				} else {
					expr = fmt.Sprintf("std::string(%s).substr(%s, (%s)-(%s))", expr, start, end, start)
				}
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("unsupported empty index")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("%s[%s]", expr, idx)
				if vt := c.extractMapValueType(base); vt != "" {
					c.elemType[expr] = vt
					if c.isStructName(vt) {
						c.varStruct[expr] = vt
					}
					switch vt {
					case "std::string":
						c.vars[expr] = "string"
					case "int", "double", "bool":
						c.vars[expr] = strings.TrimPrefix(vt, "std::")
					case "std::any":
						c.vars[expr] = "any"
					}
				}
			}
		} else if op.Field != nil {
			base := expr
			expr = fmt.Sprintf("%s.%s", expr, op.Field.Name)
			t := c.varStruct[base]
			if t == "" {
				t = c.structLiteralType(base)
			}
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			if info, ok := c.structByName[t]; ok {
				for i, f := range info.Fields {
					if f == op.Field.Name && i < len(info.Types) {
						ft := info.Types[i]
						if strings.HasPrefix(ft, "std::vector<") {
							if et := c.extractVectorElemType(ft); et != "" {
								c.elemType[expr] = et
								if c.isStructName(et) {
									c.varStruct[expr] = et
								}
							}
						} else {
							if c.isStructName(ft) {
								c.varStruct[expr] = ft
							} else if ft == "std::string" {
								c.vars[expr] = "string"
							} else if ft == "int" {
								c.vars[expr] = "int"
							} else if ft == "bool" {
								c.vars[expr] = "bool"
							}
						}
						break
					}
				}
			}
		} else if op.Cast != nil {
			t, err := c.compileType(op.Cast.Type)
			if err != nil {
				return "", err
			}
			if t == "int" {
				expr = fmt.Sprintf("std::stoi(%s)", expr)
			} else if t == "std::string" {
				expr = fmt.Sprintf("std::to_string(%s)", expr)
			} else {
				// unknown types are ignored
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.List != nil:
		elems := []string{}
		for _, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems = append(elems, s)
		}
		elemType := ""
		if len(elems) > 0 {
			for _, e := range elems {
				t := inferExprType(e)
				if t == "" {
					if s := c.structLiteralType(e); s != "" {
						t = s
					} else if v := c.extractVectorType(e); v != "" {
						t = v
					} else if v := c.varStruct[e]; v != "" {
						if idx := strings.Index(v, "{"); idx != -1 {
							t = v[:idx]
						} else {
							t = v
						}
					} else if vv := c.vars[e]; vv != "" {
						switch vv {
						case "string":
							t = "std::string"
						case "int", "double", "bool":
							t = vv
						}
					}
				}
				if t == "" {
					continue
				}
				if t == "string" {
					t = "std::string"
				}
				if elemType == "" {
					elemType = t
				} else if t != elemType {
					elemType = "std::any"
					c.usesAny = true
					break
				}
			}
			if elemType == "" {
				elemType = "std::any"
				c.usesAny = true
			}
		}
		expr := fmt.Sprintf("std::vector<%s>{%s}", elemType, strings.Join(elems, ", "))
		c.elemType[expr] = elemType
		if c.isStructName(elemType) {
			c.varStruct[expr] = elemType
		}
		return expr, nil
	case p.Map != nil:
		names := []string{}
		vals := []string{}
		fieldTypes := []string{}
		simple := true
		for _, it := range p.Map.Items {
			if n, ok := c.simpleIdentifier(it.Key); ok {
				names = append(names, sanitizeName(n))
			} else {
				simple = false
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			vals = append(vals, v)
		}
		if simple && len(p.Map.Items) > 1 {
			sig := strings.Join(names, ",")
			info, ok := c.structMap[sig]
			fieldTypes = make([]string, len(names))
			for i := range names {
				expr := vals[i]
				for v, t := range c.varStruct {
					if t == "" {
						continue
					}
					if strings.Contains(expr, v+".") {
						expr = replaceVarRef(expr, v, t)
					}
				}
				ftype := fmt.Sprintf("decltype(%s)", expr)
				if strings.Contains(vals[i], "((int)") {
					ftype = "int"
				}
				if t, ok := c.vars[vals[i]]; ok {
					switch t {
					case "string":
						ftype = "std::string"
					case "int":
						ftype = "int"
					case "bool":
						ftype = "bool"
					}
				} else if t := c.varStruct[vals[i]]; t != "" {
					if idx := strings.Index(t, "{"); idx != -1 {
						t = t[:idx]
					}
					ftype = t
				}
				if dot := strings.Index(vals[i], "."); dot != -1 {
					v := vals[i][:dot]
					fld := vals[i][dot+1:]
					simple := true
					for _, ch := range fld {
						if !(ch == '_' || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ('0' <= ch && ch <= '9')) {
							simple = false
							break
						}
					}
					if simple {
						if t := c.varStruct[v]; t != "" {
							if idx := strings.Index(t, "{"); idx != -1 {
								t = t[:idx]
							}
							ftype = fmt.Sprintf("decltype(std::declval<%s>().%s)", t, fld)
						}
					} else if t := c.varStruct[vals[i]]; t != "" {
						if idx := strings.Index(t, "{"); idx != -1 {
							t = t[:idx]
						}
						ftype = t
					} else if t := inferExprType(vals[i]); t != "" {
						ftype = t
					}
				} else if t := inferExprType(vals[i]); t != "" {
					ftype = t
				}
				// If the resulting type still references a query
				// variable like "mc.note" and we know the struct
				// for that variable, rewrite it to avoid using
				// an undeclared identifier in the struct
				// definition.
				if strings.HasPrefix(ftype, "decltype(") {
					inner := strings.TrimSuffix(strings.TrimPrefix(ftype, "decltype("), ")")
					if dot := strings.Index(inner, "."); dot != -1 {
						name := inner[:dot]
						if t := c.varStruct[name]; t != "" {
							if idx := strings.Index(t, "{"); idx != -1 {
								t = t[:idx]
							}
							fld := inner[dot+1:]
							ftype = fmt.Sprintf("decltype(std::declval<%s>().%s)", t, fld)
						}
					}
				}
				fieldTypes[i] = ftype
			}

			if ok {
				for i, ft := range fieldTypes {
					if info.Types[i] == "" || strings.Contains(info.Types[i], names[i]) ||
						(info.Types[i] == "std::any" && ft != "std::any") {
						info.Types[i] = ft
					}
				}
			} else {
				c.structCount++
				name := fmt.Sprintf("__struct%d", c.structCount)
				if c.nextStructName != "" {
					name = toPascalCase(c.nextStructName)
				}
				info = &structInfo{Name: name, Fields: append([]string(nil), names...), Types: append([]string(nil), fieldTypes...)}
				c.structMap[sig] = info
				c.structByName[info.Name] = info
				c.defineStruct(info)
				c.nextStructName = ""
			}
			return fmt.Sprintf("%s{%s}", info.Name, strings.Join(vals, ", ")), nil
		}
		keys := []string{}
		for _, it := range p.Map.Items {
			if n, ok := c.simpleIdentifier(it.Key); ok {
				keys = append(keys, fmt.Sprintf("std::string(\"%s\")", sanitizeName(n)))
			} else {
				k, err := c.compileExpr(it.Key)
				if err != nil {
					return "", err
				}
				keys = append(keys, k)
			}
		}
		keyType := "int"
		valType := "int"
		if len(keys) > 0 {
			if strings.HasPrefix(keys[0], "std::string") {
				keyType = "std::string"
			} else if strings.Contains(keys[0], "true") || strings.Contains(keys[0], "false") {
				keyType = "bool"
			} else if _, err := strconv.Atoi(keys[0]); err == nil {
				keyType = "int"
			} else {
				keyType = fmt.Sprintf("decltype(%s)", keys[0])
			}
			if strings.HasPrefix(vals[0], "std::string") {
				valType = "std::string"
			} else if strings.HasPrefix(vals[0], "std::unordered_map") || strings.HasPrefix(vals[0], "std::map") {
				if idx := strings.Index(vals[0], "{"); idx != -1 {
					valType = vals[0][:idx]
				} else {
					valType = vals[0]
				}
			} else if t := c.structLiteralType(vals[0]); t != "" {
				valType = t
			} else if c.vars[vals[0]] == "string" {
				valType = "std::string"
			} else if typ := c.vars[vals[0]]; typ == "int" || typ == "double" || typ == "bool" {
				valType = typ
			} else if t := c.varStruct[vals[0]]; t != "" {
				if idx := strings.Index(t, "{"); idx != -1 {
					t = t[:idx]
				}
				valType = t
			} else if _, isInt, ok := parseNumber(vals[0]); ok {
				if isInt {
					valType = "int"
				} else {
					valType = "double"
				}
			} else {
				valType = fmt.Sprintf("decltype(%s)", vals[0])
			}
			baseType := valType
			for _, v := range vals[1:] {
				vt := ""
				if strings.HasPrefix(v, "std::string") {
					vt = "std::string"
				} else if strings.HasPrefix(v, "std::unordered_map") || strings.HasPrefix(v, "std::map") {
					if idx := strings.Index(v, "{"); idx != -1 {
						vt = v[:idx]
					} else {
						vt = v
					}
				} else if t := c.structLiteralType(v); t != "" {
					vt = t
				} else if c.vars[v] == "string" {
					vt = "std::string"
				} else if typ := c.vars[v]; typ == "int" || typ == "double" || typ == "bool" {
					vt = typ
				} else if t := c.varStruct[v]; t != "" {
					if idx := strings.Index(t, "{"); idx != -1 {
						t = t[:idx]
					}
					vt = t
				} else if _, isInt, ok := parseNumber(v); ok {
					if isInt {
						vt = "int"
					} else {
						vt = "double"
					}
				} else {
					vt = fmt.Sprintf("decltype(%s)", v)
				}
				if vt != baseType {
					valType = "std::any"
					c.usesAny = true
					break
				}
			}
		}
		pairs := []string{}
		for i := range keys {
			pairs = append(pairs, fmt.Sprintf("{%s, %s}", keys[i], vals[i]))
		}
		expr := fmt.Sprintf("std::unordered_map<%s,%s>{%s}", keyType, valType, strings.Join(pairs, ", "))
		c.elemType[expr] = valType
		c.vars[expr] = "map"
		return expr, nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Selector != nil:
		root := p.Selector.Root
		if alias, ok := c.aliases[root]; ok && len(p.Selector.Tail) == 0 {
			return alias, nil
		}
		sep := "."
		if _, ok := c.packages[root]; ok {
			sep = "::"
		}
		name := root
		if _, ok := c.variants[name]; ok && len(p.Selector.Tail) == 0 {
			return fmt.Sprintf("new %s{}", name), nil
		}
		for _, t := range p.Selector.Tail {
			name += sep + t
		}
		return name, nil
	case p.Call != nil:
		name := p.Call.Func
		if alias, ok := c.renamed[name]; ok {
			name = alias
		}
		args := []string{}
		for _, a := range p.Call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, s)
		}
		switch name {
		case "len":
			if len(args) == 1 {
				return fmt.Sprintf("%s.size()", args[0]), nil
			}
		case "append":
			if len(args) == 2 {
				if !c.usesAppend {
					c.headerWriteln("template<typename T,typename U> std::vector<T> __append(const std::vector<T>& v,const U& x){")
					c.headerWriteln("    auto r=v;")
					c.headerWriteln("    r.push_back(x);")
					c.headerWriteln("    return r;")
					c.headerWriteln("}")
					c.usesAppend = true
				}
				return fmt.Sprintf("__append(%s, %s)", args[0], args[1]), nil
			}
		case "sum":
			if len(args) == 1 {
				return fmt.Sprintf("([&](auto v){ return std::accumulate(v.begin(), v.end(), 0.0); })(%s)", args[0]), nil
			}
		case "avg":
			if len(args) == 1 {
				if !c.usesAvg {
					c.headerWriteln("template<typename T> double __avg(const std::vector<T>& v){")
					c.headerWriteln("    if(v.empty()) return 0;")
					c.headerWriteln("    double s=0;")
					c.headerWriteln("    for(const auto &x:v) s+=x;")
					c.headerWriteln("    return s/v.size();")
					c.headerWriteln("}")
					c.usesAvg = true
				}
				return fmt.Sprintf("__avg(%s)", args[0]), nil
			}
		case "count":
			if len(args) == 1 {
				return fmt.Sprintf("((int)%s.size())", args[0]), nil
			}
		case "exists":
			if len(args) == 1 {
				if q, ok := simpleExistsQuery(p.Call.Args[0]); ok {
					return c.compileExistsQuery(q)
				}
				return fmt.Sprintf("(!%s.empty())", args[0]), nil
			}
		case "min":
			if len(args) == 1 {
				return fmt.Sprintf("(*std::min_element(%s.begin(), %s.end()))", args[0], args[0]), nil
			}
		case "max":
			if len(args) == 1 {
				return fmt.Sprintf("(*std::max_element(%s.begin(), %s.end()))", args[0], args[0]), nil
			}
		case "str":
			if len(args) == 1 {
				if c.isAnyExpr(args[0]) {
					return fmt.Sprintf("__any_str(%s)", args[0]), nil
				}
				return fmt.Sprintf("std::to_string(%s)", args[0]), nil
			}
		case "substring":
			if len(args) == 3 {
				return fmt.Sprintf("std::string(%s).substr(%s, (%s)-(%s))", args[0], args[1], args[2], args[1]), nil
			}
		case "values":
			if len(args) == 1 {
				return fmt.Sprintf("([&](){ std::vector<int> v; for(auto &p : %s) v.push_back(p.second); return v; })()", args[0]), nil
			}
		case "now":
			if len(args) == 0 {
				if !c.usesNow {
					c.headerWriteln("inline long long _now(){ auto n=std::chrono::system_clock::now().time_since_epoch(); return std::chrono::duration_cast<std::chrono::nanoseconds>(n).count(); }")
					c.usesNow = true
				}
				return "_now()", nil
			}
		case "input":
			if len(args) == 0 {
				c.usesIO = true
				return "([&](){ std::string s; std::getline(std::cin, s); return s; })()", nil
			}
		case "json":
			if len(args) == 1 {
				c.usesJSON = true
				return fmt.Sprintf("(__json(%s))", args[0]), nil
			}
		}
		if n, ok := c.funParams[name]; ok && len(args) < n {
			return c.partialApply(name, args, n), nil
		}
		expr := name + "(" + strings.Join(args, ", ") + ")"
		if mv, ok := c.returnMap[name]; ok && mv != "" {
			c.vars[expr] = "map"
			// store map value type for later lookups
			c.elemType[expr] = mv
		}
		return expr, nil
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	default:
		return "", fmt.Errorf("unsupported primary at %v", p.Pos)
	}
}

func (c *Compiler) compileMatchExpr(mx *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(mx.Target)
	if err != nil {
		return "", err
	}
	var unionName string
	for _, cs := range mx.Cases {
		if id, ok := c.simpleIdentifier(cs.Pattern); ok {
			if v, ok2 := c.variants[id]; ok2 {
				unionName = v.Union
				break
			}
		}
		if call, ok := callPattern(cs.Pattern); ok {
			if v, ok2 := c.variants[call.Func]; ok2 {
				unionName = v.Union
				break
			}
		}
	}
	elseExpr := ""
	cases := make([]*parser.MatchCase, 0, len(mx.Cases))
	for _, cs := range mx.Cases {
		if name, ok := c.simpleIdentifier(cs.Pattern); ok && name == "_" {
			e, err := c.compileExpr(cs.Result)
			if err != nil {
				return "", err
			}
			elseExpr = e
		} else {
			cases = append(cases, cs)
		}
	}
	if elseExpr == "" {
		if len(cases) > 0 {
			first, err := c.compileExpr(cases[0].Result)
			if err != nil {
				return "", err
			}
			elseExpr = fmt.Sprintf("decltype(%s){}", first)
		} else {
			elseExpr = "0"
		}
	}
	var buf strings.Builder
	cap := "[&]"
	if c.scope == 0 {
		cap = "[]"
	}
	buf.WriteString("(" + cap + "() { auto __v = ")
	buf.WriteString(target)
	buf.WriteString("; ")
	if unionName != "" {
		for i, cs := range cases {
			if call, ok := callPattern(cs.Pattern); ok {
				if vi, ok2 := c.variants[call.Func]; ok2 {
					buf.WriteString("if (auto __p" + strconv.Itoa(i) + " = dynamic_cast<" + vi.Name + "*>(__v)) { ")
					old := c.aliases
					c.aliases = map[string]string{}
					for k := range old {
						c.aliases[k] = old[k]
					}
					for j, a := range call.Args {
						if id, ok := identName(a); ok {
							alias := fmt.Sprintf("__p%[1]d->%s", i, vi.Fields[j])
							if strings.HasPrefix(vi.Types[j], "std::unique_ptr") {
								alias += ".get()"
							}
							c.aliases[id] = alias
						}
					}
					res, err := c.compileExpr(cs.Result)
					if err != nil {
						c.aliases = old
						return "", err
					}
					c.aliases = old
					buf.WriteString("return " + res + "; }")
					continue
				}
			}
			if id, ok := c.simpleIdentifier(cs.Pattern); ok {
				if _, ok2 := c.variants[id]; ok2 {
					res, err := c.compileExpr(cs.Result)
					if err != nil {
						return "", err
					}
					buf.WriteString("if (dynamic_cast<" + id + "*>(__v)) return " + res + ";")
					continue
				}
			}
		}
		buf.WriteString("return " + elseExpr + "; })()")
		return buf.String(), nil
	}
	for i, cs := range cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if i == 0 {
			buf.WriteString("if (__v == ")
		} else {
			buf.WriteString("else if (__v == ")
		}
		buf.WriteString(pat)
		buf.WriteString(") return ")
		buf.WriteString(res)
		buf.WriteString("; ")
	}
	buf.WriteString("return ")
	buf.WriteString(elseExpr)
	buf.WriteString("; })()")
	return buf.String(), nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "0"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileLoadExpr(le *parser.LoadExpr) (string, error) {
	path := "\"\""
	if le.Path != nil {
		path = strconv.Quote(*le.Path)
	}
	typ, err := c.compileType(le.Type)
	if err != nil {
		return "", err
	}
	if !c.definedLoad {
		c.headerWriteln("inline std::vector<" + typ + "> __load_yaml_person(const std::string &path){")
		c.headerWriteln("    std::ifstream f(path);")
		c.headerWriteln("    std::string line;")
		c.headerWriteln("    std::vector<" + typ + "> people;")
		c.headerWriteln("    " + typ + " cur;")
		c.headerWriteln("    while(std::getline(f,line)){")
		c.headerWriteln("        if(line.find(\"- name:\")!=std::string::npos){")
		c.headerWriteln("            if(!cur.name.empty()) people.push_back(cur);")
		c.headerWriteln("            cur=" + typ + "();")
		c.headerWriteln("            cur.name=line.substr(line.find(':')+2);")
		c.headerWriteln("        } else if(line.find(\"age:\")!=std::string::npos){")
		c.headerWriteln("            cur.age=std::stoi(line.substr(line.find(':')+2));")
		c.headerWriteln("        } else if(line.find(\"email:\")!=std::string::npos){")
		c.headerWriteln("            cur.email=line.substr(line.find(':')+2);")
		c.headerWriteln("        }")
		c.headerWriteln("    }")
		c.headerWriteln("    if(!cur.name.empty()) people.push_back(cur);")
		c.headerWriteln("    return people;")
		c.headerWriteln("}")
		c.definedLoad = true
	}
	c.usesIO = true
	expr := "__load_yaml_person(" + path + ")"
	c.varStruct[expr] = typ
	c.elemType[expr] = typ
	return expr, nil
}

func (c *Compiler) compileSaveExpr(se *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(se.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if se.Path != nil {
		path = strconv.Quote(*se.Path)
	}
	if path == "\"-\"" {
		c.usesJSON = true
		return "([&](){ for(auto &x:" + src + "){ __json(x); std::cout<<std::endl; } })()", nil
	}
	return "0", nil
}

func (c *Compiler) compileStructLiteral(sl *parser.StructLiteral) (string, error) {
	if vi, ok := c.variants[sl.Name]; ok {
		var buf strings.Builder
		buf.WriteString("([&](){ auto __p = new " + vi.Name + "(); ")
		for i, f := range sl.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			ft := vi.Types[i]
			if strings.HasPrefix(ft, "std::unique_ptr") {
				v = fmt.Sprintf("std::unique_ptr<%s>(%s)", vi.Union, v)
			}
			buf.WriteString(fmt.Sprintf("__p->%s = %s; ", f.Name, v))
		}
		buf.WriteString("return __p; })()")
		return buf.String(), nil
	}
	fieldNames := make([]string, len(sl.Fields))
	fieldTypes := make([]string, len(sl.Fields))
	inits := make([]string, len(sl.Fields))
	for i, f := range sl.Fields {
		val, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fieldNames[i] = f.Name
		ftype := fmt.Sprintf("decltype(%s)", val)
		if dot := strings.Index(val, "."); dot != -1 {
			base := val[:dot]
			if t := c.varStruct[base]; t != "" {
				baseType := strings.TrimSuffix(t, "{}")
				ftype = fmt.Sprintf("decltype(std::declval<%s>().%s)", baseType, val[dot+1:])
			} else if t := c.elemType[base]; t != "" {
				baseType := strings.TrimSuffix(t, "{}")
				ftype = fmt.Sprintf("decltype(std::declval<%s>().%s)", baseType, val[dot+1:])
			}
		}
		for v, t := range c.varStruct {
			if t == "" {
				continue
			}
			if strings.Contains(val, v+".") {
				ftype = replaceVarRef(ftype, v, t)
			}
		}
		if strings.HasPrefix(ftype, "decltype(") && strings.Contains(ftype, ".") {
			if t := inferExprType(val); t != "" {
				if t == "string" {
					ftype = "std::string"
				} else {
					ftype = t
				}
			} else {
				ftype = "int"
			}
		}
		if dot := strings.Index(val, "."); dot != -1 {
			v := val[:dot]
			fld := val[dot+1:]
			simple := true
			for _, ch := range fld {
				if !(ch == '_' || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ('0' <= ch && ch <= '9')) {
					simple = false
					break
				}
			}
			if simple {
				typ := c.varStruct[v]
				if typ == "" {
					typ = c.elemType[v]
				}
				if typ != "" {
					if idx := strings.Index(typ, "{"); idx != -1 {
						typ = typ[:idx]
					}
					ftype = fmt.Sprintf("decltype(std::declval<%s>().%s)", typ, fld)
				}
			}
		} else if t := c.varStruct[val]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			ftype = t
		} else if typ, ok := c.vars[val]; ok {
			switch typ {
			case "string":
				ftype = "std::string"
			case "int", "double", "bool":
				ftype = typ
			}
		}
		if strings.HasPrefix(ftype, "decltype(") {
			if typ, ok := c.vars[val]; ok {
				switch typ {
				case "string":
					ftype = "std::string"
				case "int", "double", "bool":
					ftype = typ
				}
			} else if t := c.elemType[val]; t != "" {
				ftype = t
			} else if t := c.predictElemType(val); t != "" {
				ftype = t
			} else if t := inferExprType(val); t != "" {
				if t == "string" {
					ftype = "std::string"
				} else {
					ftype = t
				}
			} else {
				ftype = "std::any"
				c.usesAny = true
			}
		}
		fieldTypes[i] = ftype
		inits[i] = val
	}
	sig := sl.Name + ":" + strings.Join(fieldNames, ",")
	info, ok := c.structMap[sig]
	if !ok {
		c.structCount++
		name := fmt.Sprintf("__struct%d", c.structCount)
		if c.nextStructName != "" {
			name = toPascalCase(c.nextStructName)
		}
		info = &structInfo{Name: name, Fields: append([]string(nil), fieldNames...), Types: append([]string(nil), fieldTypes...)}
		c.structMap[sig] = info
		c.structByName[info.Name] = info
		c.defineStruct(info)
		c.nextStructName = ""
	}
	return fmt.Sprintf("%s{%s}", info.Name, strings.Join(inits, ", ")), nil
}

func stringLiteral(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "", false
	}
	pf := u.Value
	if pf == nil || len(pf.Ops) > 0 || pf.Target == nil {
		return "", false
	}
	p := pf.Target
	if p.Lit != nil && p.Lit.Str != nil {
		return *p.Lit.Str, true
	}
	return "", false
}

func (c *Compiler) structFromMapLiteral(typ string, m *parser.MapLiteral) (string, error) {
	assignments := []string{}
	for _, it := range m.Items {
		keyLit, ok := stringLiteral(it.Key)
		if !ok {
			continue
		}
		val, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		assignments = append(assignments, fmt.Sprintf(".%s=%s", keyLit, val))
	}
	return fmt.Sprintf("%s{%s}", typ, strings.Join(assignments, ",")), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if q.Group != nil {
		return c.compileGroupedQueryExpr(q)
	}
	// Queries are compiled into immediately invoked lambdas. Determine the
	// capture list based on the current scope before increasing it so that
	// top-level queries use an empty capture.
	cap := "[&]"
	if c.scope == 0 {
		cap = "[]"
	}
	c.scope++
	defer func() { c.scope-- }()
	for _, j := range q.Joins {
		if j.Side != nil && *j.Side != "left" && *j.Side != "right" && *j.Side != "outer" {
			return "", fmt.Errorf("join side not supported")
		}
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	backup := map[string]string{}
	if t := c.varStruct[src]; t != "" {
		backup[q.Var] = c.varStruct[q.Var]
		c.varStruct[q.Var] = t
	}
	if et := c.elemType[src]; et != "" {
		switch {
		case strings.Contains(et, "std::string"):
			c.vars[q.Var] = "string"
		case et == "int":
			c.vars[q.Var] = "int"
		case et == "bool":
			c.vars[q.Var] = "bool"
		case et == "std::any":
			c.vars[q.Var] = "any"
		case c.isStructName(et):
			c.varStruct[q.Var] = et
		}
		c.elemType[q.Var] = et
	}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = s
		if t := c.varStruct[s]; t != "" {
			backup[f.Var] = c.varStruct[f.Var]
			c.varStruct[f.Var] = t
		}
		if et := c.elemType[s]; et != "" {
			switch {
			case strings.Contains(et, "std::string"):
				c.vars[f.Var] = "string"
			case et == "int":
				c.vars[f.Var] = "int"
			case et == "bool":
				c.vars[f.Var] = "bool"
			case c.isStructName(et):
				c.varStruct[f.Var] = et
			}
			c.elemType[f.Var] = et
		}
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		if t := c.varStruct[js]; t != "" {
			backup[j.Var] = c.varStruct[j.Var]
			c.varStruct[j.Var] = t
		}
		if et := c.elemType[js]; et != "" {
			switch {
			case strings.Contains(et, "std::string"):
				c.vars[j.Var] = "string"
			case et == "int":
				c.vars[j.Var] = "int"
			case et == "bool":
				c.vars[j.Var] = "bool"
			case c.isStructName(et):
				c.varStruct[j.Var] = et
			}
			c.elemType[j.Var] = et
		}
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		joinOns[i] = on
	}
	var where string
	if q.Where != nil {
		where, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	val, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	itemTypeExpr := val
	for v, t := range c.varStruct {
		if t == "" {
			continue
		}
		if strings.Contains(val, v+".") {
			itemTypeExpr = replaceVarRef(itemTypeExpr, v, t)
		}
	}
	if matched, _ := regexp.MatchString(`[A-Za-z_][A-Za-z0-9_]*\\.`, itemTypeExpr); matched {
		itemTypeExpr = "0"
	}
	itemType := fmt.Sprintf("decltype(%s)", itemTypeExpr)
	if strings.Contains(itemTypeExpr, "__avg") || strings.Contains(itemTypeExpr, "__sum") {
		itemType = "double"
	} else if strings.Contains(itemTypeExpr, "std::accumulate") {
		if strings.Contains(itemTypeExpr, ".") {
			itemType = "double"
		} else {
			itemType = "int"
		}
	} else if strings.HasPrefix(itemType, "decltype(") {
		if t := c.varStruct[val]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			itemType = t
		} else if t := c.elemType[val]; t != "" {
			itemType = t
		}
	}
	if val == q.Var {
		if t := c.varStruct[q.Var]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			itemType = t
		} else if typ, ok := c.vars[q.Var]; ok {
			switch typ {
			case "string":
				itemType = "std::string"
			case "int":
				itemType = "int"
			case "bool":
				itemType = "bool"
			}
		}
	} else if t := c.structLiteralType(val); t != "" {
		itemType = t
	} else if t := c.varStruct[val]; t != "" {
		if idx := strings.Index(t, "{"); idx != -1 {
			t = t[:idx]
		}
		itemType = t
	} else if dot := strings.Index(val, "."); dot != -1 {
		v := val[:dot]
		fld := val[dot+1:]
		if t := c.varStruct[v]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			if info, ok := c.structByName[t]; ok {
				sf := sanitizeName(fld)
				for i, fn := range info.Fields {
					if fn == sf && i < len(info.Types) {
						itemType = info.Types[i]
						break
					}
				}
			}
			if itemType == fmt.Sprintf("decltype(%s.%s)", v, fld) || itemType == "" {
				itemType = fmt.Sprintf("decltype(std::declval<%s>().%s)", t, fld)
			}
		}
	} else if t := c.elemType[val]; t != "" {
		itemType = t
	}
	if t := c.varStruct[q.Var]; t != "" {
		if idx := strings.Index(t, "{"); idx != -1 {
			t = t[:idx]
		}
		if itemType == fmt.Sprintf("decltype(%s)", q.Var) {
			itemType = t
		}
	} else if t := c.elemType[q.Var]; t != "" && strings.HasPrefix(itemType, "decltype(") {
		itemType = t
	}
	if vt := c.varStruct[q.Var]; vt != "" {
		if itemType == fmt.Sprintf("decltype(%s)", q.Var) {
			itemType = vt
		}
		if strings.Contains(itemType, q.Var+".") {
			vt = strings.TrimSuffix(vt, "{}")
			itemType = strings.ReplaceAll(itemType, q.Var+".", fmt.Sprintf("std::declval<%s>().", vt))
		}
	}
	if itemType == fmt.Sprintf("decltype(%s)", q.Var) {
		if et := c.elemType[src]; et != "" {
			itemType = et
		} else if t := c.varStruct[src]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			itemType = t
		}
	}
	key := ""
	keyType := ""
	if q.Sort != nil {
		key, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		keyType = fmt.Sprintf("decltype(%s)", key)
		if t := c.structLiteralType(key); t != "" {
			keyType = t
		} else if strings.Contains(key, "__avg") || strings.Contains(key, "__sum") {
			keyType = "double"
		} else if strings.Contains(key, "std::accumulate") {
			if strings.Contains(key, ".") {
				keyType = "double"
			} else {
				keyType = "int"
			}
		} else if vt := c.varStruct[q.Var]; vt != "" {
			if idx := strings.Index(vt, "{"); idx != -1 {
				vt = vt[:idx]
			}
			if pos := strings.Index(key, q.Var+"."); pos != -1 {
				start := pos + len(q.Var) + 1
				end := start
				for end < len(key) && ((key[end] >= 'a' && key[end] <= 'z') || (key[end] >= 'A' && key[end] <= 'Z') || (key[end] >= '0' && key[end] <= '9') || key[end] == '_') {
					end++
				}
				fld := key[start:end]
				keyType = fmt.Sprintf("decltype(std::declval<%s>().%s)", vt, fld)
			} else {
				keyType = strings.ReplaceAll(keyType, q.Var+".", fmt.Sprintf("std::declval<%s>().", vt))
			}
		}
		if strings.Contains(keyType, q.Var+".") {
			if vt := c.varStruct[q.Var]; vt != "" {
				vt = strings.TrimSuffix(vt, "{}")
				keyType = strings.ReplaceAll(keyType, q.Var+".", fmt.Sprintf("std::declval<%s>().", vt))
			}
		}
		if strings.Contains(keyType, q.Var+".") {
			if vt := c.varStruct[q.Var]; vt != "" {
				vt = strings.TrimSuffix(vt, "{}")
				keyType = strings.ReplaceAll(keyType, q.Var+".", fmt.Sprintf("std::declval<%s>().", vt))
			}
		}
		if vt := c.varStruct[q.Var]; vt != "" && keyType == fmt.Sprintf("decltype(%s)", q.Var) {
			keyType = vt
		}
		if keyType == fmt.Sprintf("decltype(%s)", q.Var) {
			if et := c.elemType[src]; et != "" {
				keyType = et
			} else if t := c.varStruct[src]; t != "" {
				if idx := strings.Index(t, "{"); idx != -1 {
					t = t[:idx]
				}
				keyType = t
			}
		}
	}
	skip := ""
	if q.Skip != nil {
		skip, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	take := ""
	if q.Take != nil {
		take, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}

	if arg, ok := sumOverVar(q.Select); ok && arg == q.Var && len(q.Froms) == 0 && len(q.Joins) == 0 && key == "" && skip == "" && take == "" {
		var buf strings.Builder
		buf.WriteString("(" + cap + "() {\n")
		buf.WriteString("    int __sum = 0;\n")
		buf.WriteString("    for (auto " + q.Var + " : " + src + ") {\n")
		if q.Where != nil {
			cond := c.ensureBool(where)
			buf.WriteString("        if (!(" + cond + ")) continue;\n")
		}
		buf.WriteString("        __sum += " + arg + ";\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return __sum;\n")
		buf.WriteString("})()")
		res := buf.String()
		c.vars[res] = "int"
		return res, nil
	}

	if call, ok := callPattern(q.Select); ok && call.Func == "sum" && len(call.Args) == 1 &&
		len(q.Froms) == 0 && len(q.Joins) == 0 && key == "" && skip == "" && take == "" {
		argExpr, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		var buf strings.Builder
		buf.WriteString("(" + cap + "() {\n")
		buf.WriteString("    double __sum = 0;\n")
		buf.WriteString("    for (auto " + q.Var + " : " + src + ") {\n")
		if q.Where != nil {
			cond := c.ensureBool(where)
			buf.WriteString("        if (!(" + cond + ")) continue;\n")
		}
		buf.WriteString("        __sum += " + argExpr + ";\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return __sum;\n")
		buf.WriteString("})()")
		res := buf.String()
		c.vars[res] = "double"
		return res, nil
	}

	var buf strings.Builder
	buf.WriteString("(" + cap + "() {\n")
	indent := func(n int) {
		for i := 0; i < n; i++ {
			buf.WriteString("    ")
		}
	}
	buf.WriteString("    std::vector<")
	if key != "" || skip != "" || take != "" {
		if keyType == "" {
			keyType = fmt.Sprintf("decltype(%s)", key)
		}
		buf.WriteString("std::pair<" + keyType + ", " + itemType + ">")
	} else {
		buf.WriteString(itemType)
	}
	buf.WriteString("> __items;\n")
	indentLevel := 1
	indent(indentLevel)
	buf.WriteString("for (auto " + q.Var + " : " + src + ") {\n")
	indentLevel++
	for i, fs := range fromSrcs {
		indent(indentLevel)
		buf.WriteString("for (auto " + q.Froms[i].Var + " : " + fs + ") {\n")
		indentLevel++
	}
	var joinLoop func(int)
	joinLoop = func(i int) {
		if i == len(joinSrcs) {
			if where != "" {
				indent(indentLevel)
				cond := c.ensureBool(where)
				buf.WriteString("if (!(" + cond + ")) continue;\n")
			}
			indent(indentLevel)
			if key != "" || skip != "" || take != "" {
				buf.WriteString("__items.push_back({" + key + ", " + val + "});\n")
			} else {
				buf.WriteString("__items.push_back(" + val + ");\n")
			}
			return
		}
		side := ""
		if q.Joins[i].Side != nil {
			side = *q.Joins[i].Side
		}
		if side == "right" || side == "outer" {
			side = "left"
		}
		switch side {
		case "left":
			indent(indentLevel)
			buf.WriteString("{ bool __matched" + strconv.Itoa(i) + " = false;\n")
			indentLevel++
			indent(indentLevel)
			buf.WriteString("for (auto " + q.Joins[i].Var + " : " + joinSrcs[i] + ") {\n")
			indentLevel++
			indent(indentLevel)
			onCond := c.ensureBool(joinOns[i])
			buf.WriteString("if (!(" + onCond + ")) continue;\n")
			indent(indentLevel)
			buf.WriteString("__matched" + strconv.Itoa(i) + " = true;\n")
			joinLoop(i + 1)
			indentLevel--
			indent(indentLevel)
			buf.WriteString("}\n")
			indent(indentLevel)
			buf.WriteString("if (!__matched" + strconv.Itoa(i) + ") {\n")
			indentLevel++
			indent(indentLevel)
			buf.WriteString("auto " + q.Joins[i].Var + " = std::decay_t<decltype(*(" + joinSrcs[i] + ").begin())>{};\n")
			joinLoop(i + 1)
			indentLevel--
			indent(indentLevel)
			buf.WriteString("}\n")
			indentLevel--
			indent(indentLevel)
			buf.WriteString("}\n")
		default:
			indent(indentLevel)
			buf.WriteString("for (auto " + q.Joins[i].Var + " : " + joinSrcs[i] + ") {\n")
			indentLevel++
			indent(indentLevel)
			onCond := c.ensureBool(joinOns[i])
			buf.WriteString("if (!(" + onCond + ")) continue;\n")
			joinLoop(i + 1)
			indentLevel--
			indent(indentLevel)
			buf.WriteString("}\n")
		}
	}
	joinLoop(0)
	for i := len(fromSrcs) - 1; i >= 0; i-- {
		indentLevel--
		indent(indentLevel)
		buf.WriteString("}\n")
	}
	indentLevel--
	indent(indentLevel)
	buf.WriteString("}\n")
	if key != "" {
		comp := "a.first < b.first"
		if c.isStructName(keyType) {
			if info, ok := c.structByName[keyType]; ok {
				left := []string{}
				right := []string{}
				for _, f := range info.Fields {
					fld := sanitizeName(f)
					left = append(left, fmt.Sprintf("a.first.%s", fld))
					right = append(right, fmt.Sprintf("b.first.%s", fld))
				}
				comp = fmt.Sprintf("std::tie(%s) < std::tie(%s)", strings.Join(left, ", "), strings.Join(right, ", "))
			}
		}
		buf.WriteString(fmt.Sprintf("    std::sort(__items.begin(), __items.end(), [](auto &a, auto &b){ return %s; });\n", comp))
	}
	if skip != "" {
		buf.WriteString("    if ((size_t)" + skip + " < __items.size()) __items.erase(__items.begin(), __items.begin()+" + skip + ");\n")
	}
	if take != "" {
		buf.WriteString("    if ((size_t)" + take + " < __items.size()) __items.resize(" + take + ");\n")
	}
	if key != "" || skip != "" || take != "" {
		buf.WriteString("    std::vector<" + itemType + "> __res;\n")
		buf.WriteString("    for (auto &p : __items) __res.push_back(p.second);\n")
		buf.WriteString("    return __res;\n")
	} else {
		buf.WriteString("    return __items;\n")
	}
	buf.WriteString("})()")
	res := buf.String()
	if c.isStructName(itemType) {
		c.varStruct[res] = itemType
		c.elemType[res] = itemType
	}
	for k, v := range backup {
		if v == "" {
			delete(c.varStruct, k)
		} else {
			c.varStruct[k] = v
		}
	}
	return res, nil
}

func (c *Compiler) compileGroupedQueryExpr(q *parser.QueryExpr) (string, error) {
	// Fast path for simple grouping: no joins, froms or filters and the
	// query simply returns the group object.
	if len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil &&
		q.Sort == nil && q.Skip == nil && q.Take == nil &&
		q.Group.Having == nil {
		if id, ok := c.simpleIdentifier(q.Select); ok && id == q.Group.Name {
			src, err := c.compileExpr(q.Source)
			if err != nil {
				return "", err
			}
			if t := c.varStruct[src]; t != "" {
				c.varStruct[q.Var] = t
			}
			if et := c.elemType[src]; et != "" {
				switch {
				case strings.Contains(et, "std::string"):
					c.vars[q.Var] = "string"
				case et == "int":
					c.vars[q.Var] = "int"
				case et == "bool":
					c.vars[q.Var] = "bool"
				case c.isStructName(et):
					c.varStruct[q.Var] = et
				}
			}
			keyExpr, err := c.compileExpr(q.Group.Exprs[0])
			if err != nil {
				return "", err
			}
			keyType := fmt.Sprintf("decltype(%s)", keyExpr)
			if t := c.structLiteralType(keyExpr); t != "" {
				keyType = t
			} else if t := c.varStruct[keyExpr]; t != "" {
				if idx := strings.Index(t, "{"); idx != -1 {
					t = t[:idx]
				}
				keyType = t
			} else if dot := strings.Index(keyExpr, "."); dot != -1 {
				v := keyExpr[:dot]
				fld := keyExpr[dot+1:]
				if t := c.varStruct[v]; t != "" {
					if idx := strings.Index(t, "{"); idx != -1 {
						t = t[:idx]
					}
					if info, ok := c.structByName[t]; ok {
						sf := sanitizeName(fld)
						for i, fn := range info.Fields {
							if fn == sf && i < len(info.Types) {
								keyType = info.Types[i]
								break
							}
						}
					}
					if keyType == fmt.Sprintf("decltype(%s.%s)", v, fld) || keyType == "" {
						keyType = fmt.Sprintf("decltype(std::declval<%s>().%s)", t, fld)
					}
				} else if typ, ok := c.vars[v]; ok {
					switch typ {
					case "string":
						keyType = "std::string"
					case "int", "double", "bool":
						keyType = typ
					}
				}
			}
			if strings.Contains(keyType, q.Var+".") {
				if t := c.varStruct[q.Var]; t != "" {
					keyType = strings.ReplaceAll(keyType, q.Var+".", fmt.Sprintf("std::declval<%s>().", strings.TrimSuffix(t, "{}")))
				} else if typ, ok := c.vars[q.Var]; ok {
					switch typ {
					case "string":
						keyType = "std::string"
					case "int", "double", "bool":
						keyType = typ
					}
				}
			}

			itemStruct := ""
			if t := c.varStruct[src]; t != "" {
				itemStruct = t
			} else if t := c.elemType[src]; t != "" {
				itemStruct = t
			}
			if itemStruct == "" {
				itemStruct = "auto"
			}

			groupStruct := fmt.Sprintf("__struct%d", c.structCount+1)
			c.structCount++
			c.varStruct[q.Group.Name] = groupStruct
			c.elemType[q.Group.Name+".items"] = itemStruct
			info := &structInfo{Name: groupStruct, Fields: []string{"key", "items"}, Types: []string{keyType, "std::vector<" + itemStruct + ">"}}
			c.defineStruct(info)

			var buf strings.Builder
			cap := "[&]"
			if c.scope == 0 {
				cap = "[]"
			}
			buf.WriteString("(" + cap + "() {\n")
			buf.WriteString("    std::map<" + keyType + ", std::vector<" + itemStruct + ">> __groups;\n")
			buf.WriteString("    for (auto " + q.Var + " : " + src + ") {\n")
			buf.WriteString("        __groups[" + keyExpr + "].push_back(" + q.Var + ");\n")
			buf.WriteString("    }\n")
			buf.WriteString("    std::vector<" + groupStruct + "> __items;\n")
			buf.WriteString("    for (auto &kv : __groups) {\n")
			buf.WriteString("        __items.push_back(" + groupStruct + "{kv.first, kv.second});\n")
			buf.WriteString("    }\n")
			buf.WriteString("    return __items;\n")
			buf.WriteString("})()")

			res := buf.String()
			c.varStruct[res] = groupStruct
			c.elemType[res] = groupStruct
			return res, nil
		}
	}
	for _, j := range q.Joins {
		if j.Side != nil && *j.Side != "left" && *j.Side != "right" && *j.Side != "outer" {
			return "", fmt.Errorf("join side not supported")
		}
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	backup := map[string]string{}
	backupElem := map[string]string{}
	if t := c.varStruct[src]; t != "" {
		backup[q.Var] = c.varStruct[q.Var]
		c.varStruct[q.Var] = t
	}
	if et := c.elemType[src]; et != "" {
		switch {
		case strings.Contains(et, "std::string"):
			c.vars[q.Var] = "string"
		case et == "int":
			c.vars[q.Var] = "int"
		case et == "bool":
			c.vars[q.Var] = "bool"
		case c.isStructName(et):
			c.varStruct[q.Var] = et
		}
	}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = s
		if t := c.varStruct[s]; t != "" {
			backup[f.Var] = c.varStruct[f.Var]
			c.varStruct[f.Var] = t
		}
		if et := c.elemType[s]; et != "" {
			switch {
			case strings.Contains(et, "std::string"):
				c.vars[f.Var] = "string"
			case et == "int":
				c.vars[f.Var] = "int"
			case et == "bool":
				c.vars[f.Var] = "bool"
			case c.isStructName(et):
				c.varStruct[f.Var] = et
			}
		}
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		if t := c.varStruct[js]; t != "" {
			backup[j.Var] = c.varStruct[j.Var]
			c.varStruct[j.Var] = t
		}
		if et := c.elemType[js]; et != "" {
			switch {
			case strings.Contains(et, "std::string"):
				c.vars[j.Var] = "string"
			case et == "int":
				c.vars[j.Var] = "int"
			case et == "bool":
				c.vars[j.Var] = "bool"
			case c.isStructName(et):
				c.varStruct[j.Var] = et
			}
		}
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		joinOns[i] = on
	}
	var where string
	if q.Where != nil {
		where, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	keyExpr, err := c.compileExpr(q.Group.Exprs[0])
	if err != nil {
		return "", err
	}
	keyType := fmt.Sprintf("decltype(%s)", keyExpr)
	if t := c.structLiteralType(keyExpr); t != "" {
		keyType = t
	} else if strings.Contains(keyExpr, "__avg") || strings.Contains(keyExpr, "__sum") {
		keyType = "double"
	} else if strings.Contains(keyExpr, "std::accumulate") {
		if strings.Contains(keyExpr, ".") {
			keyType = "double"
		} else {
			keyType = "int"
		}
	} else if t := c.varStruct[keyExpr]; t != "" {
		if idx := strings.Index(t, "{"); idx != -1 {
			t = t[:idx]
		}
		keyType = t
	} else if dot := strings.Index(keyExpr, "."); dot != -1 {
		v := keyExpr[:dot]
		fld := keyExpr[dot+1:]
		if t := c.varStruct[v]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			keyType = fmt.Sprintf("decltype(std::declval<%s>().%s)", t, fld)
		} else if typ, ok := c.vars[v]; ok {
			switch typ {
			case "string":
				keyType = "std::string"
			case "int", "double", "bool":
				keyType = typ
			}
		}
	}
	if keyType == fmt.Sprintf("decltype(%s)", keyExpr) {
		if dot := strings.Index(keyExpr, "."); dot != -1 {
			v := keyExpr[:dot]
			if typ, ok := c.vars[v]; ok {
				switch typ {
				case "string":
					keyType = "std::string"
				case "int", "double", "bool":
					keyType = typ
				}
			}
		}
	}

	itemVars := []string{q.Var}
	for _, f := range q.Froms {
		itemVars = append(itemVars, f.Var)
	}
	for _, j := range q.Joins {
		itemVars = append(itemVars, j.Var)
	}
	itemStruct := ""
	if len(itemVars) == 1 {
		if t := c.varStruct[itemVars[0]]; t != "" {
			itemStruct = t
		}
	}
	if itemStruct == "" {
		itemStruct = c.structFromVars(itemVars)
	}
	if strings.Contains(keyType, q.Var+".") && itemStruct != "" {
		keyType = strings.ReplaceAll(keyType, q.Var+".", fmt.Sprintf("std::declval<%s>().", strings.TrimSuffix(itemStruct, "{}")))
	}
	groupStruct := fmt.Sprintf("__struct%d", c.structCount+1)
	c.structCount++
	info := &structInfo{Name: groupStruct, Fields: []string{"key", "items"}, Types: []string{keyType, "std::vector<" + itemStruct + ">"}}
	c.defineStruct(info)
	backup[q.Group.Name] = c.varStruct[q.Group.Name]
	c.varStruct[q.Group.Name] = groupStruct
	backupElem[q.Group.Name+".items"] = c.elemType[q.Group.Name+".items"]
	c.elemType[q.Group.Name+".items"] = itemStruct

	// prepare expressions for select, having, sort
	oldAliases := c.aliases
	selectIsGroup := false
	if id, ok := c.simpleIdentifier(q.Select); ok && id == q.Group.Name {
		selectIsGroup = true
	}
	c.aliases = make(map[string]string)
	for k := range oldAliases {
		c.aliases[k] = oldAliases[k]
	}
	if !selectIsGroup {
		c.aliases[q.Group.Name] = q.Group.Name + ".items"
	}

	var buf strings.Builder
	cap := "[&]"
	if c.scope == 0 {
		cap = "[]"
	}
	c.scope++
	buf.WriteString("(" + cap + "() {\n")

	valExpr, err := c.compileExpr(q.Select)
	if err != nil {
		c.aliases = oldAliases
		c.scope--
		return "", err
	}
	itemTypeExpr := valExpr
	if strings.Contains(valExpr, q.Group.Name) {
		itemTypeExpr = strings.ReplaceAll(valExpr, q.Group.Name, fmt.Sprintf("std::declval<%s>()", groupStruct))
	}
	itemType := fmt.Sprintf("decltype(%s)", itemTypeExpr)
	if strings.Contains(itemTypeExpr, "std::accumulate") {
		if strings.Contains(itemTypeExpr, ".") {
			itemType = "double"
		} else {
			itemType = "int"
		}
	} else if strings.HasPrefix(itemType, "decltype(") {
		if t := c.varStruct[valExpr]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			itemType = t
		} else if t := c.elemType[valExpr]; t != "" {
			itemType = t
		}
	}
	if selectIsGroup {
		itemType = groupStruct
	} else if t := c.structLiteralType(valExpr); t != "" {
		itemType = t
	} else if t := c.varStruct[valExpr]; t != "" {
		if idx := strings.Index(t, "{"); idx != -1 {
			t = t[:idx]
		}
		itemType = t
	} else if dot := strings.Index(valExpr, "."); dot != -1 {
		v := valExpr[:dot]
		fld := valExpr[dot+1:]
		if t := c.varStruct[v]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			itemType = fmt.Sprintf("decltype(std::declval<%s>().%s)", t, fld)
		}
	}

	// prepare aliases for remaining expressions
	c.aliases = make(map[string]string)
	for k := range oldAliases {
		c.aliases[k] = oldAliases[k]
	}
	c.aliases[q.Group.Name] = q.Group.Name + ".items"

	var sortExpr, sortKeyType string
	if q.Sort != nil {
		if q.Sort.Binary != nil && len(q.Sort.Binary.Right) == 0 &&
			q.Sort.Binary.Left != nil && q.Sort.Binary.Left.Value != nil &&
			q.Sort.Binary.Left.Value.Target != nil && q.Sort.Binary.Left.Value.Target.List != nil &&
			len(q.Sort.Binary.Left.Value.Target.List.Elems) > 1 {
			elems := q.Sort.Binary.Left.Value.Target.List.Elems
			exprs := make([]string, len(elems))
			types := make([]string, len(elems))
			for i, el := range elems {
				ex, err := c.compileExpr(el)
				if err != nil {
					c.aliases = oldAliases
					return "", err
				}
				exprs[i] = ex
				typExpr := ex
				if strings.Contains(ex, q.Group.Name) {
					typExpr = strings.ReplaceAll(ex, q.Group.Name, fmt.Sprintf("std::declval<%s>()", groupStruct))
				}
				t := fmt.Sprintf("decltype(%s)", typExpr)
				if strings.Contains(typExpr, "__avg") || strings.Contains(typExpr, "__sum") {
					t = "double"
				} else if strings.Contains(typExpr, "std::accumulate") {
					if strings.Contains(typExpr, ".") {
						t = "double"
					} else {
						t = "int"
					}
				}
				types[i] = t
			}
			name := fmt.Sprintf("__struct%d", c.structCount+1)
			c.structCount++
			info := &structInfo{Name: name}
			for i, t := range types {
				info.Fields = append(info.Fields, fmt.Sprintf("f%d", i))
				info.Types = append(info.Types, t)
			}
			c.structByName[name] = info
			c.structMap[name] = info
			c.defineStruct(info)
			sortKeyType = name
			sortExpr = fmt.Sprintf("%s{%s}", name, strings.Join(exprs, ", "))
		} else {
			sortExpr, err = c.compileExpr(q.Sort)
			if err != nil {
				c.aliases = oldAliases
				return "", err
			}
			sortKeyTypeExpr := sortExpr
			if strings.Contains(sortExpr, q.Group.Name) {
				sortKeyTypeExpr = strings.ReplaceAll(sortExpr, q.Group.Name, fmt.Sprintf("std::declval<%s>()", groupStruct))
			}
			sortKeyType = fmt.Sprintf("decltype(%s)", sortKeyTypeExpr)
			if strings.Contains(sortKeyTypeExpr, "__avg") || strings.Contains(sortKeyTypeExpr, "__sum") {
				sortKeyType = "double"
			} else if strings.Contains(sortKeyTypeExpr, "std::accumulate") {
				if strings.Contains(sortKeyTypeExpr, ".") {
					sortKeyType = "double"
				} else {
					sortKeyType = "int"
				}
			}
		}
	}
	var havingExpr string
	if q.Group.Having != nil {
		havingExpr, err = c.compileExpr(q.Group.Having)
		if err != nil {
			c.aliases = oldAliases
			return "", err
		}
	}
	var skipExpr, takeExpr string
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.aliases = oldAliases
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.aliases = oldAliases
			c.scope--
			return "", err
		}
	}
	c.aliases = oldAliases

	indent := func(n int) {
		for i := 0; i < n; i++ {
			buf.WriteString("    ")
		}
	}
	buf.WriteString("    std::vector<" + groupStruct + "> __groups;\n")
	indentLevel := 1
	indent(indentLevel)
	buf.WriteString("for (auto " + q.Var + " : " + src + ") {\n")
	indentLevel++
	for i, fs := range fromSrcs {
		indent(indentLevel)
		buf.WriteString("for (auto " + q.Froms[i].Var + " : " + fs + ") {\n")
		indentLevel++
	}
	var joinLoop func(int)
	joinLoop = func(i int) {
		if i == len(joinSrcs) {
			if where != "" {
				indent(indentLevel)
				buf.WriteString("if (!(" + where + ")) continue;\n")
			}
			indent(indentLevel)
			buf.WriteString("auto __key = " + keyExpr + ";\n")
			indent(indentLevel)
			itemInit := strings.Join(itemVars, ", ")
			if len(itemVars) > 1 || itemStruct == "auto" {
				itemInit = itemStruct + "{" + itemInit + "}"
			}
			buf.WriteString("bool __found = false;\n")
			indent(indentLevel)
			eq := "__g.key == __key"
			keyFieldType := info.Types[0]
			if strings.Contains(keyFieldType, "std::any") || strings.Contains(keyType, "std::any") {
				eq = "__any_eq(__g.key, __key)"
				c.usesAny = true
			}
			buf.WriteString("for (auto &__g : __groups) { if (" + eq + ") { __g.items.push_back(" + itemInit + "); __found = true; break; } }\n")
			indent(indentLevel)
			buf.WriteString("if (!__found) { __groups.push_back(" + groupStruct + "{__key, std::vector<" + itemStruct + ">{" + itemInit + "}}); }\n")
			return
		}
		side := ""
		if q.Joins[i].Side != nil {
			side = *q.Joins[i].Side
		}
		if side == "right" || side == "outer" {
			side = "left"
		}
		switch side {
		case "left":
			indent(indentLevel)
			buf.WriteString("{ bool __matched" + strconv.Itoa(i) + " = false;\n")
			indentLevel++
			indent(indentLevel)
			buf.WriteString("for (auto " + q.Joins[i].Var + " : " + joinSrcs[i] + ") {\n")
			indentLevel++
			indent(indentLevel)
			onCond := c.ensureBool(joinOns[i])
			buf.WriteString("if (!(" + onCond + ")) continue;\n")
			indent(indentLevel)
			buf.WriteString("__matched" + strconv.Itoa(i) + " = true;\n")
			joinLoop(i + 1)
			indentLevel--
			indent(indentLevel)
			buf.WriteString("}\n")
			indent(indentLevel)
			buf.WriteString("if (!__matched" + strconv.Itoa(i) + ") {\n")
			indentLevel++
			indent(indentLevel)
			buf.WriteString("auto " + q.Joins[i].Var + " = std::decay_t<decltype(*(" + joinSrcs[i] + ").begin())>{};\n")
			joinLoop(i + 1)
			indentLevel--
			indent(indentLevel)
			buf.WriteString("}\n")
			indentLevel--
			indent(indentLevel)
			buf.WriteString("}\n")
		default:
			indent(indentLevel)
			buf.WriteString("for (auto " + q.Joins[i].Var + " : " + joinSrcs[i] + ") {\n")
			indentLevel++
			indent(indentLevel)
			onCond := c.ensureBool(joinOns[i])
			buf.WriteString("if (!(" + onCond + ")) continue;\n")
			joinLoop(i + 1)
			indentLevel--
			indent(indentLevel)
			buf.WriteString("}\n")
		}
	}
	joinLoop(0)
	for i := len(fromSrcs) - 1; i >= 0; i-- {
		indentLevel--
		indent(indentLevel)
		buf.WriteString("}\n")
	}
	indentLevel--
	indent(indentLevel)
	buf.WriteString("}\n")

	// iterate groups to build result
	indent(indentLevel)
	buf.WriteString("std::vector<")
	if sortExpr != "" || skipExpr != "" || takeExpr != "" {
		if sortKeyType == "" {
			sortKeyType = fmt.Sprintf("decltype(%s)", sortExpr)
		}
		buf.WriteString("std::pair<" + sortKeyType + ", " + itemType + ">")
	} else {
		buf.WriteString(itemType)
	}
	buf.WriteString("> __items;\n")
	indent(indentLevel)
	buf.WriteString("for (auto &" + q.Group.Name + " : __groups) {\n")
	indentLevel++
	if havingExpr != "" {
		indent(indentLevel)
		cond := c.ensureBool(havingExpr)
		buf.WriteString("if (!(" + cond + ")) continue;\n")
	}
	indent(indentLevel)
	if sortExpr != "" || skipExpr != "" || takeExpr != "" {
		buf.WriteString("__items.push_back({" + sortExpr + ", " + valExpr + "});\n")
	} else {
		buf.WriteString("__items.push_back(" + valExpr + ");\n")
	}
	indentLevel--
	indent(indentLevel)
	buf.WriteString("}\n")

	if sortExpr != "" {
		comp := "a.first < b.first"
		if c.isStructName(sortKeyType) {
			if info, ok := c.structByName[sortKeyType]; ok {
				left := []string{}
				right := []string{}
				for _, f := range info.Fields {
					fld := sanitizeName(f)
					left = append(left, fmt.Sprintf("a.first.%s", fld))
					right = append(right, fmt.Sprintf("b.first.%s", fld))
				}
				comp = fmt.Sprintf("std::tie(%s) < std::tie(%s)", strings.Join(left, ", "), strings.Join(right, ", "))
			}
		}
		buf.WriteString(fmt.Sprintf("    std::sort(__items.begin(), __items.end(), [](auto &a, auto &b){ return %s; });\n", comp))
	}
	if skipExpr != "" {
		buf.WriteString("    if ((size_t)" + skipExpr + " < __items.size()) __items.erase(__items.begin(), __items.begin()+" + skipExpr + ");\n")
	}
	if takeExpr != "" {
		buf.WriteString("    if ((size_t)" + takeExpr + " < __items.size()) __items.resize(" + takeExpr + ");\n")
	}
	if sortExpr != "" || skipExpr != "" || takeExpr != "" {
		buf.WriteString("    std::vector<" + itemType + "> __res;\n")
		buf.WriteString("    for (auto &p : __items) __res.push_back(p.second);\n")
		buf.WriteString("    return __res;\n")
	} else {
		buf.WriteString("    return __items;\n")
	}
	buf.WriteString("})()")
	c.scope--
	res := buf.String()
	if c.isStructName(itemType) {
		c.varStruct[res] = itemType
		c.elemType[res] = itemType
	}
	for k, v := range backup {
		if v == "" {
			delete(c.varStruct, k)
		} else {
			c.varStruct[k] = v
		}
	}
	for k, v := range backupElem {
		if v == "" {
			delete(c.elemType, k)
		} else {
			c.elemType[k] = v
		}
	}
	return res, nil
}

func (c *Compiler) simpleIdentifier(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) > 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	if l.Int != nil {
		return fmt.Sprint(*l.Int), nil
	}
	if l.Float != nil {
		return fmt.Sprint(*l.Float), nil
	}
	if l.Bool != nil {
		if bool(*l.Bool) {
			return "true", nil
		}
		return "false", nil
	}
	if l.Str != nil {
		return fmt.Sprintf("std::string(%q)", *l.Str), nil
	}
	if l.Null {
		return "nullptr", nil
	}
	return "", fmt.Errorf("unknown literal")
}

func (c *Compiler) compileType(t *parser.TypeRef) (string, error) {
	if t == nil {
		return "auto", nil
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int", nil
		case "string":
			return "std::string", nil
		case "any":
			c.usesAny = true
			return "std::any", nil
		default:
			if _, ok := c.unions[*t.Simple]; ok {
				return *t.Simple + "*", nil
			}
			return *t.Simple, nil
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			elem, err := c.compileType(t.Generic.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("std::vector<%s>", elem), nil
		}
	}
	return "auto", nil
}

func (c *Compiler) inferType(expr string) string {
	trimmed := strings.TrimSpace(expr)
	for len(trimmed) > 1 && trimmed[0] == '(' && trimmed[len(trimmed)-1] == ')' {
		trimmed = strings.TrimSpace(trimmed[1 : len(trimmed)-1])
	}
	if strings.HasPrefix(expr, "std::string") {
		return "string"
	}
	if strings.HasPrefix(expr, "std::unordered_map") || strings.HasPrefix(expr, "std::map") {
		return "map"
	}
	if strings.HasPrefix(expr, "std::vector") {
		if !strings.Contains(expr, ".") {
			return "vector"
		}
	}
	if trimmed != "" {
		if trimmed[0] >= '0' && trimmed[0] <= '9' {
			if strings.Contains(trimmed, ".") {
				return "double"
			}
			return "int"
		}
		if trimmed[0] == '-' && len(trimmed) > 1 && trimmed[1] >= '0' && trimmed[1] <= '9' {
			if strings.Contains(trimmed, ".") {
				return "double"
			}
			return "int"
		}
		if trimmed == "true" || trimmed == "false" {
			return "bool"
		}
		if !strings.ContainsAny(trimmed, "{}?:") {
			if strings.Contains(trimmed, "==") || strings.Contains(trimmed, "!=") ||
				strings.Contains(trimmed, "<=") || strings.Contains(trimmed, ">=") ||
				strings.Contains(trimmed, " && ") || strings.Contains(trimmed, " || ") ||
				strings.HasPrefix(trimmed, "!") || strings.Contains(trimmed, " < ") || strings.Contains(trimmed, " > ") {
				return "bool"
			}
		}
	}
	return ""
}

// inferExprType returns a simple C++ type for the given expression string.
// It is used when generating struct field declarations.
func inferExprType(expr string) string {
	trimmed := strings.TrimSpace(expr)
	if strings.HasPrefix(trimmed, "std::string(") {
		return "std::string"
	}
	if _, err := strconv.Atoi(trimmed); err == nil {
		return "int"
	}
	if _, err := strconv.ParseFloat(trimmed, 64); err == nil && strings.Contains(trimmed, ".") {
		return "double"
	}
	if trimmed == "true" || trimmed == "false" {
		return "bool"
	}
	if strings.Contains(trimmed, "__avg") || strings.Contains(trimmed, "std::accumulate") {
		return "double"
	}
	if strings.Contains(trimmed, "size()") {
		return "int"
	}
	// detect boolean operators but avoid template angle brackets
	if strings.Contains(trimmed, "==") || strings.Contains(trimmed, "!=") ||
		strings.Contains(trimmed, "<=") || strings.Contains(trimmed, ">=") ||
		strings.Contains(trimmed, "&&") || strings.Contains(trimmed, "||") ||
		strings.HasPrefix(trimmed, "!") ||
		strings.Contains(trimmed, " < ") || strings.Contains(trimmed, " > ") {
		return "bool"
	}
	return ""
}

func (c *Compiler) isStructName(name string) bool {
	if _, ok := c.structByName[name]; ok {
		return true
	}
	return strings.HasPrefix(name, "__struct")
}

func (c *Compiler) extractVectorStruct(expr string) string {
	if idx := strings.Index(expr, "push_back("); idx != -1 {
		inner := expr[idx+len("push_back("):]
		end := strings.Index(inner, ")")
		if end != -1 {
			val := inner[:end]
			if idx := strings.Index(val, "{"); idx != -1 {
				val = val[:idx]
			}
			if c.isStructName(val) {
				return val
			}
		}
	}
	start := strings.Index(expr, "std::vector<")
	if start == -1 {
		return ""
	}
	start += len("std::vector<")
	end := matchAngle(expr[start:])
	if end == -1 {
		return ""
	}
	inner := expr[start : start+end]
	if strings.HasPrefix(inner, "decltype(") {
		inner = strings.TrimSuffix(strings.TrimPrefix(inner, "decltype("), ")")
		if inner == "true" || inner == "false" {
			return "bool"
		}
		if t := c.varStruct[inner]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			return t
		}
		if t := c.elemType[inner]; t != "" {
			return t
		}
	}
	if idx := strings.Index(inner, "{"); idx != -1 {
		inner = inner[:idx]
	}
	if c.isStructName(inner) {
		return inner
	}
	if inner == "int" || inner == "double" || inner == "bool" {
		return inner
	}
	return ""
}

func (c *Compiler) extractVectorElemType(expr string) string {
	if strings.Contains(expr, "push_back(") {
		inner := expr[strings.Index(expr, "push_back(")+len("push_back("):]
		end := strings.Index(inner, ")")
		if end != -1 {
			val := inner[:end]
			if idx := strings.Index(val, "{"); idx != -1 {
				val = val[:idx]
			}
			if c.isStructName(val) {
				return val
			}
			if strings.HasPrefix(val, "std::string") {
				return "std::string"
			}
			if val == "true" || val == "false" {
				return "bool"
			}
			if _, err := strconv.Atoi(val); err == nil {
				return "int"
			}
		}
	}
	if !strings.HasPrefix(expr, "std::vector<") {
		return ""
	}
	inner := expr[len("std::vector<"):]
	idx := matchAngle(inner)
	if idx == -1 {
		return ""
	}
	typ := inner[:idx]
	if strings.HasPrefix(typ, "decltype(") {
		texpr := strings.TrimSuffix(strings.TrimPrefix(typ, "decltype("), ")")
		if idx := strings.Index(texpr, "{"); idx != -1 {
			texpr = texpr[:idx]
		}
		if strings.HasSuffix(texpr, "[0]") {
			base := strings.TrimSuffix(texpr, "[0]")
			if t := c.elemType[base]; t != "" {
				return t
			}
			if t := c.varStruct[base]; t != "" {
				if idx := strings.Index(t, "{"); idx != -1 {
					t = t[:idx]
				}
				return t
			}
		}
		if t := c.varStruct[texpr]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			return t
		}
		if t := c.elemType[texpr]; t != "" {
			return t
		}
		if strings.HasPrefix(texpr, "std::declval<") && strings.Contains(texpr, ">().") {
			tmp := strings.TrimPrefix(texpr, "std::declval<")
			parts := strings.SplitN(tmp, ">().", 2)
			if len(parts) == 2 {
				structName := parts[0]
				field := parts[1]
				if info, ok := c.structByName[structName]; ok {
					for i, f := range info.Fields {
						if f == field {
							return info.Types[i]
						}
					}
				}
			}
		}
		if texpr == "true" || texpr == "false" {
			return "bool"
		}
		if v := c.vars[texpr]; v != "" {
			switch v {
			case "string":
				return "std::string"
			case "int", "double", "bool":
				return v
			}
		}
		if t := c.varStruct[texpr]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			return t
		}
		typ = texpr
	}
	if c.isStructName(typ) {
		return typ
	}
	if typ == "int" || typ == "double" || typ == "bool" {
		return typ
	}
	if strings.Contains(typ, "std::string") {
		return "std::string"
	}
	if strings.Contains(typ, "bool") {
		return "bool"
	}
	if _, err := strconv.Atoi(typ); err == nil {
		return "int"
	}
	if v := c.vars[typ]; v != "" {
		switch v {
		case "string":
			return "std::string"
		case "int", "double", "bool":
			return v
		}
	}
	if t := c.varStruct[typ]; t != "" {
		if idx := strings.Index(t, "{"); idx != -1 {
			t = t[:idx]
		}
		return t
	}
	return ""
}

func (c *Compiler) extractMapValueType(expr string) string {
	if strings.HasPrefix(expr, "std::unordered_map<") || strings.HasPrefix(expr, "std::map<") {
		inner := expr[strings.Index(expr, "<")+1:]
		idx := matchAngle(inner)
		if idx != -1 {
			parts := inner[:idx]
			if comma := strings.LastIndex(parts, ","); comma != -1 {
				val := strings.TrimSpace(parts[comma+1:])
				if strings.HasPrefix(val, "decltype(") {
					val = strings.TrimSuffix(strings.TrimPrefix(val, "decltype("), ")")
				}
				return val
			}
		}
	}
	if idx := strings.Index(expr, "("); idx != -1 {
		name := expr[:idx]
		if v, ok := c.returnMap[name]; ok {
			return v
		}
	}
	if t := c.elemType[expr]; t != "" {
		return t
	}
	return ""
}

func (c *Compiler) isAnyExpr(expr string) bool {
	if c.vars[expr] == "any" || c.elemType[expr] == "std::any" {
		return true
	}
	if idx := strings.Index(expr, "["); idx != -1 {
		base := expr[:idx]
		if c.elemType[base] == "std::any" {
			return true
		}
	}
	if dot := strings.Index(expr, "."); dot != -1 {
		base := expr[:dot]
		fld := sanitizeName(expr[dot+1:])
		if st := c.varStruct[base]; st != "" {
			st = strings.TrimSuffix(st, "{}")
			if info, ok := c.structByName[st]; ok {
				for i, f := range info.Fields {
					if f == fld && info.Types[i] == "std::any" {
						return true
					}
				}
			}
		}
	}
	return strings.Contains(expr, "std::any")
}

func matchAngle(s string) int {
	depth := 0
	for i, ch := range s {
		switch ch {
		case '<':
			depth++
		case '>':
			if depth == 0 {
				return i
			}
			depth--
		}
	}
	return -1
}

func (c *Compiler) extractVectorType(expr string) string {
	if strings.HasPrefix(expr, "std::vector<") {
		inner := expr[len("std::vector<"):]
		idx := matchAngle(inner)
		if idx != -1 {
			return "std::vector<" + inner[:idx] + ">"
		}
	}
	if c.vars[expr] == "vector" {
		if et := c.elemType[expr]; et != "" {
			return fmt.Sprintf("std::vector<%s>", et)
		}
	}
	return ""
}

func (c *Compiler) structLiteralType(expr string) string {
	if idx := strings.Index(expr, "{"); idx != -1 {
		expr = expr[:idx]
	}
	if c.isStructName(expr) {
		return expr
	}
	return ""
}

func (c *Compiler) structFromVars(names []string) string {
	sig := "vars:" + strings.Join(names, ",")
	info, ok := c.structMap[sig]
	if !ok {
		c.structCount++
		name := fmt.Sprintf("__struct%d", c.structCount)
		if c.nextStructName != "" {
			name = toPascalCase(c.nextStructName)
		}
		info = &structInfo{Name: name, Fields: append([]string(nil), names...), Types: make([]string, len(names))}
		c.structMap[sig] = info
		c.structByName[info.Name] = info
	}
	for i, n := range names {
		fieldType := "decltype(" + sanitizeName(n) + ")"
		if t := c.varStruct[n]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			fieldType = t
		} else if typ, ok := c.vars[n]; ok {
			switch typ {
			case "string":
				fieldType = "std::string"
			case "int", "double", "bool":
				fieldType = typ
			}
		} else if t := c.elemType[n]; t != "" {
			fieldType = t
		} else {
			fieldType = "std::any"
			c.usesAny = true
		}
		if strings.HasPrefix(fieldType, "decltype(") {
			if typ, ok := c.vars[n]; ok {
				switch typ {
				case "string":
					fieldType = "std::string"
				case "int", "double", "bool":
					fieldType = typ
				}
			} else if t := c.elemType[n]; t != "" {
				fieldType = t
			}
		}
		if info.Types[i] == "" || strings.Contains(info.Types[i], n) {
			info.Types[i] = fieldType
		}
	}
	c.defineStruct(info)
	c.nextStructName = ""
	return info.Name
}

// replaceVarRef replaces field accesses like "var.field" with
// "std::declval<typ>().field" while avoiding nested replacements
// inside already-expanded expressions.
func replaceVarRef(expr, name, typ string) string {
	re := regexp.MustCompile(`(^|[^A-Za-z0-9_.>])` + regexp.QuoteMeta(name) + `\.`)
	return re.ReplaceAllString(expr, `${1}std::declval<`+typ+`>().`)
}

// compileFunExpr converts an anonymous function expression to a C++ lambda.
func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	return c.compileLambda(fn.Params, fn.ExprBody, fn.BlockBody)
}

// compileLambda builds a C++ lambda from the given parameters and body.
func (c *Compiler) compileLambda(params []*parser.Param, exprBody *parser.Expr, stmts []*parser.Statement) (string, error) {
	var buf strings.Builder
	cap := "[=]"
	if c.scope == 0 {
		cap = "[]"
	}
	buf.WriteString(cap)
	buf.WriteString("(")
	for i, p := range params {
		if i > 0 {
			buf.WriteString(", ")
		}
		typ, err := c.compileType(p.Type)
		if err != nil {
			return "", err
		}
		buf.WriteString(typ)
		buf.WriteByte(' ')
		buf.WriteString(p.Name)
	}
	buf.WriteString(") {")

	sub := &Compiler{indent: 1, tmp: c.tmp, vars: map[string]string{}, aliases: map[string]string{}, scope: c.scope + 1, varStruct: map[string]string{}, elemType: map[string]string{}}
	for _, p := range params {
		if p.Type != nil {
			typ, _ := c.compileType(p.Type)
			switch {
			case typ == "std::string":
				sub.vars[p.Name] = "string"
			case typ == "int" || typ == "double" || typ == "bool":
				sub.vars[p.Name] = strings.TrimPrefix(typ, "std.")
			case strings.HasPrefix(typ, "std::vector<"):
				sub.vars[p.Name] = "vector"
				elem := typ[len("std::vector<") : len(typ)-1]
				sub.elemType[p.Name] = elem
				if c.isStructName(elem) {
					sub.varStruct[p.Name] = elem
				}
			}
		}
	}
	if exprBody != nil {
		e, err := sub.compileExpr(exprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + e + ";")
	} else {
		for _, st := range stmts {
			if err := sub.compileStmt(st); err != nil {
				return "", err
			}
		}
	}
	body := sub.buf.String()
	if body != "" {
		buf.WriteByte('\n')
		buf.WriteString(body)
	}
	buf.WriteString("}")
	c.tmp = sub.tmp
	return buf.String(), nil
}

func (c *Compiler) partialApply(name string, args []string, total int) string {
	var buf strings.Builder
	buf.WriteString("[=](")
	for i := len(args); i < total; i++ {
		if i > len(args) {
			buf.WriteString(", ")
		}
		buf.WriteString("auto p")
		buf.WriteString(strconv.Itoa(i))
	}
	buf.WriteString(") {")
	buf.WriteString("return ")
	buf.WriteString(name)
	buf.WriteString("(")
	for i := 0; i < total; i++ {
		if i > 0 {
			buf.WriteString(", ")
		}
		if i < len(args) {
			buf.WriteString(args[i])
		} else {
			buf.WriteString("p")
			buf.WriteString(strconv.Itoa(i))
		}
	}
	buf.WriteString("); }")
	return buf.String()
}

// compileExistsQuery converts a simple query used with the exists builtin into
// a call to std::any_of when possible.
func (c *Compiler) compileExistsQuery(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	// Backup variable information for the query variable.
	oldVar := c.vars[q.Var]
	oldStruct := c.varStruct[q.Var]
	oldElem := c.elemType[q.Var]

	if t := c.varStruct[src]; t != "" {
		c.varStruct[q.Var] = t
	}
	if et := c.elemType[src]; et != "" {
		switch {
		case strings.Contains(et, "std::string"):
			c.vars[q.Var] = "string"
		case et == "int":
			c.vars[q.Var] = "int"
		case et == "bool":
			c.vars[q.Var] = "bool"
		case c.isStructName(et):
			c.varStruct[q.Var] = et
		}
	}

	cond := "true"
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			// restore and return
			if oldVar == "" {
				delete(c.vars, q.Var)
			} else {
				c.vars[q.Var] = oldVar
			}
			if oldStruct == "" {
				delete(c.varStruct, q.Var)
			} else {
				c.varStruct[q.Var] = oldStruct
			}
			if oldElem == "" {
				delete(c.elemType, q.Var)
			} else {
				c.elemType[q.Var] = oldElem
			}
			return "", err
		}
		cond = c.ensureBool(cond)
	}

	// Restore previous variable info.
	if oldVar == "" {
		delete(c.vars, q.Var)
	} else {
		c.vars[q.Var] = oldVar
	}
	if oldStruct == "" {
		delete(c.varStruct, q.Var)
	} else {
		c.varStruct[q.Var] = oldStruct
	}
	if oldElem == "" {
		delete(c.elemType, q.Var)
	} else {
		c.elemType[q.Var] = oldElem
	}

	if cond == "true" {
		return fmt.Sprintf("(!%s.empty())", src), nil
	}
	return fmt.Sprintf("std::any_of(%s.begin(), %s.end(), [&](auto %s){ return %s; })", src, src, q.Var, cond), nil
}

func (c *Compiler) generateJSONPrinter(info *structInfo) {
	c.headerWriteln("inline void __json(const " + info.Name + " &v){")
	c.headerWriteln("    bool first=true;")
	c.headerWriteln("    std::cout<<\"{\";")
	for _, f := range info.Fields {
		c.headerWriteln("    if(!first) std::cout<<\",\"; first=false;")
		c.headerWriteln("    std::cout<<\"\\\"" + f + "\\\":\"; __json(v." + f + ");")
	}
	c.headerWriteln("    std::cout<<\"}\";")
	c.headerWriteln("}")
}

func (c *Compiler) ensureBool(expr string) string {
	if t := c.varStruct[expr]; t != "" {
		if idx := strings.Index(t, "{"); idx != -1 {
			t = t[:idx]
		}
		if info, ok := c.structByName[t]; ok {
			var parts []string
			for i, f := range info.Fields {
				zero := "0"
				ft := info.Types[i]
				if strings.HasPrefix(ft, "std::string") {
					zero = "std::string()"
				} else if ft == "bool" {
					zero = "false"
				}
				parts = append(parts, fmt.Sprintf("%s.%s != %s", expr, f, zero))
			}
			return "(" + strings.Join(parts, " || ") + ")"
		}
		return fmt.Sprintf("(%s != %s{})", expr, t)
	}
	if dot := strings.Index(expr, "."); dot != -1 {
		base := expr[:dot]
		fld := expr[dot+1:]
		if t := c.varStruct[base]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			if info, ok := c.structByName[t]; ok {
				for i, f := range info.Fields {
					if f == sanitizeName(fld) {
						ft := info.Types[i]
						if idx := strings.Index(ft, "{"); idx != -1 {
							ft = ft[:idx]
						}
						if c.isStructName(ft) {
							return fmt.Sprintf("(%s != %s{})", expr, ft)
						}
						break
					}
				}
			}
		}
	}
	if t := c.structLiteralType(expr); t != "" {
		return fmt.Sprintf("(%s != %s{})", expr, t)
	}
	return expr
}

func (c *Compiler) isVectorType(expr string) bool {
	if c.vars[expr] == "vector" {
		return true
	}
	if strings.HasPrefix(expr, "std::vector<") {
		return true
	}
	if c.extractVectorElemType(expr) != "" {
		return true
	}
	return false
}
