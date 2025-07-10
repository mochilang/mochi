//go:build slow

package cpp

import (
	"bytes"
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"mochi/parser"
)

type structInfo struct {
	Name   string
	Fields []string
}

// Compiler translates a subset of Mochi to simple C++17 code.
type Compiler struct {
	header bytes.Buffer
	buf    bytes.Buffer
	indent int
	tmp    int
	vars   map[string]string

	funParams map[string]int

	aliases map[string]string

	structMap   map[string]*structInfo
	structCount int
	varStruct   map[string]string
	elemType    map[string]string
	groups      map[string]struct{}
	usesJSON    bool
}

// New returns a new compiler instance.
func New() *Compiler {
	return &Compiler{
		vars:      map[string]string{},
		aliases:   map[string]string{},
		structMap: map[string]*structInfo{},
		varStruct: map[string]string{},
		elemType:  map[string]string{},
		funParams: map[string]int{},
		groups:    map[string]struct{}{},
		usesJSON:  false,
	}
}

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("__tmp%d", c.tmp)
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
	c.structCount = 0
	c.usesJSON = false

	globals := []*parser.Statement{}
	encounteredFun := false
	for _, st := range p.Statements {
		if st.Fun != nil {
			encounteredFun = true
			continue
		}
		if !encounteredFun && (st.Let != nil || st.Var != nil) {
			globals = append(globals, st)
		}
	}

	globalSet := map[*parser.Statement]struct{}{}
	for _, st := range globals {
		globalSet[st] = struct{}{}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
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
	for _, st := range p.Statements {
		if st.Fun != nil {
			continue
		}
		if _, ok := globalSet[st]; ok {
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	c.writeln("return 0;")
	c.indent--
	c.writeln("}")

	if c.usesJSON {
		for _, info := range c.structMap {
			c.generateJSONPrinter(info)
		}
	}

	var body bytes.Buffer
	body.Write(c.header.Bytes())
	body.Write(c.buf.Bytes())

	src := body.Bytes()

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
	if bytes.Contains(src, []byte("std::sort")) || bytes.Contains(src, []byte("std::remove")) || bytes.Contains(src, []byte("std::min_element")) || bytes.Contains(src, []byte("std::max_element")) || bytes.Contains(src, []byte("std::unique")) || bytes.Contains(src, []byte("std::find")) {
		add("#include <algorithm>")
	}
	if bytes.Contains(src, []byte("std::accumulate")) {
		add("#include <numeric>")
	}
	if bytes.Contains(src, []byte("std::pair")) {
		add("#include <utility>")
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
		out.WriteString("template<typename T> void __json(const std::vector<T>& v){ std::cout<<\"[\"; bool first=true; for(const auto&x:v){ if(!first) std::cout<<\",\"; first=false; __json(x);} std::cout<<\"]\"; }\n")
		out.WriteString("template<typename K,typename V> void __json(const std::map<K,V>& m){ std::cout<<\"{\"; bool first=true; for(const auto&kv:m){ if(!first) std::cout<<\",\"; first=false; __json(kv.first); std::cout<<\":\"; __json(kv.second);} std::cout<<\"}\"; }\n")
		out.WriteString("template<typename K,typename V> void __json(const std::unordered_map<K,V>& m){ std::cout<<\"{\"; bool first=true; for(const auto&kv:m){ if(!first) std::cout<<\",\"; first=false; __json(kv.first); std::cout<<\":\"; __json(kv.second);} std::cout<<\"}\"; }\n")
		out.WriteByte('\n')
	}
	out.Write(src)
	return FormatCPP(out.Bytes()), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	c.funParams[fn.Name] = len(fn.Params)
	c.writeIndent()
	c.buf.WriteString("auto ")
	c.buf.WriteString(fn.Name)
	c.buf.WriteString("(")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString("auto ")
		c.buf.WriteString(p.Name)
	}
	c.buf.WriteString(") {\n")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStmt(st *parser.Statement) error {
	switch {
	case st.Type != nil:
		return c.compileTypeDecl(st.Type)
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

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Members) == 0 {
		return nil
	}
	c.writeln(fmt.Sprintf("struct %s {", t.Name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			typ, err := c.compileType(m.Field.Type)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s %s;", typ, m.Field.Name))
		}
	}
	c.indent--
	c.writeln("};")
	return nil
}

func (c *Compiler) compileLet(st *parser.LetStmt) error {
	var exprStr string
	if st.Value != nil {
		var err error
		exprStr, err = c.compileExpr(st.Value)
		if err != nil {
			return err
		}
	}

	typ, err := c.compileType(st.Type)
	if err != nil {
		return err
	}
	if st.Type == nil {
		if et := extractVectorElemType(exprStr); et != "" {
			typ = fmt.Sprintf("std::vector<%s>", et)
		}
	}

	c.writeIndent()
	c.buf.WriteString(typ)
	c.buf.WriteByte(' ')
	c.buf.WriteString(st.Name)
	if st.Value != nil {
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
		if inferred == "" && c.isVectorExpr(st.Value) {
			inferred = "vector"
		}
		c.vars[st.Name] = inferred
	}
	if s := extractVectorStruct(exprStr); s != "" {
		c.varStruct[st.Name] = s
	} else if t := c.varStruct[exprStr]; t != "" {
		c.varStruct[st.Name] = t
	}
	if et := extractVectorElemType(exprStr); et != "" {
		c.elemType[st.Name] = et
	} else if t := c.elemType[exprStr]; t != "" {
		c.elemType[st.Name] = t
	}
	return nil
}

func (c *Compiler) compileVar(st *parser.VarStmt) error {
	var exprStr string
	if st.Value != nil {
		var err error
		exprStr, err = c.compileExpr(st.Value)
		if err != nil {
			return err
		}
	}

	typ, err := c.compileType(st.Type)
	if err != nil {
		return err
	}
	if st.Type == nil {
		if et := extractVectorElemType(exprStr); et != "" {
			typ = fmt.Sprintf("std::vector<%s>", et)
		}
	}

	c.writeIndent()
	c.buf.WriteString(typ)
	c.buf.WriteByte(' ')
	c.buf.WriteString(st.Name)
	if st.Value != nil {
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
		if inferred == "" && c.isVectorExpr(st.Value) {
			inferred = "vector"
		}
		c.vars[st.Name] = inferred
	}
	if s := extractVectorStruct(exprStr); s != "" {
		c.varStruct[st.Name] = s
	} else if t := c.varStruct[exprStr]; t != "" {
		c.varStruct[st.Name] = t
	}
	if et := extractVectorElemType(exprStr); et != "" {
		c.elemType[st.Name] = et
	} else if t := c.elemType[exprStr]; t != "" {
		c.elemType[st.Name] = t
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
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	c.buf.WriteString(name + " = " + expr + ";\n")
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if st.Else != nil {
		c.writeln("else {")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
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
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
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
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; ++%s) {", st.Name, startExpr, st.Name, endExpr, st.Name))
	} else {
		c.writeln(fmt.Sprintf("for (auto %s : %s) {", st.Name, src))
		if t := c.varStruct[src]; t != "" {
			c.varStruct[st.Name] = t
		}
		if et := c.elemType[src]; et != "" {
			if strings.Contains(et, "std::string") {
				c.vars[st.Name] = "string"
			}
		}
		if c.vars[src] == "map" {
			c.vars[st.Name] = "pair"
		}
	}
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
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
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	// expectations are ignored in generated C++
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

func (c *Compiler) compilePrint(args []*parser.Expr) error {
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
		if i > 0 {
			c.buf.WriteString("std::cout << ' '; ")
		}
		switch typ {
		case "vector":
			tmp := c.newTmp()
			c.buf.WriteString("auto " + tmp + " = " + s + "; ")
			if et := c.elemType[s]; et != "" && strings.HasPrefix(et, "__struct") {
				c.buf.WriteString("for(size_t i=0;i<" + tmp + ".size();++i){ if(i) std::cout<<' '; std::cout << \"<struct>\"; } ")
			} else {
				c.buf.WriteString("for(size_t i=0;i<" + tmp + ".size();++i){ if(i) std::cout<<' '; std::cout << std::boolalpha << " + tmp + "[i]; } ")
			}
		case "bool":
			c.buf.WriteString("std::cout << std::boolalpha << (" + s + "); ")
		case "pair":
			c.buf.WriteString("std::cout << std::boolalpha << " + s + ".first << ' ' << " + s + ".second; ")
		default:
			c.buf.WriteString("std::cout << std::boolalpha << " + s + "; ")
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
					combined = fmt.Sprintf("(%s %s %s)", l, ops[i], r)
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
				expr = fmt.Sprintf("([&](auto v){ v.push_back(%s); return v; })(%s)", args[1], args[0])
			} else if base == "sum" && len(args) == 1 {
				expr = fmt.Sprintf("([&](auto v){ return std::accumulate(v.begin(), v.end(), 0); })(%s)", args[0])
			} else if base == "avg" && len(args) == 1 {
				expr = fmt.Sprintf("([&](auto v){ int s=0; for(auto x:v) s+=x; return v.empty()?0:(double)s/v.size(); })(%s)", args[0])
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
				expr = fmt.Sprintf("std::string(%s).substr(%s, (%s)-(%s))", expr, start, end, start)
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("unsupported empty index")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("%s[%s]", expr, idx)
			}
		} else if op.Field != nil {
			expr = fmt.Sprintf("%s.%s", expr, op.Field.Name)
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
		elemType := "int"
		if len(elems) > 0 {
			elemType = fmt.Sprintf("decltype(%s)", elems[0])
		}
		return fmt.Sprintf("std::vector<%s>{%s}", elemType, strings.Join(elems, ", ")), nil
	case p.Map != nil:
		names := []string{}
		vals := []string{}
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
		if simple {
			sig := strings.Join(names, ",")
			info, ok := c.structMap[sig]
			if !ok {
				c.structCount++
				info = &structInfo{Name: fmt.Sprintf("__struct%d", c.structCount), Fields: append([]string(nil), names...)}
				c.structMap[sig] = info
				var def strings.Builder
				def.WriteString("struct " + info.Name + " {")
				for i, n := range names {
					ftype := fmt.Sprintf("decltype(%s)", vals[i])
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
					def.WriteString(ftype + " " + n + "; ")
				}
				def.WriteString("};")
				c.headerWriteln(def.String())
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
			} else if t := structLiteralType(vals[0]); t != "" {
				valType = t
			} else if c.vars[vals[0]] == "string" {
				valType = "std::string"
			} else {
				valType = fmt.Sprintf("decltype(%s)", vals[0])
			}
		}
		pairs := []string{}
		for i := range keys {
			pairs = append(pairs, fmt.Sprintf("{%s, %s}", keys[i], vals[i]))
		}
		return fmt.Sprintf("std::unordered_map<%s,%s>{%s}", keyType, valType, strings.Join(pairs, ", ")), nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Selector != nil:
		name := p.Selector.Root
		if alias, ok := c.aliases[name]; ok && len(p.Selector.Tail) == 0 {
			name = alias
		}
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return name, nil
	case p.Call != nil:
		name := p.Call.Func
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
				return fmt.Sprintf("([&](auto v){ v.push_back(%s); return v; })(%s)", args[1], args[0]), nil
			}
		case "sum":
			if len(args) == 1 {
				return fmt.Sprintf("([&](auto v){ return std::accumulate(v.begin(), v.end(), 0); })(%s)", args[0]), nil
			}
		case "avg":
			if len(args) == 1 {
				return fmt.Sprintf("([&](auto v){ int s=0; for(auto x:v) s+=x; return v.empty()?0:(double)s/v.size(); })(%s)", args[0]), nil
			}
		case "count":
			if len(args) == 1 {
				return fmt.Sprintf("((int)%s.size())", args[0]), nil
			}
		case "exists":
			if len(args) == 1 {
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
		case "json":
			if len(args) == 1 {
				c.usesJSON = true
				return fmt.Sprintf("(__json(%s))", args[0]), nil
			}
		}
		if n, ok := c.funParams[name]; ok && len(args) < n {
			return c.partialApply(name, args, n), nil
		}
		return name + "(" + strings.Join(args, ", ") + ")", nil
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
	buf.WriteString("([&]() { auto __v = ")
	buf.WriteString(target)
	buf.WriteString("; ")
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

func (c *Compiler) compileStructLiteral(sl *parser.StructLiteral) (string, error) {
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
		for v, t := range c.varStruct {
			if t == "" {
				continue
			}
			if strings.Contains(val, v+".") {
				rep := fmt.Sprintf("std::declval<%s>().", t)
				ftype = strings.ReplaceAll(ftype, v+".", rep)
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
				if t := c.varStruct[v]; t != "" {
					if idx := strings.Index(t, "{"); idx != -1 {
						t = t[:idx]
					}
					ftype = fmt.Sprintf("decltype(std::declval<%s>().%s)", t, fld)
				}
			}
		} else if c.vars[val] == "string" {
			ftype = "std::string"
		}
		fieldTypes[i] = ftype
		inits[i] = val
	}
	sig := sl.Name + ":" + strings.Join(fieldNames, ",")
	info, ok := c.structMap[sig]
	if !ok {
		c.structCount++
		info = &structInfo{Name: fmt.Sprintf("__struct%d", c.structCount), Fields: append([]string(nil), fieldNames...)}
		c.structMap[sig] = info
		var def strings.Builder
		def.WriteString("struct " + info.Name + " {")
		for i := range fieldNames {
			def.WriteString(fieldTypes[i] + " " + fieldNames[i] + "; ")
		}
		def.WriteString("};")
		c.headerWriteln(def.String())
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
	for _, j := range q.Joins {
		if j.Side != nil && *j.Side != "left" {
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
		case strings.HasPrefix(et, "__struct"):
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
			case strings.HasPrefix(et, "__struct"):
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
			case strings.HasPrefix(et, "__struct"):
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
			rep := fmt.Sprintf("std::declval<%s>().", t)
			itemTypeExpr = strings.ReplaceAll(itemTypeExpr, v+".", rep)
		}
	}
	if matched, _ := regexp.MatchString(`[A-Za-z_][A-Za-z0-9_]*\\.`, itemTypeExpr); matched {
		itemTypeExpr = "0"
	}
	itemType := fmt.Sprintf("decltype(%s)", itemTypeExpr)
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
	} else if t := structLiteralType(val); t != "" {
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
			itemType = fmt.Sprintf("decltype(std::declval<%s>().%s)", t, fld)
		}
	} else if t := c.elemType[val]; t != "" {
		itemType = t
	}
	key := ""
	keyType := ""
	if q.Sort != nil {
		key, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		keyType = fmt.Sprintf("decltype(%s)", key)
		if vt := c.varStruct[q.Var]; vt != "" {
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
	for k, v := range backup {
		if v == "" {
			delete(c.varStruct, k)
		} else {
			c.varStruct[k] = v
		}
	}

	var buf strings.Builder
	buf.WriteString("([&]() {\n")
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
				buf.WriteString("if (!(" + where + ")) continue;\n")
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
		switch side {
		case "left":
			indent(indentLevel)
			buf.WriteString("{ bool __matched" + strconv.Itoa(i) + " = false;\n")
			indentLevel++
			indent(indentLevel)
			buf.WriteString("for (auto " + q.Joins[i].Var + " : " + joinSrcs[i] + ") {\n")
			indentLevel++
			indent(indentLevel)
			buf.WriteString("if (!(" + joinOns[i] + ")) continue;\n")
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
			buf.WriteString("if (!(" + joinOns[i] + ")) continue;\n")
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
		buf.WriteString("    std::sort(__items.begin(), __items.end(), [](auto &a, auto &b){ return a.first < b.first; });\n")
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
	if strings.HasPrefix(itemType, "__struct") {
		c.varStruct[res] = itemType
		c.elemType[res] = itemType
	}
	return res, nil
}

func (c *Compiler) compileGroupedQueryExpr(q *parser.QueryExpr) (string, error) {
	for _, j := range q.Joins {
		if j.Side != nil && *j.Side != "left" {
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
		case strings.HasPrefix(et, "__struct"):
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
			case strings.HasPrefix(et, "__struct"):
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
			case strings.HasPrefix(et, "__struct"):
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
	if t := structLiteralType(keyExpr); t != "" {
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
			keyType = fmt.Sprintf("decltype(std::declval<%s>().%s)", t, fld)
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
	groupStruct := fmt.Sprintf("__struct%d", c.structCount+1)
	c.structCount++
	c.headerWriteln(fmt.Sprintf("struct %s { %s key; std::vector<%s> items; };", groupStruct, keyType, itemStruct))
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
	valExpr, err := c.compileExpr(q.Select)
	if err != nil {
		c.aliases = oldAliases
		return "", err
	}
	itemTypeExpr := valExpr
	if strings.Contains(valExpr, q.Group.Name) {
		itemTypeExpr = strings.ReplaceAll(valExpr, q.Group.Name, fmt.Sprintf("std::declval<%s>()", groupStruct))
	}
	itemType := fmt.Sprintf("decltype(%s)", itemTypeExpr)
	if selectIsGroup {
		itemType = groupStruct
	} else if t := structLiteralType(valExpr); t != "" {
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
			return "", err
		}
	}
	c.aliases = oldAliases
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

	var buf strings.Builder
	buf.WriteString("([&]() {\n")
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
			itemInit := itemStruct + "{" + strings.Join(itemVars, ", ") + "}"
			buf.WriteString("bool __found = false;\n")
			indent(indentLevel)
			buf.WriteString("for (auto &__g : __groups) { if (__g.key == __key) { __g.items.push_back(" + itemInit + "); __found = true; break; } }\n")
			indent(indentLevel)
			buf.WriteString("if (!__found) { __groups.push_back(" + groupStruct + "{__key, std::vector<" + itemStruct + ">{" + itemInit + "}}); }\n")
			return
		}
		side := ""
		if q.Joins[i].Side != nil {
			side = *q.Joins[i].Side
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
			buf.WriteString("if (!(" + joinOns[i] + ")) continue;\n")
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
			buf.WriteString("if (!(" + joinOns[i] + ")) continue;\n")
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
		buf.WriteString("if (!(" + havingExpr + ")) continue;\n")
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
		buf.WriteString("    std::sort(__items.begin(), __items.end(), [](auto &a, auto &b){ return a.first < b.first; });\n")
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
	res := buf.String()
	if strings.HasPrefix(itemType, "__struct") {
		c.varStruct[res] = itemType
		c.elemType[res] = itemType
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
		default:
			return *t.Simple, nil
		}
	}
	return "auto", nil
}

func (c *Compiler) inferType(expr string) string {
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
	return ""
}

// inferExprType returns a simple C++ type for the given expression string.
// It is used when generating struct field declarations.
func inferExprType(expr string) string {
	if strings.ContainsAny(expr, "<>=!&|") {
		return "bool"
	}
	if strings.Contains(expr, "true") || strings.Contains(expr, "false") {
		return "bool"
	}
	return ""
}

func extractVectorStruct(expr string) string {
	start := strings.Index(expr, "std::vector<decltype(")
	if start == -1 {
		return ""
	}
	start += len("std::vector<decltype(")
	end := strings.Index(expr[start:], "{")
	if end == -1 {
		return ""
	}
	inner := expr[start : start+end]
	if strings.HasPrefix(inner, "__struct") {
		return inner
	}
	return ""
}

func extractVectorElemType(expr string) string {
	if !strings.HasPrefix(expr, "std::vector<") {
		return ""
	}
	inner := expr[len("std::vector<"):]
	idx := strings.Index(inner, ">")
	if idx == -1 {
		return ""
	}
	typ := inner[:idx]
	if strings.HasPrefix(typ, "decltype(") {
		texpr := strings.TrimSuffix(strings.TrimPrefix(typ, "decltype("), ")")
		if strings.HasPrefix(texpr, "__struct") {
			if idx := strings.Index(texpr, "{"); idx != -1 {
				texpr = texpr[:idx]
			}
			return texpr
		}
		if strings.Contains(texpr, "std::string") {
			return "std::string"
		}
		if strings.Contains(texpr, "true") || strings.Contains(texpr, "false") {
			return "bool"
		}
		if _, err := strconv.Atoi(texpr); err == nil {
			return "int"
		}
	}
	if strings.Contains(typ, "std::string") {
		return "std::string"
	}
	if strings.Contains(typ, "bool") {
		return "bool"
	}
	return ""
}

func structLiteralType(expr string) string {
	if strings.HasPrefix(expr, "__struct") {
		if idx := strings.Index(expr, "{"); idx != -1 {
			return expr[:idx]
		}
	}
	return ""
}

func (c *Compiler) structFromVars(names []string) string {
	sig := "vars:" + strings.Join(names, ",")
	if info, ok := c.structMap[sig]; ok {
		return info.Name
	}
	c.structCount++
	info := &structInfo{Name: fmt.Sprintf("__struct%d", c.structCount), Fields: append([]string(nil), names...)}
	c.structMap[sig] = info
	var def strings.Builder
	def.WriteString("struct " + info.Name + " {")
	for _, n := range names {
		fieldType := "decltype(" + sanitizeName(n) + ")"
		if t := c.varStruct[n]; t != "" {
			if idx := strings.Index(t, "{"); idx != -1 {
				t = t[:idx]
			}
			fieldType = t
		}
		def.WriteString(fieldType + " " + sanitizeName(n) + "; ")
	}
	def.WriteString("};")
	c.headerWriteln(def.String())
	return info.Name
}

// compileFunExpr converts an anonymous function expression to a C++ lambda.
func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	return c.compileLambda(fn.Params, fn.ExprBody, fn.BlockBody)
}

// compileLambda builds a C++ lambda from the given parameters and body.
func (c *Compiler) compileLambda(params []*parser.Param, exprBody *parser.Expr, stmts []*parser.Statement) (string, error) {
	var buf strings.Builder
	buf.WriteString("[=](")
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

	sub := &Compiler{indent: 1, tmp: c.tmp, vars: map[string]string{}, aliases: map[string]string{}}
	for _, p := range params {
		if p.Type != nil {
			typ, _ := c.compileType(p.Type)
			if typ == "std::string" {
				sub.vars[p.Name] = "string"
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
