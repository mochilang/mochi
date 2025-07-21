//go:build slow

package ctrans

import (
	"bytes"
	"fmt"
	"io"
	"os/exec"
	"sort"
	"strconv"
	"strings"
	"time"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/data"
	"mochi/types"
)

var (
	constLists     map[string]*ListLit
	constStrings   map[string]string
	structTypes    map[string]types.StructType
	currentEnv     *types.Env
	funcParamTypes map[string][]string
	structCounter  int
	currentVarName string
)

const version = "0.10.32"

// --- Simple C AST ---

type Program struct {
	Globals   []Stmt
	Functions []*Function
}

type Param struct {
	Type string
	Name string
}

type Function struct {
	Name   string
	Params []Param
	Return string
	Body   []Stmt
}

type Stmt interface {
	emit(io.Writer, int)
}

type PrintStmt struct {
	Args  []Expr
	Types []string
}

func (p *PrintStmt) emit(w io.Writer, indent int) {
	var format []string
	var exprs []Expr
	for i, a := range p.Args {
		switch v := a.(type) {
		case *ListLit:
			for j, e := range v.Elems {
				lsep := " "
				if j == len(v.Elems)-1 && i == len(p.Args)-1 {
					lsep = "\\n"
				} else if j == len(v.Elems)-1 {
					lsep = " "
				}
				writeIndent(w, indent)
				io.WriteString(w, "printf(\"")
				if exprIsString(e) {
					io.WriteString(w, "%s")
				} else {
					io.WriteString(w, "%d")
				}
				io.WriteString(w, lsep+"\", ")
				e.emitExpr(w)
				io.WriteString(w, ");\n")
			}
		default:
			if p.Types[i] == "string" || exprIsString(a) {
				format = append(format, "%s")
			} else {
				format = append(format, "%d")
			}
			exprs = append(exprs, a)
		}
	}
	if len(format) == 1 {
		writeIndent(w, indent)
		if lit, ok := exprs[0].(*StringLit); ok {
			fmt.Fprintf(w, "puts(\"%s\");\n", escape(lit.Value))
			return
		}
		if p.Types[0] == "string" || exprIsString(exprs[0]) {
			io.WriteString(w, "puts(")
			exprs[0].emitExpr(w)
			io.WriteString(w, ");\n")
			return
		}
		io.WriteString(w, "printf(\"%d\\n\", ")
		exprs[0].emitExpr(w)
		io.WriteString(w, ");\n")
		return
	}
	if len(format) > 0 {
		writeIndent(w, indent)
		fmt.Fprintf(w, "printf(\"%s\\n\"", strings.Join(format, " "))
		for _, e := range exprs {
			io.WriteString(w, ", ")
			e.emitExpr(w)
		}
		io.WriteString(w, ");\n")
	}
}

type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "break;\n")
}

type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "continue;\n")
}

type CallStmt struct {
	Func string
	Args []Expr
	Type string
}

type ReturnStmt struct {
	Expr Expr
}

type DeclStmt struct {
	Name  string
	Value Expr
	Type  string
}

type AssignStmt struct {
	Name    string
	Indexes []Expr
	Fields  []string
	Value   Expr
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

type ForStmt struct {
	Var      string
	Start    Expr
	End      Expr
	List     []Expr
	ElemType string
	Body     []Stmt
}

func (c *CallStmt) emit(w io.Writer, indent int) {
	if c.Func == "print" && len(c.Args) == 1 {
		writeIndent(w, indent)
		if c.Type == "string" {
			if lit, ok := c.Args[0].(*StringLit); ok {
				io.WriteString(w, "puts(")
				lit.emitExpr(w)
				io.WriteString(w, ");\n")
			} else {
				io.WriteString(w, "printf(\"%s\\n\", ")
				c.Args[0].emitExpr(w)
				io.WriteString(w, ");\n")
			}
		} else {
			switch arg := c.Args[0].(type) {
			case *StringLit:
				fmt.Fprintf(w, "printf(\"%s\\n\");\n", escape(arg.Value))
			default:
				io.WriteString(w, "printf(\"%d\\n\", ")
				arg.emitExpr(w)
				io.WriteString(w, ");\n")
			}
		}
		return
	}
}

func (r *ReturnStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "return")
	if r.Expr != nil {
		io.WriteString(w, " ")
		r.Expr.emitExpr(w)
	} else {
		io.WriteString(w, " 0")
	}
	io.WriteString(w, ";\n")
}

func (d *DeclStmt) emit(w io.Writer, indent int) {
	typ := d.Type
	if typ == "" {
		typ = "int"
	}
	writeIndent(w, indent)
	if strings.HasSuffix(typ, "[][]") {
		base := strings.TrimSuffix(typ, "[][]")
		rows, cols := 0, 0
		if list, ok := d.Value.(*ListLit); ok {
			rows = len(list.Elems)
			if rows > 0 {
				if sub, ok2 := list.Elems[0].(*ListLit); ok2 {
					cols = len(sub.Elems)
				}
			}
		}
		fmt.Fprintf(w, "%s %s[%d][%d]", base, d.Name, rows, cols)
	} else if strings.HasSuffix(typ, "[]") {
		io.WriteString(w, strings.TrimSuffix(typ, "[]"))
		io.WriteString(w, " ")
		io.WriteString(w, d.Name)
		io.WriteString(w, "[]")
	} else {
		io.WriteString(w, typ)
		io.WriteString(w, " ")
		io.WriteString(w, d.Name)
	}
	if d.Value != nil {
		io.WriteString(w, " = ")
		d.Value.emitExpr(w)
	} else {
		if typ == "const char*" {
			io.WriteString(w, " = \"\"")
		} else {
			io.WriteString(w, " = 0")
		}
	}
	io.WriteString(w, ";\n")
}

func (a *AssignStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, a.Name)
	for _, idx := range a.Indexes {
		io.WriteString(w, "[")
		idx.emitExpr(w)
		io.WriteString(w, "]")
	}
	for _, f := range a.Fields {
		io.WriteString(w, ".")
		io.WriteString(w, f)
	}
	io.WriteString(w, " = ")
	if a.Value != nil {
		a.Value.emitExpr(w)
	}
	io.WriteString(w, ";\n")
}

func (ws *WhileStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "while (")
	if ws.Cond != nil {
		ws.Cond.emitExpr(w)
	}
	io.WriteString(w, ") {\n")
	for _, s := range ws.Body {
		s.emit(w, indent+1)
	}
	writeIndent(w, indent)
	io.WriteString(w, "}\n")
}

func (f *ForStmt) emit(w io.Writer, indent int) {
	if len(f.List) > 0 {
		arrName := fmt.Sprintf("%s_arr", f.Var)
		lenName := fmt.Sprintf("%s_len", f.Var)
		writeIndent(w, indent)
		io.WriteString(w, "{\n")
		writeIndent(w, indent+1)
		typ := f.ElemType
		if typ == "" {
			typ = "int"
		}
		fmt.Fprintf(w, "%s %s[] = {", typ, arrName)
		for i, e := range f.List {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			e.emitExpr(w)
		}
		io.WriteString(w, "};\n")
		writeIndent(w, indent+1)
		fmt.Fprintf(w, "size_t %s = sizeof(%s) / sizeof(%s[0]);\n", lenName, arrName, arrName)
		writeIndent(w, indent+1)
		io.WriteString(w, "for (size_t i = 0; i < ")
		io.WriteString(w, lenName)
		io.WriteString(w, "; i++) {\n")
		writeIndent(w, indent+2)
		fmt.Fprintf(w, "%s %s = %s[i];\n", typ, f.Var, arrName)
		for _, s := range f.Body {
			s.emit(w, indent+2)
		}
		writeIndent(w, indent+1)
		io.WriteString(w, "}\n")
		writeIndent(w, indent)
		io.WriteString(w, "}\n")
		return
	}
	writeIndent(w, indent)
	io.WriteString(w, "for (int ")
	io.WriteString(w, f.Var)
	io.WriteString(w, " = ")
	if f.Start != nil {
		f.Start.emitExpr(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Var)
	io.WriteString(w, " < ")
	if f.End != nil {
		f.End.emitExpr(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Var)
	io.WriteString(w, "++) {\n")
	for _, s := range f.Body {
		s.emit(w, indent+1)
	}
	writeIndent(w, indent)
	io.WriteString(w, "}\n")
}

func (i *IfStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "if (")
	if i.Cond != nil {
		i.Cond.emitExpr(w)
	}
	io.WriteString(w, ") {\n")
	for _, s := range i.Then {
		s.emit(w, indent+1)
	}
	writeIndent(w, indent)
	io.WriteString(w, "}")
	if len(i.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, s := range i.Else {
			s.emit(w, indent+1)
		}
		writeIndent(w, indent)
		io.WriteString(w, "}")
	}
	io.WriteString(w, "\n")
}

type Expr interface{ emitExpr(io.Writer) }

type StringLit struct{ Value string }

func (s *StringLit) emitExpr(w io.Writer) {
	fmt.Fprintf(w, "\"%s\"", escape(s.Value))
}

type IntLit struct{ Value int }

func (i *IntLit) emitExpr(w io.Writer) {
	fmt.Fprintf(w, "%d", i.Value)
}

type FloatLit struct{ Value float64 }

func (f *FloatLit) emitExpr(w io.Writer) {
	fmt.Fprintf(w, "%g", f.Value)
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func exprIsFloat(e Expr) bool {
	switch v := e.(type) {
	case *FloatLit:
		return true
	case *UnaryExpr:
		return exprIsFloat(v.Expr)
	case *BinaryExpr:
		if v.Op == "+" || v.Op == "-" || v.Op == "*" || v.Op == "/" {
			return exprIsFloat(v.Left) || exprIsFloat(v.Right)
		}
	}
	if _, ok := evalFloat(e); ok {
		return true
	}
	return false
}

func (u *UnaryExpr) emitExpr(w io.Writer) {
	io.WriteString(w, u.Op)
	io.WriteString(w, "(")
	if u.Expr != nil {
		u.Expr.emitExpr(w)
	}
	io.WriteString(w, ")")
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emitExpr(w io.Writer) {
	io.WriteString(w, "{ ")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emitExpr(w)
	}
	io.WriteString(w, " }")
}

type StructField struct {
	Name  string
	Value Expr
}

type StructLit struct {
	Name   string
	Fields []StructField
}

func (s *StructLit) emitExpr(w io.Writer) {
	fmt.Fprintf(w, "(%s){", s.Name)
	for i, f := range s.Fields {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		fmt.Fprintf(w, ".%s = ", f.Name)
		if f.Value != nil {
			f.Value.emitExpr(w)
		}
	}
	io.WriteString(w, "}")
}

type MapItem struct {
	Key   Expr
	Value Expr
}

type MapLit struct{ Items []MapItem }

func (m *MapLit) emitExpr(w io.Writer) {
	io.WriteString(w, "{ ")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, "{")
		it.Key.emitExpr(w)
		io.WriteString(w, ": ")
		it.Value.emitExpr(w)
		io.WriteString(w, "}")
	}
	io.WriteString(w, " }")
}

type FieldExpr struct {
	Target Expr
	Name   string
}

func (f *FieldExpr) emitExpr(w io.Writer) {
	if v, ok := f.Target.(*VarRef); ok && v.Name == "math" {
		switch f.Name {
		case "pi":
			io.WriteString(w, "3.141592653589793")
			return
		case "e":
			io.WriteString(w, "2.718281828459045")
			return
		case "sqrt", "pow", "sin", "log":
			io.WriteString(w, f.Name)
			return
		}
	}
	f.Target.emitExpr(w)
	io.WriteString(w, ".")
	io.WriteString(w, f.Name)
}

type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (i *IndexExpr) emitExpr(w io.Writer) {
	if exprIsString(i.Target) {
		io.WriteString(w, "(const char[]){")
		i.Target.emitExpr(w)
		io.WriteString(w, "[")
		i.Index.emitExpr(w)
		io.WriteString(w, "], 0}")
		return
	}
	i.Target.emitExpr(w)
	io.WriteString(w, "[")
	i.Index.emitExpr(w)
	io.WriteString(w, "]")
}

type VarRef struct{ Name string }

func (v *VarRef) emitExpr(w io.Writer) {
	switch v.Name {
	case "math.pi":
		io.WriteString(w, "3.141592653589793")
	case "math.e":
		io.WriteString(w, "2.718281828459045")
	default:
		io.WriteString(w, v.Name)
	}
}

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emitExpr(w io.Writer) {
	if c.Func == "contains" && len(c.Args) == 2 {
		io.WriteString(w, "strstr(")
		c.Args[0].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[1].emitExpr(w)
		io.WriteString(w, ") != NULL")
		return
	}
	fn := c.Func
	if strings.HasPrefix(fn, "math.") {
		fn = strings.TrimPrefix(fn, "math.")
	}
	io.WriteString(w, fn)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if types, ok := funcParamTypes[c.Func]; ok && i < len(types) {
			if strings.HasPrefix(types[i], "struct ") && strings.HasSuffix(types[i], "*") {
				io.WriteString(w, "&")
			}
		}
		a.emitExpr(w)
	}
	io.WriteString(w, ")")
}

type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emitExpr(w io.Writer) {
	io.WriteString(w, "(")
	if c.Cond != nil {
		c.Cond.emitExpr(w)
	}
	io.WriteString(w, " ? ")
	if c.Then != nil {
		c.Then.emitExpr(w)
	}
	io.WriteString(w, " : ")
	if c.Else != nil {
		c.Else.emitExpr(w)
	}
	io.WriteString(w, ")")
}

func (b *BinaryExpr) emitExpr(w io.Writer) {
	if b.Op == "in" && exprIsString(b.Left) && exprIsString(b.Right) {
		io.WriteString(w, "strstr(")
		b.Right.emitExpr(w)
		io.WriteString(w, ", ")
		b.Left.emitExpr(w)
		io.WriteString(w, ") != NULL")
		return
	}
	if b.Op == "in" {
		if list, ok := evalList(b.Right); ok {
			io.WriteString(w, "(")
			if len(list.Elems) == 0 {
				io.WriteString(w, "0")
			} else {
				for i, e := range list.Elems {
					if i > 0 {
						io.WriteString(w, " || ")
					}
					if exprIsString(b.Left) || exprIsString(e) {
						io.WriteString(w, "strcmp(")
						b.Left.emitExpr(w)
						io.WriteString(w, ", ")
						e.emitExpr(w)
						io.WriteString(w, ") == 0")
					} else {
						b.Left.emitExpr(w)
						io.WriteString(w, " == ")
						e.emitExpr(w)
					}
				}
			}
			io.WriteString(w, ")")
			return
		}
	}
	if (exprIsString(b.Left) || exprIsString(b.Right)) &&
		(b.Op == "==" || b.Op == "!=" || b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=") {
		io.WriteString(w, "strcmp(")
		b.Left.emitExpr(w)
		io.WriteString(w, ", ")
		b.Right.emitExpr(w)
		io.WriteString(w, ") ")
		switch b.Op {
		case "==":
			io.WriteString(w, "== 0")
		case "!=":
			io.WriteString(w, "!= 0")
		case "<":
			io.WriteString(w, "< 0")
		case "<=":
			io.WriteString(w, "<= 0")
		case ">":
			io.WriteString(w, "> 0")
		case ">=":
			io.WriteString(w, ">= 0")
		case "in":
			io.WriteString(w, "!= 0")
		}
		return
	}
	if _, ok := b.Left.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		b.Left.emitExpr(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emitExpr(w)
	}
	fmt.Fprintf(w, " %s ", b.Op)
	if _, ok := b.Right.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		b.Right.emitExpr(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emitExpr(w)
	}
}

func escape(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, "\"", "\\\"")
	return s
}

func writeIndent(w io.Writer, n int) {
	for i := 0; i < n; i++ {
		io.WriteString(w, "    ")
	}
}

func gitTimestamp() string {
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			return t.Format("2006-01-02 15:04 -0700")
		}
	}
	return time.Now().Format("2006-01-02 15:04 -0700")
}

// repoRoot attempts to locate the repository root containing go.mod.

// Emit generates C source from AST.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	ts := gitTimestamp()
	fmt.Fprintf(&buf, "// Generated by Mochi %s on %s\n", strings.TrimSpace(version), ts)
	buf.WriteString("#include <stdio.h>\n")
	buf.WriteString("#include <string.h>\n")
	buf.WriteString("#include <math.h>\n\n")
	names := make([]string, 0, len(structTypes))
	for name := range structTypes {
		names = append(names, name)
	}
	sort.Strings(names)
	ordered := make([]string, 0, len(names))
	resolved := map[string]bool{}
	for len(ordered) < len(names) {
		progress := false
		for _, name := range names {
			if resolved[name] {
				continue
			}
			st := structTypes[name]
			depsOK := true
			for _, ft := range st.Fields {
				if sft, ok := ft.(types.StructType); ok {
					if sft.Name != name && !resolved[sft.Name] {
						depsOK = false
						break
					}
				}
			}
			if depsOK {
				ordered = append(ordered, name)
				resolved[name] = true
				progress = true
			}
		}
		if !progress {
			for _, name := range names {
				if !resolved[name] {
					ordered = append(ordered, name)
					resolved[name] = true
				}
			}
		}
	}
	for _, name := range ordered {
		fmt.Fprintf(&buf, "typedef struct %s %s;\n", name, name)
	}
	if len(ordered) > 0 {
		buf.WriteString("\n")
	}
	for _, name := range ordered {
		st := structTypes[name]
		fmt.Fprintf(&buf, "struct %s {\n", name)
		for _, field := range st.Order {
			typ := "int"
			if ft, ok := st.Fields[field]; ok {
				switch ft := ft.(type) {
				case types.StringType:
					typ = "const char*"
				case types.BoolType:
					typ = "int"
				case types.StructType:
					typ = ft.Name
				case types.ListType:
					if _, ok := ft.Elem.(types.StringType); ok {
						typ = "const char*[]"
					} else {
						typ = "int[]"
					}
				}
			}
			fmt.Fprintf(&buf, "    %s %s;\n", typ, field)
		}
		fmt.Fprintf(&buf, "};\n\n")
	}
	for _, g := range p.Globals {
		g.emit(&buf, 0)
	}
	if len(p.Globals) > 0 {
		buf.WriteString("\n")
	}
	for i, f := range p.Functions {
		ret := f.Return
		if ret == "" {
			ret = "int"
		}
		buf.WriteString(ret)
		buf.WriteString(" ")
		buf.WriteString(f.Name)
		if f.Name == "main" && len(f.Params) == 0 {
			buf.WriteString("(void)")
		} else {
			buf.WriteString("(")
			for i, p := range f.Params {
				if i > 0 {
					buf.WriteString(", ")
				}
				if p.Type == "" {
					p.Type = "int"
				}
				buf.WriteString(p.Type)
				buf.WriteString(" ")
				buf.WriteString(p.Name)
			}
			buf.WriteString(")")
		}
		buf.WriteString(" {\n")
		for _, s := range f.Body {
			s.emit(&buf, 1)
		}
		if f.Name == "main" {
			writeIndent(&buf, 1)
			buf.WriteString("return 0;\n")
		}
		buf.WriteString("}\n")
		if i < len(p.Functions)-1 {
			buf.WriteString("\n")
		}
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into a C AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	constLists = make(map[string]*ListLit)
	constStrings = make(map[string]string)
	structTypes = env.Structs()
	currentEnv = env
	funcParamTypes = make(map[string][]string)
	structCounter = 0
	p := &Program{}
	mainFn := &Function{Name: "main"}
	var globals []Stmt
	for _, st := range prog.Statements {
		if st.Fun != nil {
			body, err := compileStmts(env, st.Fun.Body)
			if err != nil {
				return nil, err
			}
			var params []Param
			var paramTypes []string
			for _, pa := range st.Fun.Params {
				typ := "int"
				if pa.Type != nil && pa.Type.Simple != nil && *pa.Type.Simple == "string" {
					typ = "const char*"
				} else if pa.Type != nil && pa.Type.Simple != nil {
					if _, ok := env.GetStruct(*pa.Type.Simple); ok {
						typ = *pa.Type.Simple
					}
				}
				paramTypes = append(paramTypes, typ)
				params = append(params, Param{Name: pa.Name, Type: typ})
			}
			funcParamTypes[st.Fun.Name] = paramTypes
			ret := "int"
			if st.Fun.Return != nil && st.Fun.Return.Simple != nil {
				switch *st.Fun.Return.Simple {
				case "string":
					ret = "const char*"
				case "float":
					ret = "double"
				default:
					if _, ok := env.GetStruct(*st.Fun.Return.Simple); ok {
						ret = *st.Fun.Return.Simple
					}
				}
			}
			p.Functions = append(p.Functions, &Function{Name: st.Fun.Name, Params: params, Return: ret, Body: body})
			continue
		}
		if st.Let != nil {
			if fn := detectFunExpr(st.Let.Value); fn != nil {
				fun, err := compileFunction(env, st.Let.Name, fn)
				if err != nil {
					return nil, err
				}
				p.Functions = append(p.Functions, fun)
				continue
			}
		}
		stmt, err := compileStmt(env, st)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			if st.Let != nil || st.Var != nil {
				var val Expr
				if st.Let != nil {
					val = convertExpr(st.Let.Value)
				} else if st.Var != nil {
					val = convertExpr(st.Var.Value)
				}
				if isConstExpr(val) {
					globals = append(globals, stmt)
				} else {
					mainFn.Body = append(mainFn.Body, stmt)
				}
			} else {
				mainFn.Body = append(mainFn.Body, stmt)
			}
		}
	}
	p.Functions = append(p.Functions, mainFn)
	p.Globals = globals
	return p, nil
}

func compileStmts(env *types.Env, list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		stmt, err := compileStmt(env, s)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			out = append(out, stmt)
		}
	}
	return out, nil
}

func compileStmt(env *types.Env, s *parser.Statement) (Stmt, error) {
	switch {
	case s.Expr != nil:
		call := s.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" {
			var args []Expr
			var typesList []string
			for _, a := range call.Args {
				ex := convertExpr(a)
				if ex == nil {
					return nil, fmt.Errorf("invalid print argument")
				}
				tname := ""
				if exprIsString(ex) {
					tname = "string"
				} else if _, ok := ex.(*FieldExpr); ok {
					ft := inferExprType(env, ex)
					if ft == "const char*" {
						tname = "string"
					}
				} else if v, ok := ex.(*VarRef); ok {
					if t, err := env.GetVar(v.Name); err == nil {
						if _, ok := t.(types.StringType); ok {
							tname = "string"
						}
					}
				}
				args = append(args, ex)
				typesList = append(typesList, tname)
			}
			return &PrintStmt{Args: args, Types: typesList}, nil
		}
	case s.Let != nil:
		currentVarName = s.Let.Name
		valExpr := convertExpr(s.Let.Value)
		currentVarName = ""
		declType := inferCType(env, s.Let.Name, valExpr)
		if list, ok := convertListExpr(s.Let.Value); ok {
			constLists[s.Let.Name] = &ListLit{Elems: list}
		} else {
			delete(constLists, s.Let.Name)
		}
		if strVal, ok := evalString(valExpr); ok {
			constStrings[s.Let.Name] = strVal
		} else {
			delete(constStrings, s.Let.Name)
		}
		if val, ok := valueFromExpr(valExpr); ok {
			env.SetValue(s.Let.Name, val, true)
		}
		if _, isMap := valExpr.(*MapLit); isMap {
			return nil, nil
		}
		return &DeclStmt{Name: s.Let.Name, Value: valExpr, Type: declType}, nil
	case s.Var != nil:
		currentVarName = s.Var.Name
		valExpr := convertExpr(s.Var.Value)
		currentVarName = ""
		declType := inferCType(env, s.Var.Name, valExpr)
		if list, ok := convertListExpr(s.Var.Value); ok {
			constLists[s.Var.Name] = &ListLit{Elems: list}
		} else {
			delete(constLists, s.Var.Name)
		}
		if strVal, ok := evalString(valExpr); ok {
			constStrings[s.Var.Name] = strVal
		} else {
			delete(constStrings, s.Var.Name)
		}
		if val, ok := valueFromExpr(valExpr); ok {
			env.SetValue(s.Var.Name, val, true)
		}
		if _, isMap := valExpr.(*MapLit); isMap {
			return nil, nil
		}
		return &DeclStmt{Name: s.Var.Name, Value: valExpr, Type: declType}, nil
	case s.Assign != nil:
		valExpr := convertExpr(s.Assign.Value)
		if list, ok := convertListExpr(s.Assign.Value); ok {
			constLists[s.Assign.Name] = &ListLit{Elems: list}
		} else {
			delete(constLists, s.Assign.Name)
		}
		if strVal, ok := evalString(valExpr); ok {
			constStrings[s.Assign.Name] = strVal
		} else {
			delete(constStrings, s.Assign.Name)
		}
		var idxs []Expr
		var fields []string
		simple := true
		for _, ix := range s.Assign.Index {
			if ix.Colon != nil || ix.End != nil || ix.Colon2 != nil || ix.Step != nil {
				simple = false
				break
			}
			ex := convertExpr(ix.Start)
			if ex == nil {
				simple = false
				break
			}
			idxs = append(idxs, ex)
		}
		for _, f := range s.Assign.Field {
			fields = append(fields, f.Name)
		}
		if !simple {
			return nil, fmt.Errorf("unsupported assignment")
		}
		if val, ok := valueFromExpr(valExpr); ok && len(idxs) == 0 && len(fields) == 0 {
			env.SetValue(s.Assign.Name, val, true)
		}
		return &AssignStmt{Name: s.Assign.Name, Indexes: idxs, Fields: fields, Value: valExpr}, nil
	case s.Return != nil:
		return &ReturnStmt{Expr: convertExpr(s.Return.Value)}, nil
	case s.While != nil:
		cond := convertExpr(s.While.Cond)
		body, err := compileStmts(env, s.While.Body)
		if err != nil {
			return nil, err
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case s.For != nil:
		body, err := compileStmts(env, s.For.Body)
		if err != nil {
			return nil, err
		}
		if s.For.RangeEnd != nil {
			start := convertExpr(s.For.Source)
			end := convertExpr(s.For.RangeEnd)
			return &ForStmt{Var: s.For.Name, Start: start, End: end, Body: body}, nil
		}
		list, ok := convertListExpr(s.For.Source)
		if !ok {
			ex := convertExpr(s.For.Source)
			if val, ok2 := valueFromExpr(ex); ok2 {
				if arr, ok3 := val.([]any); ok3 {
					var elems []Expr
					for _, it := range arr {
						e := anyToExpr(it)
						if e == nil {
							return nil, fmt.Errorf("unsupported for-loop")
						}
						elems = append(elems, e)
					}
					list = elems
					ok = true
				}
			}
		}
		if ok {
			elemType := "int"
			if len(list) > 0 {
				elemType = inferExprType(env, list[0])
			}
			return &ForStmt{Var: s.For.Name, List: list, ElemType: elemType, Body: body}, nil
		}
		return nil, fmt.Errorf("unsupported for-loop")
	case s.If != nil:
		return compileIfStmt(env, s.If)
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Test != nil:
		return nil, nil
	}
	return nil, nil
}

func compileIfStmt(env *types.Env, n *parser.IfStmt) (Stmt, error) {
	cond := convertExpr(n.Cond)
	thenBody, err := compileStmts(env, n.Then)
	if err != nil {
		return nil, err
	}
	var elseBody []Stmt
	if n.ElseIf != nil {
		s, err := compileIfStmt(env, n.ElseIf)
		if err != nil {
			return nil, err
		}
		elseBody = []Stmt{s}
	} else if len(n.Else) > 0 {
		elseBody, err = compileStmts(env, n.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenBody, Else: elseBody}, nil
}

func convertIfExpr(n *parser.IfExpr) Expr {
	if n == nil {
		return nil
	}
	cond := convertExpr(n.Cond)
	if cond == nil {
		return nil
	}
	thenExpr := convertExpr(n.Then)
	if thenExpr == nil {
		return nil
	}
	var elseExpr Expr
	if n.ElseIf != nil {
		elseExpr = convertIfExpr(n.ElseIf)
	} else if n.Else != nil {
		elseExpr = convertExpr(n.Else)
	}
	if elseExpr == nil {
		return nil
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}
}

func convertMatchExpr(n *parser.MatchExpr) Expr {
	if n == nil {
		return nil
	}
	target := convertExpr(n.Target)
	if target == nil {
		return nil
	}
	var expr Expr
	for i := len(n.Cases) - 1; i >= 0; i-- {
		c := n.Cases[i]
		res := convertExpr(c.Result)
		if res == nil {
			return nil
		}
		if id, ok := types.SimpleStringKey(c.Pattern); ok && id == "_" {
			expr = res
			continue
		}
		pat := convertExpr(c.Pattern)
		if pat == nil {
			return nil
		}
		cond := &BinaryExpr{Op: "==", Left: target, Right: pat}
		if expr == nil {
			expr = res
		} else {
			expr = &CondExpr{Cond: cond, Then: res, Else: expr}
		}
	}
	return expr
}

func detectFunExpr(e *parser.Expr) *parser.FunExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil || u.Value.Target == nil {
		return nil
	}
	return u.Value.Target.FunExpr
}

func compileFunction(env *types.Env, name string, fn *parser.FunExpr) (*Function, error) {
	var params []Param
	var paramTypes []string
	for _, p := range fn.Params {
		typ := "int"
		if p.Type != nil && p.Type.Simple != nil && *p.Type.Simple == "string" {
			typ = "const char*"
		} else if p.Type != nil && p.Type.Simple != nil {
			if _, ok := env.GetStruct(*p.Type.Simple); ok {
				typ = *p.Type.Simple
			}
		}
		paramTypes = append(paramTypes, typ)
		params = append(params, Param{Name: p.Name, Type: typ})
	}
	funcParamTypes[name] = paramTypes
	ret := "int"
	if fn.Return != nil && fn.Return.Simple != nil {
		switch *fn.Return.Simple {
		case "string":
			ret = "const char*"
		case "float":
			ret = "double"
		default:
			if _, ok := env.GetStruct(*fn.Return.Simple); ok {
				ret = *fn.Return.Simple
			}
		}
	}
	body, err := compileStmts(env, fn.BlockBody)
	if err != nil {
		return nil, err
	}
	if fn.ExprBody != nil {
		expr := convertExpr(fn.ExprBody)
		if expr == nil {
			return nil, fmt.Errorf("invalid function expression")
		}
		body = append(body, &ReturnStmt{Expr: expr})
	}
	return &Function{Name: name, Params: params, Body: body, Return: ret}, nil
}

func convertExpr(e *parser.Expr) Expr {
	if e == nil || e.Binary == nil {
		return nil
	}
	// Convert left operand
	left := convertUnary(e.Binary.Left)
	if left == nil {
		return nil
	}

	operands := []Expr{left}
	var operators []string

	// Convert remaining operators and operands
	for _, part := range e.Binary.Right {
		rhs := convertUnary(&parser.Unary{Value: part.Right})
		if rhs == nil {
			return nil
		}
		operators = append(operators, part.Op)
		operands = append(operands, rhs)
	}

	// Operator precedence levels (highest to lowest) to match interpreter
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

	// Build expression tree respecting precedence
	for _, level := range levels {
		for i := 0; i < len(operators); {
			if contains(level, operators[i]) {
				bin := &BinaryExpr{Op: operators[i], Left: operands[i], Right: operands[i+1]}
				operands[i] = bin
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil
	}
	if bin, ok := operands[0].(*BinaryExpr); ok {
		if bin.Op == "&&" {
			if v, ok := evalInt(bin.Left); ok && v == 0 {
				return &IntLit{Value: 0}
			}
		}
		if bin.Op == "||" {
			if v, ok := evalInt(bin.Left); ok && v != 0 {
				return &IntLit{Value: 1}
			}
		}
		if bin.Op == "+" {
			if l, ok := bin.Left.(*StringLit); ok {
				if r, ok2 := bin.Right.(*StringLit); ok2 {
					return &StringLit{Value: l.Value + r.Value}
				}
			}
		}
		if bin.Op == "in" {
			if list, ok := evalList(bin.Right); ok {
				if v, ok2 := evalInt(bin.Left); ok2 {
					for _, it := range list.Elems {
						iv, ok3 := evalInt(it)
						if ok3 && iv == v {
							return &IntLit{Value: 1}
						}
					}
					return &IntLit{Value: 0}
				}
				if s, ok2 := evalString(bin.Left); ok2 {
					for _, it := range list.Elems {
						sv, ok3 := evalString(it)
						if ok3 && sv == s {
							return &IntLit{Value: 1}
						}
					}
					return &IntLit{Value: 0}
				}
			}
		}
		if n, ok := evalInt(bin); ok {
			return &IntLit{Value: n}
		}
		if s, ok := evalString(bin); ok {
			return &StringLit{Value: s}
		}
		if l, ok := evalList(bin); ok {
			return l
		}
		if b, ok := evalBool(bin); ok {
			if b {
				return &IntLit{Value: 1}
			}
			return &IntLit{Value: 0}
		}
	}
	return operands[0]
}

func convertUnary(u *parser.Unary) Expr {
	if u == nil || u.Value == nil {
		return nil
	}
	if len(u.Ops) > 0 {
		base := convertUnary(&parser.Unary{Value: u.Value})
		if base == nil {
			return nil
		}
		for i := len(u.Ops) - 1; i >= 0; i-- {
			switch u.Ops[i] {
			case "-":
				if v, ok := evalInt(base); ok {
					base = &IntLit{Value: -v}
				} else {
					base = &UnaryExpr{Op: "-", Expr: base}
				}
			case "!":
				if b, ok := evalBool(base); ok {
					if b {
						base = &IntLit{Value: 0}
					} else {
						base = &IntLit{Value: 1}
					}
				} else {
					base = &UnaryExpr{Op: "!", Expr: base}
				}
			}
		}
		return base
	}
	if g := u.Value.Target.Group; g != nil {
		return convertExpr(g)
	}
	if ml := u.Value.Target.Map; ml != nil && len(u.Ops) == 0 && len(u.Value.Ops) == 0 {
		var items []MapItem
		for _, it := range ml.Items {
			key := convertExpr(it.Key)
			if key == nil {
				return nil
			}
			val := convertExpr(it.Value)
			if val == nil {
				return nil
			}
			items = append(items, MapItem{Key: key, Value: val})
		}
		return &MapLit{Items: items}
	}
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 1 && sel.Tail[0] == "contains" && len(u.Value.Ops) == 1 && u.Value.Ops[0].Call != nil && len(u.Ops) == 0 {
		base := &VarRef{Name: sel.Root}
		arg := convertExpr(u.Value.Ops[0].Call.Args[0])
		if arg == nil {
			return nil
		}
		return &CallExpr{Func: "contains", Args: []Expr{base, arg}}
	}
	if len(u.Value.Ops) == 1 && u.Value.Ops[0].Cast != nil &&
		u.Value.Ops[0].Cast.Type != nil && u.Value.Ops[0].Cast.Type.Simple != nil && len(u.Ops) == 0 {
		castType := *u.Value.Ops[0].Cast.Type.Simple
		if castType == "int" {
			if lit := u.Value.Target.Lit; lit != nil && lit.Str != nil {
				if n, err := strconv.Atoi(*lit.Str); err == nil {
					return &IntLit{Value: n}
				}
			}
		} else if ml := u.Value.Target.Map; ml != nil {
			var fields []StructField
			for _, it := range ml.Items {
				key, ok := types.SimpleStringKey(it.Key)
				if !ok {
					return nil
				}
				val := convertExpr(it.Value)
				if val == nil {
					return nil
				}
				fields = append(fields, StructField{Name: key, Value: val})
			}
			return &StructLit{Name: castType, Fields: fields}
		} else {
			base := convertUnary(&parser.Unary{Value: &parser.PostfixExpr{Target: u.Value.Target}})
			return base
		}
	}
	if len(u.Value.Ops) >= 1 && len(u.Ops) == 0 {
		current := convertUnary(&parser.Unary{Value: &parser.PostfixExpr{Target: u.Value.Target}})
		if current == nil {
			return nil
		}
		simple := true
		for _, op := range u.Value.Ops {
			if op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil && op.Index.Step == nil {
				idx := convertExpr(op.Index.Start)
				if idx == nil {
					return nil
				}
				current = &IndexExpr{Target: current, Index: idx}
				continue
			}
			if op.Field != nil {
				current = &FieldExpr{Target: current, Name: op.Field.Name}
				continue
			}
			simple = false
			break
		}
		if simple {
			if n, ok := evalInt(current); ok {
				return &IntLit{Value: n}
			}
			if s, ok := evalString(current); ok {
				return &StringLit{Value: s}
			}
			if l, ok := evalList(current); ok {
				return l
			}
			return current
		}
	}
	if len(u.Value.Ops) == 1 && u.Value.Ops[0].Index != nil &&
		u.Value.Ops[0].Index.Colon != nil && u.Value.Ops[0].Index.Colon2 == nil &&
		u.Value.Ops[0].Index.Step == nil && len(u.Ops) == 0 {
		base := convertUnary(&parser.Unary{Value: &parser.PostfixExpr{Target: u.Value.Target}})
		if base == nil {
			return nil
		}
		start := convertExpr(u.Value.Ops[0].Index.Start)
		end := convertExpr(u.Value.Ops[0].Index.End)
		if str, ok := evalString(base); ok {
			s, ok1 := evalInt(start)
			e, ok2 := evalInt(end)
			if ok1 && ok2 {
				r := []rune(str)
				if s < 0 {
					s = 0
				}
				if e > len(r) {
					e = len(r)
				}
				if s > e {
					s = e
				}
				return &StringLit{Value: string(r[s:e])}
			}
		}
		if list, ok := evalList(base); ok {
			s, ok1 := evalInt(start)
			e, ok2 := evalInt(end)
			if ok1 && ok2 {
				if s < 0 {
					s = 0
				}
				if e > len(list.Elems) {
					e = len(list.Elems)
				}
				if s > e {
					s = e
				}
				slice := make([]Expr, 0, e-s)
				for _, it := range list.Elems[s:e] {
					slice = append(slice, it)
				}
				return &ListLit{Elems: slice}
			}
		}
	}
	if call := u.Value.Target.Call; call != nil && len(u.Ops) == 0 {
		if call.Func == "len" && len(call.Args) == 1 {
			if l, ok := evalList(convertExpr(call.Args[0])); ok {
				return &IntLit{Value: len(l.Elems)}
			}
			if list, ok := convertListExpr(call.Args[0]); ok {
				return &IntLit{Value: len(list)}
			}
			arg := call.Args[0]
			if arg != nil && arg.Binary != nil && arg.Binary.Left != nil && arg.Binary.Left.Value != nil {
				t := arg.Binary.Left.Value.Target
				if t != nil {
					if t.Map != nil {
						return &IntLit{Value: len(t.Map.Items)}
					}
					if t.Lit != nil && t.Lit.Str != nil {
						return &IntLit{Value: len(*t.Lit.Str)}
					}
				}
			}
		}
		if call.Func == "count" && len(call.Args) == 1 {
			if l, ok := evalList(convertExpr(call.Args[0])); ok {
				return &IntLit{Value: len(l.Elems)}
			}
			if list, ok := convertListExpr(call.Args[0]); ok {
				return &IntLit{Value: len(list)}
			}
		}
		if call.Func == "sum" && len(call.Args) == 1 {
			if l, ok := evalList(convertExpr(call.Args[0])); ok {
				total := 0
				for _, e := range l.Elems {
					v, ok := evalInt(e)
					if !ok {
						return nil
					}
					total += v
				}
				return &IntLit{Value: total}
			}
			if list, ok := convertListExpr(call.Args[0]); ok {
				total := 0
				for _, e := range list {
					v, ok := evalInt(e)
					if !ok {
						return nil
					}
					total += v
				}
				return &IntLit{Value: total}
			}
		}
		if call.Func == "avg" && len(call.Args) == 1 {
			if l, ok := evalList(convertExpr(call.Args[0])); ok {
				total := 0
				for _, e := range l.Elems {
					v, ok := evalInt(e)
					if !ok {
						return nil
					}
					total += v
				}
				avg := float64(total) / float64(len(l.Elems))
				return &StringLit{Value: fmt.Sprintf("%.1f", avg)}
			}
			if list, ok := convertListExpr(call.Args[0]); ok {
				total := 0
				for _, e := range list {
					v, ok := evalInt(e)
					if !ok {
						return nil
					}
					total += v
				}
				avg := float64(total) / float64(len(list))
				return &StringLit{Value: fmt.Sprintf("%.1f", avg)}
			}
		}
		if (call.Func == "min" || call.Func == "max") && len(call.Args) == 1 {
			if l, ok := evalList(convertExpr(call.Args[0])); ok && len(l.Elems) > 0 {
				best, ok := evalInt(l.Elems[0])
				if !ok {
					return nil
				}
				for _, e := range l.Elems[1:] {
					v, ok := evalInt(e)
					if !ok {
						return nil
					}
					if (call.Func == "min" && v < best) || (call.Func == "max" && v > best) {
						best = v
					}
				}
				return &IntLit{Value: best}
			}
			if list, ok := convertListExpr(call.Args[0]); ok && len(list) > 0 {
				best, ok := evalInt(list[0])
				if !ok {
					return nil
				}
				for _, e := range list[1:] {
					v, ok := evalInt(e)
					if !ok {
						return nil
					}
					if (call.Func == "min" && v < best) || (call.Func == "max" && v > best) {
						best = v
					}
				}
				return &IntLit{Value: best}
			}
		}
		if call.Func == "append" && len(call.Args) == 2 {
			if l, ok := evalList(convertExpr(call.Args[0])); ok {
				elem := convertExpr(call.Args[1])
				if elem == nil {
					return nil
				}
				newElems := append(append([]Expr{}, l.Elems...), elem)
				return &ListLit{Elems: newElems}
			}
			if list, ok := convertListExpr(call.Args[0]); ok {
				elem := convertExpr(call.Args[1])
				if elem == nil {
					return nil
				}
				newElems := append(append([]Expr{}, list...), elem)
				return &ListLit{Elems: newElems}
			}
		}
		if call.Func == "values" && len(call.Args) == 1 {
			if ml := mapLiteral(call.Args[0]); ml != nil {
				var elems []Expr
				for _, it := range ml.Items {
					val := convertExpr(it.Value)
					if val == nil {
						return nil
					}
					elems = append(elems, val)
				}
				return &ListLit{Elems: elems}
			}
			if l, ok := evalMap(convertExpr(call.Args[0])); ok {
				return l
			}
		}
		if call.Func == "contains" && len(call.Args) == 2 {
			hay, ok1 := evalString(convertExpr(call.Args[0]))
			needle, ok2 := evalString(convertExpr(call.Args[1]))
			if ok1 && ok2 {
				if strings.Contains(hay, needle) {
					return &IntLit{Value: 1}
				}
				return &IntLit{Value: 0}
			}
		}
		if call.Func == "substring" && len(call.Args) >= 2 {
			strArg, ok := evalString(convertExpr(call.Args[0]))
			if !ok {
				return nil
			}
			startExpr := convertExpr(call.Args[1])
			start, ok2 := evalInt(startExpr)
			if !ok2 {
				return nil
			}
			end := len([]rune(strArg))
			if len(call.Args) == 3 {
				endExpr := convertExpr(call.Args[2])
				if v, ok := evalInt(endExpr); ok {
					end = v
				} else {
					return nil
				}
			}
			r := []rune(strArg)
			if start < 0 {
				start = 0
			}
			if end > len(r) {
				end = len(r)
			}
			if start > end {
				start = end
			}
			return &StringLit{Value: string(r[start:end])}
		}
		if call.Func == "str" && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			if lit, ok := arg.(*IntLit); ok {
				return &StringLit{Value: fmt.Sprintf("%d", lit.Value)}
			}
		}
		var args []Expr
		for _, a := range call.Args {
			ex := convertExpr(a)
			if ex == nil {
				return nil
			}
			args = append(args, ex)
		}
		return &CallExpr{Func: call.Func, Args: args}
	}
	if ifexpr := u.Value.Target.If; ifexpr != nil && len(u.Ops) == 0 {
		return convertIfExpr(ifexpr)
	}
	if mexpr := u.Value.Target.Match; mexpr != nil && len(u.Ops) == 0 {
		return convertMatchExpr(mexpr)
	}
	if qexpr := u.Value.Target.Query; qexpr != nil && len(u.Ops) == 0 {
		if res, ok := evalQueryConst(qexpr); ok {
			return res
		}
		return nil
	}
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 && len(u.Ops) == 0 {
		return &VarRef{Name: sel.Root}
	}
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) > 0 && len(u.Ops) == 0 {
		expr := Expr(&VarRef{Name: sel.Root})
		for _, f := range sel.Tail {
			expr = &FieldExpr{Target: expr, Name: f}
		}
		return expr
	}
	if st := u.Value.Target.Struct; st != nil && len(u.Ops) == 0 {
		var fields []StructField
		for _, f := range st.Fields {
			val := convertExpr(f.Value)
			if val == nil {
				return nil
			}
			fields = append(fields, StructField{Name: f.Name, Value: val})
		}
		return &StructLit{Name: st.Name, Fields: fields}
	}
	if list := u.Value.Target.List; list != nil && len(u.Ops) == 0 {
		if st, ok := types.InferStructFromList(list, currentEnv); ok {
			structCounter++
			base := strings.Title(currentVarName)
			if base == "" {
				base = fmt.Sprintf("Data%d", structCounter)
			}
			name := types.UniqueStructName(base, currentEnv, nil)
			st.Name = name
			currentEnv.SetStruct(name, st)
			structTypes = currentEnv.Structs()
			var elems []Expr
			for _, it := range list.Elems {
				ml := it.Binary.Left.Value.Target.Map
				var fields []StructField
				for i, item := range ml.Items {
					val := convertExpr(item.Value)
					if val == nil {
						return nil
					}
					fields = append(fields, StructField{Name: st.Order[i], Value: val})
				}
				elems = append(elems, &StructLit{Name: name, Fields: fields})
			}
			return &ListLit{Elems: elems}
		}
		var elems []Expr
		for _, it := range list.Elems {
			ex := convertExpr(it)
			if ex == nil {
				return nil
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}
	}
	lit := u.Value.Target.Lit
	if lit == nil {
		return nil
	}
	if lit.Str != nil && len(u.Ops) == 0 {
		return &StringLit{Value: *lit.Str}
	}
	if lit.Int != nil {
		v := int(*lit.Int)
		for _, op := range u.Ops {
			if op == "-" {
				v = -v
			}
		}
		return &IntLit{Value: v}
	}
	if lit.Float != nil && len(u.Ops) == 0 {
		return &FloatLit{Value: *lit.Float}
	}
	if lit.Bool != nil && len(u.Ops) == 0 {
		if bool(*lit.Bool) {
			return &IntLit{Value: 1}
		}
		return &IntLit{Value: 0}
	}
	return nil
}

func convertListExpr(e *parser.Expr) ([]Expr, bool) {
	if e == nil {
		return nil, false
	}
	// Handle direct list literals first.
	if e.Binary != nil {
		u := e.Binary.Left
		if u != nil && u.Value != nil && u.Value.Target != nil && u.Value.Target.List != nil {
			list := u.Value.Target.List
			var out []Expr
			for _, item := range list.Elems {
				ex := convertExpr(item)
				if ex == nil {
					return nil, false
				}
				out = append(out, ex)
			}
			return out, true
		}
	}
	// Fallback to evaluating the expression which may resolve to a
	// constant list via a previously declared variable.
	ex := convertExpr(e)
	if ex == nil {
		return nil, false
	}
	l, ok := evalList(ex)
	if !ok {
		return nil, false
	}
	var out []Expr
	for _, item := range l.Elems {
		out = append(out, item)
	}
	return out, true
}

func evalInt(e Expr) (int, bool) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value, true
	case *BinaryExpr:
		left, ok1 := evalInt(v.Left)
		right, ok2 := evalInt(v.Right)
		if ok1 && ok2 {
			switch v.Op {
			case "+":
				return left + right, true
			case "-":
				return left - right, true
			case "*":
				return left * right, true
			case "/":
				if right != 0 {
					return left / right, true
				}
			case "%":
				if right != 0 {
					return left % right, true
				}
			}
		}
	case *CallExpr:
		if v.Func == "len" && len(v.Args) == 1 {
			if list, ok := evalList(v.Args[0]); ok {
				return len(list.Elems), true
			}
			if s, ok := evalString(v.Args[0]); ok {
				return len([]rune(s)), true
			}
		}
	case *IndexExpr:
		if list, ok := evalList(v.Target); ok {
			idx, ok2 := evalInt(v.Index)
			if ok2 && idx >= 0 && idx < len(list.Elems) {
				return evalInt(list.Elems[idx])
			}
		}
		if val, ok := evalMapEntry(v.Target, v.Index); ok {
			return evalInt(val)
		}
	case *UnaryExpr:
		if v.Op == "-" {
			n, ok := evalInt(v.Expr)
			if ok {
				return -n, true
			}
		}
		if v.Op == "!" {
			n, ok := evalInt(v.Expr)
			if ok {
				if n == 0 {
					return 1, true
				}
				return 0, true
			}
		}
	}
	return 0, false
}

func evalFloat(e Expr) (float64, bool) {
	switch v := e.(type) {
	case *FloatLit:
		return v.Value, true
	case *IntLit:
		return float64(v.Value), true
	case *UnaryExpr:
		if v.Op == "-" {
			if f, ok := evalFloat(v.Expr); ok {
				return -f, true
			}
		}
	case *BinaryExpr:
		left, ok1 := evalFloat(v.Left)
		right, ok2 := evalFloat(v.Right)
		if ok1 && ok2 {
			switch v.Op {
			case "+":
				return left + right, true
			case "-":
				return left - right, true
			case "*":
				return left * right, true
			case "/":
				if right != 0 {
					return left / right, true
				}
			}
		}
	}
	return 0, false
}

func evalBool(e Expr) (bool, bool) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value != 0, true
	case *VarRef:
		if val, err := currentEnv.GetValue(v.Name); err == nil {
			if b, ok := val.(bool); ok {
				return b, true
			}
		}
	case *UnaryExpr:
		if v.Op == "!" {
			if b, ok := evalBool(v.Expr); ok {
				return !b, true
			}
		}
	case *BinaryExpr:
		if li, ok1 := evalInt(v.Left); ok1 {
			if ri, ok2 := evalInt(v.Right); ok2 {
				switch v.Op {
				case "==":
					return li == ri, true
				case "!=":
					return li != ri, true
				case "<":
					return li < ri, true
				case "<=":
					return li <= ri, true
				case ">":
					return li > ri, true
				case ">=":
					return li >= ri, true
				case "&&":
					return (li != 0) && (ri != 0), true
				case "||":
					return (li != 0) || (ri != 0), true
				}
			}
		}
		if lf, ok1 := evalFloat(v.Left); ok1 {
			if rf, ok2 := evalFloat(v.Right); ok2 {
				switch v.Op {
				case "==":
					return lf == rf, true
				case "!=":
					return lf != rf, true
				case "<":
					return lf < rf, true
				case "<=":
					return lf <= rf, true
				case ">":
					return lf > rf, true
				case ">=":
					return lf >= rf, true
				}
			}
		}
		if ls, ok1 := evalString(v.Left); ok1 {
			if rs, ok2 := evalString(v.Right); ok2 {
				switch v.Op {
				case "==":
					return ls == rs, true
				case "!=":
					return ls != rs, true
				case "<":
					return ls < rs, true
				case "<=":
					return ls <= rs, true
				case ">":
					return ls > rs, true
				case ">=":
					return ls >= rs, true
				}
			}
		}
	}
	return false, false
}

func evalString(e Expr) (string, bool) {
	switch v := e.(type) {
	case *StringLit:
		return v.Value, true
	case *VarRef:
		s, ok := constStrings[v.Name]
		return s, ok
	case *IndexExpr:
		str, ok := evalString(v.Target)
		if ok {
			idx, ok2 := evalInt(v.Index)
			if ok2 {
				r := []rune(str)
				if idx >= 0 && idx < len(r) {
					return string(r[idx]), true
				}
			}
		}
		if val, ok := evalMapEntry(v.Target, v.Index); ok {
			return evalString(val)
		}
		return "", false
	case *BinaryExpr:
		if v.Op == "+" {
			left, ok1 := evalString(v.Left)
			right, ok2 := evalString(v.Right)
			if ok1 && ok2 {
				return left + right, true
			}
		}
		return "", false
	case *UnaryExpr:
		return evalString(v.Expr)
	default:
		return "", false
	}
}

func evalList(e Expr) (*ListLit, bool) {
	switch v := e.(type) {
	case *ListLit:
		return v, true
	case *VarRef:
		l, ok := constLists[v.Name]
		return l, ok
	case *BinaryExpr:
		left, ok1 := evalList(v.Left)
		right, ok2 := evalList(v.Right)
		if ok1 && ok2 {
			switch v.Op {
			case "union_all":
				elems := append(append([]Expr{}, left.Elems...), right.Elems...)
				return &ListLit{Elems: elems}, true
			case "union":
				elems := append([]Expr{}, left.Elems...)
				for _, e2 := range right.Elems {
					found := false
					for _, e1 := range elems {
						if listElemEqual(e1, e2) {
							found = true
							break
						}
					}
					if !found {
						elems = append(elems, e2)
					}
				}
				return &ListLit{Elems: elems}, true
			case "except":
				var elems []Expr
				for _, e1 := range left.Elems {
					found := false
					for _, e2 := range right.Elems {
						if listElemEqual(e1, e2) {
							found = true
							break
						}
					}
					if !found {
						elems = append(elems, e1)
					}
				}
				return &ListLit{Elems: elems}, true
			case "intersect":
				var elems []Expr
				for _, e1 := range left.Elems {
					for _, e2 := range right.Elems {
						if listElemEqual(e1, e2) {
							elems = append(elems, e1)
							break
						}
					}
				}
				return &ListLit{Elems: elems}, true
			}
			return nil, false
		}
	default:
		return nil, false
	}
	return nil, false
}

func listElemEqual(a, b Expr) bool {
	if ai, ok := evalInt(a); ok {
		if bi, ok2 := evalInt(b); ok2 {
			return ai == bi
		}
	}
	if as, ok := evalString(a); ok {
		if bs, ok2 := evalString(b); ok2 {
			return as == bs
		}
	}
	return false
}

func isConstExpr(e Expr) bool {
	if _, ok := evalInt(e); ok {
		return true
	}
	if _, ok := evalFloat(e); ok {
		return true
	}
	if _, ok := evalString(e); ok {
		return true
	}
	if l, ok := evalList(e); ok {
		for _, it := range l.Elems {
			if !isConstExpr(it) {
				return false
			}
		}
		return true
	}
	if s, ok := e.(*StructLit); ok {
		for _, f := range s.Fields {
			if !isConstExpr(f.Value) {
				return false
			}
		}
		return true
	}
	if m, ok := e.(*MapLit); ok {
		for _, it := range m.Items {
			if !isConstExpr(it.Value) {
				return false
			}
		}
		return true
	}
	return false
}

func exprIsString(e Expr) bool {
	switch v := e.(type) {
	case *StringLit:
		return true
	case *VarRef:
		_, ok := constStrings[v.Name]
		return ok
	case *CallExpr:
		if v.Func == "str" || v.Func == "substring" {
			return true
		}
		if fn, ok := currentEnv.GetFunc(v.Func); ok && fn.Return != nil && fn.Return.Simple != nil {
			return *fn.Return.Simple == "string"
		}
		return false
	case *BinaryExpr:
		if v.Op == "+" {
			return exprIsString(v.Left) || exprIsString(v.Right)
		}
		return false
	case *IndexExpr:
		return exprIsString(v.Target)
	case *FieldExpr:
		tname := inferExprType(currentEnv, v.Target)
		if st, ok := structTypes[tname]; ok {
			if ft, ok2 := st.Fields[v.Name]; ok2 {
				if _, ok3 := ft.(types.StringType); ok3 {
					return true
				}
			}
		}
		return exprIsString(v.Target)
	case *StructLit:
		return false
	case *CondExpr:
		return exprIsString(v.Then) && exprIsString(v.Else)
	case *UnaryExpr:
		return exprIsString(v.Expr)
	default:
		return false
	}
}

func exprIsBool(e Expr) bool {
	switch v := e.(type) {
	case *IntLit:
		return v.Value == 0 || v.Value == 1
	case *FloatLit:
		return v.Value == 0.0 || v.Value == 1.0
	case *VarRef:
		if t, err := currentEnv.GetVar(v.Name); err == nil {
			if _, ok := t.(types.BoolType); ok {
				return true
			}
		}
		if val, err := currentEnv.GetValue(v.Name); err == nil {
			if _, ok := val.(bool); ok {
				return true
			}
		}
	case *UnaryExpr:
		if v.Op == "!" {
			return exprIsBool(v.Expr)
		}
	case *BinaryExpr:
		switch v.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return true
		}
	case *CondExpr:
		return exprIsBool(v.Then) && exprIsBool(v.Else)
	}
	if _, ok := evalBool(e); ok {
		return true
	}
	return false
}

func inferExprType(env *types.Env, e Expr) string {
	if exprIsBool(e) {
		return "int"
	}
	switch v := e.(type) {
	case *StringLit:
		return "const char*"
	case *IntLit:
		return "int"
	case *FloatLit:
		return "double"
	case *ListLit:
		if len(v.Elems) > 0 {
			elemType := inferExprType(env, v.Elems[0])
			uniform := elemType != ""
			for _, it := range v.Elems[1:] {
				if inferExprType(env, it) != elemType {
					uniform = false
					break
				}
			}
			if uniform {
				if strings.HasSuffix(elemType, "[]") {
					return elemType + "[]"
				}
				return elemType + "[]"
			}
		}
		allStr := true
		for _, it := range v.Elems {
			if inferExprType(env, it) != "const char*" {
				allStr = false
			}
		}
		if allStr {
			return "const char*[]"
		}
		return "int[]"
	case *StructLit:
		return v.Name
	case *CondExpr:
		t1 := inferExprType(env, v.Then)
		t2 := inferExprType(env, v.Else)
		if t1 != "" && t1 == t2 {
			return t1
		}
	case *VarRef:
		if t, err := env.GetVar(v.Name); err == nil {
			switch tt := t.(type) {
			case types.StringType:
				return "const char*"
			case types.ListType:
				if _, ok := tt.Elem.(types.StringType); ok {
					return "const char*[]"
				}
				return "int[]"
			case types.BoolType:
				return "int"
			case types.StructType:
				return tt.Name
			}
		}
		if _, ok := constStrings[v.Name]; ok {
			return "const char*"
		}
		if l, ok := constLists[v.Name]; ok {
			if len(l.Elems) > 0 {
				elemType := inferExprType(env, l.Elems[0])
				uniform := elemType != ""
				for _, it := range l.Elems[1:] {
					if inferExprType(env, it) != elemType {
						uniform = false
						break
					}
				}
				if uniform {
					if strings.HasSuffix(elemType, "[]") {
						return elemType + "[]"
					}
					return elemType + "[]"
				}
			}
			allStr := true
			for _, it := range l.Elems {
				if inferExprType(env, it) != "const char*" {
					allStr = false
				}
			}
			if allStr {
				return "const char*[]"
			}
			return "int[]"
		}
	case *FieldExpr:
		tname := inferExprType(env, v.Target)
		if st, ok := env.GetStruct(tname); ok {
			if ft, ok2 := st.Fields[v.Name]; ok2 {
				switch ft.(type) {
				case types.StringType:
					return "const char*"
				case types.BoolType:
					return "int"
				case types.StructType:
					return ft.(types.StructType).Name
				case types.ListType:
					if _, ok := ft.(types.ListType).Elem.(types.StringType); ok {
						return "const char*[]"
					}
					return "int[]"
				}
			}
		}
	case *CallExpr:
		switch v.Func {
		case "str", "substring":
			return "const char*"
		case "len":
			return "int"
		default:
			if fn, ok := env.GetFunc(v.Func); ok && fn.Return != nil && fn.Return.Simple != nil {
				switch *fn.Return.Simple {
				case "string":
					return "const char*"
				case "float":
					return "double"
				default:
					if _, ok := env.GetStruct(*fn.Return.Simple); ok {
						return *fn.Return.Simple
					}
					return "int"
				}
			}
		}
	case *BinaryExpr:
		if v.Op == "+" && (exprIsString(v.Left) || exprIsString(v.Right)) {
			return "const char*"
		}
		if exprIsFloat(v.Left) || exprIsFloat(v.Right) {
			return "double"
		}
		return "int"
	case *UnaryExpr:
		return inferExprType(env, v.Expr)
	}
	if _, ok := evalString(e); ok {
		return "const char*"
	}
	if l, ok := evalList(e); ok {
		allStr := true
		for _, it := range l.Elems {
			if inferExprType(env, it) != "const char*" {
				allStr = false
			}
		}
		if allStr {
			return "const char*[]"
		}
		return "int[]"
	}
	if _, ok := evalInt(e); ok {
		return "int"
	}
	if _, ok := evalFloat(e); ok {
		return "double"
	}
	return ""
}

func inferCType(env *types.Env, name string, e Expr) string {
	if t := inferExprType(env, e); t != "" {
		return t
	}
	if t, err := env.GetVar(name); err == nil {
		switch tt := t.(type) {
		case types.StringType:
			return "const char*"
		case types.ListType:
			if _, ok := tt.Elem.(types.StringType); ok {
				return "const char*[]"
			}
			return "int[]"
		case types.BoolType:
			return "int"
		case types.FloatType:
			return "double"
		case types.StructType:
			return tt.Name
		}
	}
	return "int"
}

func anyToExpr(v any) Expr {
	switch t := v.(type) {
	case int:
		return &IntLit{Value: t}
	case int64:
		return &IntLit{Value: int(t)}
	case float64:
		return &FloatLit{Value: t}
	case string:
		return &StringLit{Value: t}
	case []any:
		var elems []Expr
		for _, it := range t {
			ex := anyToExpr(it)
			if ex == nil {
				return nil
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}
	case map[string]any:
		var fields []StructField
		keys := make([]string, 0, len(t))
		for k := range t {
			if k == "__join__" {
				continue
			}
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			ex := anyToExpr(t[k])
			if ex == nil {
				return nil
			}
			fields = append(fields, StructField{Name: k, Value: ex})
		}
		structCounter++
		name := fmt.Sprintf("Anon%d", structCounter)
		stFields := map[string]types.Type{}
		for _, f := range fields {
			tname := inferExprType(currentEnv, f.Value)
			var tt types.Type = types.AnyType{}
			switch tname {
			case "int":
				tt = types.IntType{}
			case "double":
				tt = types.FloatType{}
			case "const char*":
				tt = types.StringType{}
			case "int[]":
				tt = types.ListType{Elem: types.IntType{}}
			case "double[]":
				tt = types.ListType{Elem: types.FloatType{}}
			case "const char*[]":
				tt = types.ListType{Elem: types.StringType{}}
			default:
				if strings.HasSuffix(tname, "[]") {
					base := strings.TrimSuffix(tname, "[]")
					if base == "const char*" {
						tt = types.ListType{Elem: types.StringType{}}
					} else if base == "int" {
						tt = types.ListType{Elem: types.IntType{}}
					} else if base == "double" {
						tt = types.ListType{Elem: types.FloatType{}}
					} else if st, ok := currentEnv.GetStruct(base); ok {
						tt = types.ListType{Elem: st}
					}
				} else if st, ok := currentEnv.GetStruct(tname); ok {
					tt = st
				}
			}
			stFields[f.Name] = tt
		}
		currentEnv.SetStruct(name, types.StructType{Name: name, Fields: stFields, Order: keys})
		structTypes = currentEnv.Structs()
		return &StructLit{Name: name, Fields: fields}
	case *data.Group:
		m := map[string]any{"key": t.Key, "items": t.Items}
		return anyToExpr(m)
	default:
		return nil
	}
}

func evalQueryConst(q *parser.QueryExpr) (*ListLit, bool) {
	results, err := data.RunQuery(q, currentEnv, data.MemoryEngine{}, func(env *types.Env, e *parser.Expr) (any, error) {
		ip := interpreter.New(&parser.Program{}, env.Copy(), "")
		return ip.EvalExpr(e)
	})
	if err != nil {
		return nil, false
	}
	var elems []Expr
	for _, r := range results {
		ex := anyToExpr(r)
		if ex == nil {
			return nil, false
		}
		elems = append(elems, ex)
	}
	return &ListLit{Elems: elems}, true
}

func valueFromExpr(e Expr) (any, bool) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value, true
	case *FloatLit:
		return v.Value, true
	case *StringLit:
		return v.Value, true
	case *ListLit:
		var out []any
		for _, it := range v.Elems {
			val, ok := valueFromExpr(it)
			if !ok {
				return nil, false
			}
			out = append(out, val)
		}
		return out, true
	case *StructLit:
		m := map[string]any{}
		for _, f := range v.Fields {
			val, ok := valueFromExpr(f.Value)
			if !ok {
				return nil, false
			}
			m[f.Name] = val
		}
		return m, true
	case *MapLit:
		m := map[string]any{}
		for _, it := range v.Items {
			keyVal, ok := valueFromExpr(it.Key)
			if !ok {
				return nil, false
			}
			ks, ok := keyVal.(string)
			if !ok {
				return nil, false
			}
			val, ok := valueFromExpr(it.Value)
			if !ok {
				return nil, false
			}
			m[ks] = val
		}
		return m, true
	case *VarRef:
		if val, err := currentEnv.GetValue(v.Name); err == nil {
			return val, true
		}
		return nil, false
	default:
		return nil, false
	}
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

func evalMap(e Expr) (*ListLit, bool) {
	switch v := e.(type) {
	case *StructLit:
		var out []Expr
		for _, f := range v.Fields {
			out = append(out, f.Value)
		}
		return &ListLit{Elems: out}, true
	case *VarRef:
		if val, err := currentEnv.GetValue(v.Name); err == nil {
			if m, ok := val.(map[string]any); ok {
				keys := make([]string, 0, len(m))
				for k := range m {
					keys = append(keys, k)
				}
				sort.Strings(keys)
				var elems []Expr
				for _, k := range keys {
					ex := anyToExpr(m[k])
					if ex == nil {
						return nil, false
					}
					elems = append(elems, ex)
				}
				return &ListLit{Elems: elems}, true
			}
		}
	}
	return nil, false
}

func evalMapEntry(target Expr, key Expr) (Expr, bool) {
	val, ok := valueFromExpr(target)
	if !ok {
		return nil, false
	}
	m, ok := val.(map[string]any)
	if !ok {
		return nil, false
	}
	ks, ok := evalString(key)
	if !ok {
		return nil, false
	}
	if v, ok := m[ks]; ok {
		return anyToExpr(v), true
	}
	return nil, false
}
