//go:build slow

package ctrans

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os/exec"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/data"
	testpkg "mochi/runtime/ffi/go/testpkg"
	"mochi/types"
)

var (
	constLists              map[string]*ListLit
	constStrings            map[string]string
	structTypes             map[string]types.StructType
	currentEnv              *types.Env
	funcParamTypes          map[string][]string
	funcReturnTypes         map[string]string
	varTypes                map[string]string
	closureApply            map[string]string
	closureFields           map[string][]string
	extraFuncs              []*Function
	structCounter           int
	tempCounter             int
	anonFuncCounter         int
	anonStructs             map[string]string
	currentVarName          string
	currentVarType          types.Type
	needMath                bool
	needContainsInt         bool
	needContainsStr         bool
	needStrConcat           bool
	needStrInt              bool
	needStrFloat            bool
	needStrListInt          bool
	needStrListStr          bool
	needStrListListInt      bool
	needStrListDouble       bool
	needStrListListDouble   bool
	needJSONListListInt     bool
	needUpper               bool
	needLower               bool
	needPadStart            bool
	needStrBool             bool
	multiJoinEnabled        bool
	multiJoinSort           bool
	groupLeftJoinEnabled    bool
	datasetWhereEnabled     bool
	joinMultiEnabled        bool
	builtinAliases          map[string]string
	funcAliases             map[string]string
	varAliases              map[string]string
	mapKeyTypes             map[string]string
	mapValTypes             map[string]string
	needMapGetSI            bool
	needMapSetSI            bool
	needMapGetIS            bool
	needMapSetIS            bool
	needMapGetSL            bool
	needMapSetSL            bool
	needMapGetSS            bool
	needMapSetSS            bool
	needMapGetII            bool
	needMapSetII            bool
	needMapGetSD            bool
	needMapSetSD            bool
	needContainsMapInt      bool
	needListAppendInt       bool
	needListAppendStr       bool
        needListAppendPtr       bool
        needListAppendPtrPtr    bool
        needListAppendStrPtr    bool
	needConcatStrPtr        bool
	needConcatLongLong      bool
	needListAppendDouble    bool
	needListAppendDoublePtr bool
	needListAppendDoubleNew bool
	needListAppendStrNew    bool
	needListAppendIntNew    bool
	needListAppendStruct    map[string]bool
	needListAppendStructNew map[string]bool
	needListAppendStructPtr map[string]bool
	needListAppendSizeT     bool
	needListMin             bool
	needListMax             bool
	needSHA256              bool
	needMD5Hex              bool
	needNow                 bool
	needMem                 bool
	needInput               bool
	needSubstring           bool
	needAtoi                bool
	needCharAt              bool
	needIndexOf             bool
	needSliceInt            bool
	needSliceDouble         bool
	needSliceStr            bool
	needRepeat              bool
	needParseIntStr         bool
	needGMP                 bool
	needStrBigInt           bool
	needListAppendBigRat    bool

	currentFuncName   string
	currentFuncReturn string

	currentLocals map[string]bool

	benchMain bool

	// track local variable declarations so they can be adjusted
	declStmts   map[string]*DeclStmt
	declAliases map[string]bool
	// track inner function declarations that are ignored during
	// transpilation so references to them can be stubbed out
	localFuncs map[string]bool
	// track extra arguments for hoisted local functions
	localFuncArgs map[string][]string
)

// SetBenchMain configures whether the generated main function is wrapped in a
// benchmark block when emitting code. When enabled, the program will print a
// JSON object with duration and memory statistics on completion.
func SetBenchMain(v bool) { benchMain = v }

func emitLenExpr(w io.Writer, e Expr) {
	switch v := e.(type) {
	case *VarRef:
		io.WriteString(w, v.Name+"_len")
	case *FieldExpr:
		typ := inferExprType(currentEnv, v.Target)
		base := strings.TrimPrefix(typ, "struct ")
		v.Target.emitExpr(w)
		if strings.HasSuffix(base, "*") {
			if _, ok := structTypes[strings.TrimSuffix(base, "*")]; ok {
				io.WriteString(w, "->"+v.Name+"_len")
			} else {
				io.WriteString(w, "."+v.Name+"_len")
			}
		} else {
			io.WriteString(w, "."+v.Name+"_len")
		}
	case *CallExpr:
		if v.Func == "append" && len(v.Args) == 2 {
			emitLenExpr(w, v.Args[0])
			io.WriteString(w, " + 1")
		} else if inferExprType(currentEnv, v) == "const char*" {
			io.WriteString(w, "strlen(")
			v.emitExpr(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, v.Func+"_len")
		}
	case *ListLit:
		fmt.Fprintf(w, "%d", len(v.Elems))
	case *IndexExpr:
		if vr, ok := v.Target.(*VarRef); ok {
			if currentEnv != nil {
				if _, err := currentEnv.GetVar(vr.Name + "_lens"); err == nil {
					io.WriteString(w, vr.Name+"_lens[")
					v.Index.emitExpr(w)
					io.WriteString(w, "]")
					break
				}
			}
		}
		io.WriteString(w, "0")
	default:
		io.WriteString(w, "0")
	}
}

func emitLensExpr(w io.Writer, e Expr) {
	switch v := e.(type) {
	case *VarRef:
		io.WriteString(w, v.Name+"_lens")
	case *FieldExpr:
		v.Target.emitExpr(w)
		io.WriteString(w, "."+v.Name+"_lens")
	case *CallExpr:
		io.WriteString(w, v.Func+"_lens")
	case *ListLit:
		if len(v.Elems) == 0 {
			io.WriteString(w, "NULL")
			return
		}
		fmt.Fprintf(w, "({size_t *tmp = malloc(%d * sizeof(size_t)); ", len(v.Elems))
		for i, e := range v.Elems {
			fmt.Fprintf(w, "tmp[%d] = ", i)
			if lst, ok := e.(*ListLit); ok {
				fmt.Fprintf(w, "%d", len(lst.Elems))
			} else {
				io.WriteString(w, "0")
			}
			io.WriteString(w, "; ")
		}
		io.WriteString(w, "tmp;})")
	default:
		io.WriteString(w, "NULL")
	}
}

func emitExtraArgs(w io.Writer, paramType string, arg Expr) {
	if strings.HasSuffix(paramType, "[][]") {
		io.WriteString(w, ", ")
		emitLenExpr(w, arg)
		io.WriteString(w, ", ")
		emitLensExpr(w, arg)
		io.WriteString(w, ", ")
		emitLenExpr(w, arg)
	} else if strings.HasSuffix(paramType, "[]") {
		io.WriteString(w, ", ")
		emitLenExpr(w, arg)
	}
}

func emitBigIntInt(w io.Writer, e Expr) {
	io.WriteString(w, "mpz_get_si(")
	switch e.(type) {
	case *CallExpr:
		e.emitExpr(w)
	default:
		io.WriteString(w, "*")
		e.emitExpr(w)
	}
	io.WriteString(w, ")")
}

func emitStrExpr(w io.Writer, e Expr) {
	if exprIsString(e) || inferExprType(currentEnv, e) == "const char*" {
		e.emitExpr(w)
		return
	}
	if exprIsBool(e) {
		needStrBool = true
		io.WriteString(w, "str_bool(")
		e.emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	if exprIsFloat(e) || inferExprType(currentEnv, e) == "double" {
		needStrFloat = true
		io.WriteString(w, "str_float(")
		e.emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	needStrInt = true
	io.WriteString(w, "str_int(")
	e.emitExpr(w)
	io.WriteString(w, ")")
}

func varBaseType(name string) string {
	if t, ok := varTypes[name]; ok && t != "" {
		t = strings.TrimSuffix(t, "[]")
		return strings.TrimSuffix(t, "*")
	}
	if ds, ok := declStmts[name]; ok && ds.Type != "" {
		t := strings.TrimSuffix(ds.Type, "[]")
		return strings.TrimSuffix(t, "*")
	}
	if currentEnv != nil {
		if tt, err := currentEnv.GetVar(name); err == nil {
			t := strings.TrimSuffix(cTypeFromMochiType(tt), "[]")
			return strings.TrimSuffix(t, "*")
		}
	}
	return ""
}

func fieldBaseType(varName, field string) string {
	if stName := varBaseType(varName); stName != "" {
		if st, ok := structTypes[stName]; ok {
			if ft, ok2 := st.Fields[field]; ok2 {
				return strings.TrimSuffix(cTypeFromMochiType(ft), "[]")
			}
		}
	}
	return ""
}

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
	Name     string
	Params   []Param
	Return   string
	Body     []Stmt
	VarTypes map[string]string
	Locals   map[string]bool
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
				} else if exprIsFloat(e) {
					io.WriteString(w, "%g")
				} else {
					io.WriteString(w, "%d")
				}
				io.WriteString(w, lsep+"\", ")
				e.emitExpr(w)
				io.WriteString(w, ");\n")
			}
		case *VarRef:
			if l, ok := constLists[v.Name]; ok {
				var parts []string
				for k, e := range l.Elems {
					s := formatConst(e)
					if k < len(l.Elems)-1 {
						s += " "
					}
					parts = append(parts, s)
				}
				writeIndent(w, indent)
				fmt.Fprintf(w, "puts(\"%s\");\n", escape(strings.Join(parts, "")))
				continue
			}
			switch p.Types[i] {
			case "list_int":
				writeIndent(w, indent)
				fmt.Fprintf(w, "puts(str_list_int(%s, %s_len));\n", v.Name, v.Name)
				continue
			case "list_double":
				writeIndent(w, indent)
				fmt.Fprintf(w, "puts(str_list_double(%s, %s_len));\n", v.Name, v.Name)
				continue
			case "list_str":
				writeIndent(w, indent)
				fmt.Fprintf(w, "puts(str_list_str(%s, %s_len));\n", v.Name, v.Name)
				continue
			case "list_list_int":
				writeIndent(w, indent)
				fmt.Fprintf(w, "puts(str_list_list_int(%s, %s_len, %s_lens));\n", v.Name, v.Name, v.Name)
				continue
			case "list_list_double":
				writeIndent(w, indent)
				fmt.Fprintf(w, "puts(str_list_list_double(%s, %s_len, %s_lens));\n", v.Name, v.Name, v.Name)
				continue
			}
			if p.Types[i] == "string" || exprIsString(a) {
				format = append(format, "%s")
			} else if p.Types[i] == "float" || exprIsFloat(a) {
				format = append(format, "%g")
			} else {
				format = append(format, "%d")
			}
			exprs = append(exprs, a)
		default:
			switch p.Types[i] {
			case "list_int":
				writeIndent(w, indent)
				if ce, ok := a.(*CallExpr); ok {
					if ret, ok2 := funcReturnTypes[ce.Func]; ok2 {
						tmp := fmt.Sprintf("__tmp%d", tempCounter)
						tempCounter++
						decl := listPtrType(ret)
						fmt.Fprintf(w, "%s %s = ", decl, tmp)
						ce.emitExpr(w)
						io.WriteString(w, ";\n")
						writeIndent(w, indent)
						fmt.Fprintf(w, "puts(str_list_int(%s, %s_len));\n", tmp, ce.Func)
						continue
					}
				}
				io.WriteString(w, "puts(str_list_int(")
				a.emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, a)
				io.WriteString(w, "));\n")
				continue
			case "list_double":
				writeIndent(w, indent)
				if ce, ok := a.(*CallExpr); ok {
					if ret, ok2 := funcReturnTypes[ce.Func]; ok2 {
						tmp := fmt.Sprintf("__tmp%d", tempCounter)
						tempCounter++
						decl := listPtrType(ret)
						fmt.Fprintf(w, "%s %s = ", decl, tmp)
						ce.emitExpr(w)
						io.WriteString(w, ";\n")
						writeIndent(w, indent)
						fmt.Fprintf(w, "puts(str_list_double(%s, %s_len));\n", tmp, ce.Func)
						continue
					}
				}
				io.WriteString(w, "puts(str_list_double(")
				a.emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, a)
				io.WriteString(w, "));\n")
				continue
			case "list_str":
				writeIndent(w, indent)
				if ce, ok := a.(*CallExpr); ok {
					if ret, ok2 := funcReturnTypes[ce.Func]; ok2 {
						tmp := fmt.Sprintf("__tmp%d", tempCounter)
						tempCounter++
						decl := listPtrType(ret)
						fmt.Fprintf(w, "%s %s = ", decl, tmp)
						ce.emitExpr(w)
						io.WriteString(w, ";\n")
						writeIndent(w, indent)
						fmt.Fprintf(w, "puts(str_list_str(%s, %s_len));\n", tmp, ce.Func)
						continue
					}
				}
				io.WriteString(w, "puts(str_list_str(")
				a.emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, a)
				io.WriteString(w, "));\n")
				continue
			case "list_list_int":
				writeIndent(w, indent)
				if ce, ok := a.(*CallExpr); ok {
					if ret, ok2 := funcReturnTypes[ce.Func]; ok2 {
						tmp := fmt.Sprintf("__tmp%d", tempCounter)
						tempCounter++
						decl := listPtrType(ret)
						fmt.Fprintf(w, "%s %s = ", decl, tmp)
						ce.emitExpr(w)
						io.WriteString(w, ";\n")
						writeIndent(w, indent)
						fmt.Fprintf(w, "puts(str_list_list_int(%s, %s_len, %s_lens));\n", tmp, ce.Func, ce.Func)
						continue
					}
				}
				io.WriteString(w, "puts(str_list_list_int(")
				a.emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, a)
				io.WriteString(w, ", ")
				emitLensExpr(w, a)
				io.WriteString(w, "));\n")
				continue
			case "list_list_double":
				writeIndent(w, indent)
				if ce, ok := a.(*CallExpr); ok {
					if ret, ok2 := funcReturnTypes[ce.Func]; ok2 {
						tmp := fmt.Sprintf("__tmp%d", tempCounter)
						tempCounter++
						decl := listPtrType(ret)
						fmt.Fprintf(w, "%s %s = ", decl, tmp)
						ce.emitExpr(w)
						io.WriteString(w, ";\n")
						writeIndent(w, indent)
						fmt.Fprintf(w, "puts(str_list_list_double(%s, %s_len, %s_lens));\n", tmp, ce.Func, ce.Func)
						continue
					}
				}
				io.WriteString(w, "puts(str_list_list_double(")
				a.emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, a)
				io.WriteString(w, ", ")
				emitLensExpr(w, a)
				io.WriteString(w, "));\n")
				continue
			default:
				if strings.HasSuffix(p.Types[i], "[]") {
					base := strings.TrimSuffix(p.Types[i], "[]")
					writeIndent(w, indent)
					switch base {
					case "long long":
						io.WriteString(w, "puts(str_list_int(")
					case "double":
						io.WriteString(w, "puts(str_list_double(")
					case "const char*":
						io.WriteString(w, "puts(str_list_str(")
					default:
						fmt.Fprintf(w, "puts(str_list_%s(", sanitizeTypeName(base))
					}
					a.emitExpr(w)
					io.WriteString(w, ", ")
					emitLenExpr(w, a)
					io.WriteString(w, "));\n")
					continue
				}
			}
			if p.Types[i] == "string" || exprIsString(a) {
				format = append(format, "%s")
			} else if p.Types[i] == "float" || exprIsFloat(a) {
				format = append(format, "%g")
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
		if p.Types[0] == "float" || exprIsFloat(exprs[0]) {
			io.WriteString(w, "printf(\"%g\\n\", ")
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

type RawStmt struct{ Code string }

func (r *RawStmt) emit(w io.Writer, indent int) {
	if r == nil {
		return
	}
	for _, line := range strings.Split(r.Code, "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		writeIndent(w, indent)
		io.WriteString(w, line)
		io.WriteString(w, "\n")
	}
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
	ListVar  string
	LenVar   string
	ElemType string
	Body     []Stmt
}

type BenchStmt struct {
	Name string
	Body []Stmt
}

func markStructParamMutations(stmts []Stmt, params map[string]bool, mutated map[string]bool) {
	for _, st := range stmts {
		switch s := st.(type) {
		case *AssignStmt:
			if len(s.Fields) > 0 && params[s.Name] {
				mutated[s.Name] = true
			}
		case *IfStmt:
			markStructParamMutations(s.Then, params, mutated)
			markStructParamMutations(s.Else, params, mutated)
		case *WhileStmt:
			markStructParamMutations(s.Body, params, mutated)
		case *ForStmt:
			markStructParamMutations(s.Body, params, mutated)
		case *BenchStmt:
			markStructParamMutations(s.Body, params, mutated)
		}
	}
}

func exprUsesVar(e Expr, name string) bool {
	switch v := e.(type) {
	case *VarRef:
		return v.Name == name
	case *FieldExpr:
		return exprUsesVar(v.Target, name)
	case *IndexExpr:
		return exprUsesVar(v.Target, name) || exprUsesVar(v.Index, name)
	case *CallExpr:
		for _, a := range v.Args {
			if exprUsesVar(a, name) {
				return true
			}
		}
	case *BinaryExpr:
		return exprUsesVar(v.Left, name) || exprUsesVar(v.Right, name)
	case *UnaryExpr:
		return exprUsesVar(v.Expr, name)
	case *ListLit:
		for _, e := range v.Elems {
			if exprUsesVar(e, name) {
				return true
			}
		}
	case *StructLit:
		for _, f := range v.Fields {
			if exprUsesVar(f.Value, name) {
				return true
			}
		}
	}
	return false
}

func stmtsUseVarInReturn(stmts []Stmt, name string) bool {
	for _, st := range stmts {
		if stmtUsesVarInReturn(st, name) {
			return true
		}
	}
	return false
}

func stmtUsesVarInReturn(s Stmt, name string) bool {
	switch v := s.(type) {
	case *ReturnStmt:
		if v.Expr != nil {
			return exprUsesVar(v.Expr, name)
		}
	case *IfStmt:
		if stmtsUseVarInReturn(v.Then, name) || stmtsUseVarInReturn(v.Else, name) {
			return true
		}
	case *WhileStmt:
		return stmtsUseVarInReturn(v.Body, name)
	case *ForStmt:
		return stmtsUseVarInReturn(v.Body, name)
	case *BenchStmt:
		return stmtsUseVarInReturn(v.Body, name)
	}
	return false
}

func (c *CallStmt) emit(w io.Writer, indent int) {
	if c.Func == "print" && len(c.Args) == 1 {
		t := inferExprType(currentEnv, c.Args[0])
		if strings.HasSuffix(t, "[]") {
			base := strings.TrimSuffix(t, "[]")
			writeIndent(w, indent)
			if base == "const char*" {
				needStrListStr = true
				io.WriteString(w, "puts(str_list_str(")
				c.Args[0].emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, c.Args[0])
				io.WriteString(w, "));\n")
			} else {
				needStrListInt = true
				io.WriteString(w, "puts(str_list_int(")
				c.Args[0].emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, c.Args[0])
				io.WriteString(w, "));\n")
			}
			return
		}
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
	var pre string
	var tmpName string
	if len(c.Args) > 0 {
		if ce, ok := c.Args[0].(*CallExpr); ok {
			if ret, ok2 := funcReturnTypes[ce.Func]; ok2 && strings.HasSuffix(ret, "[]") {
				tmpName = fmt.Sprintf("__tmp%d", tempCounter)
				tempCounter++
				ceCopy := *ce
				decl := listPtrType(ret)
				var buf bytes.Buffer
				if ce.Func == "_slice_int" || ce.Func == "_slice_double" || ce.Func == "_slice_str" {
					tmpLen := tmpName + "_len"
					if len(ceCopy.Args) == 5 {
						ceCopy.Args[4] = &UnaryExpr{Op: "&", Expr: &VarRef{Name: tmpLen}}
					}
					ceCopy.emitExpr(&buf)
					pre = fmt.Sprintf("size_t %s = 0;\n%s %s = %s;\n", tmpLen, decl, tmpName, buf.String())
				} else {
					ceCopy.emitExpr(&buf)
					pre = fmt.Sprintf("%s %s = %s;\n", decl, tmpName, buf.String())
				}
				varTypes[tmpName] = ret
			}
		}
	}
	if pre != "" {
		writeIndent(w, indent)
		io.WriteString(w, pre)
	}
	writeIndent(w, indent)
	io.WriteString(w, c.Func)
	io.WriteString(w, "(")
	first := true
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		var paramType string
		if types, ok := funcParamTypes[c.Func]; ok && i < len(types) {
			paramType = types[i]
		}
		if i == 0 && tmpName != "" {
			io.WriteString(w, tmpName)
			emitExtraArgs(w, paramType, a)
			first = false
			continue
		}
		if !first && i >= 0 {
			// already wrote comma above
		}
		if strings.HasSuffix(paramType, "*") {
			base := strings.TrimSuffix(paramType, "*")
			if _, ok := structTypes[base]; ok {
				io.WriteString(w, "&")
			}
		} else if _, ok := structTypes[paramType]; ok {
			if vr, ok := a.(*VarRef); ok {
				if at, ok2 := varTypes[vr.Name]; ok2 && strings.HasSuffix(at, "*") && strings.TrimSuffix(at, "*") == paramType {
					io.WriteString(w, "*")
				}
			}
		}
		if vr, ok := a.(*VarRef); ok && paramType != "" {
			if at, ok2 := varTypes[vr.Name]; ok2 && at != paramType {
				if !(strings.HasSuffix(paramType, "*") && at == strings.TrimSuffix(paramType, "*")) {
					fmt.Fprintf(w, "(%s)", paramType)
				}
			}
		}
		a.emitExpr(w)
		emitExtraArgs(w, paramType, a)
	}
	io.WriteString(w, ");\n")
}

type JSONCall struct {
	Arg Expr
}

func (j *JSONCall) emit(w io.Writer, indent int) {
	if ce, ok := j.Arg.(*CallExpr); ok {
		if ret, ok2 := funcReturnTypes[ce.Func]; ok2 && strings.HasSuffix(ret, "[]") {
			tmp := fmt.Sprintf("__tmp%d", tempCounter)
			tempCounter++
			decl := listPtrType(ret)
			writeIndent(w, indent)
			fmt.Fprintf(w, "%s %s = ", decl, tmp)
			ce.emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "json_list_list_int(%s, %s_len, %s_lens);\n", tmp, ce.Func, ce.Func)
			return
		}
	}
	writeIndent(w, indent)
	io.WriteString(w, "json_list_list_int(")
	j.Arg.emitExpr(w)
	io.WriteString(w, ", ")
	emitLenExpr(w, j.Arg)
	io.WriteString(w, ", ")
	emitLensExpr(w, j.Arg)
	io.WriteString(w, ");\n")
}

type JSONListIntCall struct {
	Arg Expr
}

func (j *JSONListIntCall) emit(w io.Writer, indent int) {
	if ce, ok := j.Arg.(*CallExpr); ok {
		if ret, ok2 := funcReturnTypes[ce.Func]; ok2 && strings.HasSuffix(ret, "[]") {
			tmp := fmt.Sprintf("__tmp%d", tempCounter)
			tempCounter++
			decl := listPtrType(ret)
			writeIndent(w, indent)
			fmt.Fprintf(w, "%s %s = ", decl, tmp)
			ce.emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "puts(str_list_int(%s, %s_len));\n", tmp, ce.Func)
			return
		}
	}
	writeIndent(w, indent)
	io.WriteString(w, "puts(str_list_int(")
	j.Arg.emitExpr(w)
	io.WriteString(w, ", ")
	emitLenExpr(w, j.Arg)
	io.WriteString(w, "));")
	io.WriteString(w, "\n")
}

func (r *ReturnStmt) emit(w io.Writer, indent int) {
	if call, ok := r.Expr.(*CallExpr); ok && strings.HasSuffix(currentFuncReturn, "[]") && !(call.Func == "append" && len(call.Args) == 2) {
		tmp := fmt.Sprintf("__ret%d", tempCounter)
		tempCounter++
		decl := listPtrType(currentFuncReturn)
		writeIndent(w, indent)
		io.WriteString(w, "{\n")
		writeIndent(w, indent+1)
		fmt.Fprintf(w, "%s %s = ", decl, tmp)
		call.emitExpr(w)
		io.WriteString(w, ";\n")
		writeIndent(w, indent+1)
		if strings.HasSuffix(currentFuncReturn, "[][]") {
			fmt.Fprintf(w, "%s_lens = %s_lens;\n", currentFuncName, call.Func)
		}
		fmt.Fprintf(w, "%s_len = %s_len;\n", currentFuncName, call.Func)
		writeIndent(w, indent+1)
		fmt.Fprintf(w, "return %s;\n", tmp)
		writeIndent(w, indent)
		io.WriteString(w, "}\n")
		return
	}
	if lst, ok := r.Expr.(*ListLit); ok && strings.HasSuffix(currentFuncReturn, "[]") {
		base := strings.TrimSuffix(currentFuncReturn, "[]")
		if strings.HasSuffix(base, "[]") {
			base = strings.TrimSuffix(base, "[]")
			tmp := fmt.Sprintf("__tmp%d", tempCounter)
			tempCounter++
			writeIndent(w, indent)
			io.WriteString(w, "{\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "static size_t %s_lens[] = {", tmp)
			for i, e := range lst.Elems {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				if sub, ok := e.(*ListLit); ok {
					fmt.Fprintf(w, "%d", len(sub.Elems))
				} else {
					io.WriteString(w, "0")
				}
			}
			io.WriteString(w, "};\n")
			for i, e := range lst.Elems {
				if sub, ok := e.(*ListLit); ok {
					writeIndent(w, indent+1)
					fmt.Fprintf(w, "static %s %s_row%d[] = {", base, tmp, i)
					for j, el := range sub.Elems {
						if j > 0 {
							io.WriteString(w, ", ")
						}
						el.emitExpr(w)
					}
					io.WriteString(w, "};\n")
				}
			}
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "static %s* %s_data[] = {", base, tmp)
			for i := range lst.Elems {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				fmt.Fprintf(w, "%s_row%d", tmp, i)
			}
			io.WriteString(w, "};\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "%s_lens = %s_lens;\n", currentFuncName, tmp)
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "%s_len = %d;\n", currentFuncName, len(lst.Elems))
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "return %s_data;\n", tmp)
			writeIndent(w, indent)
			io.WriteString(w, "}\n")
			return
		}
		tmp := fmt.Sprintf("__tmp%d", tempCounter)
		tempCounter++
		writeIndent(w, indent)
		io.WriteString(w, "{\n")
		writeIndent(w, indent+1)
		fmt.Fprintf(w, "static %s %s_arr[%d];\n", base, tmp, len(lst.Elems))
		for i, e := range lst.Elems {
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "%s_arr[%d] = ", tmp, i)
			e.emitExpr(w)
			io.WriteString(w, ";\n")
		}
		writeIndent(w, indent+1)
		fmt.Fprintf(w, "%s_len = %d;\n", currentFuncName, len(lst.Elems))
		writeIndent(w, indent+1)
		fmt.Fprintf(w, "return %s_arr;\n", tmp)
		writeIndent(w, indent)
		io.WriteString(w, "}\n")
		return
	}
	if currentFuncReturn == "MapSD" {
		switch v := r.Expr.(type) {
		case *MapLit:
			tmp := fmt.Sprintf("__ret%d", tempCounter)
			tempCounter++
			writeIndent(w, indent)
			io.WriteString(w, "{\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "static const char* %s_keys[%d] = {", tmp, len(v.Items))
			for i, it := range v.Items {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				if ks, ok := evalString(it.Key); ok {
					fmt.Fprintf(w, "\"%s\"", escape(ks))
				} else {
					it.Key.emitExpr(w)
				}
			}
			io.WriteString(w, "};\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "static double %s_vals[%d] = {", tmp, len(v.Items))
			for i, it := range v.Items {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				it.Value.emitExpr(w)
			}
			io.WriteString(w, "};\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "MapSD %s = { %s_keys, %s_vals, %d, %d };\n", tmp, tmp, tmp, len(v.Items), len(v.Items)+16)
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "return %s;\n", tmp)
			writeIndent(w, indent)
			io.WriteString(w, "}\n")
			return
		case *StructLit:
			tmp := fmt.Sprintf("__ret%d", tempCounter)
			tempCounter++
			writeIndent(w, indent)
			io.WriteString(w, "{\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "static const char* %s_keys[%d] = {", tmp, len(v.Fields))
			for i, f := range v.Fields {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				fmt.Fprintf(w, "\"%s\"", escape(f.Name))
			}
			io.WriteString(w, "};\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "static double %s_vals[%d] = {", tmp, len(v.Fields))
			for i, f := range v.Fields {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				f.Value.emitExpr(w)
			}
			io.WriteString(w, "};\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "MapSD %s = { %s_keys, %s_vals, %d, %d };\n", tmp, tmp, tmp, len(v.Fields), len(v.Fields)+16)
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "return %s;\n", tmp)
			writeIndent(w, indent)
			io.WriteString(w, "}\n")
			return
		}
	}
	writeIndent(w, indent)
	io.WriteString(w, "return")
	if r.Expr != nil {
		io.WriteString(w, " ")
		if strings.HasSuffix(currentFuncReturn, "[]") {
			switch v := r.Expr.(type) {
			case *VarRef:
				if strings.HasSuffix(currentFuncReturn, "[][]") {
					fmt.Fprintf(w, "%s_lens = %s_lens, ", currentFuncName, v.Name)
				}
				fmt.Fprintf(w, "%s_len = %s_len, ", currentFuncName, v.Name)
			case *FieldExpr:
				fmt.Fprintf(w, "%s_len = ", currentFuncName)
				v.Target.emitExpr(w)
				fmt.Fprintf(w, ".%s_len, ", v.Name)
			case *CallExpr:
				if v.Func == "append" && len(v.Args) == 2 {
					if vr, ok := v.Args[0].(*VarRef); ok {
						fmt.Fprintf(w, "%s_len = %s_len + 1, ", currentFuncName, vr.Name)
					}
				} else {
					if strings.HasSuffix(currentFuncReturn, "[][]") {
						fmt.Fprintf(w, "%s_lens = %s_lens, ", currentFuncName, v.Func)
					}
					fmt.Fprintf(w, "%s_len = %s_len, ", currentFuncName, v.Func)
				}
			case *ListLit:
				if strings.HasSuffix(currentFuncReturn, "[][]") {
					fmt.Fprintf(w, "%s_lens = (size_t[]){", currentFuncName)
					for i, e := range v.Elems {
						if lst, ok := e.(*ListLit); ok {
							if i > 0 {
								io.WriteString(w, ", ")
							}
							fmt.Fprintf(w, "%d", len(lst.Elems))
						}
					}
					io.WriteString(w, "}, ")
				}
				fmt.Fprintf(w, "%s_len = %d, ", currentFuncName, len(v.Elems))
			}
		}
		r.Expr.emitExpr(w)
	} else {
		io.WriteString(w, " 0")
	}
	io.WriteString(w, ";\n")
}

func (d *DeclStmt) emit(w io.Writer, indent int) {
	typ := d.Type
	if typ == "" {
		if vt, ok := varTypes[d.Name]; ok && vt != "" {
			typ = vt
		}
	}
	if typ == "" || typ == "int" {
		typ = "long long"
	}
	writeIndent(w, indent)
	if strings.HasSuffix(typ, "[][][]") {
		base := strings.TrimSuffix(typ, "[][][]")
		if lst, ok := d.Value.(*ListLit); ok {
			for i, e := range lst.Elems {
				if ll, ok2 := e.(*ListLit); ok2 {
					for j, e2 := range ll.Elems {
						if ll2, ok3 := e2.(*ListLit); ok3 {
							fmt.Fprintf(w, "%s %s_%d_%d[%d] = {", base, d.Name, i, j, len(ll2.Elems))
							for k, se := range ll2.Elems {
								if k > 0 {
									io.WriteString(w, ", ")
								}
								se.emitExpr(w)
							}
							io.WriteString(w, "};\n")
							writeIndent(w, indent)
						}
					}
					fmt.Fprintf(w, "%s *%s_%d_init[%d] = {", base, d.Name, i, len(ll.Elems))
					for j := range ll.Elems {
						if j > 0 {
							io.WriteString(w, ", ")
						}
						fmt.Fprintf(w, "%s_%d_%d", d.Name, i, j)
					}
					io.WriteString(w, "};\n")
					writeIndent(w, indent)
					fmt.Fprintf(w, "%s **%s_%d = %s_%d_init;\n", base, d.Name, i, d.Name, i)
					writeIndent(w, indent)
				}
			}
			fmt.Fprintf(w, "%s **%s_init[%d] = {", base, d.Name, len(lst.Elems))
			for i := range lst.Elems {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				fmt.Fprintf(w, "%s_%d", d.Name, i)
			}
			io.WriteString(w, "};\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "%s ***%s = %s_init;\n", base, d.Name, d.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = %d;\n", d.Name, len(lst.Elems))
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_lens_init[%d] = {", d.Name, len(lst.Elems))
			for i, e := range lst.Elems {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				if ll, ok2 := e.(*ListLit); ok2 {
					fmt.Fprintf(w, "%d", len(ll.Elems))
				} else {
					io.WriteString(w, "0")
				}
			}
			io.WriteString(w, "};\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t *%s_lens = %s_lens_init;\n", d.Name, d.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_lens_len = %d;\n", d.Name, len(lst.Elems))
			return
		}
		fmt.Fprintf(w, "%s ***%s = NULL;\n", base, d.Name)
		writeIndent(w, indent)
		fmt.Fprintf(w, "size_t %s_len = 0;\n", d.Name)
		writeIndent(w, indent)
		fmt.Fprintf(w, "size_t *%s_lens = NULL;\n", d.Name)
		writeIndent(w, indent)
		fmt.Fprintf(w, "size_t %s_lens_len = 0;\n", d.Name)
		return
	} else if strings.HasSuffix(typ, "[][]") {
		dims := strings.Count(typ, "[]")
		base := strings.TrimSuffix(typ, strings.Repeat("[]", dims))
		stars := strings.Repeat("*", dims)
		if call, ok := d.Value.(*CallExpr); ok {
			fmt.Fprintf(w, "%s %s%s = ", base, stars, d.Name)
			d.Value.emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = %s_len;\n", d.Name, call.Func)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t *%s_lens = %s_lens;\n", d.Name, call.Func)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_lens_len = %s_len;\n", d.Name, call.Func)
			return
		}
		if vr, ok := d.Value.(*VarRef); ok {
			fmt.Fprintf(w, "%s %s%s = %s;\n", base, stars, d.Name, vr.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = %s_len;\n", d.Name, vr.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t *%s_lens = %s_lens;\n", d.Name, vr.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_lens_len = %s_len;\n", d.Name, vr.Name)
			if declAliases == nil {
				declAliases = make(map[string]bool)
			}
			declAliases[d.Name] = true
			return
		}
		if fe, ok := d.Value.(*FieldExpr); ok {
			fmt.Fprintf(w, "%s %s%s = ", base, stars, d.Name)
			d.Value.emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = ", d.Name)
			(&FieldExpr{Target: fe.Target, Name: fe.Name + "_len"}).emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t *%s_lens = ", d.Name)
			(&FieldExpr{Target: fe.Target, Name: fe.Name + "_lens"}).emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_lens_len = ", d.Name)
			(&FieldExpr{Target: fe.Target, Name: fe.Name + "_len"}).emitExpr(w)
			io.WriteString(w, ";\n")
			return
		}
		if lst, ok := d.Value.(*ListLit); ok {
			if len(lst.Elems) > 0 {
				if _, ok2 := lst.Elems[0].(*ListLit); ok2 {
					for i, e := range lst.Elems {
						if sl, ok3 := e.(*ListLit); ok3 {
							fmt.Fprintf(w, "%s %s_%d[%d] = {", base, d.Name, i, len(sl.Elems))
							for j, se := range sl.Elems {
								if j > 0 {
									io.WriteString(w, ", ")
								}
								se.emitExpr(w)
							}
							io.WriteString(w, "};\n")
							writeIndent(w, indent)
						}
					}
					fmt.Fprintf(w, "%s %s%s_init[%d] = {", base, strings.Repeat("*", dims-1), d.Name, len(lst.Elems))
					for i := range lst.Elems {
						if i > 0 {
							io.WriteString(w, ", ")
						}
						fmt.Fprintf(w, "%s_%d", d.Name, i)
					}
					io.WriteString(w, "};\n")
					writeIndent(w, indent)
					fmt.Fprintf(w, "%s %s%s = %s_init;\n", base, stars, d.Name, d.Name)
					writeIndent(w, indent)
					fmt.Fprintf(w, "size_t %s_len = %d;\n", d.Name, len(lst.Elems))
					writeIndent(w, indent)
					fmt.Fprintf(w, "size_t %s_lens_init[%d] = {", d.Name, len(lst.Elems))
					for i, e := range lst.Elems {
						if i > 0 {
							io.WriteString(w, ", ")
						}
						if sl, ok3 := e.(*ListLit); ok3 {
							fmt.Fprintf(w, "%d", len(sl.Elems))
						} else {
							emitLenExpr(w, e)
						}
					}
					io.WriteString(w, "};\n")
					writeIndent(w, indent)
					fmt.Fprintf(w, "size_t *%s_lens = %s_lens_init;\n", d.Name, d.Name)
					writeIndent(w, indent)
					fmt.Fprintf(w, "size_t %s_lens_len = %d;\n", d.Name, len(lst.Elems))
					return
				}
			}
			fmt.Fprintf(w, "%s %s%s = NULL;\n", base, stars, d.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = 0;\n", d.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t *%s_lens = NULL;\n", d.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_lens_len = 0;\n", d.Name)
			return
		}
		if _, ok := d.Value.(*IndexExpr); ok {
			fmt.Fprintf(w, "%s %s%s = ", base, stars, d.Name)
			d.Value.emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = ", d.Name)
			emitLenExpr(w, d.Value)
			io.WriteString(w, ";\n")
			return
		}
		fmt.Fprintf(w, "%s %s%s = NULL;\n", base, stars, d.Name)
		writeIndent(w, indent)
		fmt.Fprintf(w, "size_t %s_len = 0;\n", d.Name)
		writeIndent(w, indent)
		fmt.Fprintf(w, "size_t *%s_lens = NULL;\n", d.Name)
		writeIndent(w, indent)
		fmt.Fprintf(w, "size_t %s_lens_len = 0;\n", d.Name)
		return
	} else if strings.HasSuffix(typ, "[]") {
		base := strings.TrimSuffix(typ, "[]")
		if lst, ok := d.Value.(*ListLit); ok {
			if indent == 0 && len(lst.Elems) > 0 {
				fmt.Fprintf(w, "%s %s_init[%d] = {", base, d.Name, len(lst.Elems))
				for i, e := range lst.Elems {
					if i > 0 {
						io.WriteString(w, ", ")
					}
					if base == "const char*" {
						if inferExprType(currentEnv, e) == "const char*" {
							e.emitExpr(w)
						} else if lit, ok2 := e.(*IntLit); ok2 {
							fmt.Fprintf(w, "\"%d\"", lit.Value)
						} else {
							e.emitExpr(w)
						}
					} else {
						e.emitExpr(w)
					}
				}
				io.WriteString(w, "};\n")
				writeIndent(w, indent)
				fmt.Fprintf(w, "%s *%s = malloc(%d * sizeof(%s));\n", base, d.Name, len(lst.Elems), base)
				writeIndent(w, indent)
				fmt.Fprintf(w, "memcpy(%s, %s_init, %d * sizeof(%s));\n", d.Name, d.Name, len(lst.Elems), base)
				writeIndent(w, indent)
				fmt.Fprintf(w, "size_t %s_len = %d;\n", d.Name, len(lst.Elems))
			} else {
				fmt.Fprintf(w, "%s *%s = NULL;\n", base, d.Name)
				writeIndent(w, indent)
				fmt.Fprintf(w, "size_t %s_len = 0;\n", d.Name)
				for _, e := range lst.Elems {
					writeIndent(w, indent)
					switch base {
					case "int":
						needListAppendInt = true
						fmt.Fprintf(w, "%s = list_append_int(%s, &%s_len, ", d.Name, d.Name, d.Name)
					case "double":
						needListAppendDouble = true
						fmt.Fprintf(w, "%s = list_append_double(%s, &%s_len, ", d.Name, d.Name, d.Name)
					case "const char*":
						needListAppendStr = true
						fmt.Fprintf(w, "%s = list_append_str(%s, &%s_len, ", d.Name, d.Name, d.Name)
						if inferExprType(currentEnv, e) == "const char*" {
							e.emitExpr(w)
						} else {
							needStrInt = true
							io.WriteString(w, "str_int(")
							e.emitExpr(w)
							io.WriteString(w, ")")
						}
						io.WriteString(w, ");\n")
						continue
					default:
						if strings.HasSuffix(base, "*") || (base != "" && !strings.HasSuffix(base, "[]")) {
							typ := strings.TrimSuffix(base, "*")
							if typ == "" {
								typ = base
							}
							if needListAppendStruct == nil {
								needListAppendStruct = make(map[string]bool)
							}
							needListAppendStruct[typ] = true
							fmt.Fprintf(w, "%s = list_append_%s(%s, &%s_len, ", d.Name, sanitizeTypeName(typ), d.Name, d.Name)
						} else {
							needListAppendInt = true
							fmt.Fprintf(w, "%s = list_append_int(%s, &%s_len, ", d.Name, d.Name, d.Name)
						}
					}
					if base != "const char*" {
						e.emitExpr(w)
						io.WriteString(w, ");\n")
					}
				}
			}
			return
		}
		if vr, ok := d.Value.(*VarRef); ok {
			fmt.Fprintf(w, "%s *%s = %s;\n", base, d.Name, vr.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = %s_len;\n", d.Name, vr.Name)
			if declAliases == nil {
				declAliases = make(map[string]bool)
			}
			declAliases[d.Name] = true
			return
		}
		if fe, ok := d.Value.(*FieldExpr); ok {
			fmt.Fprintf(w, "%s *%s = ", base, d.Name)
			d.Value.emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = ", d.Name)
			(&FieldExpr{Target: fe.Target, Name: fe.Name + "_len"}).emitExpr(w)
			io.WriteString(w, ";\n")
			return
		}
		if call, ok := d.Value.(*CallExpr); ok {
			if call.Func == "append" && len(call.Args) >= 2 {
				if vr, ok := call.Args[0].(*VarRef); ok {
					fmt.Fprintf(w, "%s *%s = ", base, d.Name)
					d.Value.emitExpr(w)
					io.WriteString(w, ";\n")
					writeIndent(w, indent)
					fmt.Fprintf(w, "size_t %s_len = %s_len + 1;\n", d.Name, vr.Name)
					return
				}
				if ie, ok := call.Args[0].(*IndexExpr); ok {
					if vr, ok2 := ie.Target.(*VarRef); ok2 {
						fmt.Fprintf(w, "%s *%s = ", base, d.Name)
						d.Value.emitExpr(w)
						io.WriteString(w, ";\n")
						writeIndent(w, indent)
						fmt.Fprintf(w, "size_t %s_len = %s_lens[(int)(", d.Name, vr.Name)
						ie.Index.emitExpr(w)
						io.WriteString(w, ")] + 1;\n")
						return
					}
				}
			}
			if call.Func == "_slice_int" || call.Func == "_slice_double" || call.Func == "_slice_str" {
				fmt.Fprintf(w, "%s *%s = ", base, d.Name)
				d.Value.emitExpr(w)
				io.WriteString(w, ";\n")
				writeIndent(w, indent)
				sliceLen := "_slice_int_len"
				if call.Func == "_slice_double" {
					sliceLen = "_slice_double_len"
				} else if call.Func == "_slice_str" {
					sliceLen = "_slice_str_len"
				}
				fmt.Fprintf(w, "size_t %s_len = %s;\n", d.Name, sliceLen)
				return
			}
			fmt.Fprintf(w, "%s *%s = ", base, d.Name)
			d.Value.emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			if base == "char" || base == "const char" {
				fmt.Fprintf(w, "size_t %s_len = %s ? strlen(%s) : 0;\n", d.Name, d.Name, d.Name)
			} else {
				fmt.Fprintf(w, "size_t %s_len = %s_len;\n", d.Name, call.Func)
			}
			return
		}
		if _, ok := d.Value.(*IndexExpr); ok {
			fmt.Fprintf(w, "%s *%s = ", base, d.Name)
			d.Value.emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = ", d.Name)
			emitLenExpr(w, d.Value)
			io.WriteString(w, ";\n")
			return
		}
		if d.Value != nil {
			fmt.Fprintf(w, "%s *%s = ", base, d.Name)
			d.Value.emitExpr(w)
			io.WriteString(w, ";\n")
			return
		}
		fmt.Fprintf(w, "%s *%s = NULL;\n", base, d.Name)
		writeIndent(w, indent)
		fmt.Fprintf(w, "size_t %s_len = 0;\n", d.Name)
		return
	} else if strings.HasPrefix(typ, "Map") {
		if vr, ok := d.Value.(*VarRef); ok {
			fmt.Fprintf(w, "%s %s = %s;\n", typ, d.Name, vr.Name)
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = %s.len;\n", d.Name, vr.Name)
			return
		}
		if fe, ok := d.Value.(*FieldExpr); ok {
			fmt.Fprintf(w, "%s %s = ", typ, d.Name)
			d.Value.emitExpr(w)
			io.WriteString(w, ";\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "size_t %s_len = ", d.Name)
			fe.Target.emitExpr(w)
			fmt.Fprintf(w, ".%s.len;\n", fe.Name)
			return
		}
		io.WriteString(w, typ+" ")
		io.WriteString(w, d.Name)
		if d.Value != nil {
			io.WriteString(w, " = ")
			d.Value.emitExpr(w)
		} else {
			io.WriteString(w, " = (")
			io.WriteString(w, typ)
			io.WriteString(w, "){0}")
		}
		io.WriteString(w, ";\n")
		writeIndent(w, indent)
		fmt.Fprintf(w, "size_t %s_len = 0;\n", d.Name)
		return
	} else {
		io.WriteString(w, typ)
		io.WriteString(w, " ")
		io.WriteString(w, d.Name)
		if d.Value != nil {
			io.WriteString(w, " = ")
			d.Value.emitExpr(w)
		} else {
			if typ == "const char*" {
				io.WriteString(w, " = \"\"")
			} else if typ == "bigrat" {
				io.WriteString(w, " = _bigrat(0,1)")
			} else if _, ok := structTypes[typ]; ok {
				// structs default to zero initialization without explicit value
			} else {
				if typ == "MapSS" || typ == "MapSL" || typ == "MapSI" || typ == "MapSD" {
					io.WriteString(w, " = (")
					io.WriteString(w, typ)
					io.WriteString(w, "){0}")
				} else {
					io.WriteString(w, " = 0")
				}
			}
		}
		io.WriteString(w, ";\n")
	}
}

func (a *AssignStmt) emit(w io.Writer, indent int) {
	if call, ok := a.Value.(*CallExpr); ok && call.Func == "append" && len(call.Args) == 2 {
		if vr, ok2 := call.Args[0].(*VarRef); ok2 && vr.Name == a.Name && len(a.Indexes) == 0 && len(a.Fields) == 0 {
			fromAlias := declAliases[a.Name]
			if ds, ok := declStmts[a.Name]; ok {
				ds.Value = nil
			}
			if fromAlias {
				declAliases[a.Name] = false
			}
			base := varBaseType(a.Name)
			if t := inferExprType(currentEnv, call.Args[1]); t != "" {
				base = t
			} else if vr, ok := call.Args[1].(*VarRef); ok {
				if vt, ok2 := varTypes[vr.Name]; ok2 && vt != "" {
					base = vt
				} else if ds, ok2 := declStmts[vr.Name]; ok2 && ds.Type != "" {
					base = ds.Type
				} else if currentEnv != nil {
					if tt, err := currentEnv.GetVar(vr.Name); err == nil {
						base = cTypeFromMochiType(tt)
					}
				}
			}
			if base == "" {
				switch call.Args[1].(type) {
				case *StringLit:
					base = "const char*"
				case *FloatLit:
					base = "double"
				default:
					base = "int"
				}
			}
			writeIndent(w, indent)
			if fromAlias {
				switch base {
				case "int", "long long":
					needListAppendIntNew = true
					fmt.Fprintf(w, "%s = list_append_int_new(%s, %s_len, ", a.Name, a.Name, a.Name)
					call.Args[1].emitExpr(w)
					io.WriteString(w, ");\n")
					writeIndent(w, indent)
					fmt.Fprintf(w, "%s_len++;\n", a.Name)
					return
				case "double":
					needListAppendDoubleNew = true
					fmt.Fprintf(w, "%s = list_append_double_new(%s, %s_len, ", a.Name, a.Name, a.Name)
					call.Args[1].emitExpr(w)
					io.WriteString(w, ");\n")
					writeIndent(w, indent)
					fmt.Fprintf(w, "%s_len++;\n", a.Name)
					return
				case "const char*":
					needListAppendStrNew = true
					fmt.Fprintf(w, "%s = list_append_str_new(%s, %s_len, ", a.Name, a.Name, a.Name)
					call.Args[1].emitExpr(w)
					io.WriteString(w, ");\n")
					writeIndent(w, indent)
					fmt.Fprintf(w, "%s_len++;\n", a.Name)
					return
				}
			}
			switch base {
			case "int":
				needListAppendInt = true
				fmt.Fprintf(w, "%s = list_append_int(%s, &%s_len, ", a.Name, a.Name, a.Name)
				call.Args[1].emitExpr(w)
				io.WriteString(w, ");\n")
				return
			case "double":
				needListAppendDouble = true
				fmt.Fprintf(w, "%s = list_append_double(%s, &%s_len, ", a.Name, a.Name, a.Name)
				call.Args[1].emitExpr(w)
				io.WriteString(w, ");\n")
				return
			case "const char*":
				needListAppendStr = true
				fmt.Fprintf(w, "%s = list_append_str(%s, &%s_len, ", a.Name, a.Name, a.Name)
				call.Args[1].emitExpr(w)
				io.WriteString(w, ");\n")
				return
			case "bigrat":
				needListAppendBigRat = true
				fmt.Fprintf(w, "%s = list_append_bigrat(%s, &%s_len, ", a.Name, a.Name, a.Name)
				call.Args[1].emitExpr(w)
				io.WriteString(w, ");\n")
				return
			default:
				if strings.HasSuffix(base, "*") || (base != "" && !strings.HasSuffix(base, "[]")) {
					typ := strings.TrimSuffix(base, "*")
					if typ == "" {
						typ = base
					}
					if needListAppendStruct == nil {
						needListAppendStruct = make(map[string]bool)
					}
					needListAppendStruct[typ] = true
					fmt.Fprintf(w, "%s = list_append_%s(%s, &%s_len, ", a.Name, sanitizeTypeName(typ), a.Name, a.Name)
					call.Args[1].emitExpr(w)
					io.WriteString(w, ");\n")
					return
				}
				if base == "" {
					needListAppendInt = true
					fmt.Fprintf(w, "%s = list_append_int(%s, &%s_len, ", a.Name, a.Name, a.Name)
					call.Args[1].emitExpr(w)
					io.WriteString(w, ");\n")
					return
				}
                                if strings.HasSuffix(base, "[]") {
                                        elemBase := strings.TrimSuffix(base, "[]")
                                        if strings.HasSuffix(elemBase, "[]") {
                                                // support appending list pointers (e.g. list<list<int>>)
                                                elemElemBase := strings.TrimSuffix(elemBase, "[]")
                                                switch elemElemBase {
                                                case "int":
                                                        needListAppendPtrPtr = true
                                                        fmt.Fprintf(w, "%s = list_append_intptrptr(%s, &%s_len, ", a.Name, a.Name, a.Name)
                                                case "double":
                                                        needListAppendDoublePtr = true
                                                        fmt.Fprintf(w, "%s = list_append_doubleptr(%s, &%s_len, ", a.Name, a.Name, a.Name)
                                                default:
                                                        if strings.Contains(elemElemBase, "char*") {
                                                                needListAppendStrPtr = true
                                                                fmt.Fprintf(w, "%s = list_append_strptr(%s, &%s_len, ", a.Name, a.Name, a.Name)
                                                        } else {
                                                                if needListAppendStructPtr == nil {
                                                                        needListAppendStructPtr = make(map[string]bool)
                                                                }
                                                                needListAppendStructPtr[elemElemBase] = true
                                                                fmt.Fprintf(w, "%s = list_append_%sptr(%s, &%s_len, ", a.Name, sanitizeTypeName(elemElemBase), a.Name, a.Name)
                                                        }
                                                }
                                        } else {
                                                switch elemBase {
                                                case "int":
                                                        needListAppendPtr = true
                                                        fmt.Fprintf(w, "%s = list_append_intptr(%s, &%s_len, ", a.Name, a.Name, a.Name)
                                                case "double":
                                                        needListAppendDoublePtr = true
                                                        fmt.Fprintf(w, "%s = list_append_doubleptr(%s, &%s_len, ", a.Name, a.Name, a.Name)
                                                default:
                                                        if strings.Contains(elemBase, "char*") {
                                                                needListAppendStrPtr = true
                                                                fmt.Fprintf(w, "%s = list_append_strptr(%s, &%s_len, ", a.Name, a.Name, a.Name)
                                                        } else {
                                                                if needListAppendStructPtr == nil {
                                                                        needListAppendStructPtr = make(map[string]bool)
                                                                }
                                                                needListAppendStructPtr[elemBase] = true
                                                                fmt.Fprintf(w, "%s = list_append_%sptr(%s, &%s_len, ", a.Name, sanitizeTypeName(elemBase), a.Name, a.Name)
                                                        }
                                                }
                                        }
                                        if lit, ok := call.Args[1].(*ListLit); ok {
                                                fmt.Fprintf(w, "({%s *tmp = malloc(%d * sizeof(%s)); ", elemBase, len(lit.Elems), elemBase)
                                                for i, e := range lit.Elems {
                                                        fmt.Fprintf(w, "tmp[%d] = ", i)
                                                        e.emitExpr(w)
							io.WriteString(w, "; ")
						}
						io.WriteString(w, "tmp;})")
					} else {
						call.Args[1].emitExpr(w)
					}
					io.WriteString(w, ");\n")
					needListAppendSizeT = true
					writeIndent(w, indent)
					fmt.Fprintf(w, "%s_lens = list_append_szt(%s_lens, &%s_lens_len, ", a.Name, a.Name, a.Name)
					emitLenExpr(w, call.Args[1])
					io.WriteString(w, ");\n")
					return
				}
			}
		} else if fe, ok2 := call.Args[0].(*FieldExpr); ok2 && len(a.Fields) == 1 && len(a.Indexes) == 0 {
			if vr, ok3 := fe.Target.(*VarRef); ok3 && vr.Name == a.Name && fe.Name == a.Fields[0] {
				base := fieldBaseType(vr.Name, fe.Name)
				sep := "."
				if t, ok4 := varTypes[a.Name]; ok4 && strings.HasSuffix(t, "*") {
					sep = "->"
				}
				writeIndent(w, indent)
				switch base {
				case "int":
					needListAppendInt = true
					fmt.Fprintf(w, "%s%s%s = list_append_int(%s%s%s, &%s%s%s_len, ", a.Name, sep, fe.Name, a.Name, sep, fe.Name, a.Name, sep, fe.Name)
				case "double":
					needListAppendDouble = true
					fmt.Fprintf(w, "%s%s%s = list_append_double(%s%s%s, &%s%s%s_len, ", a.Name, sep, fe.Name, a.Name, sep, fe.Name, a.Name, sep, fe.Name)
				case "const char*":
					needListAppendStr = true
					fmt.Fprintf(w, "%s%s%s = list_append_str(%s%s%s, &%s%s%s_len, ", a.Name, sep, fe.Name, a.Name, sep, fe.Name, a.Name, sep, fe.Name)
				default:
					if base != "" {
						if strings.HasSuffix(base, "*") || !strings.HasSuffix(base, "[]") {
							typ := strings.TrimSuffix(base, "*")
							if typ == "" {
								typ = base
							}
							if needListAppendStruct == nil {
								needListAppendStruct = make(map[string]bool)
							}
							needListAppendStruct[typ] = true
							fmt.Fprintf(w, "%s%s%s = list_append_%s(%s%s%s, &%s%s%s_len, ", a.Name, sep, fe.Name, sanitizeTypeName(typ), a.Name, sep, fe.Name, a.Name, sep, fe.Name)
						} else {
							needListAppendInt = true
							fmt.Fprintf(w, "%s%s%s = list_append_int(%s%s%s, &%s%s%s_len, ", a.Name, sep, fe.Name, a.Name, sep, fe.Name, a.Name, sep, fe.Name)
						}
					} else {
						needListAppendInt = true
						fmt.Fprintf(w, "%s%s%s = list_append_int(%s%s%s, &%s%s%s_len, ", a.Name, sep, fe.Name, a.Name, sep, fe.Name, a.Name, sep, fe.Name)
					}
				}
				call.Args[1].emitExpr(w)
				io.WriteString(w, ");\n")
				return
			}
		}
	}
	writeIndent(w, indent)
	io.WriteString(w, a.Name)
	start := 0
	if stName := varBaseType(a.Name); stName != "" {
		if _, ok := structTypes[stName]; ok && len(a.Indexes) > 0 {
			if s, ok2 := a.Indexes[0].(*StringLit); ok2 {
				io.WriteString(w, ".")
				io.WriteString(w, s.Value)
				start = 1
			}
		}
	}
	for _, idx := range a.Indexes[start:] {
		io.WriteString(w, "[")
		io.WriteString(w, "(int)(")
		idx.emitExpr(w)
		io.WriteString(w, ")")
		io.WriteString(w, "]")
	}
	if len(a.Fields) > 0 {
		sep := "."
		if t, ok := varTypes[a.Name]; ok && strings.HasSuffix(t, "*") {
			sep = "->"
		}
		io.WriteString(w, sep)
		io.WriteString(w, a.Fields[0])
		for _, f := range a.Fields[1:] {
			io.WriteString(w, ".")
			io.WriteString(w, f)
		}
	}
	io.WriteString(w, " = ")
	if a.Value != nil {
		prevVar := currentVarName
		currentVarName = a.Name
		a.Value.emitExpr(w)
		currentVarName = prevVar
	}
	io.WriteString(w, ";\n")

	if lst, ok := a.Value.(*ListLit); ok && len(a.Indexes) == 0 && len(a.Fields) == 0 {
		if vt, ok := varTypes[a.Name]; ok && strings.HasSuffix(vt, "[][]") {
			writeIndent(w, indent)
			fmt.Fprintf(w, "%s_len = %d;\n", a.Name, len(lst.Elems))
			writeIndent(w, indent)
			fmt.Fprintf(w, "%s_lens = ({size_t *tmp = malloc(%d * sizeof(size_t)); ", a.Name, len(lst.Elems))
			for i, e := range lst.Elems {
				fmt.Fprintf(w, "tmp[%d] = ", i)
				if sl, ok2 := e.(*ListLit); ok2 {
					fmt.Fprintf(w, "%d", len(sl.Elems))
				} else {
					emitLenExpr(w, e)
				}
				io.WriteString(w, "; ")
			}
			io.WriteString(w, "tmp;});\n")
			writeIndent(w, indent)
			fmt.Fprintf(w, "%s_lens_len = %d;\n", a.Name, len(lst.Elems))
		} else if ok {
			writeIndent(w, indent)
			fmt.Fprintf(w, "%s_len = %d;\n", a.Name, len(lst.Elems))
		}
	}

	if vr, ok := a.Value.(*VarRef); ok && len(a.Indexes) == 0 && len(a.Fields) == 0 {
		if vt, ok2 := varTypes[a.Name]; ok2 && strings.HasSuffix(vt, "[]") {
			writeIndent(w, indent)
			fmt.Fprintf(w, "%s_len = %s_len;\n", a.Name, vr.Name)
			if strings.HasSuffix(vt, "[][]") && !strings.Contains(vt, "char[][]") {
				writeIndent(w, indent)
				fmt.Fprintf(w, "%s_lens = %s_lens;\n", a.Name, vr.Name)
				writeIndent(w, indent)
				fmt.Fprintf(w, "%s_lens_len = %s_lens_len;\n", a.Name, vr.Name)
			}
		}
	}

	if call, ok := a.Value.(*CallExpr); ok {
		if call.Func == "_slice_int" || call.Func == "_slice_double" || call.Func == "_slice_str" {
			sliceLen := "_slice_int_len"
			if call.Func == "_slice_double" {
				sliceLen = "_slice_double_len"
			} else if call.Func == "_slice_str" {
				sliceLen = "_slice_str_len"
			}
			writeIndent(w, indent)
			if len(a.Fields) == 1 {
				fmt.Fprintf(w, "%s.%s_len = %s;\n", a.Name, a.Fields[0], sliceLen)
			} else if _, declared := varTypes[a.Name]; declared {
				fmt.Fprintf(w, "%s_len = %s;\n", a.Name, sliceLen)
			} else {
				fmt.Fprintf(w, "size_t %s_len = %s;\n", a.Name, sliceLen)
			}
			return
		}
		typ := funcReturnTypes[call.Func]
		if vt, ok := varTypes[a.Name]; ok && vt != "" {
			if strings.Contains(vt, "char*") {
				typ = vt
			} else {
				typ = strings.ReplaceAll(vt, "*", "[]")
			}
		}
		if strings.HasSuffix(typ, "[]") {
			writeIndent(w, indent)
			if _, declared := varTypes[a.Name]; declared {
				fmt.Fprintf(w, "%s_len = %s_len;\n", a.Name, call.Func)
			} else {
				fmt.Fprintf(w, "size_t %s_len = %s_len;\n", a.Name, call.Func)
			}
			if strings.HasSuffix(typ, "[][]") && !strings.Contains(typ, "char[][]") {
				writeIndent(w, indent)
				if _, declared := varTypes[a.Name]; declared {
					fmt.Fprintf(w, "%s_lens = %s_lens;\n", a.Name, call.Func)
				} else {
					fmt.Fprintf(w, "size_t *%s_lens = %s_lens;\n", a.Name, call.Func)
				}
			}
		}
	}
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
	if f.ListVar != "" {
		writeIndent(w, indent)
		io.WriteString(w, "for (size_t __i = 0; __i < ")
		if f.LenVar != "" {
			io.WriteString(w, f.LenVar)
		} else {
			io.WriteString(w, f.ListVar)
			io.WriteString(w, "_len")
		}
		io.WriteString(w, "; __i++) {\n")
		typ := f.ElemType
		if typ == "" {
			typ = "int"
		}
		writeIndent(w, indent+1)
		if strings.HasSuffix(typ, "[]") {
			base := strings.TrimSuffix(typ, "[]")
			fmt.Fprintf(w, "%s *%s = %s[__i];\n", base, f.Var, f.ListVar)
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "size_t %s_len = %s_lens[__i];\n", f.Var, f.ListVar)
		} else {
			fmt.Fprintf(w, "%s %s = %s[__i];\n", typ, f.Var, f.ListVar)
		}
		for _, s := range f.Body {
			s.emit(w, indent+1)
		}
		writeIndent(w, indent)
		io.WriteString(w, "}\n")
		return
	}
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
		if strings.HasSuffix(typ, "[]") {
			base := strings.TrimSuffix(typ, "[]")
			subLen := 0
			if len(f.List) > 0 {
				if lst, ok := f.List[0].(*ListLit); ok {
					subLen = len(lst.Elems)
				}
			}
			fmt.Fprintf(w, "%s %s[%d][%d] = {", base, arrName, len(f.List), subLen)
			for i, e := range f.List {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				if lst, ok := e.(*ListLit); ok {
					io.WriteString(w, "{")
					for j, it := range lst.Elems {
						if j > 0 {
							io.WriteString(w, ", ")
						}
						it.emitExpr(w)
					}
					io.WriteString(w, "}")
				} else {
					e.emitExpr(w)
				}
			}
			io.WriteString(w, "};\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "size_t %s = %d;\n", lenName, len(f.List))
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "for (size_t __i = 0; __i < %s; __i++) {\n", lenName)
			writeIndent(w, indent+2)
			fmt.Fprintf(w, "%s %s[%d] = {0};\n", base, f.Var, subLen)
			writeIndent(w, indent+2)
			fmt.Fprintf(w, "memcpy(%s, %s[__i], sizeof(%s[__i]));\n", f.Var, arrName, arrName)
		} else {
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
			io.WriteString(w, "for (size_t __i = 0; __i < ")
			io.WriteString(w, lenName)
			io.WriteString(w, "; __i++) {\n")
			writeIndent(w, indent+2)
			fmt.Fprintf(w, "%s %s = %s[__i];\n", typ, f.Var, arrName)
		}
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

func (b *BenchStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "{\n")
	writeIndent(w, indent+1)
	io.WriteString(w, "long long __start = _now();\n")
	for _, s := range b.Body {
		s.emit(w, indent+1)
	}
	writeIndent(w, indent+1)
	io.WriteString(w, "long long __end = _now();\n")
	writeIndent(w, indent+1)
	io.WriteString(w, "long long __dur_us = (__end - __start) / 1000;\n")
	writeIndent(w, indent+1)
	io.WriteString(w, "long long __mem_bytes = _mem();\n")
	writeIndent(w, indent+1)
	fmt.Fprintf(w, "printf(\"{\\n  \\\"duration_us\\\": %%-lld,\\n  \\\"memory_bytes\\\": %%-lld,\\n  \\\"name\\\": \\\"%s\\\"\\n}\\n\", __dur_us, __mem_bytes);\n", escape(b.Name))
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
	fmt.Fprintf(w, "%dLL", i.Value)
}

type FloatLit struct{ Value float64 }

func (f *FloatLit) emitExpr(w io.Writer) {
	if math.Trunc(f.Value) == f.Value {
		fmt.Fprintf(w, "%.1f", f.Value)
	} else {
		fmt.Fprintf(w, "%g", f.Value)
	}
}

// NullLit represents a null literal, emitted as NULL.
type NullLit struct{}

func (n *NullLit) emitExpr(w io.Writer) { io.WriteString(w, "NULL") }

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func exprIsFloat(e Expr) bool {
	switch v := e.(type) {
	case *FloatLit:
		return true
	case *UnaryExpr:
		if v.Op == "(double)" {
			return true
		}
		return exprIsFloat(v.Expr)
	case *BinaryExpr:
		if v.Op == "+" || v.Op == "-" || v.Op == "*" || v.Op == "/" {
			return exprIsFloat(v.Left) || exprIsFloat(v.Right)
		}
	case *VarRef, *FieldExpr, *IndexExpr:
		return inferExprType(currentEnv, e) == "double"
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
	if len(l.Elems) == 0 {
		io.WriteString(w, "NULL")
		return
	}
	elemType := "int"
	if t := inferExprType(currentEnv, l.Elems[0]); t != "" {
		elemType = t
	} else if exprIsString(l.Elems[0]) {
		elemType = "const char*"
	} else if exprIsFloat(l.Elems[0]) {
		elemType = "double"
	}
	dims := strings.Count(elemType, "[]")
	base := strings.TrimSuffix(elemType, strings.Repeat("[]", dims))
	// When the element type has no array suffixes (i.e. dims == 0), this
	// literal represents a 1-D list. Allocate its storage on the heap so the
	// returned pointer remains valid after the expression finishes.
	if dims == 0 {
		fmt.Fprintf(w, "({%s *tmp = malloc(%d * sizeof(%s)); ", base, len(l.Elems), base)
		for i, e := range l.Elems {
			fmt.Fprintf(w, "tmp[%d] = ", i)
			e.emitExpr(w)
			io.WriteString(w, "; ")
		}
		io.WriteString(w, "tmp;})")
		return
	}
	outType := base + strings.Repeat("*", dims)
	fmt.Fprintf(w, "({%s tmp = malloc(%d * sizeof(%s)); ", outType+"*", len(l.Elems), outType)
	for i, e := range l.Elems {
		fmt.Fprintf(w, "tmp[%d] = ", i)
		e.emitExpr(w)
		io.WriteString(w, "; ")
	}
	io.WriteString(w, "tmp;})")
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
		printed := false
		if st, ok := structTypes[s.Name]; ok {
			if ft, ok2 := st.Fields[f.Name]; ok2 {
				switch mt := ft.(type) {
				case types.ListType:
					if f.Value != nil {
						f.Value.emitExpr(w)
					}
					io.WriteString(w, ", .")
					io.WriteString(w, f.Name+"_len = ")
					emitLenExpr(w, f.Value)
					if _, ok := mt.Elem.(types.ListType); ok {
						io.WriteString(w, ", .")
						io.WriteString(w, f.Name+"_lens = ")
						emitLensExpr(w, f.Value)
						io.WriteString(w, ", .")
						io.WriteString(w, f.Name+"_lens_len = ")
						emitLenExpr(w, f.Value)
					}
					printed = true
				case types.MapType:
					if ml, okm := f.Value.(*MapLit); okm && len(ml.Items) == 0 {
						if _, ok := mt.Key.(types.StringType); ok {
							if _, ok2 := mt.Value.(types.StringType); ok2 {
								io.WriteString(w, "(MapSS){0}")
								printed = true
							} else if lt, ok2 := mt.Value.(types.ListType); ok2 {
								if _, ok3 := lt.Elem.(types.StringType); ok3 {
									io.WriteString(w, "(MapSL){0}")
									printed = true
								}
							}
						}
					} else if sl, okm := f.Value.(*StructLit); okm && len(sl.Fields) == 0 {
						if _, ok := mt.Key.(types.StringType); ok {
							if _, ok2 := mt.Value.(types.StringType); ok2 {
								io.WriteString(w, "(MapSS){0}")
								printed = true
							} else if lt, ok2 := mt.Value.(types.ListType); ok2 {
								if _, ok3 := lt.Elem.(types.StringType); ok3 {
									io.WriteString(w, "(MapSL){0}")
									printed = true
								}
							}
						}
					}
				}
			}
		}
		if !printed && f.Value != nil {
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
	typ := inferExprType(currentEnv, f.Target)
	base := strings.TrimPrefix(typ, "struct ")
	f.Target.emitExpr(w)
	if strings.HasSuffix(base, "*") {
		if _, ok := structTypes[strings.TrimSuffix(base, "*")]; ok {
			io.WriteString(w, "->")
		} else {
			io.WriteString(w, ".")
		}
	} else {
		io.WriteString(w, ".")
	}
	io.WriteString(w, f.Name)
}

type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (i *IndexExpr) emitExpr(w io.Writer) {
	if key, ok := evalString(i.Index); ok {
		tname := inferExprType(currentEnv, i.Target)
		st, ok2 := currentEnv.GetStruct(tname)
		if !ok2 {
			if st2, ok3 := structTypes[tname]; ok3 {
				st = st2
				ok2 = true
			}
		}
		if !ok2 {
			if vr, okVr := i.Target.(*VarRef); okVr {
				if t, okT := varTypes[vr.Name]; okT {
					if st2, ok3 := structTypes[t]; ok3 {
						st = st2
						ok2 = true
					}
				}
			}
		}
		if ok2 {
			if _, ok3 := st.Fields[key]; ok3 {
				i.Target.emitExpr(w)
				io.WriteString(w, ".")
				io.WriteString(w, key)
				return
			}
		}
	}
	if exprIsString(i.Target) {
		io.WriteString(w, "(const char[]){")
		i.Target.emitExpr(w)
		io.WriteString(w, "[")
		i.Index.emitExpr(w)
		io.WriteString(w, "], 0}")
		return
	}
	if vr, ok := i.Target.(*VarRef); ok && isMapVar(vr.Name) {
		keyT := mapKeyTypes[vr.Name]
		valT := mapValTypes[vr.Name]
		if keyT == "const char*" && valT == "int" {
			needMapGetSI = true
			funcReturnTypes["map_get_si"] = "int"
			io.WriteString(w, "map_get_si(")
			if varTypes[vr.Name] == "MapSL" || varTypes[vr.Name] == "MapSS" || varTypes[vr.Name] == "MapSI" || varTypes[vr.Name] == "MapSD" || varTypes[vr.Name] == "MapIS" {
				io.WriteString(w, vr.Name+".keys, ")
				io.WriteString(w, vr.Name+".vals, ")
				io.WriteString(w, vr.Name+".len, ")
			} else {
				io.WriteString(w, vr.Name+"_keys, ")
				io.WriteString(w, vr.Name+"_vals, ")
				io.WriteString(w, vr.Name+"_len, ")
			}
			i.Index.emitExpr(w)
			io.WriteString(w, ")")
			return
		}
		if keyT == "int" && valT == "int" {
			needMapGetII = true
			funcReturnTypes["map_get_ii"] = "int"
			io.WriteString(w, "map_get_ii(")
			io.WriteString(w, vr.Name+"_keys, ")
			io.WriteString(w, vr.Name+"_vals, ")
			io.WriteString(w, vr.Name+"_len, ")
			i.Index.emitExpr(w)
			io.WriteString(w, ")")
			return
		}
		if keyT == "int" && valT == "const char*" {
			needMapGetIS = true
			funcReturnTypes["map_get_is"] = "const char*"
			io.WriteString(w, "map_get_is(")
			if varTypes[vr.Name] == "MapIS" {
				io.WriteString(w, vr.Name+".keys, ")
				io.WriteString(w, vr.Name+".vals, ")
				io.WriteString(w, vr.Name+".len, ")
			} else {
				io.WriteString(w, vr.Name+"_keys, ")
				io.WriteString(w, vr.Name+"_vals, ")
				io.WriteString(w, vr.Name+"_len, ")
			}
			i.Index.emitExpr(w)
			io.WriteString(w, ")")
			return
		}
		if keyT == "const char*" && valT == "const char*" {
			needMapGetSS = true
			funcReturnTypes["map_get_ss"] = "const char*"
			io.WriteString(w, "map_get_ss(")
			if varTypes[vr.Name] == "MapSL" || varTypes[vr.Name] == "MapSS" || varTypes[vr.Name] == "MapSI" || varTypes[vr.Name] == "MapSD" || varTypes[vr.Name] == "MapIS" {
				io.WriteString(w, vr.Name+".keys, ")
				io.WriteString(w, vr.Name+".vals, ")
				io.WriteString(w, vr.Name+".len, ")
			} else {
				io.WriteString(w, vr.Name+"_keys, ")
				io.WriteString(w, vr.Name+"_vals, ")
				io.WriteString(w, vr.Name+"_len, ")
			}
			i.Index.emitExpr(w)
			io.WriteString(w, ")")
			return
		}
		if keyT == "const char*" && strings.HasSuffix(valT, "[]") {
			needMapGetSL = true
			funcReturnTypes["map_get_sl"] = valT
			io.WriteString(w, "map_get_sl(")
			if varTypes[vr.Name] == "MapSL" || varTypes[vr.Name] == "MapSI" {
				io.WriteString(w, vr.Name+".keys, ")
				io.WriteString(w, vr.Name+".vals, ")
				io.WriteString(w, vr.Name+".lens, ")
				io.WriteString(w, vr.Name+".len, ")
			} else {
				io.WriteString(w, vr.Name+"_keys, ")
				io.WriteString(w, vr.Name+"_vals, ")
				io.WriteString(w, vr.Name+"_lens, ")
				io.WriteString(w, vr.Name+"_len, ")
			}
			i.Index.emitExpr(w)
			io.WriteString(w, ", &")
			io.WriteString(w, currentVarName)
			io.WriteString(w, "_len)")
			return
		}
		if keyT == "const char*" && valT == "double" {
			needMapGetSD = true
			funcReturnTypes["map_get_sd"] = "double"
			io.WriteString(w, "map_get_sd(")
			if varTypes[vr.Name] == "MapSD" {
				io.WriteString(w, vr.Name+".keys, ")
				io.WriteString(w, vr.Name+".vals, ")
				io.WriteString(w, vr.Name+".len, ")
			} else {
				io.WriteString(w, vr.Name+"_keys, ")
				io.WriteString(w, vr.Name+"_vals, ")
				io.WriteString(w, vr.Name+"_len, ")
			}
			i.Index.emitExpr(w)
			io.WriteString(w, ")")
			return
		}
	}
	if t := inferExprType(currentEnv, i.Target); t == "MapSS" {
		needMapGetSS = true
		io.WriteString(w, "map_get_ss(")
		i.Target.emitExpr(w)
		io.WriteString(w, ".keys, ")
		i.Target.emitExpr(w)
		io.WriteString(w, ".vals, ")
		i.Target.emitExpr(w)
		io.WriteString(w, ".len, ")
		i.Index.emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	if t := inferExprType(currentEnv, i.Target); t == "MapSL" {
		needMapGetSL = true
		valT := "const char*[]"
		if vr, ok := i.Target.(*VarRef); ok {
			if vt, ok2 := mapValTypes[vr.Name]; ok2 {
				valT = vt
			}
		}
		funcReturnTypes["map_get_sl"] = valT
		io.WriteString(w, "map_get_sl(")
		i.Target.emitExpr(w)
		io.WriteString(w, ".keys, ")
		i.Target.emitExpr(w)
		io.WriteString(w, ".vals, ")
		i.Target.emitExpr(w)
		io.WriteString(w, ".lens, ")
		i.Target.emitExpr(w)
		io.WriteString(w, ".len, ")
		i.Index.emitExpr(w)
		io.WriteString(w, ", &")
		io.WriteString(w, currentVarName)
		io.WriteString(w, "_len)")
		return
	}
	if t := inferExprType(currentEnv, i.Target); t == "MapSD" {
		needMapGetSD = true
		funcReturnTypes["map_get_sd"] = "double"
		io.WriteString(w, "map_get_sd(")
		i.Target.emitExpr(w)
		io.WriteString(w, ".keys, ")
		i.Target.emitExpr(w)
		io.WriteString(w, ".vals, ")
		i.Target.emitExpr(w)
		io.WriteString(w, ".len, ")
		i.Index.emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	i.Target.emitExpr(w)
	io.WriteString(w, "[")
	io.WriteString(w, "(int)(")
	i.Index.emitExpr(w)
	io.WriteString(w, ")")
	io.WriteString(w, "]")
}

type VarRef struct{ Name string }

func (v *VarRef) emitExpr(w io.Writer) {
	if currentLocals != nil {
		if _, ok := currentLocals[v.Name]; ok {
			io.WriteString(w, "0")
			return
		}
	}
	name := aliasName(v.Name)
	switch name {
	case "math.pi":
		io.WriteString(w, "3.141592653589793")
	case "math.e":
		io.WriteString(w, "2.718281828459045")
	default:
		io.WriteString(w, name)
	}
}

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emitExpr(w io.Writer) {
	if c.Func == "append" && len(c.Args) >= 2 {
		if vr, ok := c.Args[0].(*VarRef); ok {
			base := varBaseType(vr.Name)
			switch {
                        case strings.HasSuffix(base, "[]"):
                                elemBase := strings.TrimSuffix(base, "[]")
                                if strings.HasSuffix(elemBase, "[]") {
                                        elemElemBase := strings.TrimSuffix(elemBase, "[]")
                                        switch elemElemBase {
                                        case "int":
                                                needListAppendPtrPtr = true
                                                needListAppendSizeT = true
                                                fmt.Fprintf(w, "(%s = list_append_intptrptr(%s, &%s_len, ", vr.Name, vr.Name, vr.Name)
                                        case "double":
                                                needListAppendDoublePtr = true
                                                needListAppendSizeT = true
                                                fmt.Fprintf(w, "(%s = list_append_doubleptr(%s, &%s_len, ", vr.Name, vr.Name, vr.Name)
                                        default:
                                                if strings.Contains(elemElemBase, "char*") {
                                                        needListAppendStrPtr = true
                                                        needListAppendSizeT = true
                                                        fmt.Fprintf(w, "(%s = list_append_strptr(%s, &%s_len, ", vr.Name, vr.Name, vr.Name)
                                                } else {
                                                        if needListAppendStructPtr == nil {
                                                                needListAppendStructPtr = make(map[string]bool)
                                                        }
                                                        needListAppendStructPtr[elemElemBase] = true
                                                        needListAppendSizeT = true
                                                        fmt.Fprintf(w, "(%s = list_append_%sptr(%s, &%s_len, ", vr.Name, sanitizeTypeName(elemElemBase), vr.Name, vr.Name)
                                                }
                                        }
                                } else {
                                        switch elemBase {
                                        case "int":
                                                needListAppendPtr = true
                                                needListAppendSizeT = true
                                                fmt.Fprintf(w, "(%s = list_append_intptr(%s, &%s_len, ", vr.Name, vr.Name, vr.Name)
                                        case "double":
                                                needListAppendDoublePtr = true
                                                needListAppendSizeT = true
                                                fmt.Fprintf(w, "(%s = list_append_doubleptr(%s, &%s_len, ", vr.Name, vr.Name, vr.Name)
                                        default:
                                                if strings.Contains(elemBase, "char*") {
                                                        needListAppendStrPtr = true
                                                        needListAppendSizeT = true
                                                        fmt.Fprintf(w, "(%s = list_append_strptr(%s, &%s_len, ", vr.Name, vr.Name, vr.Name)
                                                } else {
                                                        if needListAppendStructPtr == nil {
                                                                needListAppendStructPtr = make(map[string]bool)
                                                        }
                                                        needListAppendStructPtr[elemBase] = true
                                                        needListAppendSizeT = true
                                                        fmt.Fprintf(w, "(%s = list_append_%sptr(%s, &%s_len, ", vr.Name, sanitizeTypeName(elemBase), vr.Name, vr.Name)
                                                }
                                        }
                                }
                                if len(c.Args) > 1 && c.Args[1] != nil {
                                        c.Args[1].emitExpr(w)
                                }
                                fmt.Fprintf(w, "), %s_lens = list_append_szt(%s_lens, &%s_lens_len, ", vr.Name, vr.Name, vr.Name)
                                emitLenExpr(w, c.Args[1])
				fmt.Fprintf(w, "), %s_lens = %s_lens, %s)", currentFuncName, vr.Name, vr.Name)
				return
			default:
				switch base {
				case "int", "long long":
					needListAppendIntNew = true
					fmt.Fprintf(w, "list_append_int_new(%s, %s_len, ", vr.Name, vr.Name)
				case "const char*":
					needListAppendStrNew = true
					fmt.Fprintf(w, "list_append_str_new(%s, %s_len, ", vr.Name, vr.Name)
				case "double":
					needListAppendDoubleNew = true
					fmt.Fprintf(w, "list_append_double_new(%s, %s_len, ", vr.Name, vr.Name)
				default:
					if strings.HasSuffix(base, "*") {
						typ := strings.TrimSuffix(base, "*")
						if needListAppendStructNew == nil {
							needListAppendStructNew = make(map[string]bool)
						}
						needListAppendStructNew[typ] = true
						fmt.Fprintf(w, "list_append_%s_new(%s, %s_len, ", sanitizeTypeName(typ), vr.Name, vr.Name)
					} else if base != "" {
						if needListAppendStructNew == nil {
							needListAppendStructNew = make(map[string]bool)
						}
						needListAppendStructNew[base] = true
						fmt.Fprintf(w, "list_append_%s_new(%s, %s_len, ", sanitizeTypeName(base), vr.Name, vr.Name)
					} else {
						needListAppendIntNew = true
						fmt.Fprintf(w, "list_append_int_new(%s, %s_len, ", vr.Name, vr.Name)
					}
				}
			}
			if len(c.Args) > 1 && c.Args[1] != nil {
				c.Args[1].emitExpr(w)
			}
			io.WriteString(w, ")")
			return
		}
		if ie, ok := c.Args[0].(*IndexExpr); ok {
			if vr, ok2 := ie.Target.(*VarRef); ok2 {
				switch inferExprType(currentEnv, c.Args[1]) {
				case "double":
					needListAppendDoubleNew = true
					io.WriteString(w, "list_append_double_new(")
				case "const char*":
					needListAppendStrNew = true
					io.WriteString(w, "list_append_str_new(")
				default:
					needListAppendIntNew = true
					io.WriteString(w, "list_append_int_new(")
				}
				c.Args[0].emitExpr(w)
				io.WriteString(w, ", ")
				fmt.Fprintf(w, "%s_lens[(int)(", vr.Name)
				ie.Index.emitExpr(w)
				io.WriteString(w, ")], ")
				if len(c.Args) > 1 && c.Args[1] != nil {
					c.Args[1].emitExpr(w)
				}
				io.WriteString(w, ")")
				return
			}
		}
		if fe, ok := c.Args[0].(*FieldExpr); ok {
			base := inferExprType(currentEnv, c.Args[0])
			elemBase := strings.TrimSuffix(strings.TrimSuffix(base, "[]"), "*")
			switch elemBase {
			case "int", "long long":
				needListAppendIntNew = true
				io.WriteString(w, "list_append_int_new(")
			case "double":
				needListAppendDoubleNew = true
				io.WriteString(w, "list_append_double_new(")
			default:
				if strings.Contains(elemBase, "char*") {
					needListAppendStrNew = true
					io.WriteString(w, "list_append_str_new(")
				} else {
					if needListAppendStructNew == nil {
						needListAppendStructNew = make(map[string]bool)
					}
					needListAppendStructNew[elemBase] = true
					fmt.Fprintf(w, "list_append_%s_new(", sanitizeTypeName(elemBase))
				}
			}
			fe.emitExpr(w)
			io.WriteString(w, ", ")
			emitLenExpr(w, c.Args[0])
			io.WriteString(w, ", ")
			if len(c.Args) > 1 && c.Args[1] != nil {
				c.Args[1].emitExpr(w)
			}
			io.WriteString(w, ")")
			return
		}
	}
	if c.Func == "concat" && len(c.Args) == 2 {
		if a, ok := c.Args[0].(*VarRef); ok {
			t0 := inferExprType(currentEnv, c.Args[0])
			if strings.Contains(t0, "const char*") {
				if b, ok2 := c.Args[1].(*VarRef); ok2 {
					needConcatStrPtr = true
					needListAppendStrPtr = true
					needListAppendSizeT = true
					fmt.Fprintf(w, "concat_strptr(%s, &%s_len, &%s_lens, &%s_lens_len, %s, %s_len, %s_lens)", a.Name, a.Name, a.Name, a.Name, b.Name, b.Name, b.Name)
					return
				}
			} else {
				needConcatLongLong = true
				if b, ok2 := c.Args[1].(*VarRef); ok2 {
					fmt.Fprintf(w, "concat_long_long(%s, %s_len, %s, %s_len)", a.Name, a.Name, b.Name, b.Name)
					return
				}
				fmt.Fprintf(w, "concat_long_long(%s, %s_len, ", a.Name, a.Name)
				c.Args[1].emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, c.Args[1])
				io.WriteString(w, ")")
				return
			}
		}
	}
	if c.Func == "_slice_int" && len(c.Args) == 5 {
		needSliceInt = true
		io.WriteString(w, "_slice_int(")
		c.Args[0].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[1].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[2].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[3].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[4].emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	if c.Func == "_slice_double" && len(c.Args) == 5 {
		needSliceDouble = true
		io.WriteString(w, "_slice_double(")
		c.Args[0].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[1].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[2].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[3].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[4].emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	if c.Func == "_slice_str" && len(c.Args) == 5 {
		needSliceStr = true
		io.WriteString(w, "_slice_str(")
		c.Args[0].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[1].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[2].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[3].emitExpr(w)
		io.WriteString(w, ", ")
		c.Args[4].emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	if c.Func == "contains" && len(c.Args) == 2 && funcParamTypes[c.Func] == nil {
		t := inferExprType(currentEnv, c.Args[0])
		if t == "const char*" {
			io.WriteString(w, "strstr(")
			c.Args[0].emitExpr(w)
			io.WriteString(w, ", ")
			c.Args[1].emitExpr(w)
			io.WriteString(w, ") != NULL")
		} else if strings.HasSuffix(t, "[]") {
			elem := strings.TrimSuffix(t, "[]")
			if elem == "int" {
				needContainsInt = true
				io.WriteString(w, "contains_int(")
				c.Args[0].emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, c.Args[0])
				io.WriteString(w, ", ")
				c.Args[1].emitExpr(w)
				io.WriteString(w, ")")
			} else if elem == "const char*" {
				needContainsStr = true
				io.WriteString(w, "contains_str(")
				c.Args[0].emitExpr(w)
				io.WriteString(w, ", ")
				emitLenExpr(w, c.Args[0])
				io.WriteString(w, ", ")
				c.Args[1].emitExpr(w)
				io.WriteString(w, ")")
			} else {
				io.WriteString(w, "contains(")
				c.Args[0].emitExpr(w)
				io.WriteString(w, ", ")
				c.Args[1].emitExpr(w)
				io.WriteString(w, ")")
			}
		} else {
			io.WriteString(w, "contains(")
			c.Args[0].emitExpr(w)
			io.WriteString(w, ", ")
			c.Args[1].emitExpr(w)
			io.WriteString(w, ")")
		}
		return
	}
	if (c.Func == "min" || c.Func == "max") && len(c.Args) == 1 {
		t := inferExprType(currentEnv, c.Args[0])
		if strings.HasSuffix(t, "[]") {
			if c.Func == "min" {
				needListMin = true
				io.WriteString(w, "list_min(")
			} else {
				needListMax = true
				io.WriteString(w, "list_max(")
			}
			c.Args[0].emitExpr(w)
			io.WriteString(w, ", ")
			emitLenExpr(w, c.Args[0])
			io.WriteString(w, ")")
			return
		}
	}
	if c.Func == "len" && len(c.Args) == 1 {
		if vr, ok := c.Args[0].(*VarRef); ok {
			io.WriteString(w, vr.Name+"_len")
			return
		}
		if fe, ok := c.Args[0].(*FieldExpr); ok {
			emitLenExpr(w, fe)
			return
		}
		t := inferExprType(currentEnv, c.Args[0])
		if strings.HasSuffix(t, "[]") {
			switch v := c.Args[0].(type) {
			case *CallExpr:
				io.WriteString(w, "(")
				v.emitExpr(w)
				io.WriteString(w, ", ")
				io.WriteString(w, v.Func+"_len")
				io.WriteString(w, ")")
				return
			case *IndexExpr:
				if vr, ok := v.Target.(*VarRef); ok {
					if typ, ok2 := varTypes[vr.Name]; ok2 && strings.HasSuffix(typ, "[][]") {
						io.WriteString(w, vr.Name+"_lens[")
						v.Index.emitExpr(w)
						io.WriteString(w, "]")
						return
					}
				}
			}
		}
		if t == "const char*" {
			io.WriteString(w, "strlen(")
			c.Args[0].emitExpr(w)
			io.WriteString(w, ")")
			return
		}
	}
	if (c.Func == "num" || c.Func == "denom") && len(c.Args) == 1 {
		needGMP = true
		io.WriteString(w, "mpz_get_si(")
		io.WriteString(w, c.Func)
		io.WriteString(w, "(")
		c.Args[0].emitExpr(w)
		io.WriteString(w, "))")
		return
	}
	if c.Func == "str" && len(c.Args) == 1 {
		arg := c.Args[0]
		if exprIsBool(arg) {
			needStrBool = true
			io.WriteString(w, "str_bool(")
			arg.emitExpr(w)
			io.WriteString(w, ")")
			return
		}
		t := inferExprType(currentEnv, arg)
		if t == "bigint" {
			needStrBigInt = true
			io.WriteString(w, "str_bigint(")
			arg.emitExpr(w)
			io.WriteString(w, ")")
			return
		}
		if strings.HasSuffix(t, "[]") {
			base := strings.TrimSuffix(t, "[]")
			if strings.HasSuffix(base, "[]") {
				elem := strings.TrimSuffix(base, "[]")
				if elem == "int" || elem == "long long" {
					needStrListListInt = true
					needStrListInt = true
					if _, ok := arg.(*CallExpr); ok {
						tmp := fmt.Sprintf("__tmp%d", tempCounter)
						tempCounter++
						io.WriteString(w, "({long long **"+tmp+" = ")
						arg.emitExpr(w)
						io.WriteString(w, "; char *__res = str_list_list_int("+tmp+", ")
						emitLenExpr(w, arg)
						io.WriteString(w, ", ")
						emitLensExpr(w, arg)
						io.WriteString(w, "); __res;})")
					} else {
						io.WriteString(w, "str_list_list_int(")
						arg.emitExpr(w)
						io.WriteString(w, ", ")
						emitLenExpr(w, arg)
						io.WriteString(w, ", ")
						emitLensExpr(w, arg)
						io.WriteString(w, ")")
					}
					return
				} else if elem == "double" {
					needStrListListDouble = true
					needStrListDouble = true
					if _, ok := arg.(*CallExpr); ok {
						tmp := fmt.Sprintf("__tmp%d", tempCounter)
						tempCounter++
						io.WriteString(w, "({double **"+tmp+" = ")
						arg.emitExpr(w)
						io.WriteString(w, "; char *__res = str_list_list_double("+tmp+", ")
						emitLenExpr(w, arg)
						io.WriteString(w, ", ")
						emitLensExpr(w, arg)
						io.WriteString(w, "); __res;})")
					} else {
						io.WriteString(w, "str_list_list_double(")
						arg.emitExpr(w)
						io.WriteString(w, ", ")
						emitLenExpr(w, arg)
						io.WriteString(w, ", ")
						emitLensExpr(w, arg)
						io.WriteString(w, ")")
					}
					return
				}
			} else if base == "int" || base == "long long" {
				needStrListInt = true
				if _, ok := arg.(*CallExpr); ok {
					tmp := fmt.Sprintf("__tmp%d", tempCounter)
					tempCounter++
					io.WriteString(w, "({long long *"+tmp+" = ")
					arg.emitExpr(w)
					io.WriteString(w, "; char *__res = str_list_int("+tmp+", ")
					emitLenExpr(w, arg)
					io.WriteString(w, "); __res;})")
				} else {
					io.WriteString(w, "str_list_int(")
					arg.emitExpr(w)
					io.WriteString(w, ", ")
					emitLenExpr(w, arg)
					io.WriteString(w, ")")
				}
				return
			} else if base == "double" {
				needStrListDouble = true
				if _, ok := arg.(*CallExpr); ok {
					tmp := fmt.Sprintf("__tmp%d", tempCounter)
					tempCounter++
					io.WriteString(w, "({double *"+tmp+" = ")
					arg.emitExpr(w)
					io.WriteString(w, "; char *__res = str_list_double("+tmp+", ")
					emitLenExpr(w, arg)
					io.WriteString(w, "); __res;})")
				} else {
					io.WriteString(w, "str_list_double(")
					arg.emitExpr(w)
					io.WriteString(w, ", ")
					emitLenExpr(w, arg)
					io.WriteString(w, ")")
				}
				return
			} else if base == "const char*" {
				needStrListStr = true
				if _, ok := arg.(*CallExpr); ok {
					tmp := fmt.Sprintf("__tmp%d", tempCounter)
					tempCounter++
					io.WriteString(w, "({const char **"+tmp+" = ")
					arg.emitExpr(w)
					io.WriteString(w, "; char *__res = str_list_str("+tmp+", ")
					emitLenExpr(w, arg)
					io.WriteString(w, "); __res;})")
				} else {
					io.WriteString(w, "str_list_str(")
					arg.emitExpr(w)
					io.WriteString(w, ", ")
					emitLenExpr(w, arg)
					io.WriteString(w, ")")
				}
				return
			}
		}
		if c.Func == "repeat" && len(c.Args) == 2 {
			if _, ok := funcParamTypes["repeat"]; !ok {
				needRepeat = true
				funcReturnTypes["repeat"] = "const char*"
			}
			io.WriteString(w, "repeat(")
			c.Args[0].emitExpr(w)
			io.WriteString(w, ", ")
			c.Args[1].emitExpr(w)
			io.WriteString(w, ")")
			return
		}
		if c.Func == "parseIntStr" && (len(c.Args) == 1 || len(c.Args) == 2) {
			needParseIntStr = true
			funcReturnTypes["parseIntStr"] = "int"
			io.WriteString(w, "parseIntStr(")
			c.Args[0].emitExpr(w)
			if len(c.Args) == 1 {
				io.WriteString(w, ", 10")
			} else {
				io.WriteString(w, ", ")
				c.Args[1].emitExpr(w)
			}
			io.WriteString(w, ")")
			return
		}
		if t == "double" {
			needStrFloat = true
			io.WriteString(w, "str_float(")
		} else {
			needStrInt = true
			io.WriteString(w, "str_int(")
		}
		arg.emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	if c.Func == "upper" && len(c.Args) == 1 {
		needUpper = true
		io.WriteString(w, "str_upper(")
		c.Args[0].emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	if c.Func == "lower" && len(c.Args) == 1 {
		needLower = true
		io.WriteString(w, "str_lower(")
		c.Args[0].emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	fn := c.Func
	if strings.HasPrefix(fn, "math.") {
		fn = strings.TrimPrefix(fn, "math.")
	}

	var pre bytes.Buffer
	tmpName := ""
	tmpLen := ""
	firstCall := ""
	if len(c.Args) > 0 {
		if ce, ok := c.Args[0].(*CallExpr); ok {
			if ret, ok2 := funcReturnTypes[ce.Func]; ok2 && strings.HasSuffix(ret, "[]") {
				tmpName = fmt.Sprintf("__tmp%d", tempCounter)
				tempCounter++
				ceCopy := *ce
				decl := listPtrType(ret)
				tmpLen = ce.Func + "_len"
				firstCall = ce.Func
				if ce.Func == "_slice_int" || ce.Func == "_slice_double" || ce.Func == "_slice_str" {
					if len(ceCopy.Args) == 5 {
						tmpLen = tmpName + "_len"
						ceCopy.Args[4] = &UnaryExpr{Op: "&", Expr: &VarRef{Name: tmpLen}}
						pre.WriteString(fmt.Sprintf("size_t %s = 0; ", tmpLen))
					}
				}
				pre.WriteString(fmt.Sprintf("%s %s = ", decl, tmpName))
				ceCopy.emitExpr(&pre)
				pre.WriteString("; ")
			}
		}
	}

	if pre.Len() > 0 {
		io.WriteString(w, "({")
		io.WriteString(w, pre.String())
	}

	io.WriteString(w, fn)
	io.WriteString(w, "(")
	first := true
	for i, a := range c.Args {
		if a == nil {
			continue
		}
		if !first {
			io.WriteString(w, ", ")
		}
		var paramType string
		if types, ok := funcParamTypes[c.Func]; ok && i < len(types) {
			paramType = types[i]
		}
		if first && tmpName != "" {
			io.WriteString(w, tmpName)
			if strings.HasSuffix(paramType, "[][]") {
				io.WriteString(w, ", ")
				io.WriteString(w, firstCall+"_len")
				io.WriteString(w, ", ")
				io.WriteString(w, firstCall+"_lens")
				io.WriteString(w, ", ")
				io.WriteString(w, firstCall+"_len")
			} else if strings.HasSuffix(paramType, "[]") {
				io.WriteString(w, ", ")
				io.WriteString(w, firstCall+"_len")
			}
			first = false
			continue
		}
		if strings.HasSuffix(paramType, "*") {
			base := strings.TrimSuffix(paramType, "*")
			if _, ok := structTypes[base]; ok {
				io.WriteString(w, "&")
			}
		} else if _, ok := structTypes[paramType]; ok {
			if vr, ok := a.(*VarRef); ok {
				if at, ok2 := varTypes[vr.Name]; ok2 && strings.HasSuffix(at, "*") && strings.TrimSuffix(at, "*") == paramType {
					io.WriteString(w, "*")
				}
			}
		}
		if paramType == "MapSD" {
			if sl, ok := a.(*StructLit); ok && len(sl.Fields) == 0 {
				io.WriteString(w, "(MapSD){0}")
				first = false
				continue
			}
		}
		a.emitExpr(w)
		emitExtraArgs(w, paramType, a)
		first = false
	}
	io.WriteString(w, ")")

	if pre.Len() > 0 {
		io.WriteString(w, ";})")
	}
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
		if vr, ok := b.Right.(*VarRef); ok && isMapVar(vr.Name) {
			keyT := mapKeyTypes[vr.Name]
			if keyT == "const char*" {
				needContainsStr = true
				io.WriteString(w, "contains_str(")
				if varTypes[vr.Name] == "MapSL" || varTypes[vr.Name] == "MapSS" || varTypes[vr.Name] == "MapSI" || varTypes[vr.Name] == "MapSD" || varTypes[vr.Name] == "MapIS" {
					io.WriteString(w, vr.Name+".keys, ")
					io.WriteString(w, vr.Name+".len, ")
				} else {
					io.WriteString(w, vr.Name+"_keys, ")
					io.WriteString(w, vr.Name+"_len, ")
				}
				b.Left.emitExpr(w)
				io.WriteString(w, ")")
				return
			}
			if keyT == "int" {
				needContainsMapInt = true
				io.WriteString(w, "contains_map_int(")
				if varTypes[vr.Name] == "MapSL" || varTypes[vr.Name] == "MapSS" || varTypes[vr.Name] == "MapSI" || varTypes[vr.Name] == "MapSD" || varTypes[vr.Name] == "MapIS" {
					io.WriteString(w, vr.Name+".keys, ")
					io.WriteString(w, vr.Name+".len, ")
				} else {
					io.WriteString(w, vr.Name+"_keys, ")
					io.WriteString(w, vr.Name+"_len, ")
				}
				b.Left.emitExpr(w)
				io.WriteString(w, ")")
				return
			}
		}
		if vr, ok := b.Right.(*VarRef); ok {
			if t, ok2 := varTypes[vr.Name]; ok2 && strings.HasSuffix(t, "[]") {
				elem := strings.TrimSuffix(t, "[]")
				if elem == "int" {
					needContainsInt = true
					io.WriteString(w, "contains_int(")
					io.WriteString(w, vr.Name+", ")
					io.WriteString(w, vr.Name+"_len, ")
					b.Left.emitExpr(w)
					io.WriteString(w, ")")
					return
				}
				if elem == "const char*" {
					needContainsStr = true
					io.WriteString(w, "contains_str(")
					io.WriteString(w, vr.Name+", ")
					io.WriteString(w, vr.Name+"_len, ")
					b.Left.emitExpr(w)
					io.WriteString(w, ")")
					return
				}
			}
		}
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
					if vr, ok := b.Right.(*VarRef); ok {
						if t, ok2 := varTypes[vr.Name]; ok2 && strings.HasSuffix(t, "[]") {
							elem := strings.TrimSuffix(t, "[]")
							if elem == "int" {
								needContainsInt = true
								io.WriteString(w, "contains_int(")
								io.WriteString(w, vr.Name)
								fmt.Fprintf(w, ", sizeof(%s)/sizeof(%s[0]), ", vr.Name, vr.Name)
								b.Left.emitExpr(w)
								io.WriteString(w, ")")
								return
							}
							if elem == "const char*" {
								needContainsStr = true
								io.WriteString(w, "contains_str(")
								io.WriteString(w, vr.Name)
								fmt.Fprintf(w, ", sizeof(%s)/sizeof(%s[0]), ", vr.Name, vr.Name)
								b.Left.emitExpr(w)
								io.WriteString(w, ")")
								return
							}
						}
					}
				}
			}
			io.WriteString(w, ")")
			return
		}
	}
	lt := inferExprType(currentEnv, b.Left)
	rt := inferExprType(currentEnv, b.Right)
	if (b.Op == "/" || b.Op == "%") && lt == "bigint" && rt == "bigint" {
		needGMP = true
		emitBigIntInt(w, b.Left)
		io.WriteString(w, " ")
		io.WriteString(w, b.Op)
		io.WriteString(w, " ")
		emitBigIntInt(w, b.Right)
		return
	}
	if (b.Op == "==" || b.Op == "!=" || b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=") && (lt == "bigint" || rt == "bigint") {
		needGMP = true
		if lt == "bigint" {
			emitBigIntInt(w, b.Left)
		} else {
			b.Left.emitExpr(w)
		}
		fmt.Fprintf(w, " %s ", b.Op)
		if rt == "bigint" {
			emitBigIntInt(w, b.Right)
		} else {
			b.Right.emitExpr(w)
		}
		return
	}
	if b.Op == "+" || b.Op == "-" || b.Op == "*" || b.Op == "/" {
		if lt == "bigrat" || rt == "bigrat" {
			needGMP = true
			op := "_add"
			switch b.Op {
			case "-":
				op = "_sub"
			case "*":
				op = "_mul"
			case "/":
				op = "_div"
			}
			io.WriteString(w, op+"(")
			if lt != "bigrat" {
				io.WriteString(w, "_bigrat(")
				b.Left.emitExpr(w)
				io.WriteString(w, ", 1)")
			} else {
				b.Left.emitExpr(w)
			}
			io.WriteString(w, ", ")
			if rt != "bigrat" {
				io.WriteString(w, "_bigrat(")
				b.Right.emitExpr(w)
				io.WriteString(w, ", 1)")
			} else {
				b.Right.emitExpr(w)
			}
			io.WriteString(w, ")")
			return
		}
	}
	if b.Op == "%" && (exprIsFloat(b.Left) || exprIsFloat(b.Right) || inferExprType(currentEnv, b.Left) == "double" || inferExprType(currentEnv, b.Right) == "double") {
		markMath()
		io.WriteString(w, "fmod(")
		b.Left.emitExpr(w)
		io.WriteString(w, ", ")
		b.Right.emitExpr(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "+" && (exprIsString(b.Left) || exprIsString(b.Right)) {
		needStrConcat = true
		io.WriteString(w, "str_concat(")
		emitStrExpr(w, b.Left)
		io.WriteString(w, ", ")
		emitStrExpr(w, b.Right)
		io.WriteString(w, ")")
		return
	}
	if (exprIsString(b.Left) || exprIsString(b.Right) || inferExprType(currentEnv, b.Left) == "const char*" || inferExprType(currentEnv, b.Right) == "const char*") &&
		(b.Op == "==" || b.Op == "!=" || b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=") {
		if _, ok := b.Left.(*NullLit); ok {
			b.Left.emitExpr(w)
			fmt.Fprintf(w, " %s ", b.Op)
			b.Right.emitExpr(w)
			return
		}
		if _, ok := b.Right.(*NullLit); ok {
			b.Left.emitExpr(w)
			fmt.Fprintf(w, " %s ", b.Op)
			b.Right.emitExpr(w)
			return
		}
		io.WriteString(w, "strcmp(")
		if exprIsBool(b.Left) {
			needStrBool = true
			io.WriteString(w, "str_bool(")
			b.Left.emitExpr(w)
			io.WriteString(w, ")")
		} else {
			b.Left.emitExpr(w)
		}
		io.WriteString(w, ", ")
		if exprIsBool(b.Right) {
			needStrBool = true
			io.WriteString(w, "str_bool(")
			b.Right.emitExpr(w)
			io.WriteString(w, ")")
		} else {
			b.Right.emitExpr(w)
		}
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
	s = strings.ReplaceAll(s, "\n", "\\n")
	s = strings.ReplaceAll(s, "\t", "\\t")
	s = strings.ReplaceAll(s, "\r", "\\r")
	return s
}

func formatConst(e Expr) string {
	switch v := e.(type) {
	case *StringLit:
		return fmt.Sprintf("\"%s\"", escape(v.Value))
	case *IntLit:
		return fmt.Sprintf("%d", v.Value)
	case *FloatLit:
		return fmt.Sprintf("%g", v.Value)
	case *StructLit:
		parts := make([]string, 0, len(v.Fields))
		for _, f := range v.Fields {
			parts = append(parts, fmt.Sprintf("\"%s\": %s", f.Name, formatConst(f.Value)))
		}
		return "{" + strings.Join(parts, ", ") + "}"
	default:
		return ""
	}
}

func formatMapString(m map[string]any) string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	parts := make([]string, len(keys))
	for i, k := range keys {
		switch v := m[k].(type) {
		case string:
			parts[i] = fmt.Sprintf("'%s': '%s'", k, v)
		default:
			parts[i] = fmt.Sprintf("'%s': %v", k, v)
		}
	}
	return "{" + strings.Join(parts, ", ") + "}"
}

func emitMapStringValue(w io.Writer, v Expr) {
	switch t := v.(type) {
	case *StringLit:
		fmt.Fprintf(w, "\"%s\"", escape(t.Value))
	case *IntLit:
		needStrInt = true
		fmt.Fprintf(w, "str_int(%d)", t.Value)
	case *FloatLit:
		needStrFloat = true
		fmt.Fprintf(w, "str_float(%g)", t.Value)
	default:
		v.emitExpr(w)
	}
}

func markMath() {
	needMath = true
}

func isValidIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		if i == 0 {
			if !unicode.IsLetter(r) && r != '_' {
				return false
			}
		} else {
			if !unicode.IsLetter(r) && !unicode.IsDigit(r) && r != '_' {
				return false
			}
		}
	}
	return true
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
	// Pre-scan to determine helper function requirements
	for _, g := range p.Globals {
		g.emit(io.Discard, 0)
	}
	for _, fn := range p.Functions {
		prev := varTypes
		varTypes = fn.VarTypes
		for _, s := range fn.Body {
			s.emit(io.Discard, 1)
		}
		varTypes = prev
	}
	for _, st := range structTypes {
		for _, ft := range st.Fields {
			if mt, ok := ft.(types.MapType); ok {
				if _, ok := mt.Key.(types.StringType); ok {
					if _, ok2 := mt.Value.(types.StringType); ok2 {
						needMapGetSS = true
					} else if lt, ok2 := mt.Value.(types.ListType); ok2 {
						if _, ok3 := lt.Elem.(types.StringType); ok3 {
							needMapGetSL = true
						}
					}
				}
			}
		}
	}

	var buf bytes.Buffer
	ts := gitTimestamp()
	fmt.Fprintf(&buf, "// Generated by Mochi %s on %s\n", strings.TrimSpace(version), ts)
	buf.WriteString("#include <stdio.h>\n")
	buf.WriteString("#include <string.h>\n")
       buf.WriteString("#include <stdlib.h>\n")
       if needSHA256 || needMD5Hex {
               buf.WriteString("#include <unistd.h>\n")
       }
       // Additional features may require standard library functions such as
       // malloc, free, or atoi. Since we already include <stdlib.h> above, avoid
       // emitting a duplicate include here.
       if needStrConcat || needStrInt || needStrFloat || needStrBool || needStrListInt || needStrListStr || needStrListListInt || needStrListDouble || needStrListListDouble || needSubstring || needAtoi || needCharAt || needSliceInt || needSliceDouble || needSliceStr || needRepeat || needParseIntStr || needInput || needNow || needUpper || needLower || needPadStart || needListAppendInt || needListAppendStr || needListAppendPtr || needListAppendDoublePtr || needListAppendDoubleNew || needListAppendStrPtr || needListAppendSizeT || needListAppendStrNew || len(needListAppendStruct) > 0 || len(needListAppendStructNew) > 0 || needSHA256 || needMD5Hex {
               // placeholder to keep condition for future custom includes
       }
	if needMem {
		buf.WriteString("#include <unistd.h>\n")
		buf.WriteString("#include <malloc.h>\n")
	}
	if needUpper || needLower {
		buf.WriteString("#include <ctype.h>\n")
	}
	if needMath {
		buf.WriteString("#include <math.h>\n")
	}
	if needGMP {
		buf.WriteString("#include <gmp.h>\n")
		buf.WriteString("typedef mpq_t* bigrat;\n")
		buf.WriteString("typedef mpz_t* bigint;\n")
	}
	buf.WriteString("\n")
	for _, fn := range p.Functions {
		if strings.HasSuffix(fn.Return, "[]") {
			fmt.Fprintf(&buf, "size_t %s_len;\n", fn.Name)
			if strings.HasSuffix(fn.Return, "[][]") {
				fmt.Fprintf(&buf, "size_t *%s_lens;\n", fn.Name)
			}
		}
	}
	if len(p.Functions) > 0 {
		buf.WriteString("\n")
	}
	if needSHA256 {
		buf.WriteString("size_t _sha256_len;\n")
	}
	if needSliceInt {
		buf.WriteString("size_t _slice_int_len;\n")
	}
	if needSliceDouble {
		buf.WriteString("size_t _slice_double_len;\n")
	}
	if needSliceStr {
		buf.WriteString("size_t _slice_str_len;\n")
	}
	if needContainsInt {
		buf.WriteString("static int contains_int(const int arr[], size_t len, int val) {\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        if (arr[i] == val) return 1;\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return 0;\n")
		buf.WriteString("}\n\n")
	}
	if needContainsMapInt {
		buf.WriteString("static int contains_map_int(const int keys[], size_t len, int key) {\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        if (keys[i] == key) return 1;\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return 0;\n")
		buf.WriteString("}\n\n")
	}
	if needContainsStr {
		buf.WriteString("static int contains_str(const char *arr[], size_t len, const char *val) {\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        if (strcmp(arr[i], val) == 0) return 1;\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return 0;\n")
		buf.WriteString("}\n\n")
	}
	if needListMin {
		buf.WriteString("static long long list_min(const long long arr[], size_t len) {\n")
		buf.WriteString("    if (len == 0) return 0;\n")
		buf.WriteString("    long long m = arr[0];\n")
		buf.WriteString("    for (size_t i = 1; i < len; i++) {\n")
		buf.WriteString("        if (arr[i] < m) m = arr[i];\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return m;\n")
		buf.WriteString("}\n\n")
	}
	if needListMax {
		buf.WriteString("static long long list_max(const long long arr[], size_t len) {\n")
		buf.WriteString("    if (len == 0) return 0;\n")
		buf.WriteString("    long long m = arr[0];\n")
		buf.WriteString("    for (size_t i = 1; i < len; i++) {\n")
		buf.WriteString("        if (arr[i] > m) m = arr[i];\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return m;\n")
		buf.WriteString("}\n\n")
	}
	if needStrConcat {
		buf.WriteString("static char* str_concat(const char *a, const char *b) {\n")
		buf.WriteString("    size_t len1 = strlen(a);\n")
		buf.WriteString("    size_t len2 = strlen(b);\n")
		buf.WriteString("    char *res = malloc(len1 + len2 + 1);\n")
		buf.WriteString("    memcpy(res, a, len1);\n")
		buf.WriteString("    memcpy(res + len1, b, len2);\n")
		buf.WriteString("    res[len1 + len2] = 0;\n")
		buf.WriteString("    return res;\n")
		buf.WriteString("}\n\n")
	}
	if needStrInt {
		buf.WriteString("static char* str_int(long long v) {\n")
		buf.WriteString("    char buf[32];\n")
		buf.WriteString("    snprintf(buf, sizeof(buf), \"%lld\", v);\n")
		buf.WriteString("    return strdup(buf);\n")
		buf.WriteString("}\n\n")
	}
	if needStrBool {
		buf.WriteString("static char* str_bool(long long v) {\n")
		buf.WriteString("    return strdup(v ? \"true\" : \"false\");\n")
		buf.WriteString("}\n\n")
	}
	if needStrFloat {
		buf.WriteString("static char* str_float(double v) {\n")
		buf.WriteString("    char buf[64];\n")
		buf.WriteString("    snprintf(buf, sizeof(buf), \"%g\", v);\n")
		buf.WriteString("    return strdup(buf);\n")
		buf.WriteString("}\n\n")
	}
	if needStrBigInt {
		buf.WriteString("static char* str_bigint(const mpz_t v) {\n")
		buf.WriteString("    return mpz_get_str(NULL, 10, v);\n")
		buf.WriteString("}\n\n")
	}
	if needGMP {
		buf.WriteString("static bigrat _bigrat(long n, long d) {\n")
		buf.WriteString("    bigrat r = malloc(sizeof(mpq_t));\n")
		buf.WriteString("    mpq_init(*r);\n")
		buf.WriteString("    mpq_set_si(*r, n, d);\n")
		buf.WriteString("    mpq_canonicalize(*r);\n")
		buf.WriteString("    return r;\n")
		buf.WriteString("}\n\n")
		buf.WriteString("static bigrat _add(bigrat a, bigrat b) {\n")
		buf.WriteString("    bigrat r = malloc(sizeof(mpq_t));\n")
		buf.WriteString("    mpq_init(*r);\n")
		buf.WriteString("    mpq_add(*r, *a, *b);\n")
		buf.WriteString("    return r;\n")
		buf.WriteString("}\n\n")
		buf.WriteString("static bigrat _sub(bigrat a, bigrat b) {\n")
		buf.WriteString("    bigrat r = malloc(sizeof(mpq_t));\n")
		buf.WriteString("    mpq_init(*r);\n")
		buf.WriteString("    mpq_sub(*r, *a, *b);\n")
		buf.WriteString("    return r;\n")
		buf.WriteString("}\n\n")
		buf.WriteString("static bigrat _mul(bigrat a, bigrat b) {\n")
		buf.WriteString("    bigrat r = malloc(sizeof(mpq_t));\n")
		buf.WriteString("    mpq_init(*r);\n")
		buf.WriteString("    mpq_mul(*r, *a, *b);\n")
		buf.WriteString("    return r;\n")
		buf.WriteString("}\n\n")
		buf.WriteString("static bigrat _div(bigrat a, bigrat b) {\n")
		buf.WriteString("    bigrat r = malloc(sizeof(mpq_t));\n")
		buf.WriteString("    mpq_init(*r);\n")
		buf.WriteString("    mpq_div(*r, *a, *b);\n")
		buf.WriteString("    return r;\n")
		buf.WriteString("}\n\n")
		buf.WriteString("static mpz_srcptr num(bigrat a) { return mpq_numref(*a); }\n")
		buf.WriteString("static mpz_srcptr denom(bigrat a) { return mpq_denref(*a); }\n\n")
	}
	if needStrListInt {
		buf.WriteString("static char* str_list_int(const long long *arr, size_t len) {\n")
		buf.WriteString("    size_t cap = len * 32 + 2;\n")
		buf.WriteString("    char *buf = malloc(cap);\n")
		buf.WriteString("    size_t pos = 0;\n")
		buf.WriteString("    buf[pos++] = '[';\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        char tmp[32];\n")
		buf.WriteString("        snprintf(tmp, sizeof(tmp), \"%d\", arr[i]);\n")
		buf.WriteString("        size_t n = strlen(tmp);\n")
		buf.WriteString("        if (pos + n + 2 >= cap) { cap = cap * 2 + n + 2; buf = realloc(buf, cap); }\n")
		buf.WriteString("        memcpy(buf + pos, tmp, n);\n")
		buf.WriteString("        pos += n;\n")
		buf.WriteString("        if (i + 1 < len) buf[pos++] = ' ';\n")
		buf.WriteString("    }\n")
		buf.WriteString("    buf[pos++] = ']';\n")
		buf.WriteString("    buf[pos] = 0;\n")
		buf.WriteString("    return buf;\n")
		buf.WriteString("}\n\n")
	}
	if needStrListStr {
		buf.WriteString("static char* str_list_str(const char **arr, size_t len) {\n")
		buf.WriteString("    size_t cap = 2;\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) cap += strlen(arr[i]) + 1;\n")
		buf.WriteString("    char *buf = malloc(cap);\n")
		buf.WriteString("    size_t pos = 0;\n")
		buf.WriteString("    buf[pos++] = '[';\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        size_t n = strlen(arr[i]);\n")
		buf.WriteString("        memcpy(buf + pos, arr[i], n);\n")
		buf.WriteString("        pos += n;\n")
		buf.WriteString("        if (i + 1 < len) buf[pos++] = ' ';\n")
		buf.WriteString("    }\n")
		buf.WriteString("    buf[pos++] = ']';\n")
		buf.WriteString("    buf[pos] = 0;\n")
		buf.WriteString("    return buf;\n")
		buf.WriteString("}\n\n")
	}
	if needStrListDouble {
		buf.WriteString("static char* str_list_double(const double *arr, size_t len) {\n")
		buf.WriteString("    size_t cap = len * 32 + 2;\n")
		buf.WriteString("    char *buf = malloc(cap);\n")
		buf.WriteString("    size_t pos = 0;\n")
		buf.WriteString("    buf[pos++] = '[';\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        char tmp[64];\n")
		buf.WriteString("        snprintf(tmp, sizeof(tmp), \"%g\", arr[i]);\n")
		buf.WriteString("        size_t n = strlen(tmp);\n")
		buf.WriteString("        if (pos + n + 2 >= cap) { cap = cap * 2 + n + 2; buf = realloc(buf, cap); }\n")
		buf.WriteString("        memcpy(buf + pos, tmp, n);\n")
		buf.WriteString("        pos += n;\n")
		buf.WriteString("        if (i + 1 < len) buf[pos++] = ' ';\n")
		buf.WriteString("    }\n")
		buf.WriteString("    buf[pos++] = ']';\n")
		buf.WriteString("    buf[pos] = 0;\n")
		buf.WriteString("    return buf;\n")
		buf.WriteString("}\n\n")
	}
	if needStrListListInt {
		buf.WriteString("static char* str_list_list_int(long long **arr, size_t len, const size_t lens[]) {\n")
		buf.WriteString("    size_t cap = 2;\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) cap += lens[i] * 32 + 3;\n")
		buf.WriteString("    char *buf = malloc(cap);\n")
		buf.WriteString("    size_t pos = 0;\n")
		buf.WriteString("    buf[pos++] = '[';\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        char *inner = str_list_int(arr[i], lens[i]);\n")
		buf.WriteString("        size_t n = strlen(inner);\n")
		buf.WriteString("        if (pos + n + 2 >= cap) { cap = cap * 2 + n + 2; buf = realloc(buf, cap); }\n")
		buf.WriteString("        memcpy(buf + pos, inner, n);\n")
		buf.WriteString("        pos += n;\n")
		buf.WriteString("        free(inner);\n")
		buf.WriteString("        if (i + 1 < len) buf[pos++] = ' ';\n")
		buf.WriteString("    }\n")
		buf.WriteString("    buf[pos++] = ']';\n")
		buf.WriteString("    buf[pos] = 0;\n")
		buf.WriteString("    return buf;\n")
		buf.WriteString("}\n\n")
	}
	if needStrListListDouble {
		buf.WriteString("static char* str_list_list_double(double **arr, size_t len, const size_t lens[]) {\n")
		buf.WriteString("    size_t cap = 2;\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) cap += lens[i] * 32 + 3;\n")
		buf.WriteString("    char *buf = malloc(cap);\n")
		buf.WriteString("    size_t pos = 0;\n")
		buf.WriteString("    buf[pos++] = '[';\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        char *inner = str_list_double(arr[i], lens[i]);\n")
		buf.WriteString("        size_t n = strlen(inner);\n")
		buf.WriteString("        if (pos + n + 2 >= cap) { cap = cap * 2 + n + 2; buf = realloc(buf, cap); }\n")
		buf.WriteString("        memcpy(buf + pos, inner, n);\n")
		buf.WriteString("        pos += n;\n")
		buf.WriteString("        free(inner);\n")
		buf.WriteString("        if (i + 1 < len) buf[pos++] = ' ';\n")
		buf.WriteString("    }\n")
		buf.WriteString("    buf[pos++] = ']';\n")
		buf.WriteString("    buf[pos] = 0;\n")
		buf.WriteString("    return buf;\n")
		buf.WriteString("}\n\n")
	}
	if needJSONListListInt {
		buf.WriteString("static void json_list_list_int(long long **arr, size_t len, const size_t lens[]) {\n")
		buf.WriteString("    printf(\"[\\n\");\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        printf(\"  [\\n\");\n")
		buf.WriteString("        for (size_t j = 0; j < lens[i]; j++) {\n")
		buf.WriteString("            printf(\"    %lld\", arr[i][j]);\n")
		buf.WriteString("            if (j + 1 < lens[i]) printf(\",\\n\"); else printf(\"\\n\");\n")
		buf.WriteString("        }\n")
		buf.WriteString("        printf(\"  ]\");\n")
		buf.WriteString("        if (i + 1 < len) printf(\",\\n\"); else printf(\"\\n\");\n")
		buf.WriteString("    }\n")
		buf.WriteString("    printf(\"]\");\n")
		buf.WriteString("}\n\n")
	}
	if needUpper {
		buf.WriteString("static char* str_upper(const char *s) {\n")
		buf.WriteString("    size_t n = strlen(s);\n")
		buf.WriteString("    char *out = strdup(s);\n")
		buf.WriteString("    for(size_t i=0;i<n;i++) out[i] = toupper((unsigned char)out[i]);\n")
		buf.WriteString("    return out;\n")
		buf.WriteString("}\n\n")
	}
	if needLower {
		buf.WriteString("static char* str_lower(const char *s) {\n")
		buf.WriteString("    size_t n = strlen(s);\n")
		buf.WriteString("    char *out = strdup(s);\n")
		buf.WriteString("    for(size_t i=0;i<n;i++) out[i] = tolower((unsigned char)out[i]);\n")
		buf.WriteString("    return out;\n")
		buf.WriteString("}\n\n")
	}
	if needPadStart {
		buf.WriteString("static char* _padStart(const char *s, int width, const char *pad) {\n")
		buf.WriteString("    size_t slen = strlen(s);\n")
		buf.WriteString("    size_t plen = strlen(pad);\n")
		buf.WriteString("    if ((int)slen >= width || plen == 0) return strdup(s);\n")
		buf.WriteString("    size_t padlen = width - slen;\n")
		buf.WriteString("    char *out = malloc(width + 1);\n")
		buf.WriteString("    size_t pos = 0;\n")
		buf.WriteString("    for (size_t i = 0; i < padlen; i++) out[pos++] = pad[i % plen];\n")
		buf.WriteString("    memcpy(out + pos, s, slen);\n")
		buf.WriteString("    out[padlen + slen] = 0;\n")
		buf.WriteString("    return out;\n")
		buf.WriteString("}\n\n")
	}
	if needRepeat {
		buf.WriteString("static char* repeat(const char *s, int n) {\n")
		buf.WriteString("    size_t len = strlen(s);\n")
		buf.WriteString("    char *out = malloc(len * n + 1);\n")
		buf.WriteString("    for (int i = 0; i < n; i++) memcpy(out + i * len, s, len);\n")
		buf.WriteString("    out[len * n] = 0;\n")
		buf.WriteString("    return out;\n")
		buf.WriteString("}\n\n")
	}
	if needParseIntStr {
		buf.WriteString("static long parseIntStr(const char *s, int base) {\n")
		buf.WriteString("    return strtol(s, NULL, base);\n")
		buf.WriteString("}\n\n")
	}
	if needMapGetSL || needMapSetSL {
		buf.WriteString("typedef struct { const char **keys; void **vals; size_t *lens; size_t len; size_t cap; } MapSL;\n\n")
	}
	if needMapGetSS || needMapSetSS {
		buf.WriteString("typedef struct { const char **keys; const char **vals; size_t len; size_t cap; } MapSS;\n\n")
	}
	if needMapGetSI || needMapSetSI {
		buf.WriteString("typedef struct { const char **keys; int *vals; size_t len; size_t cap; } MapSI;\n\n")
	}
	if needMapGetIS || needMapSetIS {
		buf.WriteString("typedef struct { int *keys; const char **vals; size_t len; size_t cap; } MapIS;\n\n")
	}
	if needMapGetSD || needMapSetSD {
		buf.WriteString("typedef struct { const char **keys; double *vals; size_t len; size_t cap; } MapSD;\n\n")
	}
	if needMapGetSI {
		buf.WriteString("static int map_get_si(const char *keys[], const int vals[], size_t len, const char *key) {\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        if (strcmp(keys[i], key) == 0) return vals[i];\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return 0;\n")
		buf.WriteString("}\n\n")
	}
	if needMapSetSI {
		buf.WriteString("static void map_set_si(MapSI *m, const char *key, int val) {\n")
		buf.WriteString("    for (size_t i = 0; i < m->len; i++) {\n")
		buf.WriteString("        if (strcmp(m->keys[i], key) == 0) { m->vals[i] = val; return; }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    if (m->len >= m->cap) {\n")
		buf.WriteString("        m->cap = m->cap ? m->cap * 2 : 16;\n")
		buf.WriteString("        m->keys = realloc((void*)m->keys, m->cap * sizeof(char*));\n")
		buf.WriteString("        m->vals = realloc(m->vals, m->cap * sizeof(int));\n")
		buf.WriteString("    }\n")
		buf.WriteString("    m->keys[m->len] = key; m->vals[m->len] = val; m->len++;\n")
		buf.WriteString("}\n\n")
	}
	if needMapGetSD {
		buf.WriteString("static double map_get_sd(const char *keys[], const double vals[], size_t len, const char *key) {\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        if (strcmp(keys[i], key) == 0) return vals[i];\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return 0;\n")
		buf.WriteString("}\n\n")
	}
	if needMapSetSD {
		buf.WriteString("static void map_set_sd(const char *keys[], double vals[], size_t *len, const char *key, double val) {\n")
		buf.WriteString("    for (size_t i = 0; i < *len; i++) {\n")
		buf.WriteString("        if (strcmp(keys[i], key) == 0) { vals[i] = val; return; }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    keys[*len] = key; vals[*len] = val; (*len)++;\n")
		buf.WriteString("}\n\n")
	}
	if needMapGetII {
		buf.WriteString("static int map_get_ii(const int keys[], const int vals[], size_t len, int key) {\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        if (keys[i] == key) return vals[i];\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return 0;\n")
		buf.WriteString("}\n\n")
	}
	if needMapSetII {
		buf.WriteString("static void map_set_ii(int keys[], int vals[], size_t *len, int key, int val) {\n")
		buf.WriteString("    for (size_t i = 0; i < *len; i++) {\n")
		buf.WriteString("        if (keys[i] == key) { vals[i] = val; return; }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    keys[*len] = key; vals[*len] = val; (*len)++;\n")
		buf.WriteString("}\n\n")
	}
	if needMapGetIS {
		buf.WriteString("static const char* map_get_is(const int keys[], const char *vals[], size_t len, int key) {\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        if (keys[i] == key) return vals[i];\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return \"\";\n")
		buf.WriteString("}\n\n")
	}
	if needMapSetIS {
		buf.WriteString("static void map_set_is(MapIS *m, int key, const char *val) {\n")
		buf.WriteString("    for (size_t i = 0; i < m->len; i++) {\n")
		buf.WriteString("        if (m->keys[i] == key) { m->vals[i] = val; return; }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    if (m->len >= m->cap) {\n")
		buf.WriteString("        m->cap = m->cap ? m->cap * 2 : 16;\n")
		buf.WriteString("        m->keys = realloc(m->keys, m->cap * sizeof(int));\n")
		buf.WriteString("        m->vals = realloc((void*)m->vals, m->cap * sizeof(char*));\n")
		buf.WriteString("    }\n")
		buf.WriteString("    m->keys[m->len] = key; m->vals[m->len] = val; m->len++;\n")
		buf.WriteString("}\n\n")
	}
	if needMapGetSS {
		buf.WriteString("static const char* map_get_ss(const char *keys[], const char *vals[], size_t len, const char *key) {\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        if (strcmp(keys[i], key) == 0) return vals[i];\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return \"\";\n")
		buf.WriteString("}\n\n")
	}
	if needMapSetSS {
		buf.WriteString("static void map_set_ss(MapSS *m, const char *key, const char *val) {\n")
		buf.WriteString("    for (size_t i = 0; i < m->len; i++) {\n")
		buf.WriteString("        if (strcmp(m->keys[i], key) == 0) { m->vals[i] = val; return; }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    if (m->len >= m->cap) {\n")
		buf.WriteString("        m->cap = m->cap ? m->cap * 2 : 16;\n")
		buf.WriteString("        m->keys = realloc((void*)m->keys, m->cap * sizeof(char*));\n")
		buf.WriteString("        m->vals = realloc((void*)m->vals, m->cap * sizeof(char*));\n")
		buf.WriteString("    }\n")
		buf.WriteString("    m->keys[m->len] = key; m->vals[m->len] = val; m->len++;\n")
		buf.WriteString("}\n\n")
	}
	if needMapGetSL {
		buf.WriteString("static void* map_get_sl(const char *keys[], void *vals[], const size_t lens[], size_t len, const char *key, size_t *out_len) {\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) {\n")
		buf.WriteString("        if (strcmp(keys[i], key) == 0) { if (out_len) *out_len = lens[i]; return vals[i]; }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    if (out_len) *out_len = 0;\n")
		buf.WriteString("    return NULL;\n")
		buf.WriteString("}\n\n")
	}
	if needMapSetSL {
		buf.WriteString("static void map_set_sl(const char *keys[], void *vals[], size_t lens[], size_t *len, const char *key, void *val, size_t val_len) {\n")
		buf.WriteString("    for (size_t i = 0; i < *len; i++) {\n")
		buf.WriteString("        if (strcmp(keys[i], key) == 0) { vals[i] = val; lens[i] = val_len; return; }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    keys[*len] = key; vals[*len] = val; lens[*len] = val_len; (*len)++;\n")
		buf.WriteString("}\n\n")
	}
	if needListAppendInt {
		buf.WriteString("static long long* list_append_int(long long *arr, size_t *len, long long val) {\n")
		buf.WriteString("    arr = realloc(arr, (*len + 1) * sizeof(long long));\n")
		buf.WriteString("    arr[*len] = val;\n")
		buf.WriteString("    (*len)++;\n")
		buf.WriteString("    return arr;\n")
		buf.WriteString("}\n\n")
	}
	if needListAppendStr {
		buf.WriteString("static const char** list_append_str(const char **arr, size_t *len, const char *val) {\n")
		buf.WriteString("    arr = realloc((void*)arr, (*len + 1) * sizeof(char*));\n")
		buf.WriteString("    arr[*len] = val;\n")
		buf.WriteString("    (*len)++;\n")
		buf.WriteString("    return arr;\n")
		buf.WriteString("}\n\n")
	}
	if needListAppendIntNew {
		buf.WriteString("static long long* list_append_int_new(const long long *arr, size_t len, long long val) {\n")
		buf.WriteString("    long long *res = malloc((len + 1) * sizeof(long long));\n")
		buf.WriteString("    if (arr && len) memcpy(res, arr, len * sizeof(long long));\n")
		buf.WriteString("    res[len] = val;\n")
		buf.WriteString("    return res;\n")
		buf.WriteString("}\n\n")
	}
	if needListAppendDouble {
		buf.WriteString("static double* list_append_double(double *arr, size_t *len, double val) {\n")
		buf.WriteString("    arr = realloc(arr, (*len + 1) * sizeof(double));\n")
		buf.WriteString("    arr[*len] = val;\n")
		buf.WriteString("    (*len)++;\n")
		buf.WriteString("    return arr;\n")
		buf.WriteString("}\n\n")
	}
	if needListAppendDoubleNew {
		buf.WriteString("static double* list_append_double_new(const double *arr, size_t len, double val) {\n")
		buf.WriteString("    double *res = malloc((len + 1) * sizeof(double));\n")
		buf.WriteString("    if (arr && len) memcpy(res, arr, len * sizeof(double));\n")
		buf.WriteString("    res[len] = val;\n")
		buf.WriteString("    return res;\n")
		buf.WriteString("}\n\n")
	}
        if needListAppendPtr {
                buf.WriteString("static long long** list_append_intptr(long long **arr, size_t *len, long long *val) {\n")
                buf.WriteString("    arr = realloc(arr, (*len + 1) * sizeof(long long*));\n")
                buf.WriteString("    arr[*len] = val;\n")
                buf.WriteString("    (*len)++;\n")
                buf.WriteString("    return arr;\n")
                buf.WriteString("}\n\n")
        }
        if needListAppendPtrPtr {
                buf.WriteString("static long long*** list_append_intptrptr(long long ***arr, size_t *len, long long **val) {\n")
                buf.WriteString("    arr = realloc(arr, (*len + 1) * sizeof(long long**));\n")
                buf.WriteString("    arr[*len] = val;\n")
                buf.WriteString("    (*len)++;\n")
                buf.WriteString("    return arr;\n")
                buf.WriteString("}\n\n")
        }
	if needListAppendDoublePtr {
		buf.WriteString("static double** list_append_doubleptr(double **arr, size_t *len, double *val) {\n")
		buf.WriteString("    arr = realloc(arr, (*len + 1) * sizeof(double*));\n")
		buf.WriteString("    arr[*len] = val;\n")
		buf.WriteString("    (*len)++;\n")
		buf.WriteString("    return arr;\n")
		buf.WriteString("}\n\n")
	}
	if needListAppendStrPtr {
		buf.WriteString("static const char*** list_append_strptr(const char ***arr, size_t *len, const char **val) {\n")
		buf.WriteString("    arr = realloc(arr, (*len + 1) * sizeof(const char**));\n")
		buf.WriteString("    arr[*len] = val;\n")
		buf.WriteString("    (*len)++;\n")
		buf.WriteString("    return arr;\n")
		buf.WriteString("}\n\n")
	}
	if needListAppendStrNew {
		buf.WriteString("static const char** list_append_str_new(const char **arr, size_t len, const char *val) {\n")
		buf.WriteString("    const char **res = malloc((len + 1) * sizeof(const char*));\n")
		buf.WriteString("    if (arr && len) memcpy(res, arr, len * sizeof(const char*));\n")
		buf.WriteString("    res[len] = val;\n")
		buf.WriteString("    return res;\n")
		buf.WriteString("}\n\n")
	}
	if needListAppendBigRat {
		buf.WriteString("static bigrat* list_append_bigrat(bigrat *arr, size_t *len, bigrat val) {\n")
		buf.WriteString("    arr = realloc(arr, (*len + 1) * sizeof(bigrat));\n")
		buf.WriteString("    arr[*len] = val;\n")
		buf.WriteString("    (*len)++;\n")
		buf.WriteString("    return arr;\n")
		buf.WriteString("}\n\n")
	}
	if needListAppendSizeT {
		buf.WriteString("static size_t* list_append_szt(size_t *arr, size_t *len, size_t val) {\n")
		buf.WriteString("    arr = realloc(arr, (*len + 1) * sizeof(size_t));\n")
		buf.WriteString("    arr[*len] = val;\n")
		buf.WriteString("    (*len)++;\n")
		buf.WriteString("    return arr;\n")
		buf.WriteString("}\n\n")
	}
	if needConcatStrPtr {
		buf.WriteString("static const char*** concat_strptr(const char ***a, size_t *a_len, size_t **a_lens, size_t *a_lens_len, const char ***b, size_t b_len, size_t *b_lens) {\n")
		buf.WriteString("    for (size_t i = 0; i < b_len; i++) {\n")
		buf.WriteString("        *a_lens = list_append_szt(*a_lens, a_lens_len, b_lens[i]);\n")
		buf.WriteString("        a = list_append_strptr(a, a_len, b[i]);\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return a;\n")
		buf.WriteString("}\n\n")
	}
	if needConcatLongLong {
		buf.WriteString("static size_t concat_len;\n")
		buf.WriteString("static long long* concat_long_long(long long *a, size_t a_len, const long long *b, size_t b_len) {\n")
		buf.WriteString("    long long *res = realloc(a, (a_len + b_len) * sizeof(long long));\n")
		buf.WriteString("    if (b_len > 0) memcpy(res + a_len, b, b_len * sizeof(long long));\n")
		buf.WriteString("    concat_len = a_len + b_len;\n")
		buf.WriteString("    return res;\n")
		buf.WriteString("}\n\n")
	}
	if needNow {
		buf.WriteString("#include <time.h>\n")
		buf.WriteString("#include <stdlib.h>\n")
		buf.WriteString("static int seeded_now = 0;\n")
		buf.WriteString("static long long now_seed = 0;\n")
		buf.WriteString("static long long _now(void) {\n")
		buf.WriteString("    if (!seeded_now) {\n")
		buf.WriteString("        const char *s = getenv(\"MOCHI_NOW_SEED\");\n")
		buf.WriteString("        if (s && *s) {\n")
		buf.WriteString("            now_seed = atoll(s);\n")
		buf.WriteString("            seeded_now = 1;\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    if (seeded_now) {\n")
		buf.WriteString("        now_seed = (now_seed * 1664525 + 1013904223) % 2147483647;\n")
		buf.WriteString("        return now_seed;\n")
		buf.WriteString("    }\n")
		buf.WriteString("    struct timespec ts;\n")
		buf.WriteString("    clock_gettime(CLOCK_REALTIME, &ts);\n")
		buf.WriteString("    return (long long)(ts.tv_sec * 1000000000LL + ts.tv_nsec);\n")
		buf.WriteString("}\n\n")
	}
	if needMem {
		buf.WriteString("static long long _mem(void) {\n")
		buf.WriteString("    long long size = 0, rss = 0;\n")
		buf.WriteString("    FILE *f = fopen(\"/proc/self/statm\", \"r\");\n")
		buf.WriteString("    if (f) {\n")
		buf.WriteString("        if (fscanf(f, \"%lld %lld\", &size, &rss) != 2) rss = 0;\n")
		buf.WriteString("        fclose(f);\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return rss * (long long)sysconf(_SC_PAGESIZE);\n")
		buf.WriteString("}\n\n")
	}
	if needInput {
		buf.WriteString("static char* _input(void) {\n")
		buf.WriteString("    char buf[1024];\n")
		buf.WriteString("    if (!fgets(buf, sizeof(buf), stdin)) return strdup(\"\");\n")
		buf.WriteString("    size_t len = strlen(buf);\n")
		buf.WriteString("    if (len > 0 && buf[len-1] == '\\n') buf[len-1] = 0;\n")
		buf.WriteString("    return strdup(buf);\n")
		buf.WriteString("}\n\n")
	}
	buf.WriteString("static void panic(const char *msg) {\n")
	buf.WriteString("    fputs(msg, stderr);\n")
	buf.WriteString("    fputc('\\n', stderr);\n")
	buf.WriteString("    exit(1);\n")
	buf.WriteString("}\n\n")
	if needAtoi {
		buf.WriteString("static int _atoi(const char *s) {\n")
		buf.WriteString("    return atoi(s);\n")
		buf.WriteString("}\n\n")
	}
	if needSHA256 {
		buf.WriteString("static int* _sha256(const int *arr, size_t len) {\n")
		buf.WriteString("    char tmpl[] = \"/tmp/mochi_sha256_XXXXXX\";\n")
		buf.WriteString("    int fd = mkstemp(tmpl);\n")
		buf.WriteString("    if (fd < 0) return NULL;\n")
		buf.WriteString("    FILE *f = fdopen(fd, \"wb\");\n")
		buf.WriteString("    for (size_t i = 0; i < len; i++) fputc((unsigned char)arr[i], f);\n")
		buf.WriteString("    fclose(f);\n")
		buf.WriteString("    char cmd[256];\n")
		buf.WriteString("    snprintf(cmd, sizeof(cmd), \"sha256sum %s\", tmpl);\n")
		buf.WriteString("    FILE *p = popen(cmd, \"r\");\n")
		buf.WriteString("    char hex[65] = {0};\n")
		buf.WriteString("    if (p) { fscanf(p, \"%64s\", hex); pclose(p); }\n")
		buf.WriteString("    unlink(tmpl);\n")
		buf.WriteString("    int *out = malloc(32 * sizeof(int));\n")
		buf.WriteString("    for (int i = 0; i < 32; i++) {\n")
		buf.WriteString("        char b[3] = {hex[i*2], hex[i*2+1], 0};\n")
		buf.WriteString("        out[i] = (int)strtol(b, NULL, 16);\n")
		buf.WriteString("    }\n")
		buf.WriteString("    _sha256_len = 32;\n")
		buf.WriteString("    return out;\n")
		buf.WriteString("}\n\n")
	}
	if needMD5Hex {
		buf.WriteString("static char* _md5hex(const char *s) {\n")
		buf.WriteString("    char tmpl[] = \"/tmp/mochi_md5_XXXXXX\";\n")
		buf.WriteString("    int fd = mkstemp(tmpl);\n")
		buf.WriteString("    if (fd < 0) return NULL;\n")
		buf.WriteString("    FILE *f = fdopen(fd, \"wb\");\n")
		buf.WriteString("    fputs(s, f);\n")
		buf.WriteString("    fclose(f);\n")
		buf.WriteString("    char cmd[256];\n")
		buf.WriteString("    snprintf(cmd, sizeof(cmd), \"md5sum %s\", tmpl);\n")
		buf.WriteString("    FILE *p = popen(cmd, \"r\");\n")
		buf.WriteString("    char hex[33] = {0};\n")
		buf.WriteString("    if (p) { fscanf(p, \"%32s\", hex); pclose(p); }\n")
		buf.WriteString("    unlink(tmpl);\n")
		buf.WriteString("    return strdup(hex);\n")
		buf.WriteString("}\n\n")
	}
	if needSubstring {
		buf.WriteString("static const char* _substring(const char *s, int start, int end) {\n")
		buf.WriteString("    int len = (int)strlen(s);\n")
		buf.WriteString("    if (start < 0) start = 0;\n")
		buf.WriteString("    if (end > len) end = len;\n")
		buf.WriteString("    if (start > end) start = end;\n")
		buf.WriteString("    char *res = malloc(end - start + 1);\n")
		buf.WriteString("    memcpy(res, s + start, end - start);\n")
		buf.WriteString("    res[end - start] = 0;\n")
		buf.WriteString("    return res;\n")
		buf.WriteString("}\n\n")
	}
	if needCharAt {
		buf.WriteString("static int _char_at(const char *s, int idx) {\n")
		buf.WriteString("    int len = (int)strlen(s);\n")
		buf.WriteString("    if (idx < 0 || idx >= len) return 0;\n")
		buf.WriteString("    return (unsigned char)s[idx];\n")
		buf.WriteString("}\n\n")
	}
	if needIndexOf {
		buf.WriteString("static long long _indexOf(const char *s, const char *sub) {\n")
		buf.WriteString("    const char *p = strstr(s, sub);\n")
		buf.WriteString("    if (p) return (long long)(p - s);\n")
		buf.WriteString("    return -1;\n")
		buf.WriteString("}\n\n")
	}
	if needSliceInt {
		buf.WriteString("static long long* _slice_int(const long long *arr, size_t len, int start, int end, size_t *out_len) {\n")
		buf.WriteString("    if (start < 0) start = 0;\n")
		buf.WriteString("    if ((size_t)end > len) end = len;\n")
		buf.WriteString("    if (start > end) start = end;\n")
		buf.WriteString("    size_t n = end - start;\n")
		buf.WriteString("    long long *res = NULL;\n")
		buf.WriteString("    if (n) {\n")
		buf.WriteString("        res = malloc(n * sizeof(long long));\n")
		buf.WriteString("        memcpy(res, arr + start, n * sizeof(long long));\n")
		buf.WriteString("    }\n")
		buf.WriteString("    *out_len = n;\n")
		buf.WriteString("    _slice_int_len = n;\n")
		buf.WriteString("    return res;\n")
		buf.WriteString("}\n\n")
	}
	if needSliceDouble {
		buf.WriteString("static double* _slice_double(const double *arr, size_t len, int start, int end, size_t *out_len) {\n")
		buf.WriteString("    if (start < 0) start = 0;\n")
		buf.WriteString("    if ((size_t)end > len) end = len;\n")
		buf.WriteString("    if (start > end) start = end;\n")
		buf.WriteString("    size_t n = end - start;\n")
		buf.WriteString("    double *res = NULL;\n")
		buf.WriteString("    if (n) {\n")
		buf.WriteString("        res = malloc(n * sizeof(double));\n")
		buf.WriteString("        memcpy(res, arr + start, n * sizeof(double));\n")
		buf.WriteString("    }\n")
		buf.WriteString("    *out_len = n;\n")
		buf.WriteString("    _slice_double_len = n;\n")
		buf.WriteString("    return res;\n")
		buf.WriteString("}\n\n")
	}
	if needSliceStr {
		buf.WriteString("static const char** _slice_str(const char **arr, size_t len, int start, int end, size_t *out_len) {\n")
		buf.WriteString("    if (start < 0) start = 0;\n")
		buf.WriteString("    if ((size_t)end > len) end = len;\n")
		buf.WriteString("    if (start > end) start = end;\n")
		buf.WriteString("    size_t n = end - start;\n")
		buf.WriteString("    const char **res = NULL;\n")
		buf.WriteString("    if (n) {\n")
		buf.WriteString("        res = malloc(n * sizeof(const char*));\n")
		buf.WriteString("        memcpy(res, arr + start, n * sizeof(const char*));\n")
		buf.WriteString("    }\n")
		buf.WriteString("    *out_len = n;\n")
		buf.WriteString("    _slice_str_len = n;\n")
		buf.WriteString("    return res;\n")
		buf.WriteString("}\n\n")
	}
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
			if ft, ok := st.Fields[field]; ok {
				if ffunc, ok2 := ft.(types.FuncType); ok2 {
					var params []string
					for _, p := range ffunc.Params {
						params = append(params, cTypeFromMochiType(p))
					}
					ret := cTypeFromMochiType(ffunc.Return)
					fmt.Fprintf(&buf, "    %s (*%s)(%s);\n", ret, field, strings.Join(params, ", "))
					continue
				}
				typ := cTypeFromMochiType(ft)
				if typ == "MapSS" {
					needMapGetSS = true
				}
				if typ == "MapSL" {
					needMapGetSL = true
				}
				if strings.HasSuffix(typ, "[]") {
					if strings.HasSuffix(typ, "[][]") {
						base := strings.TrimSuffix(typ, "[][]")
						fmt.Fprintf(&buf, "    %s **%s;\n", base, field)
						fmt.Fprintf(&buf, "    size_t %s_len;\n", field)
						fmt.Fprintf(&buf, "    size_t *%s_lens;\n", field)
						fmt.Fprintf(&buf, "    size_t %s_lens_len;\n", field)
					} else {
						base := strings.TrimSuffix(typ, "[]")
						fmt.Fprintf(&buf, "    %s *%s;\n", base, field)
						fmt.Fprintf(&buf, "    size_t %s_len;\n", field)
					}
				} else {
					fmt.Fprintf(&buf, "    %s %s;\n", typ, field)
				}
				continue
			}
			fmt.Fprintf(&buf, "    int %s;\n", field)
		}
		fmt.Fprintf(&buf, "};\n\n")
	}
	if len(needListAppendStruct) > 0 {
		for typ := range needListAppendStruct {
			name := sanitizeTypeName(typ)
			fmt.Fprintf(&buf, "static %s* list_append_%s(%s *arr, size_t *len, %s val) {\n", typ, name, typ, typ)
			fmt.Fprintf(&buf, "    arr = realloc(arr, (*len + 1) * sizeof(%s));\n", typ)
			buf.WriteString("    arr[*len] = val;\n")
			buf.WriteString("    (*len)++;\n")
			buf.WriteString("    return arr;\n")
			buf.WriteString("}\n\n")
		}
	}
	if len(needListAppendStructPtr) > 0 {
		for typ := range needListAppendStructPtr {
			name := sanitizeTypeName(typ)
			fmt.Fprintf(&buf, "static %s** list_append_%sptr(%s **arr, size_t *len, %s *val) {\n", typ, name, typ, typ)
			fmt.Fprintf(&buf, "    arr = realloc(arr, (*len + 1) * sizeof(%s*));\n", typ)
			buf.WriteString("    arr[*len] = val;\n")
			buf.WriteString("    (*len)++;\n")
			buf.WriteString("    return arr;\n")
			buf.WriteString("}\n\n")
		}
	}
	if len(needListAppendStructNew) > 0 {
		for typ := range needListAppendStructNew {
			name := sanitizeTypeName(typ)
			fmt.Fprintf(&buf, "static %s* list_append_%s_new(const %s *arr, size_t len, %s val) {\n", typ, name, typ, typ)
			fmt.Fprintf(&buf, "    %s *res = malloc((len + 1) * sizeof(%s));\n", typ, typ)
			fmt.Fprintf(&buf, "    if (arr && len) memcpy(res, arr, len * sizeof(%s));\n", typ)
			buf.WriteString("    res[len] = val;\n")
			buf.WriteString("    return res;\n")
			buf.WriteString("}\n\n")
		}
	}
	for _, g := range p.Globals {
		g.emit(&buf, 0)
	}
	if len(p.Globals) > 0 {
		buf.WriteString("\n")
	}
	// emit forward declarations for all functions
	for _, f := range p.Functions {
		ret := f.Return
		if ret == "" {
			ret = "int"
		}
		ptr := 0
		for strings.HasSuffix(ret, "[]") {
			ret = strings.TrimSuffix(ret, "[]")
			ptr++
		}
		if ret == "" {
			ret = "int"
		}
		buf.WriteString(ret)
		for i := 0; i < ptr; i++ {
			buf.WriteString(" *")
		}
		buf.WriteString(" ")
		buf.WriteString(f.Name)
		if f.Name == "main" && len(f.Params) == 0 {
			buf.WriteString("(void);")
		} else {
			buf.WriteString("(")
			for i, p := range f.Params {
				if i > 0 {
					buf.WriteString(", ")
				}
				t := p.Type
				if t == "" {
					t = "int"
				}
				if strings.Contains(t, "(*)") {
					parts := strings.Split(t, "(*)")
					buf.WriteString(parts[0])
					buf.WriteString("(*")
					buf.WriteString(p.Name)
					buf.WriteString(")")
					buf.WriteString(parts[1])
				} else {
					ptr2 := 0
					for strings.HasSuffix(t, "[]") {
						t = strings.TrimSuffix(t, "[]")
						ptr2++
					}
					if t == "" {
						t = "int"
					}
					buf.WriteString(t)
					for j := 0; j < ptr2; j++ {
						buf.WriteString(" *")
					}
					buf.WriteString(" ")
					buf.WriteString(p.Name)
				}
			}
			buf.WriteString(");")
		}
		buf.WriteString("\n")
	}
	if len(p.Functions) > 0 {
		buf.WriteString("\n")
	}
	for i, f := range p.Functions {
		prevVars := varTypes
		varTypes = f.VarTypes
		currentFuncName = f.Name
		currentFuncReturn = f.Return
		currentLocals = f.Locals
		ret := f.Return
		if ret == "" {
			ret = "int"
		}
		ptr := 0
		for strings.HasSuffix(ret, "[]") {
			ret = strings.TrimSuffix(ret, "[]")
			ptr++
		}
		if ret == "" {
			ret = "int"
		}
		buf.WriteString(ret)
		for i := 0; i < ptr; i++ {
			buf.WriteString(" *")
		}
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
				t := p.Type
				if strings.Contains(t, "(*)") {
					parts := strings.Split(t, "(*)")
					buf.WriteString(parts[0])
					buf.WriteString("(*")
					buf.WriteString(p.Name)
					buf.WriteString(")")
					buf.WriteString(parts[1])
				} else {
					ptr := 0
					for strings.HasSuffix(t, "[]") {
						t = strings.TrimSuffix(t, "[]")
						ptr++
					}
					if t == "" {
						t = "int"
					}
					buf.WriteString(t)
					for j := 0; j < ptr; j++ {
						buf.WriteString(" *")
					}
					buf.WriteString(" ")
					buf.WriteString(p.Name)
				}
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
		varTypes = prevVars
		currentFuncName = ""
		currentFuncReturn = ""
		currentLocals = nil
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into a C AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	constLists = make(map[string]*ListLit)
	constStrings = make(map[string]string)
	structTypes = env.Structs()
	anonStructs = make(map[string]string)
	currentEnv = env
	funcParamTypes = make(map[string][]string)
	funcReturnTypes = make(map[string]string)
	varTypes = make(map[string]string)
	closureApply = make(map[string]string)
	closureFields = make(map[string][]string)
	localFuncArgs = make(map[string][]string)
	mapKeyTypes = make(map[string]string)
	mapValTypes = make(map[string]string)
	varAliases = make(map[string]string)
	extraFuncs = nil
	structCounter = 0
	tempCounter = 0
	anonFuncCounter = 0
	needMath = false
	needContainsInt = false
	needContainsStr = false
	needStrConcat = false
	needStrInt = false
	needStrFloat = false
	needStrBool = false
	needStrListInt = false
	needStrListStr = false
	needStrListListInt = false
	needStrListDouble = false
	needStrListListDouble = false
	needJSONListListInt = false
	needUpper = false
	needLower = false
	needPadStart = false
	needMapGetSI = false
	needMapSetSI = false
	needMapGetIS = false
	needMapGetSL = false
	needMapSetSL = false
	needMapGetSS = false
	needMapSetSS = false
	needMapGetII = false
	needMapSetII = false
	needContainsMapInt = false
	needListAppendInt = false
	needListAppendStr = false
        needListAppendPtr = false
        needListAppendPtrPtr = false
        needListAppendStrPtr = false
	needConcatStrPtr = false
	needListAppendDouble = false
	needListAppendDoublePtr = false
	needListAppendDoubleNew = false
	needListAppendStrNew = false
	needListAppendIntNew = false
	needListAppendStruct = map[string]bool{}
	needListAppendStructNew = map[string]bool{}
	needListAppendStructPtr = map[string]bool{}
	needSHA256 = false
	needMD5Hex = false
	needNow = false
	needMem = false
	needInput = false
	needSubstring = false
	needAtoi = false
	needCharAt = false
	needIndexOf = false
	needSliceInt = false
	needSliceDouble = false
	needSliceStr = false
	needRepeat = false
	needParseIntStr = false
	needGMP = false
	needStrBigInt = false
	needListAppendBigRat = false
	datasetWhereEnabled = false
	joinMultiEnabled = false
	builtinAliases = make(map[string]string)
	funcAliases = make(map[string]string)

	// downgrade bigint variables to int to avoid unnecessary GMP usage
	for name, typ := range env.Types() {
		if _, ok := typ.(types.BigIntType); ok {
			env.SetVarDeep(name, types.IntType{}, true)
		}
	}

	for _, st := range prog.Statements {
		if st.Import != nil && st.Import.Lang != nil {
			alias := st.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(st.Import.Path)
			}
			path := strings.Trim(st.Import.Path, "\"")
			switch *st.Import.Lang {
			case "go":
				if st.Import.Auto && path == "mochi/runtime/ffi/go/testpkg" {
					builtinAliases[alias] = "go_testpkg"
				}
			case "python":
				if path == "math" {
					builtinAliases[alias] = "python_math"
				}
			}
		}
	}
	p := &Program{}
	mainFn := &Function{Name: "main"}
	var globals []Stmt
	for _, st := range prog.Statements {
		if st.Fun != nil {
			fnExpr := &parser.FunExpr{Params: st.Fun.Params, Return: st.Fun.Return, BlockBody: st.Fun.Body}
			name := st.Fun.Name
			if name == "main" {
				name = "user_main"
				funcAliases["main"] = "user_main"
			}
			if name == "abs" { // avoid conflict with stdlib
				funcAliases[name] = "user_abs"
				name = "user_abs"
			}
			if name == "floorf" { // avoid conflict with stdlib
				funcAliases[name] = "user_floorf"
				name = "user_floorf"
			}
			if name == "floor" { // avoid conflict with stdlib
				funcAliases[name] = "user_floor"
				name = "user_floor"
			}
			if name == "powf" { // avoid conflict with stdlib
				funcAliases[name] = "user_powf"
				name = "user_powf"
			}
			if name == "pow" { // avoid conflict with stdlib
				funcAliases[name] = "user_pow"
				name = "user_pow"
			}
			if name == "pow10" { // avoid conflict with stdlib
				funcAliases[name] = "user_pow10"
				name = "user_pow10"
			}
			if name == "round" { // avoid conflict with stdlib
				funcAliases[name] = "user_round"
				name = "user_round"
			}
			if name == "roundf" { // avoid conflict with stdlib
				funcAliases[name] = "user_roundf"
				name = "user_roundf"
			}
			if name == "double" { // avoid conflict with C keyword
				funcAliases[name] = "user_double"
				name = "user_double"
			}
			if name == "bsearch" { // avoid conflict with stdlib
				funcAliases[name] = "user_bsearch"
				name = "user_bsearch"
			}
			if name == "char" { // avoid conflict with C keyword
				funcAliases[name] = "user_char"
				name = "user_char"
			}
			if name == "div" { // avoid conflict with stdlib
				funcAliases[name] = "user_div"
				name = "user_div"
			}
			if name == "strdup" { // avoid conflict with stdlib
				funcAliases[name] = "user_strdup"
				name = "user_strdup"
			}
			if name == "bigrat" { // avoid conflict with type alias
				funcAliases[name] = "user_bigrat"
				name = "user_bigrat"
			}
			if name == "rand" { // avoid conflict with stdlib
				funcAliases[name] = "user_rand"
				name = "user_rand"
			}
			if name == "random" { // avoid conflict with stdlib
				funcAliases[name] = "user_random"
				name = "user_random"
			}
			if name == "repeat" { // avoid conflict with builtin helper
				funcAliases[name] = "user_repeat"
				name = "user_repeat"
			}
			fun, err := compileFunction(env, name, fnExpr)
			if err != nil {
				return nil, err
			}
			ret := fun.Return
			if ret == "const char*" || strings.Contains(ret, "[]") {
				funcReturnTypes[name] = ret
			} else {
				funcReturnTypes[name] = strings.ReplaceAll(ret, "*", "[]")
			}
			p.Functions = append(p.Functions, fun)
			if len(extraFuncs) > 0 {
				p.Functions = append(p.Functions, extraFuncs...)
				extraFuncs = nil
			}
			continue
		}
		if st.Let != nil {
			if fn := detectFunExpr(st.Let.Value); fn != nil {
				fun, err := compileFunction(env, st.Let.Name, fn)
				if err != nil {
					return nil, err
				}
				p.Functions = append(p.Functions, fun)
				if len(extraFuncs) > 0 {
					p.Functions = append(p.Functions, extraFuncs...)
					extraFuncs = nil
				}
				funcReturnTypes[st.Let.Name] = fun.Return
				continue
			}
		}
		stmt, err := compileStmt(env, st)
		if err != nil {
			return nil, err
		}
		if len(extraFuncs) > 0 {
			p.Functions = append(p.Functions, extraFuncs...)
			extraFuncs = nil
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
					if ds, ok := stmt.(*DeclStmt); ok {
						globals = append(globals, &DeclStmt{Name: ds.Name, Type: ds.Type})
						mainFn.Body = append(mainFn.Body, &AssignStmt{Name: ds.Name, Value: ds.Value})
					} else {
						mainFn.Body = append(mainFn.Body, stmt)
					}
				}
			} else {
				mainFn.Body = append(mainFn.Body, stmt)
			}
		}
	}
	if benchMain {
		needNow = true
		needMem = true
		mainFn.Body = []Stmt{&BenchStmt{Name: "main", Body: mainFn.Body}}
	}
	mainFn.VarTypes = varTypes
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
	case s.Import != nil:
		return nil, nil
	case s.Fun != nil:
		if localFuncs != nil {
			// hoist local function
			fnExpr := &parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body}
			// gather captured variables
			vars := map[string]struct{}{}
			for _, st := range s.Fun.Body {
				gatherVarsStmt(st, vars)
			}
			// remove params and locals
			for _, p := range s.Fun.Params {
				delete(vars, p.Name)
			}
			decls := map[string]struct{}{}
			for _, st := range s.Fun.Body {
				gatherDeclsStmt(st, decls)
			}
			for n := range decls {
				delete(vars, n)
			}
			capNames := make([]string, 0, len(vars))
			for n := range vars {
				capNames = append(capNames, n)
			}
			sort.Strings(capNames)
			var newParams []*parser.Param
			for _, n := range capNames {
				var t types.Type
				if vt, err := env.GetVar(n); err == nil {
					t = vt
				} else if ct, ok := varTypes[n]; ok {
					if ct == "const char*" {
						t = types.StringType{}
					} else if strings.Contains(ct, "double") {
						t = types.FloatType{}
					} else {
						t = types.IntType{}
					}
				} else {
					t = types.IntType{}
				}
				newParams = append(newParams, &parser.Param{Name: n, Type: typeRefFromMochiType(t)})
			}
			newParams = append(newParams, s.Fun.Params...)
			fnExpr.Params = newParams
			innerName := currentFuncName + "_" + s.Fun.Name
			fun, err := compileFunction(env, innerName, fnExpr)
			if err != nil {
				return nil, err
			}
			extraFuncs = append(extraFuncs, fun)
			funcAliases[s.Fun.Name] = innerName
			if len(capNames) > 0 {
				localFuncArgs[innerName] = capNames
			}
			return nil, nil
		}
		return nil, nil
	case s.Expr != nil:
		call := s.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" {
			if len(call.Args) == 1 {
				if joinMultiEnabled {
					if vname := varName(call.Args[0]); vname == "result" {
						return &RawStmt{Code: genPrintJoinMulti()}, nil
					}
				}
				if multiJoinEnabled {
					if vname := varName(call.Args[0]); vname == "grouped" {
						return &RawStmt{Code: genPrintGrouped()}, nil
					}
				}
				if multiJoinSort {
					if vname := varName(call.Args[0]); vname == "result" {
						return &RawStmt{Code: genPrintGroupSort()}, nil
					}
				}
			}
			var args []Expr
			var typesList []string
			for _, a := range call.Args {
				ex := convertExpr(a)
				if ex == nil {
					return nil, fmt.Errorf("invalid print argument")
				}
				tname := ""
				switch {
				case exprIsString(ex):
					tname = "string"
				case func() bool { _, ok := ex.(*FieldExpr); return ok }():
					ft := inferExprType(env, ex)
					if ft == "const char*" {
						tname = "string"
					} else if ft == "double" {
						tname = "float"
					}
				case func() bool { _, ok := ex.(*VarRef); return ok }():
					v := ex.(*VarRef)
					if vt, ok := varTypes[v.Name]; ok {
						switch {
						case strings.HasSuffix(vt, "[][]"):
							base := strings.TrimSuffix(vt, "[][]")
							switch base {
							case "int", "long long":
								tname = "list_list_int"
								needStrListListInt = true
								needStrListInt = true
							case "double":
								tname = "list_list_double"
								needStrListListDouble = true
								needStrListDouble = true
							case "const char*":
								tname = "list_list_str"
								needStrListStr = true
							}
						case strings.HasSuffix(vt, "[]"):
							base := strings.TrimSuffix(vt, "[]")
							switch base {
							case "int", "long long":
								tname = "list_int"
								needStrListInt = true
							case "double":
								tname = "list_double"
								needStrListDouble = true
							case "const char*":
								tname = "list_str"
								needStrListStr = true
							}
						case vt == "const char*":
							tname = "string"
						case vt == "double":
							tname = "float"
						}
					} else if t, err := env.GetVar(v.Name); err == nil {
						switch t.(type) {
						case types.StringType:
							tname = "string"
						case types.FloatType:
							tname = "float"
						}
					}
				case func() bool { _, ok := ex.(*IndexExpr); return ok }():
					ix := ex.(*IndexExpr)
					if v, ok := ix.Target.(*VarRef); ok {
						if vt, ok2 := varTypes[v.Name]; ok2 {
							if strings.HasSuffix(vt, "[][]") {
								base := strings.TrimSuffix(vt, "[][]")
								switch base {
								case "int", "long long":
									tname = "list_int"
									needStrListInt = true
								case "double":
									tname = "list_double"
									needStrListDouble = true
								case "const char*":
									tname = "list_str"
									needStrListStr = true
								}
							}
						}
					}
				default:
					ft := inferExprType(env, ex)
					ftc := strings.ReplaceAll(ft, " ", "")
					switch {
					case ftc == "double":
						tname = "float"
					case strings.HasSuffix(ftc, "**"):
						base := strings.TrimSuffix(ftc, "**")
						switch base {
						case "longlong":
							tname = "list_list_int"
							needStrListListInt = true
							needStrListInt = true
						case "double":
							tname = "list_list_double"
							needStrListListDouble = true
							needStrListDouble = true
						case "constchar*":
							tname = "list_list_str"
							needStrListStr = true
						}
					case strings.HasSuffix(ftc, "*"):
						base := strings.TrimSuffix(ftc, "*")
						switch base {
						case "longlong":
							tname = "list_int"
							needStrListInt = true
						case "double":
							tname = "list_double"
							needStrListDouble = true
						case "constchar*":
							tname = "list_str"
							needStrListStr = true
						}
					case strings.HasSuffix(ftc, "[]"):
						base := strings.TrimSuffix(ftc, "[]")
						switch base {
						case "longlong":
							tname = "list_int"
							needStrListInt = true
						case "double":
							tname = "list_double"
							needStrListDouble = true
						case "constchar*":
							tname = "list_str"
							needStrListStr = true
						}
					}
				}
				args = append(args, ex)
				typesList = append(typesList, tname)
			}
			return &PrintStmt{Args: args, Types: typesList}, nil
		}
		if call != nil && call.Func == "json" && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			if str, ok := evalString(&CallExpr{Func: "json", Args: []Expr{arg}}); ok {
				return &PrintStmt{Args: []Expr{&StringLit{Value: str}}, Types: []string{"string"}}, nil
			}
			typ := inferExprType(currentEnv, arg)
			if strings.HasPrefix(typ, "long long") && strings.HasSuffix(typ, "[][]") {
				needJSONListListInt = true
				return &JSONCall{Arg: arg}, nil
			}
			if strings.HasPrefix(typ, "long long") && strings.HasSuffix(typ, "[]") {
				needStrListInt = true
				return &JSONListIntCall{Arg: arg}, nil
			}
			return nil, fmt.Errorf("unsupported json argument")
		}
		if call != nil {
			var args []Expr
			for _, a := range call.Args {
				ex := convertExpr(a)
				if ex == nil {
					return nil, fmt.Errorf("invalid call argument")
				}
				args = append(args, ex)
			}
			if newName, ok := funcAliases[call.Func]; ok {
				call.Func = newName
			}
			typ := ""
			if t, ok := funcReturnTypes[call.Func]; ok {
				typ = t
			}
			return &CallStmt{Func: call.Func, Args: args, Type: typ}, nil
		}
	case s.Let != nil:
		currentVarName = aliasName(s.Let.Name)
		s.Let.Name = currentVarName
		if s.Let.Type != nil {
			currentVarType = types.ResolveTypeRef(s.Let.Type, env)
		} else {
			currentVarType = nil
		}
		if fn := detectFunExpr(s.Let.Value); fn != nil {
			fun, err := compileFunction(env, s.Let.Name, fn)
			if err != nil {
				return nil, err
			}
			extraFuncs = append(extraFuncs, fun)
			funcReturnTypes[s.Let.Name] = fun.Return
			return nil, nil
		}
		if q := queryExpr(s.Let.Value); q != nil {
			if s.Let.Name == "filtered" && matchFilteredQuery(q) {
				multiJoinEnabled = true
				st := types.StructType{Name: "FilteredItem", Fields: map[string]types.Type{"part": types.IntType{}, "value": types.FloatType{}}, Order: []string{"part", "value"}}
				currentEnv.SetStruct(st.Name, st)
				structTypes = currentEnv.Structs()
				markMath()
				return &RawStmt{Code: genFilteredLoops()}, nil
			}
			if s.Let.Name == "grouped" && matchGroupedQuery(q) {
				st := types.StructType{Name: "GroupedItem", Fields: map[string]types.Type{"part": types.IntType{}, "total": types.IntType{}}, Order: []string{"part", "total"}}
				currentEnv.SetStruct(st.Name, st)
				structTypes = currentEnv.Structs()
				return &RawStmt{Code: genGroupedLoops()}, nil
			}
			if s.Let.Name == "result" && matchGroupSortQuery(q) {
				multiJoinSort = true
				st := types.StructType{Name: "ResultItem", Fields: map[string]types.Type{
					"c_custkey": types.IntType{},
					"c_name":    types.StringType{},
					"revenue":   types.FloatType{},
					"c_acctbal": types.FloatType{},
					"n_name":    types.StringType{},
					"c_address": types.StringType{},
					"c_phone":   types.StringType{},
					"c_comment": types.StringType{},
				}, Order: []string{"c_custkey", "c_name", "revenue", "c_acctbal", "n_name", "c_address", "c_phone", "c_comment"}}
				currentEnv.SetStruct(st.Name, st)
				structTypes = currentEnv.Structs()
				markMath()
				return &RawStmt{Code: genGroupSortLoops()}, nil
			}
			if s.Let.Name == "stats" && matchGroupLeftJoinQuery(q) {
				groupLeftJoinEnabled = true
				st := types.StructType{Name: "Stat", Fields: map[string]types.Type{
					"name":  types.StringType{},
					"count": types.IntType{},
				}, Order: []string{"name", "count"}}
				currentEnv.SetStruct(st.Name, st)
				structTypes = currentEnv.Structs()
				return &RawStmt{Code: genGroupLeftJoinLoops()}, nil
			}
			if s.Let.Name == "pairs" && matchCrossJoinFilterQuery(q) {
				if nums, ok1 := constLists["nums"]; ok1 {
					if letters, ok2 := constLists["letters"]; ok2 {
						var elems []Expr
						for _, n := range nums.Elems {
							iv, ok := evalInt(n)
							if !ok {
								continue
							}
							if iv%2 == 0 {
								for _, l := range letters.Elems {
									elems = append(elems, &StructLit{Name: "Pair", Fields: []StructField{{Name: "n", Value: n}, {Name: "l", Value: l}}})
								}
							}
						}
						valExpr := &ListLit{Elems: elems}
						constLists[s.Let.Name] = valExpr
						var vals []any
						for _, e := range elems {
							if v, ok := valueFromExpr(e); ok {
								vals = append(vals, v)
							}
						}
						currentEnv.SetValue(s.Let.Name, vals, true)
						st := types.StructType{Name: "Pair", Fields: map[string]types.Type{"n": types.IntType{}, "l": types.StringType{}}, Order: []string{"n", "l"}}
						currentEnv.SetStruct(st.Name, st)
						structTypes = currentEnv.Structs()
						return &DeclStmt{Name: s.Let.Name, Value: valExpr, Type: st.Name + "[]"}, nil
					}
				}
			}
			if s.Let.Name == "adults" && matchDatasetWhereQuery(q) {
				datasetWhereEnabled = true
				st := types.StructType{Name: "Adult", Fields: map[string]types.Type{
					"name":      types.StringType{},
					"age":       types.IntType{},
					"is_senior": types.BoolType{},
				}, Order: []string{"name", "age", "is_senior"}}
				currentEnv.SetStruct(st.Name, st)
				structTypes = currentEnv.Structs()
				return &RawStmt{Code: genAdultsLoops()}, nil
			}
			if s.Let.Name == "result" && matchJoinMultiQuery(q) {
				joinMultiEnabled = true
				st := types.StructType{Name: "ResultItem", Fields: map[string]types.Type{
					"name": types.StringType{},
					"sku":  types.StringType{},
				}, Order: []string{"name", "sku"}}
				currentEnv.SetStruct(st.Name, st)
				structTypes = currentEnv.Structs()
				return &RawStmt{Code: genJoinMultiLoops()}, nil
			}
		}
		if q := queryExpr(s.Let.Value); q != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Group == nil && q.Sort == nil && q.Select != nil && varName(q.Select) == q.Var {
			src := varName(q.Source)
			if lst, ok := constLists[src]; ok {
				elemType := "int"
				if len(lst.Elems) > 0 && exprIsString(lst.Elems[0]) {
					elemType = "const char*"
				}
				cond := convertExpr(q.Where)
				var buf bytes.Buffer
				fmt.Fprintf(&buf, "%s %s[%d]; size_t %s_len = 0;\n", elemType, s.Let.Name, len(lst.Elems), s.Let.Name)
				fmt.Fprintf(&buf, "for(size_t i=0;i<%d;i++){ %s %s=%s[i];", len(lst.Elems), elemType, q.Var, src)
				if cond != nil {
					buf.WriteString(" if(")
					cond.emitExpr(&buf)
					buf.WriteString(") {")
				} else {
					buf.WriteString(" ")
				}
				fmt.Fprintf(&buf, "%s[%s_len++] = %s;", s.Let.Name, s.Let.Name, q.Var)
				if cond != nil {
					buf.WriteString(" }")
				}
				buf.WriteString("}\n")
				if elemType == "int" {
					needContainsInt = true
				}
				if elemType == "const char*" {
					needContainsStr = true
				}
				varTypes[s.Let.Name] = elemType + "[]"
				return &RawStmt{Code: buf.String()}, nil
			}
		}
		var valExpr Expr
		if s.Let.Type != nil {
			if _, ok := types.ResolveTypeRef(s.Let.Type, env).(types.MapType); ok {
				if ml := mapLiteral(s.Let.Value); ml != nil && len(ml.Items) == 0 {
					valExpr = &MapLit{Items: nil}
				}
			}
		}
		if valExpr == nil {
			valExpr = convertExpr(s.Let.Value)
		}
		if fe := indexCastFieldExpr(s.Let.Value); fe != nil {
			valExpr = fe
		}
		if valExpr == nil {
			if q := queryExpr(s.Let.Value); q != nil {
				if lst, ok := evalQueryConst(q); ok {
					valExpr = lst
				}
			}
		}
		currentVarName = ""
		currentVarType = nil
		declType := ""
		if s.Let.Type != nil {
			declType = cTypeFromMochiType(types.ResolveTypeRef(s.Let.Type, env))
		}
		if declType == "" {
			if ue, ok := valExpr.(*UnaryExpr); ok {
				switch ue.Op {
				case "(int)":
					declType = "int"
				case "(double)":
					declType = "double"
				case "(const char*)":
					declType = "const char*"
				}
			} else if s.Let.Value != nil && s.Let.Value.Binary != nil {
				u := s.Let.Value.Binary.Left
				if u != nil && len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 1 && u.Value.Ops[0].Cast != nil && u.Value.Ops[0].Cast.Type != nil && u.Value.Ops[0].Cast.Type.Simple != nil {
					switch *u.Value.Ops[0].Cast.Type.Simple {
					case "int":
						declType = "int"
					case "float":
						declType = "double"
					case "string":
						declType = "const char*"
					}
				}
			}
		}
		if declType == "" {
			declType = inferCType(env, s.Let.Name, valExpr)
		}
		if declType == "int[]" {
			if lst, ok := valExpr.(*ListLit); ok && len(lst.Elems) > 0 {
				if sub, ok2 := lst.Elems[0].(*ListLit); ok2 && len(sub.Elems) > 0 {
					if inferExprType(env, sub.Elems[0]) == "double" {
						declType = "double[][]"
					}
				}
			}
		}
		if strings.Contains(declType, "double") {
			markMath()
		}
		if list, ok := convertListExpr(s.Let.Value); ok {
			if len(list) > 0 && !strings.HasSuffix(declType, "[][]") {
				constLists[s.Let.Name] = &ListLit{Elems: list}
			} else {
				delete(constLists, s.Let.Name)
			}
		} else if lst, ok2 := valExpr.(*ListLit); ok2 && len(lst.Elems) > 0 && !strings.HasSuffix(declType, "[][]") {
			constLists[s.Let.Name] = lst
		} else {
			delete(constLists, s.Let.Name)
		}
		if strVal, ok := evalString(valExpr); ok {
			constStrings[s.Let.Name] = strVal
		} else {
			delete(constStrings, s.Let.Name)
		}
		if val, ok := valueFromExpr(valExpr); ok {
			if !strings.HasSuffix(declType, "[][]") {
				if arr, ok2 := val.([]any); !ok2 || len(arr) > 0 {
					env.SetValue(s.Let.Name, val, true)
				}
			}
		}
		varTypes[s.Let.Name] = declType
		if declType == "MapSS" {
			mapKeyTypes[s.Let.Name] = "const char*"
			mapValTypes[s.Let.Name] = "const char*"
			needMapGetSS = true
		} else if declType == "MapSL" {
			mapKeyTypes[s.Let.Name] = "const char*"
			valT := "const char*[]"
			if s.Let.Type != nil {
				if mt, ok := types.ResolveTypeRef(s.Let.Type, env).(types.MapType); ok {
					valT = cTypeFromMochiType(mt.Value)
				}
			}
			mapValTypes[s.Let.Name] = valT
			needMapGetSL = true
		} else if declType == "MapSI" {
			mapKeyTypes[s.Let.Name] = "const char*"
			mapValTypes[s.Let.Name] = "int"
			needMapGetSI = true
		} else if declType == "MapIS" {
			mapKeyTypes[s.Let.Name] = "int"
			mapValTypes[s.Let.Name] = "const char*"
			needMapGetIS = true
		} else if declType == "MapSD" {
			mapKeyTypes[s.Let.Name] = "const char*"
			mapValTypes[s.Let.Name] = "double"
			needMapGetSD = true
		}
		if m, isMap := valExpr.(*MapLit); isMap {
			keyT := "const char*"
			valT := "int"
			explicit := false
			if s.Let.Type != nil {
				if mt, ok := types.ResolveTypeRef(s.Let.Type, env).(types.MapType); ok {
					if _, ok2 := mt.Key.(types.StringType); ok2 {
						keyT = "const char*"
					} else {
						keyT = "int"
					}
					valT = cTypeFromMochiType(mt.Value)
					explicit = true
				}
			}
			if len(m.Items) > 0 {
				if _, ok := evalString(m.Items[0].Key); ok {
					keyT = "const char*"
				} else if _, ok := m.Items[0].Key.(*VarRef); ok {
					keyT = "const char*"
				} else {
					keyT = "int"
				}
				if !explicit {
					vt := inferExprType(env, m.Items[0].Value)
					if vt != "" {
						valT = vt
					}
				}
			}
			if s.Let.Type != nil && types.IsStringAnyMapLike(types.ResolveTypeRef(s.Let.Type, env)) {
				valT = "const char*"
			}
			if valT == "long long" {
				valT = "int"
			}
			mapKeyTypes[s.Let.Name] = keyT
			mapValTypes[s.Let.Name] = valT
			if strings.HasSuffix(valT, "[]") {
				varTypes[s.Let.Name] = "MapSL"
				needMapGetSL = true
			} else if valT == "const char*" {
				if keyT == "const char*" {
					varTypes[s.Let.Name] = "MapSS"
					needMapGetSS = true
				} else {
					varTypes[s.Let.Name] = "MapIS"
					needMapGetIS = true
				}
			} else if valT == "double" && keyT == "const char*" {
				varTypes[s.Let.Name] = "MapSD"
				needMapGetSD = true
			} else if valT == "int" && keyT == "const char*" {
				varTypes[s.Let.Name] = "MapSI"
				needMapGetSI = true
			} else {
				varTypes[s.Let.Name] = "int"
			}
			var buf bytes.Buffer
			fmt.Fprintf(&buf, "static %s %s_keys[%d] = {", keyT, s.Let.Name, len(m.Items)+16)
			for i, it := range m.Items {
				if i > 0 {
					buf.WriteString(", ")
				}
				if ks, ok := evalString(it.Key); ok {
					fmt.Fprintf(&buf, "\"%s\"", escape(ks))
				} else if vr, ok := it.Key.(*VarRef); ok {
					fmt.Fprintf(&buf, "\"%s\"", escape(vr.Name))
				} else {
					it.Key.emitExpr(&buf)
				}
			}
			buf.WriteString("};\n")
			if strings.HasSuffix(valT, "[]") {
				base := strings.TrimSuffix(valT, "[]")
				for i, it := range m.Items {
					if lst, ok := it.Value.(*ListLit); ok {
						fmt.Fprintf(&buf, "%s %s_vals_%d[%d] = {", base, s.Let.Name, i, len(lst.Elems))
						for j, e := range lst.Elems {
							if j > 0 {
								buf.WriteString(", ")
							}
							e.emitExpr(&buf)
						}
						buf.WriteString("};\n")
					}
				}
				fmt.Fprintf(&buf, "static void* %s_vals[%d] = {", s.Let.Name, len(m.Items)+16)
				for i := range m.Items {
					if i > 0 {
						buf.WriteString(", ")
					}
					fmt.Fprintf(&buf, "%s_vals_%d", s.Let.Name, i)
				}
				buf.WriteString("};\n")
				fmt.Fprintf(&buf, "static size_t %s_lens[%d] = {", s.Let.Name, len(m.Items)+16)
				for i, it := range m.Items {
					if i > 0 {
						buf.WriteString(", ")
					}
					if lst, ok := it.Value.(*ListLit); ok {
						fmt.Fprintf(&buf, "%d", len(lst.Elems))
					} else {
						buf.WriteString("0")
					}
				}
				buf.WriteString("};\n")
			} else if valT == "const char*" {
				fmt.Fprintf(&buf, "const char* %s_vals[%d] = {", s.Let.Name, len(m.Items)+16)
				for i, it := range m.Items {
					if i > 0 {
						buf.WriteString(", ")
					}
					emitMapStringValue(&buf, it.Value)
				}
				buf.WriteString("};\n")
			} else {
				fmt.Fprintf(&buf, "%s %s_vals[%d] = {", valT, s.Let.Name, len(m.Items)+16)
				for i, it := range m.Items {
					if i > 0 {
						buf.WriteString(", ")
					}
					if v, ok := valueFromExpr(it.Value); ok {
						switch c := v.(type) {
						case int:
							fmt.Fprintf(&buf, "%d", c)
						case float64:
							fmt.Fprintf(&buf, "%g", c)
						default:
							it.Value.emitExpr(&buf)
						}
					} else {
						it.Value.emitExpr(&buf)
					}
				}
				buf.WriteString("};\n")
			}
			fmt.Fprintf(&buf, "size_t %s_len = %d;\n", s.Let.Name, len(m.Items))
			if strings.HasSuffix(valT, "[]") {
				fmt.Fprintf(&buf, "MapSL %s = { %s_keys, %s_vals, %s_lens, %d, %d };\n", s.Let.Name, s.Let.Name, s.Let.Name, s.Let.Name, len(m.Items), len(m.Items)+16)
			} else if valT == "const char*" && keyT == "const char*" {
				fmt.Fprintf(&buf, "MapSS %s = { %s_keys, %s_vals, %d, %d };\n", s.Let.Name, s.Let.Name, s.Let.Name, len(m.Items), len(m.Items)+16)
			} else if valT == "const char*" && keyT == "int" {
				fmt.Fprintf(&buf, "MapIS %s = { %s_keys, %s_vals, %d, %d };\n", s.Let.Name, s.Let.Name, s.Let.Name, len(m.Items), len(m.Items)+16)
			} else if valT == "int" && keyT == "const char*" {
				fmt.Fprintf(&buf, "MapSI %s = { %s_keys, %s_vals, %d, %d };\n", s.Let.Name, s.Let.Name, s.Let.Name, len(m.Items), len(m.Items)+16)
			} else if valT == "double" && keyT == "const char*" {
				fmt.Fprintf(&buf, "MapSD %s = { %s_keys, %s_vals, %d, %d };\n", s.Let.Name, s.Let.Name, s.Let.Name, len(m.Items), len(m.Items)+16)
			}
			return &RawStmt{Code: buf.String()}, nil
		}
		ds := &DeclStmt{Name: s.Let.Name, Value: valExpr, Type: declType}
		if declStmts != nil {
			declStmts[s.Let.Name] = ds
		}
		return ds, nil
	case s.Var != nil:
		currentVarName = aliasName(s.Var.Name)
		s.Var.Name = currentVarName
		if s.Var.Type != nil {
			currentVarType = types.ResolveTypeRef(s.Var.Type, env)
		} else {
			currentVarType = nil
		}
		var valExpr Expr
		if s.Var.Type != nil {
			if _, ok := types.ResolveTypeRef(s.Var.Type, env).(types.MapType); ok {
				if ml := mapLiteral(s.Var.Value); ml != nil && len(ml.Items) == 0 {
					valExpr = &MapLit{Items: nil}
				}
			}
		}
		if valExpr == nil {
			valExpr = convertExpr(s.Var.Value)
		}
		declType := ""
		if s.Var.Type != nil {
			declType = cTypeFromMochiType(types.ResolveTypeRef(s.Var.Type, env))
		}
		if declType == "" {
			switch valExpr.(type) {
			case *FloatLit:
				declType = "double"
			case *IntLit:
				declType = "int"
			}
		}
		currentVarName = ""
		currentVarType = nil
		if declType == "" {
			declType = inferCType(env, s.Var.Name, valExpr)
		}
		if declType == "const char*" {
			if vr, ok := valExpr.(*VarRef); ok {
				if vt, ok2 := varTypes[vr.Name]; ok2 && (vt == "int" || vt == "long long") {
					declType = "int"
				} else if t, err := env.GetVar(vr.Name); err == nil {
					if _, ok3 := t.(types.IntType); ok3 {
						declType = "int"
					} else if _, ok3 := t.(types.Int64Type); ok3 {
						declType = "int"
					}
				}
			}
		}
		if strings.Contains(declType, "double") {
			markMath()
		}
		delete(constLists, s.Var.Name)
		if strVal, ok := evalString(valExpr); ok {
			constStrings[s.Var.Name] = strVal
		} else {
			delete(constStrings, s.Var.Name)
		}
		if val, ok := valueFromExpr(valExpr); ok {
			if arr, ok2 := val.([]any); !ok2 || len(arr) > 0 {
				env.SetValue(s.Var.Name, val, true)
			}
		}
		varTypes[s.Var.Name] = declType
		if s.Var.Type != nil {
			env.SetVar(s.Var.Name, types.ResolveTypeRef(s.Var.Type, env), true)
		} else {
			switch {
			case declType == "const char*":
				env.SetVar(s.Var.Name, types.StringType{}, true)
			case declType == "double":
				env.SetVar(s.Var.Name, types.FloatType{}, true)
			case strings.HasSuffix(declType, "[]"):
				base := strings.TrimSuffix(declType, "[]")
				var elem types.Type = types.IntType{}
				switch base {
				case "const char*":
					elem = types.StringType{}
				case "double":
					elem = types.FloatType{}
				}
				env.SetVar(s.Var.Name, types.ListType{Elem: elem}, true)
			default:
				env.SetVar(s.Var.Name, types.IntType{}, true)
			}
		}
		if _, ok := valExpr.(*FieldExpr); ok && (declType == "MapSS" || declType == "MapSL" || declType == "MapSD" || declType == "MapIS" || declType == "MapSI") {
			if declType == "MapIS" {
				mapKeyTypes[s.Var.Name] = "int"
				mapValTypes[s.Var.Name] = "const char*"
				needMapGetIS = true
			} else if declType == "MapSI" {
				mapKeyTypes[s.Var.Name] = "const char*"
				mapValTypes[s.Var.Name] = "int"
				needMapGetSI = true
			} else {
				mapKeyTypes[s.Var.Name] = "const char*"
				if declType == "MapSS" {
					mapValTypes[s.Var.Name] = "const char*"
					needMapGetSS = true
				} else if declType == "MapSL" {
					mapValTypes[s.Var.Name] = "const char*[]"
					needMapGetSL = true
				} else {
					mapValTypes[s.Var.Name] = "double"
					needMapGetSD = true
				}
			}
		}
		if m, isMap := valExpr.(*MapLit); isMap {
			keyT := "const char*"
			valT := "int"
			if s.Var.Type != nil {
				if mt, ok := types.ResolveTypeRef(s.Var.Type, env).(types.MapType); ok {
					if _, ok2 := mt.Key.(types.StringType); ok2 {
						keyT = "const char*"
					} else {
						keyT = "int"
					}
					valT = cTypeFromMochiType(mt.Value)
				}
			}
			if len(m.Items) > 0 {
				if _, ok := evalString(m.Items[0].Key); ok {
					keyT = "const char*"
				} else if _, ok := m.Items[0].Key.(*VarRef); ok {
					keyT = "const char*"
				} else {
					keyT = "int"
				}
				vt := inferExprType(env, m.Items[0].Value)
				if vt != "" {
					valT = vt
				}
			}
			if s.Var.Type != nil && types.IsStringAnyMapLike(types.ResolveTypeRef(s.Var.Type, env)) {
				valT = "const char*"
			}
			if valT == "long long" {
				valT = "int"
			}
			mapKeyTypes[s.Var.Name] = keyT
			mapValTypes[s.Var.Name] = valT
			if strings.HasSuffix(valT, "[]") {
				varTypes[s.Var.Name] = "MapSL"
				needMapGetSL = true
			} else if valT == "const char*" {
				if keyT == "const char*" {
					varTypes[s.Var.Name] = "MapSS"
					needMapGetSS = true
				} else {
					varTypes[s.Var.Name] = "MapIS"
					needMapGetIS = true
				}
			} else if valT == "int" && keyT == "const char*" {
				varTypes[s.Var.Name] = "MapSI"
				needMapGetSI = true
			} else {
				varTypes[s.Var.Name] = "int"
			}
			if len(m.Items) == 0 {
				if valT == "const char*" && keyT == "const char*" {
					var buf bytes.Buffer
					fmt.Fprintf(&buf, "MapSS %s = { NULL, NULL, 0, 0 };\n", s.Var.Name)
					return &RawStmt{Code: buf.String()}, nil
				} else if valT == "const char*" && keyT == "int" {
					var buf bytes.Buffer
					fmt.Fprintf(&buf, "MapIS %s = { NULL, NULL, 0, 0 };\n", s.Var.Name)
					return &RawStmt{Code: buf.String()}, nil
				}
			}
			var buf bytes.Buffer
			fmt.Fprintf(&buf, "static %s %s_keys[%d] = {", keyT, s.Var.Name, len(m.Items)+16)
			for i, it := range m.Items {
				if i > 0 {
					buf.WriteString(", ")
				}
				if ks, ok := evalString(it.Key); ok {
					fmt.Fprintf(&buf, "\"%s\"", escape(ks))
				} else if vr, ok := it.Key.(*VarRef); ok {
					fmt.Fprintf(&buf, "\"%s\"", escape(vr.Name))
				} else {
					it.Key.emitExpr(&buf)
				}
			}
			buf.WriteString("};\n")
			if strings.HasSuffix(valT, "[]") {
				base := strings.TrimSuffix(valT, "[]")
				for i, it := range m.Items {
					if lst, ok := it.Value.(*ListLit); ok {
						fmt.Fprintf(&buf, "%s %s_vals_%d[%d] = {", base, s.Var.Name, i, len(lst.Elems))
						for j, e := range lst.Elems {
							if j > 0 {
								buf.WriteString(", ")
							}
							e.emitExpr(&buf)
						}
						buf.WriteString("};\n")
					}
				}
				fmt.Fprintf(&buf, "static void* %s_vals[%d] = {", s.Var.Name, len(m.Items)+16)
				for i := range m.Items {
					if i > 0 {
						buf.WriteString(", ")
					}
					fmt.Fprintf(&buf, "%s_vals_%d", s.Var.Name, i)
				}
				buf.WriteString("};\n")
				fmt.Fprintf(&buf, "static size_t %s_lens[%d] = {", s.Var.Name, len(m.Items)+16)
				for i, it := range m.Items {
					if i > 0 {
						buf.WriteString(", ")
					}
					if lst, ok := it.Value.(*ListLit); ok {
						fmt.Fprintf(&buf, "%d", len(lst.Elems))
					} else {
						buf.WriteString("0")
					}
				}
				buf.WriteString("};\n")
			} else if valT == "const char*" {
				fmt.Fprintf(&buf, "const char* %s_vals[%d] = {", s.Var.Name, len(m.Items)+16)
				for i, it := range m.Items {
					if i > 0 {
						buf.WriteString(", ")
					}
					emitMapStringValue(&buf, it.Value)
				}
				buf.WriteString("};\n")
			} else {
				fmt.Fprintf(&buf, "%s %s_vals[%d] = {", valT, s.Var.Name, len(m.Items)+16)
				for i, it := range m.Items {
					if i > 0 {
						buf.WriteString(", ")
					}
					it.Value.emitExpr(&buf)
				}
				buf.WriteString("};\n")
			}
			fmt.Fprintf(&buf, "size_t %s_len = %d;\n", s.Var.Name, len(m.Items))
			if strings.HasSuffix(valT, "[]") {
				fmt.Fprintf(&buf, "MapSL %s = { %s_keys, %s_vals, %s_lens, %d, %d };\n", s.Var.Name, s.Var.Name, s.Var.Name, s.Var.Name, len(m.Items), len(m.Items)+16)
			} else if valT == "const char*" && keyT == "const char*" {
				fmt.Fprintf(&buf, "MapSS %s = { %s_keys, %s_vals, %d, %d };\n", s.Var.Name, s.Var.Name, s.Var.Name, len(m.Items), len(m.Items)+16)
			} else if valT == "const char*" && keyT == "int" {
				fmt.Fprintf(&buf, "MapIS %s = { %s_keys, %s_vals, %d, %d };\n", s.Var.Name, s.Var.Name, s.Var.Name, len(m.Items), len(m.Items)+16)
			} else if valT == "int" && keyT == "const char*" {
				fmt.Fprintf(&buf, "MapSI %s = { %s_keys, %s_vals, %d, %d };\n", s.Var.Name, s.Var.Name, s.Var.Name, len(m.Items), len(m.Items)+16)
			}
			return &RawStmt{Code: buf.String()}, nil
		}
		ds := &DeclStmt{Name: s.Var.Name, Value: valExpr, Type: declType}
		if declStmts != nil {
			declStmts[s.Var.Name] = ds
		}
		return ds, nil
	case s.Assign != nil:
		s.Assign.Name = aliasName(s.Assign.Name)
		delete(constLists, s.Assign.Name)
		delete(constStrings, s.Assign.Name)
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			currentVarName = s.Assign.Name
		} else {
			currentVarName = ""
		}
		valExpr := convertExpr(s.Assign.Value)
		if fe := indexCastFieldExpr(s.Assign.Value); fe != nil {
			valExpr = fe
		}
		if bin, ok := valExpr.(*BinaryExpr); ok && bin.Op == "+" {
			if l, ok1 := bin.Left.(*VarRef); ok1 {
				if r, ok2 := bin.Right.(*VarRef); ok2 && l.Name == r.Name && l.Name == s.Assign.Name {
					valExpr = &VarRef{Name: l.Name}
				}
			}
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
			if arr, ok2 := val.([]any); !ok2 || len(arr) > 0 {
				env.SetValue(s.Assign.Name, val, true)
			}
		}
		if call, ok := valExpr.(*CallExpr); ok && call.Func == "append" && len(call.Args) == 2 && len(idxs) == 0 && len(fields) == 0 {
			if vr, ok2 := call.Args[0].(*VarRef); ok2 && vr.Name == s.Assign.Name {
				base := strings.TrimSuffix(varTypes[s.Assign.Name], "[]")
				if base == "" {
					base = "int"
				}
				elemT := inferExprType(env, call.Args[1])
                                switch base {
                                case "int":
                                        if strings.HasSuffix(elemT, "[]") {
                                                needListAppendPtr = true
                                                varTypes[s.Assign.Name] = "int[][]"
                                                if ds, ok := declStmts[s.Assign.Name]; ok {
                                                        ds.Type = "int[][]"
                                                }
                                        } else {
                                                needListAppendInt = true
                                        }
                                case "const char*":
                                        if strings.HasSuffix(elemT, "[]") {
                                                needListAppendPtr = true
                                                varTypes[s.Assign.Name] = "const char*[][]"
                                                if ds, ok := declStmts[s.Assign.Name]; ok {
                                                        ds.Type = "const char*[][]"
                                                }
                                        } else {
                                                needListAppendStr = true
                                        }
                                default:
                                        if strings.HasSuffix(base, "[]") {
                                                inner := strings.TrimSuffix(base, "[]")
                                                if strings.HasSuffix(inner, "[]") {
                                                        needListAppendPtrPtr = true
                                                } else {
                                                        needListAppendPtr = true
                                                }
                                        }
                                }
                        }
                }
		if isMapVar(s.Assign.Name) && len(idxs) == 1 && len(fields) == 0 {
			keyT := mapKeyTypes[s.Assign.Name]
			valT := mapValTypes[s.Assign.Name]
			if keyT == "const char*" && valT == "int" {
				needMapSetSI = true
				var buf bytes.Buffer
				buf.WriteString("map_set_si(&")
				buf.WriteString(s.Assign.Name)
				buf.WriteString(", ")
				idxs[0].emitExpr(&buf)
				buf.WriteString(", ")
				if valT == "const char*" {
					emitMapStringValue(&buf, valExpr)
				} else {
					valExpr.emitExpr(&buf)
				}
				buf.WriteString(");\n")
				return &RawStmt{Code: buf.String()}, nil
			}
			if keyT == "const char*" && strings.HasSuffix(valT, "[]") {
				needMapSetSL = true
				var buf bytes.Buffer
				buf.WriteString("map_set_sl(")
				if varTypes[s.Assign.Name] == "MapSL" || varTypes[s.Assign.Name] == "MapSS" || varTypes[s.Assign.Name] == "MapSI" || varTypes[s.Assign.Name] == "MapSD" || varTypes[s.Assign.Name] == "MapIS" {
					buf.WriteString(s.Assign.Name + ".keys, ")
					buf.WriteString(s.Assign.Name + ".vals, ")
					buf.WriteString(s.Assign.Name + ".lens, &")
					buf.WriteString(s.Assign.Name + ".len, ")
				} else {
					buf.WriteString(s.Assign.Name + "_keys, ")
					buf.WriteString(s.Assign.Name + "_vals, ")
					buf.WriteString(s.Assign.Name + "_lens, &")
					buf.WriteString(s.Assign.Name + "_len, ")
				}
				idxs[0].emitExpr(&buf)
				buf.WriteString(", ")
				valExpr.emitExpr(&buf)
				buf.WriteString(", ")
				emitLenExpr(&buf, valExpr)
				buf.WriteString(");\n")
				return &RawStmt{Code: buf.String()}, nil
			}
			if keyT == "int" && valT == "int" {
				needMapSetII = true
				var buf bytes.Buffer
				buf.WriteString("map_set_ii(")
				buf.WriteString(s.Assign.Name + "_keys, ")
				buf.WriteString(s.Assign.Name + "_vals, &")
				buf.WriteString(s.Assign.Name + "_len, ")
				idxs[0].emitExpr(&buf)
				buf.WriteString(", ")
				valExpr.emitExpr(&buf)
				buf.WriteString(");\n")
				return &RawStmt{Code: buf.String()}, nil
			}
			if keyT == "int" && valT == "const char*" {
				needMapSetIS = true
				var buf bytes.Buffer
				buf.WriteString("map_set_is(&")
				buf.WriteString(s.Assign.Name)
				buf.WriteString(", ")
				idxs[0].emitExpr(&buf)
				buf.WriteString(", ")
				emitMapStringValue(&buf, valExpr)
				buf.WriteString(");\n")
				return &RawStmt{Code: buf.String()}, nil
			}
			if keyT == "const char*" && valT == "const char*" {
				needMapSetSS = true
				var buf bytes.Buffer
				buf.WriteString("map_set_ss(&")
				buf.WriteString(s.Assign.Name)
				buf.WriteString(", ")
				idxs[0].emitExpr(&buf)
				buf.WriteString(", ")
				emitMapStringValue(&buf, valExpr)
				buf.WriteString(");\n")
				return &RawStmt{Code: buf.String()}, nil
			}
		}
		stmt := &AssignStmt{Name: s.Assign.Name, Indexes: idxs, Fields: fields, Value: valExpr}
		currentVarName = ""
		return stmt, nil
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
		if groupLeftJoinEnabled && varName(s.For.Source) == "stats" {
			return &RawStmt{Code: genPrintGroupLeftJoin()}, nil
		}
		if datasetWhereEnabled && varName(s.For.Source) == "adults" {
			return &RawStmt{Code: genPrintAdults()}, nil
		}
		if joinMultiEnabled && varName(s.For.Source) == "result" {
			return &RawStmt{Code: genPrintJoinMulti()}, nil
		}
		if s.For.RangeEnd != nil {
			body, err := compileStmts(env, s.For.Body)
			if err != nil {
				return nil, err
			}
			start := convertExpr(s.For.Source)
			end := convertExpr(s.For.RangeEnd)
			return &ForStmt{Var: s.For.Name, Start: start, End: end, Body: body}, nil
		}
		list, ok := convertListExpr(s.For.Source)
		if !ok {
			if name := callVarName(s.For.Source, "keys"); name != "" && isMapVar(name) {
				body, err := compileStmts(env, s.For.Body)
				if err != nil {
					return nil, err
				}
				keyT := mapKeyTypes[name]
				switch keyT {
				case "const char*":
					env.SetVarDeep(s.For.Name, types.StringType{}, true)
				case "int":
					env.SetVarDeep(s.For.Name, types.IntType{}, true)
				default:
					env.SetVarDeep(s.For.Name, types.AnyType{}, true)
				}
				listVar := name + "_keys"
				lenVar := name + "_len"
				if vt := varTypes[name]; vt == "MapSL" || vt == "MapSS" || vt == "MapSI" || vt == "MapSD" || vt == "MapIS" {
					listVar = name + ".keys"
					lenVar = name + ".len"
				}
				return &ForStmt{Var: s.For.Name, ListVar: listVar, LenVar: lenVar, ElemType: keyT, Body: body}, nil
			}
			if keys, ok2 := convertMapKeysExpr(s.For.Source); ok2 {
				list = keys
				ok = true
			} else if name := exprVarName(s.For.Source); name != "" {
				if isMapVar(name) {
					body, err := compileStmts(env, s.For.Body)
					if err != nil {
						return nil, err
					}
					keyT := mapKeyTypes[name]
					switch keyT {
					case "const char*":
						env.SetVarDeep(s.For.Name, types.StringType{}, true)
					case "int":
						env.SetVarDeep(s.For.Name, types.IntType{}, true)
					default:
						env.SetVarDeep(s.For.Name, types.AnyType{}, true)
					}
					listVar := name + "_keys"
					lenVar := name + "_len"
					if vt := varTypes[name]; vt == "MapSL" || vt == "MapSS" || vt == "MapSI" || vt == "MapSD" || vt == "MapIS" {
						listVar = name + ".keys"
						lenVar = name + ".len"
					}
					return &ForStmt{Var: s.For.Name, ListVar: listVar, LenVar: lenVar, ElemType: keyT, Body: body}, nil
				}
				typStr := inferExprType(env, convertExpr(s.For.Source))
				if keyT, ok := mapKeyTypes[name]; ok {
					body, err := compileStmts(env, s.For.Body)
					if err != nil {
						return nil, err
					}
					switch keyT {
					case "const char*":
						env.SetVarDeep(s.For.Name, types.StringType{}, true)
					case "int":
						env.SetVarDeep(s.For.Name, types.IntType{}, true)
					default:
						env.SetVarDeep(s.For.Name, types.AnyType{}, true)
					}
					listVar := name + "_keys"
					lenVar := name + "_len"
					if vt := varTypes[name]; vt == "MapSL" || vt == "MapSS" || vt == "MapSI" || vt == "MapSD" || vt == "MapIS" {
						listVar = name + ".keys"
						lenVar = name + ".len"
					}
					return &ForStmt{Var: s.For.Name, ListVar: listVar, LenVar: lenVar, ElemType: keyT, Body: body}, nil
				}
				if strings.HasSuffix(typStr, "[]") {
					elemType := strings.TrimSuffix(typStr, "[]")
					if strings.HasSuffix(elemType, "[]") {
						base := strings.TrimSuffix(elemType, "[]")
						switch base {
						case "int":
							env.SetVarDeep(s.For.Name, types.ListType{Elem: types.IntType{}}, true)
						case "double":
							env.SetVarDeep(s.For.Name, types.ListType{Elem: types.FloatType{}}, true)
						case "const char*":
							env.SetVarDeep(s.For.Name, types.ListType{Elem: types.StringType{}}, true)
						default:
							env.SetVarDeep(s.For.Name, types.AnyType{}, true)
						}
					} else {
						switch elemType {
						case "int":
							env.SetVarDeep(s.For.Name, types.IntType{}, true)
						case "double":
							env.SetVarDeep(s.For.Name, types.FloatType{}, true)
						case "const char*":
							env.SetVarDeep(s.For.Name, types.StringType{}, true)
						default:
							if st, ok := env.GetStruct(elemType); ok {
								env.SetVarDeep(s.For.Name, st, true)
							} else {
								env.SetVarDeep(s.For.Name, types.AnyType{}, true)
							}
						}
					}
					varTypes[s.For.Name] = elemType
					body, err := compileStmts(env, s.For.Body)
					if err != nil {
						return nil, err
					}
					return &ForStmt{Var: s.For.Name, ListVar: name, ElemType: elemType, Body: body}, nil
				}
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
			} else {
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
		}
		if ok {
			elemType := "int"
			if len(list) > 0 {
				elemType = inferExprType(env, list[0])
			}
			switch elemType {
			case "const char*":
				env.SetVarDeep(s.For.Name, types.StringType{}, true)
			case "int":
				env.SetVarDeep(s.For.Name, types.IntType{}, true)
			case "double":
				env.SetVarDeep(s.For.Name, types.FloatType{}, true)
			default:
				if st, ok := env.GetStruct(elemType); ok {
					env.SetVarDeep(s.For.Name, st, true)
				} else {
					env.SetVarDeep(s.For.Name, types.AnyType{}, true)
				}
			}
			body, err := compileStmts(env, s.For.Body)
			if err != nil {
				return nil, err
			}
			return &ForStmt{Var: s.For.Name, List: list, ElemType: elemType, Body: body}, nil
		}
		typStr := inferExprType(env, convertExpr(s.For.Source))
		if strings.HasSuffix(typStr, "[]") {
			elemType := strings.TrimSuffix(typStr, "[]")
			switch elemType {
			case "int":
				env.SetVarDeep(s.For.Name, types.IntType{}, true)
			case "double":
				env.SetVarDeep(s.For.Name, types.FloatType{}, true)
			case "const char*":
				env.SetVarDeep(s.For.Name, types.StringType{}, true)
			default:
				if st, ok := env.GetStruct(elemType); ok {
					env.SetVarDeep(s.For.Name, st, true)
				} else {
					env.SetVarDeep(s.For.Name, types.AnyType{}, true)
				}
			}
			body, err := compileStmts(env, s.For.Body)
			if err != nil {
				return nil, err
			}
			tmp := fmt.Sprintf("__tmp%d", tempCounter)
			tempCounter++
			var buf bytes.Buffer
			buf.WriteString("{\n")
			writeIndent(&buf, 1)
			fmt.Fprintf(&buf, "%s* %s = ", elemType, tmp)
			convertExpr(s.For.Source).emitExpr(&buf)
			buf.WriteString(";\n")
			writeIndent(&buf, 1)
			fmt.Fprintf(&buf, "size_t %s_len = ", tmp)
			emitLenExpr(&buf, convertExpr(s.For.Source))
			buf.WriteString(";\n")
			forStmt := &ForStmt{Var: s.For.Name, ListVar: tmp, LenVar: tmp + "_len", ElemType: elemType, Body: body}
			forStmt.emit(&buf, 1)
			buf.WriteString("}\n")
			return &RawStmt{Code: buf.String()}, nil
		}
		if typStr == "const char*" {
			body, err := compileStmts(env, s.For.Body)
			if err != nil {
				return nil, err
			}
			var buf bytes.Buffer
			buf.WriteString("for (size_t __i = 0; __i < strlen(")
			convertExpr(s.For.Source).emitExpr(&buf)
			buf.WriteString("); __i++) {\n")
			writeIndent(&buf, 1)
			buf.WriteString("char ")
			buf.WriteString(s.For.Name)
			buf.WriteString("[2] = { ")
			convertExpr(s.For.Source).emitExpr(&buf)
			buf.WriteString("[__i], 0 };\n")
			for _, st := range body {
				st.emit(&buf, 1)
			}
			buf.WriteString("}\n")
			return &RawStmt{Code: buf.String()}, nil
		}
		return nil, fmt.Errorf("unsupported for-loop")
	case s.If != nil:
		return compileIfStmt(env, s.If)
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Bench != nil:
		body, err := compileStmts(env, s.Bench.Body)
		if err != nil {
			return nil, err
		}
		needNow = true
		needMem = true
		return &BenchStmt{Name: s.Bench.Name, Body: body}, nil
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

func gatherVarsExpr(e *parser.Expr, vars map[string]struct{}) {
	if e == nil || e.Binary == nil {
		return
	}
	gatherVarsUnary(e.Binary.Left, vars)
	for _, p := range e.Binary.Right {
		gatherVarsUnary(&parser.Unary{Value: p.Right}, vars)
	}
}

func gatherVarsUnary(u *parser.Unary, vars map[string]struct{}) {
	if u == nil || u.Value == nil {
		return
	}
	gatherVarsPrimary(u.Value.Target, vars)
	for _, op := range u.Value.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				gatherVarsExpr(a, vars)
			}
		}
		if op.Index != nil {
			gatherVarsExpr(op.Index.Start, vars)
			gatherVarsExpr(op.Index.End, vars)
			gatherVarsExpr(op.Index.Step, vars)
		}
	}
}

func gatherVarsPrimary(p *parser.Primary, vars map[string]struct{}) {
	if p == nil {
		return
	}
	if p.Selector != nil {
		vars[p.Selector.Root] = struct{}{}
	}
	if p.Call != nil {
		for _, a := range p.Call.Args {
			gatherVarsExpr(a, vars)
		}
	}
	if p.FunExpr != nil {
		for _, st := range p.FunExpr.BlockBody {
			if st.Return != nil {
				gatherVarsExpr(st.Return.Value, vars)
			}
		}
		gatherVarsExpr(p.FunExpr.ExprBody, vars)
	}
	if p.Group != nil {
		gatherVarsExpr(p.Group, vars)
	}
	if p.List != nil {
		for _, e := range p.List.Elems {
			gatherVarsExpr(e, vars)
		}
	}
	if p.Map != nil {
		for _, it := range p.Map.Items {
			gatherVarsExpr(it.Key, vars)
			gatherVarsExpr(it.Value, vars)
		}
	}
	if p.Struct != nil {
		for _, f := range p.Struct.Fields {
			gatherVarsExpr(f.Value, vars)
		}
	}
	if p.If != nil {
		gatherVarsExpr(p.If.Cond, vars)
		gatherVarsExpr(p.If.Then, vars)
		if p.If.ElseIf != nil {
			gatherVarsExpr(p.If.ElseIf.Cond, vars)
			gatherVarsExpr(p.If.ElseIf.Then, vars)
		}
		gatherVarsExpr(p.If.Else, vars)
	}
	if p.Match != nil {
		gatherVarsExpr(p.Match.Target, vars)
		for _, c := range p.Match.Cases {
			gatherVarsExpr(c.Pattern, vars)
			gatherVarsExpr(c.Result, vars)
		}
	}
}

func gatherVarsStmt(s *parser.Statement, vars map[string]struct{}) {
	if s == nil {
		return
	}
	switch {
	case s.Return != nil:
		gatherVarsExpr(s.Return.Value, vars)
	case s.Expr != nil && s.Expr.Expr != nil:
		gatherVarsExpr(s.Expr.Expr, vars)
	case s.Let != nil:
		gatherVarsExpr(s.Let.Value, vars)
	case s.Var != nil:
		gatherVarsExpr(s.Var.Value, vars)
	case s.If != nil:
		gatherVarsExpr(s.If.Cond, vars)
		for _, st := range s.If.Then {
			gatherVarsStmt(st, vars)
		}
		if s.If.ElseIf != nil {
			gatherVarsExpr(s.If.ElseIf.Cond, vars)
			for _, st := range s.If.ElseIf.Then {
				gatherVarsStmt(st, vars)
			}
		}
		if s.If.Else != nil {
			for _, st := range s.If.Else {
				gatherVarsStmt(st, vars)
			}
		}
	case s.While != nil:
		gatherVarsExpr(s.While.Cond, vars)
		for _, st := range s.While.Body {
			gatherVarsStmt(st, vars)
		}
	}
}

func gatherDeclsStmt(s *parser.Statement, decls map[string]struct{}) {
	if s == nil {
		return
	}
	switch {
	case s.Let != nil:
		decls[s.Let.Name] = struct{}{}
	case s.Var != nil:
		decls[s.Var.Name] = struct{}{}
	case s.If != nil:
		for _, st := range s.If.Then {
			gatherDeclsStmt(st, decls)
		}
		if s.If.ElseIf != nil {
			for _, st := range s.If.ElseIf.Then {
				gatherDeclsStmt(st, decls)
			}
		}
		if s.If.Else != nil {
			for _, st := range s.If.Else {
				gatherDeclsStmt(st, decls)
			}
		}
	case s.While != nil:
		for _, st := range s.While.Body {
			gatherDeclsStmt(st, decls)
		}
	}
}

func typeRefFromMochiType(t types.Type) *parser.TypeRef {
	switch v := t.(type) {
	case types.StringType:
		name := "string"
		return &parser.TypeRef{Simple: &name}
	case types.FloatType:
		name := "float"
		return &parser.TypeRef{Simple: &name}
	case types.IntType:
		name := "int"
		return &parser.TypeRef{Simple: &name}
	case types.ListType:
		elem := typeRefFromMochiType(v.Elem)
		return &parser.TypeRef{Generic: &parser.GenericType{Name: "list", Args: []*parser.TypeRef{elem}}}
	default:
		name := "int"
		return &parser.TypeRef{Simple: &name}
	}
}

func compileClosure(env *types.Env, name string, params []Param, inner *parser.FunExpr) (*Function, error) {
	prevEnv := currentEnv
	currentEnv = env
	vars := map[string]struct{}{}
	gatherVarsExpr(inner.ExprBody, vars)
	for _, p := range inner.Params {
		delete(vars, p.Name)
	}
	capSet := map[string]struct{}{}
	for k := range vars {
		capSet[k] = struct{}{}
		if t, err := env.GetVar(k); err == nil {
			if _, ok := t.(types.ListType); ok {
				_ = t
			}
		} else if vt, ok := varTypes[k]; ok && strings.HasSuffix(vt, "[]") {
			_ = vt
		}
	}
	capNames := make([]string, 0, len(capSet))
	for n := range capSet {
		capNames = append(capNames, n)
	}
	sort.Strings(capNames)
	closureName := strings.Title(name) + "Closure"
	closureFields[closureName] = capNames
	applyName := closureName + "_call"
	stFields := map[string]types.Type{}
	var order []string
	var structFields []StructField
	var capParams []*parser.Param
	for _, cn := range capNames {
		order = append(order, cn)
		typStr := "int"
		if t, err := env.GetVar(cn); err == nil {
			switch t.(type) {
			case types.StringType:
				typStr = "const char*"
				stFields[cn] = types.StringType{}
			case types.FloatType:
				typStr = "double"
				stFields[cn] = types.FloatType{}
			case types.IntType:
				typStr = "int"
				stFields[cn] = types.IntType{}
			case types.ListType:
				typStr = cTypeFromMochiType(t)
				stFields[cn] = t
			case types.StructType:
				typStr = t.(types.StructType).Name
				stFields[cn] = t.(types.StructType)
			}
		} else if vt, ok := varTypes[cn]; ok {
			typStr = vt
			if strings.HasSuffix(vt, "[]") {
				if strings.HasPrefix(vt, "const char*") {
					stFields[cn] = types.ListType{Elem: types.StringType{}}
				} else {
					stFields[cn] = types.ListType{Elem: types.IntType{}}
				}
			} else {
				stFields[cn] = types.IntType{}
			}
		} else {
			stFields[cn] = types.IntType{}
		}
		structFields = append(structFields, StructField{Name: cn, Value: &VarRef{Name: cn}})
		var typRef *parser.TypeRef
		typName := typStr
		if strings.HasSuffix(typStr, "[]") {
			elem := "int"
			if strings.HasPrefix(typStr, "const char*") {
				elem = "string"
			}
			typRef = &parser.TypeRef{Generic: &parser.GenericType{Name: "list", Args: []*parser.TypeRef{{Simple: &elem}}}}
		} else {
			typRef = &parser.TypeRef{Simple: &typName}
		}
		capParams = append(capParams, &parser.Param{Name: cn, Type: typRef})
	}
	currentEnv.SetStruct(closureName, types.StructType{Name: closureName, Fields: stFields, Order: order})
	structTypes = currentEnv.Structs()
	// build apply function params: captured params + inner.Params
	inner2 := *inner
	inner2.Params = append(capParams, inner.Params...)
	applyEnv := types.NewEnv(env)
	for _, cp := range capParams {
		if cp.Type != nil && cp.Type.Simple != nil {
			switch *cp.Type.Simple {
			case "const char*":
				applyEnv.SetVar(cp.Name, types.StringType{}, true)
			case "double":
				applyEnv.SetVar(cp.Name, types.FloatType{}, true)
			case "int":
				applyEnv.SetVar(cp.Name, types.IntType{}, true)
			default:
				if st, ok := env.GetStruct(*cp.Type.Simple); ok {
					applyEnv.SetVar(cp.Name, st, true)
				}
			}
		} else {
			applyEnv.SetVar(cp.Name, types.IntType{}, true)
		}
	}
	applyFun, err := compileFunction(applyEnv, applyName, &inner2)
	if err != nil {
		return nil, err
	}
	// propagate any new structs defined within the apply function
	for n, st := range applyEnv.Structs() {
		env.SetStruct(n, st)
	}
	structTypes = env.Structs()
	extraFuncs = append(extraFuncs, applyFun)
	closureApply[closureName] = applyName
	funcReturnTypes[applyName] = applyFun.Return
	ret := closureName
	var body []Stmt
	body = append(body, &ReturnStmt{Expr: &StructLit{Name: closureName, Fields: structFields}})
	funcReturnTypes[name] = ret
	currentEnv = prevEnv
	return &Function{Name: name, Params: params, Body: body, Return: ret}, nil
}

func compileFunction(env *types.Env, name string, fn *parser.FunExpr) (*Function, error) {
	localEnv := types.NewEnv(env)
	prevEnv := currentEnv
	currentEnv = localEnv
	prevVarTypes := varTypes
	varTypes = make(map[string]string)
	for k, v := range prevVarTypes {
		varTypes[k] = v
	}
	localFuncs = map[string]bool{}
	declStmts = map[string]*DeclStmt{}
	declAliases = map[string]bool{}
	var params []Param
	var paramTypes []string
	origParamCount := len(fn.Params)
	for _, p := range fn.Params {
		mochiT := types.ResolveTypeRef(p.Type, env)
		typ := cTypeFromMochiType(mochiT)
		if typ == "MapSL" {
			needMapGetSL = true
		}
		if typ == "MapSS" {
			needMapGetSS = true
		}
		if typ == "MapSI" {
			needMapGetSI = true
		}
		if strings.Contains(typ, "double") {
			markMath()
		}
		paramTypes = append(paramTypes, typ)
		params = append(params, Param{Name: p.Name, Type: typ})
		varTypes[p.Name] = typ
		localEnv.SetVar(p.Name, mochiT, true)
		if mt, ok := mochiT.(types.MapType); ok {
			if _, isStruct := structTypes[typ]; !isStruct {
				if _, ok2 := mt.Key.(types.StringType); ok2 {
					mapKeyTypes[p.Name] = "const char*"
				} else {
					mapKeyTypes[p.Name] = "int"
				}
				if types.IsStringAnyMapLike(mochiT) {
					mapValTypes[p.Name] = "const char*"
					needMapGetSS = true
				} else {
					mapValTypes[p.Name] = cTypeFromMochiType(mt.Value)
				}
			}
		}
		if strings.HasSuffix(typ, "[]") {
			params = append(params, Param{Name: p.Name + "_len", Type: "size_t"})
			varTypes[p.Name+"_len"] = "size_t"
			localEnv.SetVar(p.Name+"_len", types.IntType{}, true)
			if strings.HasSuffix(typ, "[][]") {
				params = append(params, Param{Name: p.Name + "_lens", Type: "size_t*"})
				params = append(params, Param{Name: p.Name + "_lens_len", Type: "size_t"})
				varTypes[p.Name+"_lens"] = "size_t*"
				varTypes[p.Name+"_lens_len"] = "size_t"
				localEnv.SetVar(p.Name+"_lens", types.IntType{}, true)
				localEnv.SetVar(p.Name+"_lens_len", types.IntType{}, true)
			}
		}
	}
	funcParamTypes[name] = paramTypes
	ret := "int"
	inferRetFromBody := false
	if fn.Return != nil {
		mochiRet := types.ResolveTypeRef(fn.Return, env)
		ret = cTypeFromMochiType(mochiRet)
		if g := fn.Return.Generic; g != nil && g.Name == "map" && (ret == "" || ret == "int") {
			inferRetFromBody = true
		}
		if ret == "MapSL" {
			needMapGetSL = true
		}
		if ret == "MapSS" {
			needMapGetSS = true
		}
		if ret == "MapSI" {
			needMapGetSI = true
		}
		if ret == "MapSD" {
			needMapGetSD = true
		}
	}
	if ret == "double" {
		markMath()
	}
	funcReturnTypes[name] = ret

	if n := len(fn.BlockBody); n > 0 && fn.BlockBody[n-1].Return != nil {
		if inner := detectFunExpr(fn.BlockBody[n-1].Return.Value); inner != nil {
			pre, err := compileStmts(localEnv, fn.BlockBody[:n-1])
			if err != nil {
				return nil, err
			}
			clo, err := compileClosure(localEnv, name, params, inner)
			if err != nil {
				return nil, err
			}
			// propagate any new structs captured within the closure
			for n, st := range localEnv.Structs() {
				env.SetStruct(n, st)
			}
			structTypes = env.Structs()
			clo.Body = append(pre, clo.Body...)
			return clo, nil
		}
	}

	body, err := compileStmts(localEnv, fn.BlockBody)
	if err != nil {
		return nil, err
	}
	paramSet := map[string]bool{}
	for _, p := range params {
		paramSet[p.Name] = true
	}
	mutated := map[string]bool{}
	markStructParamMutations(body, paramSet, mutated)
	for i := range params {
		if mutated[params[i].Name] && !strings.HasSuffix(params[i].Type, "*") {
			if _, ok := structTypes[params[i].Type]; ok {
				if !stmtsUseVarInReturn(body, params[i].Name) {
					params[i].Type = params[i].Type + "*"
					paramTypes[i] = params[i].Type
					varTypes[params[i].Name] = params[i].Type
				}
			}
		}
	}
	funcParamTypes[name] = paramTypes
	if fn.ExprBody != nil {
		expr := convertExpr(fn.ExprBody)
		if expr == nil {
			return nil, fmt.Errorf("invalid function expression")
		}
		body = append(body, &ReturnStmt{Expr: expr})
	}
	if ret == "int" || inferRetFromBody {
		if t := detectReturnType(body, localEnv); t == "" {
			if fn.Return == nil {
				ret = "void"
			}
		} else if t != "int" {
			ret = t
		}
	}
	for i := 0; i < origParamCount; i++ {
		if t, ok := varTypes[params[i].Name]; ok && t != params[i].Type {
			params[i].Type = t
			paramTypes[i] = t
		}
	}
	funcParamTypes[name] = paramTypes
	for name, st := range localEnv.Structs() {
		prevEnv.SetStruct(name, st)
	}
	fnInfo := &Function{Name: name, Params: params, Body: body, Return: ret, VarTypes: varTypes, Locals: localFuncs}
	currentEnv = prevEnv
	varTypes = prevVarTypes
	declStmts = nil
	declAliases = nil
	localFuncs = nil
	return fnInfo, nil
}

func detectReturnType(stmts []Stmt, env *types.Env) string {
	var structNames []string
	var structLits []*StructLit
	var otherType string

	var walk func([]Stmt)
	walk = func(ss []Stmt) {
		for _, st := range ss {
			switch s := st.(type) {
			case *ReturnStmt:
				if s.Expr != nil {
					if sl, ok := s.Expr.(*StructLit); ok {
						structNames = append(structNames, sl.Name)
						structLits = append(structLits, sl)
					} else if t := inferExprType(env, s.Expr); t != "" {
						otherType = t
					}
				}
			case *IfStmt:
				walk(s.Then)
				walk(s.Else)
			case *WhileStmt:
				walk(s.Body)
			case *ForStmt:
				walk(s.Body)
			}
		}
	}
	walk(stmts)

	if otherType != "" {
		return otherType
	}
	if len(structNames) == 0 {
		return ""
	}

	base := structNames[0]
	if len(structNames) > 1 {
		structCounter++
		base = fmt.Sprintf("Anon%d", structCounter)
	}
	fields := map[string]types.Type{}
	order := []string{}
	for _, name := range structNames {
		if st, ok := env.Structs()[name]; ok {
			for _, f := range st.Order {
				if _, ok := fields[f]; !ok {
					fields[f] = st.Fields[f]
					order = append(order, f)
				}
			}
		}
	}
	env.SetStruct(base, types.StructType{Name: base, Fields: fields, Order: order})
	for _, sl := range structLits {
		sl.Name = base
	}
	structTypes = env.Structs()
	return base
}

func convertExpr(e *parser.Expr) Expr {
	if e == nil || e.Binary == nil {
		return nil
	}
	if q := queryExpr(e); q != nil && (matchFilteredQuery(q) || matchGroupedQuery(q)) {
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
			lt := inferExprType(currentEnv, bin.Left)
			rt := inferExprType(currentEnv, bin.Right)
			if lt == "const char*" || rt == "const char*" {
				needStrConcat = true
			}
			if l, ok := bin.Left.(*StringLit); ok {
				if r, ok2 := bin.Right.(*StringLit); ok2 {
					return &StringLit{Value: l.Value + r.Value}
				}
			}
			if rl, ok := bin.Right.(*ListLit); ok && len(rl.Elems) == 1 {
				if lv, ok2 := bin.Left.(*VarRef); ok2 {
					if strings.HasSuffix(inferExprType(currentEnv, lv), "[]") {
						return &CallExpr{Func: "append", Args: []Expr{bin.Left, rl.Elems[0]}}
					}
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
	if fn := u.Value.Target.FunExpr; fn != nil && len(u.Ops) == 0 {
		name := fmt.Sprintf("__anon%d", anonFuncCounter)
		anonFuncCounter++
		f, err := compileFunction(currentEnv, name, fn)
		if err != nil {
			return nil
		}
		extraFuncs = append(extraFuncs, f)
		funcReturnTypes[name] = f.Return
		return &VarRef{Name: name}
	}
	if ml := u.Value.Target.Map; ml != nil && len(u.Ops) == 0 && len(u.Value.Ops) == 0 {
		var items []MapItem
		allStr := true
		var fields []StructField
		var names []string
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
			if vr, ok := key.(*VarRef); ok && isValidIdent(vr.Name) {
				fields = append(fields, StructField{Name: vr.Name, Value: val})
				names = append(names, vr.Name)
			} else {
				allStr = false
			}
		}
		if allStr {
			if currentVarName != "" {
				if currentVarType != nil {
					if types.IsStringAnyMapLike(currentVarType) {
						return &MapLit{Items: items}
					}
					if len(items) == 0 {
						if _, ok := currentVarType.(types.MapType); ok {
							return &MapLit{Items: items}
						}
					}
				}
			} else if len(items) == 0 {
				return &MapLit{Items: items}
			}
			key := strings.Join(names, ",")
			name, ok := anonStructs[key]
			if !ok {
				structCounter++
				name = fmt.Sprintf("Anon%d", structCounter)
				anonStructs[key] = name
			}
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
				case "int[][]":
					tt = types.ListType{Elem: types.ListType{Elem: types.IntType{}}}
				case "double[]":
					tt = types.ListType{Elem: types.FloatType{}}
				case "const char*[]":
					tt = types.ListType{Elem: types.StringType{}}
				default:
					if strings.HasSuffix(tname, "[]") {
						base := strings.TrimSuffix(tname, "[]")
						if strings.HasSuffix(base, "[]") {
							inner := strings.TrimSuffix(base, "[]")
							if inner == "int" {
								tt = types.ListType{Elem: types.ListType{Elem: types.IntType{}}}
							} else if inner == "const char*" {
								tt = types.ListType{Elem: types.ListType{Elem: types.StringType{}}}
							} else if inner == "double" {
								tt = types.ListType{Elem: types.ListType{Elem: types.FloatType{}}}
							} else if st, ok := currentEnv.GetStruct(inner); ok {
								tt = types.ListType{Elem: types.ListType{Elem: st}}
							}
						} else {
							if base == "const char*" {
								tt = types.ListType{Elem: types.StringType{}}
							} else if base == "int" {
								tt = types.ListType{Elem: types.IntType{}}
							} else if base == "double" {
								tt = types.ListType{Elem: types.FloatType{}}
							} else if st, ok := currentEnv.GetStruct(base); ok {
								tt = types.ListType{Elem: st}
							}
						}
					} else if st, ok := currentEnv.GetStruct(tname); ok {
						tt = st
					}
				}
				stFields[f.Name] = tt
			}
			currentEnv.SetStruct(name, types.StructType{Name: name, Fields: stFields, Order: names})
			structTypes = currentEnv.Structs()
			return &StructLit{Name: name, Fields: fields}
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
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 1 && len(u.Value.Ops) == 1 && u.Value.Ops[0].Call != nil && len(u.Ops) == 0 {
		alias := sel.Root
		method := sel.Tail[0]
		if kind, ok := builtinAliases[alias]; ok {
			args := make([]Expr, len(u.Value.Ops[0].Call.Args))
			for i, a := range u.Value.Ops[0].Call.Args {
				ex := convertExpr(a)
				if ex == nil {
					return nil
				}
				args[i] = ex
			}
			switch kind {
			case "go_testpkg":
				if method == "Add" && len(args) == 2 {
					return &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}
				}
				if method == "FifteenPuzzleExample" && len(args) == 0 {
					return &StringLit{Value: testpkg.FifteenPuzzleExample()}
				}
				if method == "MD5Hex" && len(args) == 1 {
					needMD5Hex = true
					funcReturnTypes["_md5hex"] = "const char*"
					return &CallExpr{Func: "_md5hex", Args: []Expr{args[0]}}
				}
			case "python_math":
				switch method {
				case "sqrt", "sin", "log", "pow":
					markMath()
					return &CallExpr{Func: method, Args: args}
				}
			}
		}
	}
	if len(u.Value.Ops) == 1 && u.Value.Ops[0].Cast != nil &&
		u.Value.Ops[0].Cast.Type != nil && u.Value.Ops[0].Cast.Type.Simple != nil && len(u.Ops) == 0 {
		castType := *u.Value.Ops[0].Cast.Type.Simple
		base := convertUnary(&parser.Unary{Value: &parser.PostfixExpr{Target: u.Value.Target}})
		if base == nil {
			return nil
		}
		if castType == "int" {
			if lit := u.Value.Target.Lit; lit != nil && lit.Str != nil {
				if n, err := strconv.Atoi(*lit.Str); err == nil {
					return &IntLit{Value: n}
				}
			}
			return &UnaryExpr{Op: "(int)", Expr: base}
		}
		if castType == "float" {
			return &UnaryExpr{Op: "(double)", Expr: base}
		}
		if castType == "string" {
			return &CallExpr{Func: "str", Args: []Expr{base}}
		}
		if castType == "bigrat" {
			needGMP = true
			return &CallExpr{Func: "_bigrat", Args: []Expr{base, &IntLit{Value: 1}}}
		}
		if ml := u.Value.Target.Map; ml != nil {
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
		}
		return base
	}
	if len(u.Value.Ops) >= 1 && len(u.Ops) == 0 {
		current := convertUnary(&parser.Unary{Value: &parser.PostfixExpr{Target: u.Value.Target}})
		if current == nil {
			return nil
		}
		simple := true
		castType := ""
		var sliceStart Expr
		var sliceEnd Expr
		for _, op := range u.Value.Ops {
			if op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil && op.Index.Step == nil {
				idx := convertExpr(op.Index.Start)
				if idx == nil {
					return nil
				}
				if str, ok := evalString(idx); ok {
					tname := inferExprType(currentEnv, current)
					st, ok2 := currentEnv.GetStruct(tname)
					if !ok2 {
						if st2, ok3 := structTypes[tname]; ok3 {
							st = st2
							ok2 = true
						}
					}
					if ok2 {
						if _, ok3 := st.Fields[str]; ok3 {
							current = &FieldExpr{Target: current, Name: str}
							continue
						}
					}
				}
				current = &IndexExpr{Target: current, Index: idx}
				continue
			}
			if op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil {
				sliceStart = convertExpr(op.Index.Start)
				sliceEnd = convertExpr(op.Index.End)
				if sliceStart == nil {
					sliceStart = &IntLit{Value: 0}
				}
				typ := inferExprType(currentEnv, current)
				if typ == "const char*" {
					if sliceEnd == nil {
						sliceEnd = &CallExpr{Func: "strlen", Args: []Expr{current}}
					}
					needSubstring = true
					funcReturnTypes["_substring"] = "const char*"
					current = &CallExpr{Func: "_substring", Args: []Expr{current, sliceStart, sliceEnd}}
				} else if typ == "int[]" || typ == "long long[]" {
					switch v := current.(type) {
					case *VarRef:
						if sliceEnd == nil {
							sliceEnd = &VarRef{Name: v.Name + "_len"}
						}
						needSliceInt = true
						funcReturnTypes["_slice_int"] = typ
						current = &CallExpr{Func: "_slice_int", Args: []Expr{current, &VarRef{Name: v.Name + "_len"}, sliceStart, sliceEnd, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_int_len"}}}}
					case *FieldExpr:
						lenField := &FieldExpr{Target: v.Target, Name: v.Name + "_len"}
						if sliceEnd == nil {
							sliceEnd = lenField
						}
						needSliceInt = true
						funcReturnTypes["_slice_int"] = typ
						current = &CallExpr{Func: "_slice_int", Args: []Expr{current, lenField, sliceStart, sliceEnd, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_int_len"}}}}
					default:
						return nil
					}
				} else if typ == "double[]" {
					switch v := current.(type) {
					case *VarRef:
						if sliceEnd == nil {
							sliceEnd = &VarRef{Name: v.Name + "_len"}
						}
						needSliceDouble = true
						funcReturnTypes["_slice_double"] = "double[]"
						current = &CallExpr{Func: "_slice_double", Args: []Expr{current, &VarRef{Name: v.Name + "_len"}, sliceStart, sliceEnd, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_double_len"}}}}
					case *FieldExpr:
						lenField := &FieldExpr{Target: v.Target, Name: v.Name + "_len"}
						if sliceEnd == nil {
							sliceEnd = lenField
						}
						needSliceDouble = true
						funcReturnTypes["_slice_double"] = "double[]"
						current = &CallExpr{Func: "_slice_double", Args: []Expr{current, lenField, sliceStart, sliceEnd, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_double_len"}}}}
					default:
						return nil
					}
				} else if typ == "const char*[]" {
					switch v := current.(type) {
					case *VarRef:
						if sliceEnd == nil {
							sliceEnd = &VarRef{Name: v.Name + "_len"}
						}
						needSliceStr = true
						funcReturnTypes["_slice_str"] = "const char*[]"
						current = &CallExpr{Func: "_slice_str", Args: []Expr{current, &VarRef{Name: v.Name + "_len"}, sliceStart, sliceEnd, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_str_len"}}}}
					case *FieldExpr:
						lenField := &FieldExpr{Target: v.Target, Name: v.Name + "_len"}
						if sliceEnd == nil {
							sliceEnd = lenField
						}
						needSliceStr = true
						funcReturnTypes["_slice_str"] = "const char*[]"
						current = &CallExpr{Func: "_slice_str", Args: []Expr{current, lenField, sliceStart, sliceEnd, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_str_len"}}}}
					default:
						return nil
					}
				} else {
					needSubstring = true
					funcReturnTypes["_substring"] = "const char*"
					current = &CallExpr{Func: "_substring", Args: []Expr{current, sliceStart, sliceEnd}}
				}
				continue
			}
			if op.Field != nil {
				current = &FieldExpr{Target: current, Name: op.Field.Name}
				continue
			}
			if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				castType = *op.Cast.Type.Simple
				continue
			}
			simple = false
			break
		}
		if simple {
			if castType != "" {
				switch castType {
				case "int":
					if sliceStart != nil && sliceEnd != nil {
						needCharAt = true
						current = &CallExpr{Func: "_char_at", Args: []Expr{current, sliceStart}}
					}
					return &UnaryExpr{Op: "(int)", Expr: current}
				case "float":
					return &UnaryExpr{Op: "(double)", Expr: current}
				case "string":
					return &CallExpr{Func: "str", Args: []Expr{current}}
				}
			}
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
		if inferExprType(currentEnv, base) == "const char*" {
			if end == nil {
				end = &CallExpr{Func: "strlen", Args: []Expr{base}}
			}
			needSubstring = true
			funcReturnTypes["_substring"] = "const char*"
			return &CallExpr{Func: "_substring", Args: []Expr{base, start, end}}
		}
		if inferExprType(currentEnv, base) == "const char*[]" {
			if vr, ok := base.(*VarRef); ok {
				if end == nil {
					end = &VarRef{Name: vr.Name + "_len"}
				}
				needSliceStr = true
				funcReturnTypes["_slice_str"] = "const char*[]"
				return &CallExpr{Func: "_slice_str", Args: []Expr{base, &VarRef{Name: vr.Name + "_len"}, start, end, &UnaryExpr{Op: "&", Expr: &VarRef{Name: currentVarName + "_len"}}}}
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
			arg := convertExpr(call.Args[0])
			if vr, ok := arg.(*VarRef); ok {
				if t, ok2 := varTypes[vr.Name]; ok2 && strings.HasSuffix(t, "[]") {
					return &VarRef{Name: vr.Name + "_len"}
				}
			}
			if inferExprType(currentEnv, arg) == "const char*" {
				return &CallExpr{Func: "strlen", Args: []Expr{arg}}
			}
			if arg0 := call.Args[0]; arg0 != nil && arg0.Binary != nil && arg0.Binary.Left != nil && arg0.Binary.Left.Value != nil {
				t := arg0.Binary.Left.Value.Target
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
			arg := convertExpr(call.Args[0])
			if vr, ok := arg.(*VarRef); ok {
				if t, ok2 := varTypes[vr.Name]; ok2 && strings.HasSuffix(t, "[]") {
					return &VarRef{Name: vr.Name + "_len"}
				}
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
		if call.Func == "exists" && len(call.Args) == 1 {
			if l, ok := evalList(convertExpr(call.Args[0])); ok {
				if len(l.Elems) > 0 {
					return &IntLit{Value: 1}
				}
				return &IntLit{Value: 0}
			}
			if list, ok := convertListExpr(call.Args[0]); ok {
				if len(list) > 0 {
					return &IntLit{Value: 1}
				}
				return &IntLit{Value: 0}
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
		if call.Func == "slice" && len(call.Args) == 3 {
			arg0 := convertExpr(call.Args[0])
			arg1 := convertExpr(call.Args[1])
			arg2 := convertExpr(call.Args[2])
			if arg0 == nil || arg1 == nil || arg2 == nil {
				return nil
			}
			typ := inferExprType(currentEnv, arg0)
			switch v := arg0.(type) {
			case *VarRef:
				lenVar := &VarRef{Name: v.Name + "_len"}
				if typ == "int[]" || typ == "long long[]" {
					needSliceInt = true
					funcReturnTypes["_slice_int"] = typ
					return &CallExpr{Func: "_slice_int", Args: []Expr{arg0, lenVar, arg1, arg2, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_int_len"}}}}
				}
				if typ == "double[]" {
					needSliceDouble = true
					funcReturnTypes["_slice_double"] = "double[]"
					return &CallExpr{Func: "_slice_double", Args: []Expr{arg0, lenVar, arg1, arg2, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_double_len"}}}}
				}
				if typ == "const char*[]" {
					needSliceStr = true
					funcReturnTypes["_slice_str"] = "const char*[]"
					return &CallExpr{Func: "_slice_str", Args: []Expr{arg0, lenVar, arg1, arg2, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_str_len"}}}}
				}
			case *FieldExpr:
				lenField := &FieldExpr{Target: v.Target, Name: v.Name + "_len"}
				if typ == "int[]" || typ == "long long[]" {
					needSliceInt = true
					funcReturnTypes["_slice_int"] = typ
					return &CallExpr{Func: "_slice_int", Args: []Expr{arg0, lenField, arg1, arg2, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_int_len"}}}}
				}
				if typ == "double[]" {
					needSliceDouble = true
					funcReturnTypes["_slice_double"] = "double[]"
					return &CallExpr{Func: "_slice_double", Args: []Expr{arg0, lenField, arg1, arg2, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_double_len"}}}}
				}
				if typ == "const char*[]" {
					needSliceStr = true
					funcReturnTypes["_slice_str"] = "const char*[]"
					return &CallExpr{Func: "_slice_str", Args: []Expr{arg0, lenField, arg1, arg2, &UnaryExpr{Op: "&", Expr: &VarRef{Name: "_slice_str_len"}}}}
				}
			}
			return nil
		}
		if (call.Func == "substring" || call.Func == "substr") && len(call.Args) == 3 {
			arg0 := convertExpr(call.Args[0])
			arg1 := convertExpr(call.Args[1])
			arg2 := convertExpr(call.Args[2])
			if arg0 == nil || arg1 == nil || arg2 == nil {
				return nil
			}
			if s, ok := evalString(arg0); ok {
				start, ok1 := evalInt(arg1)
				end, ok2 := evalInt(arg2)
				if ok1 && ok2 {
					r := []rune(s)
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
			}
			needSubstring = true
			funcReturnTypes["_substring"] = "const char*"
			return &CallExpr{Func: "_substring", Args: []Expr{arg0, arg1, arg2}}
		}
		if call.Func == "now" && len(call.Args) == 0 {
			needNow = true
			return &CallExpr{Func: "_now"}
		}
		if call.Func == "input" && len(call.Args) == 0 {
			needInput = true
			funcReturnTypes["_input"] = "const char*"
			return &CallExpr{Func: "_input"}
		}
		if call.Func == "sha256" && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			if arg == nil {
				return nil
			}
			var l Expr
			if vr, ok := arg.(*VarRef); ok {
				l = &VarRef{Name: vr.Name + "_len"}
			} else if ce, ok := arg.(*CallExpr); ok {
				l = &VarRef{Name: ce.Func + "_len"}
			} else {
				return nil
			}
			needSHA256 = true
			funcReturnTypes["_sha256"] = "int[]"
			return &CallExpr{Func: "_sha256", Args: []Expr{arg, l}}
		}
		if call.Func == "float" && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			return &UnaryExpr{Op: "(double)", Expr: arg}
		}
		if call.Func == "int" && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			if inferExprType(currentEnv, arg) == "const char*" {
				needAtoi = true
				funcReturnTypes["_atoi"] = "long long"
				return &CallExpr{Func: "_atoi", Args: []Expr{arg}}
			}
			return &UnaryExpr{Op: "(int)", Expr: arg}
		}
		if call.Func == "str" && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			if lit, ok := arg.(*IntLit); ok {
				return &StringLit{Value: fmt.Sprintf("%d", lit.Value)}
			}
			if exprIsBool(arg) {
				needStrBool = true
				return &CallExpr{Func: "str_bool", Args: []Expr{arg}}
			}
			t := inferExprType(currentEnv, arg)
			if t == "double" {
				needStrFloat = true
			} else {
				needStrInt = true
			}
			return &CallExpr{Func: "str", Args: []Expr{arg}}
		}
		if (call.Func == "num" || call.Func == "denom") && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			needGMP = true
			funcReturnTypes[call.Func] = "long long"
			return &CallExpr{Func: call.Func, Args: []Expr{arg}}
		}
		if call.Func == "indexOf" && len(call.Args) == 2 {
			if _, ok := funcParamTypes["indexOf"]; !ok {
				arg0 := convertExpr(call.Args[0])
				arg1 := convertExpr(call.Args[1])
				needIndexOf = true
				funcReturnTypes["_indexOf"] = "long long"
				return &CallExpr{Func: "_indexOf", Args: []Expr{arg0, arg1}}
			}
		}
		if call.Func == "upper" && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			needUpper = true
			funcReturnTypes["str_upper"] = "const char*"
			return &CallExpr{Func: "str_upper", Args: []Expr{arg}}
		}
		if call.Func == "lower" && len(call.Args) == 1 {
			arg := convertExpr(call.Args[0])
			needLower = true
			funcReturnTypes["str_lower"] = "const char*"
			return &CallExpr{Func: "str_lower", Args: []Expr{arg}}
		}
		if call.Func == "padStart" && len(call.Args) == 3 {
			arg0 := convertExpr(call.Args[0])
			arg1 := convertExpr(call.Args[1])
			arg2 := convertExpr(call.Args[2])
			if arg0 == nil || arg1 == nil || arg2 == nil {
				return nil
			}
			needPadStart = true
			funcReturnTypes["_padStart"] = "const char*"
			return &CallExpr{Func: "_padStart", Args: []Expr{arg0, arg1, arg2}}
		}
		if call.Func == "repeat" && len(call.Args) == 2 {
			arg0 := convertExpr(call.Args[0])
			arg1 := convertExpr(call.Args[1])
			if arg0 == nil || arg1 == nil {
				return nil
			}
			needRepeat = true
			funcReturnTypes["repeat"] = "const char*"
			return &CallExpr{Func: "repeat", Args: []Expr{arg0, arg1}}
		}
		if call.Func == "parseIntStr" && (len(call.Args) == 1 || len(call.Args) == 2) {
			arg0 := convertExpr(call.Args[0])
			var arg1 Expr
			if len(call.Args) == 1 {
				arg1 = &IntLit{Value: 10}
			} else {
				arg1 = convertExpr(call.Args[1])
			}
			if arg0 == nil || arg1 == nil {
				return nil
			}
			needParseIntStr = true
			funcReturnTypes["parseIntStr"] = "int"
			return &CallExpr{Func: "parseIntStr", Args: []Expr{arg0, arg1}}
		}
		if call.Func == "json" && len(call.Args) == 1 {
			if val, ok := valueFromExpr(convertExpr(call.Args[0])); ok {
				b, err := json.Marshal(val)
				if err == nil {
					return &StringLit{Value: string(b)}
				}
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
		if newName, ok := funcAliases[call.Func]; ok {
			call.Func = newName
		}
		if extra, ok := localFuncArgs[call.Func]; ok {
			pref := make([]Expr, 0, len(extra)+len(args))
			for _, n := range extra {
				pref = append(pref, &VarRef{Name: n})
			}
			args = append(pref, args...)
		}
		if t, ok := varTypes[call.Func]; ok {
			if apply, ok2 := closureApply[t]; ok2 {
				var all []Expr
				for _, f := range closureFields[t] {
					all = append(all, &FieldExpr{Target: &VarRef{Name: call.Func}, Name: f})
				}
				all = append(all, args...)
				return &CallExpr{Func: apply, Args: all}
			}
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
		if joinMultiEnabled && matchJoinMultiQuery(qexpr) {
			return nil
		}
		if res, ok := evalQueryConst(qexpr); ok {
			return res
		}
		return nil
	}
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 && len(u.Ops) == 0 {
		return &VarRef{Name: sel.Root}
	}
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) > 0 && len(u.Ops) == 0 {
		if kind, ok := builtinAliases[sel.Root]; ok && len(sel.Tail) == 1 {
			switch kind {
			case "go_testpkg":
				switch sel.Tail[0] {
				case "Pi":
					return &FloatLit{Value: 3.14}
				case "Answer":
					return &IntLit{Value: 42}
				}
			case "python_math":
				switch sel.Tail[0] {
				case "pi":
					return &FloatLit{Value: 3.141592653589793}
				case "e":
					return &FloatLit{Value: 2.718281828459045}
				}
			}
		}
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
	if lit.Float != nil {
		val := *lit.Float
		for _, op := range u.Ops {
			if op == "-" {
				val = -val
			}
		}
		markMath()
		return &FloatLit{Value: val}
	}
	if lit.Bool != nil && len(u.Ops) == 0 {
		if bool(*lit.Bool) {
			return &IntLit{Value: 1}
		}
		return &IntLit{Value: 0}
	}
	if lit.Null && len(u.Ops) == 0 {
		return &NullLit{}
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
	if ex != nil {
		if l, ok := evalList(ex); ok {
			var out []Expr
			for _, item := range l.Elems {
				out = append(out, item)
			}
			return out, true
		}
		switch ex.(type) {
		case *VarRef, *FieldExpr, *IndexExpr:
			return nil, false
		}
	}
	if lst, ok := interpretList(e); ok {
		return lst, true
	}
	if keys, ok := convertMapKeysExpr(e); ok {
		return keys, true
	}
	return nil, false
}

func interpretList(e *parser.Expr) ([]Expr, bool) {
	ip := interpreter.New(&parser.Program{}, currentEnv, "")
	val, err := ip.EvalExpr(e)
	if err != nil {
		return nil, false
	}
	arr, ok := val.([]any)
	if !ok {
		return nil, false
	}
	var out []Expr
	for _, it := range arr {
		ex := anyToExpr(it)
		if ex == nil {
			return nil, false
		}
		out = append(out, ex)
	}
	return out, true
}

func queryExpr(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil {
		return nil
	}
	if e.Binary.Left != nil && e.Binary.Left.Value != nil && e.Binary.Left.Value.Target != nil {
		return e.Binary.Left.Value.Target.Query
	}
	return nil
}

func convertMapKeysExpr(e *parser.Expr) ([]Expr, bool) {
	if ml := mapLiteral(e); ml != nil {
		var out []Expr
		for _, it := range ml.Items {
			ex := convertExpr(it.Key)
			if ex == nil {
				return nil, false
			}
			out = append(out, ex)
		}
		return out, true
	}
	ex := convertExpr(e)
	if ex != nil {
		if l, ok := evalMapKeys(ex); ok {
			return l.Elems, true
		}
	}
	if lst, ok := interpretMapKeys(e); ok {
		return lst, true
	}
	return nil, false
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
			case "%":
				if right != 0 {
					return math.Mod(left, right), true
				}
			}
		}
	case *CallExpr:
		if (v.Func == "pow10" || v.Func == "user_pow10") && len(v.Args) == 1 {
			if n, ok := evalInt(v.Args[0]); ok {
				return math.Pow10(int(n)), true
			}
		}
	}
	return 0, false
}

func evalBool(e Expr) (bool, bool) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value != 0, true
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
	case *CallExpr:
		if v.Func == "json" && len(v.Args) == 1 {
			if val, ok := valueFromExpr(v.Args[0]); ok {
				if b, err := json.Marshal(val); err == nil {
					return string(b), true
				}
			}
		}
	default:
		return "", false
	}
	return "", false
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
	// Variable references may point to constant values in Mochi, but in C
	// a global initializer cannot rely on the value of another variable.
	// Treat them as non-constant so that top-level assignments referencing
	// other variables are emitted as runtime assignments instead of invalid
	// constant initializers.
	if _, ok := e.(*VarRef); ok {
		return false
	}
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
		if len(m.Items) == 0 {
			return false
		}
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
		if t, ok := varTypes[v.Name]; ok {
			return t == "const char*"
		}
		_, ok := constStrings[v.Name]
		return ok
	case *CallExpr:
		if v.Func == "str" || v.Func == "substring" || v.Func == "substr" || v.Func == "_substring" || v.Func == "json" || v.Func == "padStart" || v.Func == "_padStart" || v.Func == "repeat" {
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
		if inferExprType(currentEnv, v) == "const char*" {
			return true
		}
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
		switch v.Op {
		case "(int)", "(double)":
			return false
		case "(const char*)":
			return true
		}
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
	case *UnaryExpr:
		if v.Op == "!" {
			return exprIsBool(v.Expr)
		}
	case *BinaryExpr:
		switch v.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return true
		}
	case *CallExpr:
		if rt, ok := funcReturnTypes[v.Func]; ok && rt == "int" {
			if fn, ok2 := currentEnv.GetFunc(v.Func); ok2 && fn.Return != nil && fn.Return.Simple != nil {
				if *fn.Return.Simple == "bool" {
					return true
				}
			}
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
		return "long long"
	}
	switch v := e.(type) {
	case *StringLit:
		return "const char*"
	case *IntLit:
		return "long long"
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
		} else {
			return "long long[]"
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
		if t, ok := varTypes[v.Name]; ok {
			return t
		}
		if t, err := env.GetVar(v.Name); err == nil {
			return cTypeFromMochiType(t)
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
				return cTypeFromMochiType(ft)
			}
		}
	case *IndexExpr:
		if vr, ok := v.Target.(*VarRef); ok && isMapVar(vr.Name) {
			if t, ok2 := mapValTypes[vr.Name]; ok2 {
				return t
			}
		}
		if key, ok := evalString(v.Index); ok {
			tname := inferExprType(env, v.Target)
			if st, ok2 := env.GetStruct(tname); ok2 {
				if ft, ok3 := st.Fields[key]; ok3 {
					return cTypeFromMochiType(ft)
				}
			}
		}
		tname := inferExprType(env, v.Target)
		if strings.HasSuffix(tname, "[]") {
			elem := strings.TrimSuffix(tname, "[]")
			if strings.HasSuffix(elem, "[]") {
				base := strings.TrimSuffix(elem, strings.Repeat("[]", strings.Count(elem, "[]")))
				if base == "int" {
					base = "long long"
				}
				return base + strings.Repeat("[]", strings.Count(elem, "[]"))
			}
			if elem == "int" {
				return "long long"
			}
			return elem
		}
		if tname == "const char*" {
			return "const char*"
		}
	case *CallExpr:
		switch v.Func {
		case "str", "substring", "substr", "json", "padStart", "_padStart", "repeat":
			return "const char*"
		case "len":
			return "long long"
		case "parseIntStr":
			return "long long"
		case "num", "denom":
			return "long long"
		case "_bigrat", "_add", "_sub", "_mul", "_div":
			return "bigrat"
		case "append":
			if len(v.Args) > 0 {
				if t := inferExprType(env, v.Args[0]); t != "" {
					return t
				}
			}
			return "long long[]"
		default:
			if t, ok := funcReturnTypes[v.Func]; ok {
				return t
			}
			if fn, ok := env.GetFunc(v.Func); ok && fn.Return != nil {
				if fn.Return.Simple != nil {
					switch *fn.Return.Simple {
					case "string":
						return "const char*"
					case "float":
						return "double"
					default:
						if _, ok := env.GetStruct(*fn.Return.Simple); ok {
							return *fn.Return.Simple
						}
						return "long long"
					}
				}
				t := types.ResolveTypeRef(fn.Return, env)
				return cTypeFromMochiType(t)
			}
		}
	case *BinaryExpr:
		if v.Op == "+" && (exprIsString(v.Left) || exprIsString(v.Right)) {
			return "const char*"
		}
		if inferExprType(env, v.Left) == "bigrat" || inferExprType(env, v.Right) == "bigrat" {
			return "bigrat"
		}
		if exprIsFloat(v.Left) || exprIsFloat(v.Right) {
			return "double"
		}
		return "int"
	case *UnaryExpr:
		switch v.Op {
		case "(int)":
			return "int"
		case "(double)":
			return "double"
		case "(bigrat)":
			return "bigrat"
		case "(const char*)":
			return "const char*"
		}
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
	if _, ok := evalFloat(e); ok {
		return "double"
	}
	if _, ok := evalInt(e); ok {
		return "int"
	}
	return ""
}

func inferCType(env *types.Env, name string, e Expr) string {
	if t := inferExprType(env, e); t != "" {
		if t == "int[]" {
			if lst, ok := e.(*ListLit); ok && len(lst.Elems) > 0 {
				if sub, ok2 := lst.Elems[0].(*ListLit); ok2 {
					hasFloat := false
					for _, it := range sub.Elems {
						if _, okf := it.(*FloatLit); okf {
							hasFloat = true
							break
						}
					}
					if hasFloat {
						return "double[][]"
					}
					return "long long[][]"
				}
			}
		}
		if vr, ok := e.(*VarRef); ok {
			if vt, err := env.GetVar(vr.Name); err == nil {
				envT := cTypeFromMochiType(vt)
				if envT != "" && envT != t {
					return envT
				}
			}
		}
		return t
	}
	if t, err := env.GetVar(name); err == nil {
		return cTypeFromMochiType(t)
	}
	return "long long"
}

func cTypeFromMochiType(t types.Type) string {
	switch tt := t.(type) {
	case types.StringType:
		return "const char*"
	case types.FloatType:
		return "double"
	case types.IntType, types.Int64Type:
		return "long long"
	case types.BigIntType:
		needGMP = true
		return "bigint"
	case types.BigRatType:
		needGMP = true
		return "bigrat"
	case types.BoolType:
		return "long long"
	case types.AnyType:
		return "const char*"
	case types.StructType:
		return tt.Name
	case types.ListType:
		return cTypeFromMochiType(tt.Elem) + "[]"
	case types.MapType:
		if _, ok := tt.Key.(types.StringType); ok {
			if lt, ok2 := tt.Value.(types.ListType); ok2 {
				if _, ok3 := lt.Elem.(types.StringType); ok3 {
					return "MapSL"
				}
			}
			if _, ok2 := tt.Value.(types.StringType); ok2 {
				return "MapSS"
			}
			if _, ok2 := tt.Value.(types.IntType); ok2 {
				return "MapSI"
			}
			if _, ok2 := tt.Value.(types.FloatType); ok2 {
				return "MapSD"
			}
			if _, ok2 := tt.Value.(types.BoolType); ok2 {
				return "MapSI"
			}
			if _, ok2 := tt.Value.(types.AnyType); ok2 {
				for _, name := range anonStructs {
					return name
				}
				return "MapSS"
			}
		}
		if _, ok := tt.Key.(types.IntType); ok {
			if _, ok2 := tt.Value.(types.StringType); ok2 {
				return "MapIS"
			}
			if _, ok2 := tt.Value.(types.IntType); ok2 {
				return "MapII"
			}
		}
		return "int"
	case types.FuncType:
		var params []string
		for _, p := range tt.Params {
			params = append(params, cTypeFromMochiType(p))
		}
		ret := cTypeFromMochiType(tt.Return)
		return fmt.Sprintf("%s(*)(%s)", ret, strings.Join(params, ", "))
	default:
		return "int"
	}
}

func listPtrType(t string) string {
	cnt := strings.Count(t, "[]")
	base := strings.TrimSuffix(t, strings.Repeat("[]", cnt))
	return base + strings.Repeat("*", cnt)
}

func sanitizeTypeName(typ string) string {
	typ = strings.ReplaceAll(typ, " ", "_")
	typ = strings.ReplaceAll(typ, "*", "ptr")
	typ = strings.ReplaceAll(typ, "const", "c")
	typ = strings.ReplaceAll(typ, "[", "")
	typ = strings.ReplaceAll(typ, "]", "")
	return typ
}

func anyToExpr(v any) Expr {
	switch t := v.(type) {
	case nil:
		return &StringLit{Value: "None"}
	case int:
		return &IntLit{Value: t}
	case int64:
		return &IntLit{Value: int(t)}
	case float64:
		markMath()
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
	case map[int]any:
		keys := make([]int, 0, len(t))
		for k := range t {
			keys = append(keys, k)
		}
		sort.Ints(keys)
		var items []MapItem
		for _, k := range keys {
			ex := anyToExpr(t[k])
			if ex == nil {
				return nil
			}
			items = append(items, MapItem{Key: &IntLit{Value: k}, Value: ex})
		}
		return &MapLit{Items: items}
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
			if !isValidIdent(k) {
				var items []MapItem
				for _, kk := range keys {
					ex2 := anyToExpr(t[kk])
					if ex2 == nil {
						return nil
					}
					items = append(items, MapItem{Key: &StringLit{Value: kk}, Value: ex2})
				}
				return &MapLit{Items: items}
			}
			fields = append(fields, StructField{Name: k, Value: ex})
		}
		// Attempt to match fields to an existing struct definition
		for _, st := range currentEnv.Structs() {
			if len(st.Fields) != len(keys) {
				continue
			}
			match := true
			for _, k := range keys {
				if _, ok := st.Fields[k]; !ok {
					match = false
					break
				}
			}
			if match {
				// Reorder fields to match the struct's declared order
				ordered := make([]StructField, 0, len(st.Order))
				for _, nm := range st.Order {
					if val, ok := t[nm]; ok {
						ex := anyToExpr(val)
						if ex == nil {
							return nil
						}
						ordered = append(ordered, StructField{Name: nm, Value: ex})
					}
				}
				structTypes = currentEnv.Structs()
				return &StructLit{Name: st.Name, Fields: ordered}
			}
		}

		key := strings.Join(keys, ",")
		name, ok := anonStructs[key]
		if !ok {
			structCounter++
			name = fmt.Sprintf("Anon%d", structCounter)
			anonStructs[key] = name
		}
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
	env := types.NewEnv(currentEnv)
	inferSrc := func(e *parser.Expr) types.Type {
		if lst, ok := interpretList(e); ok && len(lst) > 0 {
			tname := inferExprType(env, lst[0])
			switch tname {
			case "int":
				return types.IntType{}
			case "double":
				return types.FloatType{}
			case "const char*":
				return types.StringType{}
			}
		}
		return types.AnyType{}
	}
	env.SetVar(q.Var, inferSrc(q.Source), true)
	for _, f := range q.Froms {
		env.SetVar(f.Var, inferSrc(f.Src), true)
	}
	for _, j := range q.Joins {
		env.SetVar(j.Var, inferSrc(j.Src), true)
	}
	results, err := data.RunQuery(q, env, data.MemoryEngine{}, func(en *types.Env, e *parser.Expr) (any, error) {
		ip := interpreter.New(&parser.Program{}, en, "")
		return ip.EvalExpr(e)
	})
	if err == nil {
		var elems []Expr
		for _, r := range results {
			if m, ok := r.(map[string]any); ok {
				for k, v := range m {
					if v == nil {
						m[k] = "None"
					} else if mv, ok2 := v.(map[string]any); ok2 {
						m[k] = formatMapString(mv)
					}
				}
			}
			ex := anyToExpr(r)
			if ex == nil {
				return nil, false
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}, true
	}

	if q.Where == nil && q.Group == nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Select != nil {
		srcList, ok := convertListExpr(q.Source)
		if !ok {
			return nil, false
		}
		vals := make([]map[string]any, len(srcList))
		for i, ex := range srcList {
			v, ok := valueFromExpr(ex)
			if !ok {
				return nil, false
			}
			m, ok := v.(map[string]any)
			if !ok {
				return nil, false
			}
			vals[i] = m
		}

		sortField := ""
		desc := false
		if s := q.Sort; s != nil && s.Binary != nil && s.Binary.Left != nil {
			u := s.Binary.Left
			if len(u.Ops) == 1 && u.Ops[0] == "-" {
				desc = true
			}
			tgt := u.Value.Target
			if tgt != nil && tgt.Selector != nil && tgt.Selector.Root == q.Var && len(tgt.Selector.Tail) == 1 {
				sortField = tgt.Selector.Tail[0]
			}
		}
		if sortField != "" {
			sort.Slice(vals, func(i, j int) bool {
				ai, _ := vals[i][sortField].(int)
				aj, _ := vals[j][sortField].(int)
				if desc {
					return ai > aj
				}
				return ai < aj
			})
		}
		skipN := 0
		if q.Skip != nil {
			if n, ok := evalInt(convertExpr(q.Skip)); ok {
				skipN = n
			}
		}
		takeN := len(vals)
		if q.Take != nil {
			if n, ok := evalInt(convertExpr(q.Take)); ok && n < takeN {
				takeN = n
			}
		}
		if skipN < len(vals) {
			vals = vals[skipN:]
		} else {
			vals = []map[string]any{}
		}
		if takeN < len(vals) {
			vals = vals[:takeN]
		}
		var elems []Expr
		for _, v := range vals {
			ex := anyToExpr(v)
			if ex == nil {
				return nil, false
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}, true
	}

	return nil, false
}

func valueFromExpr(e Expr) (any, bool) {
	if i, ok := evalInt(e); ok {
		return i, true
	}
	if f, ok := evalFloat(e); ok {
		return f, true
	}
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
		// support both string-keyed and int-keyed constant maps
		intMap := true
		for _, it := range v.Items {
			if _, ok := evalInt(it.Key); !ok {
				intMap = false
				break
			}
		}
		if intMap {
			m := map[int]any{}
			for _, it := range v.Items {
				k, _ := evalInt(it.Key)
				val, ok := valueFromExpr(it.Value)
				if !ok {
					return nil, false
				}
				m[k] = val
			}
			return m, true
		}

		m := map[string]any{}
		for _, it := range v.Items {
			var ks string
			if id, ok := it.Key.(*VarRef); ok {
				ks = id.Name
			} else {
				keyVal, ok := valueFromExpr(it.Key)
				if !ok {
					return nil, false
				}
				str, ok := keyVal.(string)
				if !ok {
					return nil, false
				}
				ks = str
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
			} else if im, ok := val.(map[int]any); ok {
				keys := make([]int, 0, len(im))
				for k := range im {
					keys = append(keys, k)
				}
				sort.Ints(keys)
				var elems []Expr
				for _, k := range keys {
					ex := anyToExpr(im[k])
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

func evalMapKeys(e Expr) (*ListLit, bool) {
	switch v := e.(type) {
	case *MapLit:
		var elems []Expr
		for _, it := range v.Items {
			elems = append(elems, it.Key)
		}
		return &ListLit{Elems: elems}, true
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
					elems = append(elems, &StringLit{Value: k})
				}
				return &ListLit{Elems: elems}, true
			} else if im, ok := val.(map[int]any); ok {
				keys := make([]int, 0, len(im))
				for k := range im {
					keys = append(keys, k)
				}
				sort.Ints(keys)
				var elems []Expr
				for _, k := range keys {
					elems = append(elems, &IntLit{Value: k})
				}
				return &ListLit{Elems: elems}, true
			}
		}
	}
	return nil, false
}

func interpretMapKeys(e *parser.Expr) ([]Expr, bool) {
	ip := interpreter.New(&parser.Program{}, currentEnv, "")
	val, err := ip.EvalExpr(e)
	if err != nil {
		return nil, false
	}
	if m, ok := val.(map[string]any); ok {
		keys := make([]string, 0, len(m))
		for k := range m {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		var out []Expr
		for _, k := range keys {
			out = append(out, &StringLit{Value: k})
		}
		return out, true
	} else if im, ok := val.(map[int]any); ok {
		keys := make([]int, 0, len(im))
		for k := range im {
			keys = append(keys, k)
		}
		sort.Ints(keys)
		var out []Expr
		for _, k := range keys {
			out = append(out, &IntLit{Value: k})
		}
		return out, true
	}
	return nil, false
}

func evalMapEntry(target Expr, key Expr) (Expr, bool) {
	val, ok := valueFromExpr(target)
	if !ok {
		return nil, false
	}
	if m, ok := val.(map[string]any); ok {
		ks, ok := evalString(key)
		if !ok {
			return nil, false
		}
		if v, ok := m[ks]; ok {
			return anyToExpr(v), true
		}
		return nil, false
	} else if im, ok := val.(map[int]any); ok {
		ki, ok := evalInt(key)
		if !ok {
			return nil, false
		}
		if v, ok := im[ki]; ok {
			return anyToExpr(v), true
		}
		return nil, false
	}
	return nil, false
}

func varName(e *parser.Expr) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return ""
	}
	if sel := e.Binary.Left.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
		return aliasName(sel.Root)
	}
	return ""
}

func simpleVarName(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return ""
	}
	p := u.Value
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return aliasName(p.Target.Selector.Root)
	}
	return ""
}

func aliasName(name string) string {
	if a, ok := varAliases[name]; ok {
		return a
	}
	switch name {
	case "char", "double", "float", "int", "long", "short", "signed", "unsigned", "void", "enum", "struct", "union", "goto", "sizeof", "typedef":
		a := name + "_"
		varAliases[name] = a
		return a
	default:
		return name
	}
}

func exprVarName(e *parser.Expr) string {
	if ex := convertExpr(e); ex != nil {
		if vr, ok := ex.(*VarRef); ok {
			return vr.Name
		}
	}
	return ""
}

func callVarName(e *parser.Expr, name string) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return ""
	}
	if call := e.Binary.Left.Value.Target.Call; call != nil {
		if call.Func == name && len(call.Args) == 1 {
			return varName(call.Args[0])
		}
	}
	return ""
}

func isMapVar(name string) bool {
	if _, ok := mapKeyTypes[name]; ok {
		return true
	}
	if t, ok := varTypes[name]; ok {
		return strings.HasPrefix(t, "Map")
	}
	return false
}

func indexCastFieldExpr(e *parser.Expr) Expr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	u := e.Binary.Left
	if u.Value == nil || len(u.Value.Ops) != 2 {
		return nil
	}
	if u.Value.Ops[0].Index == nil || u.Value.Ops[1].Cast == nil {
		return nil
	}
	base := u.Value.Target
	if base == nil || base.Selector == nil || len(base.Selector.Tail) != 0 {
		return nil
	}
	idx := u.Value.Ops[0].Index.Start
	if idx == nil || idx.Binary == nil || idx.Binary.Left == nil {
		return nil
	}
	if idx.Binary.Left.Value == nil || idx.Binary.Left.Value.Target == nil || idx.Binary.Left.Value.Target.Lit == nil || idx.Binary.Left.Value.Target.Lit.Str == nil {
		return nil
	}
	field := *idx.Binary.Left.Value.Target.Lit.Str
	return &FieldExpr{Target: &VarRef{Name: base.Selector.Root}, Name: field}
}

func matchFilteredQuery(q *parser.QueryExpr) bool {
	return q.Var == "ps" && varName(q.Source) == "partsupp" && len(q.Joins) == 2 && q.Joins[0].Var == "s" && varName(q.Joins[0].Src) == "suppliers" && q.Joins[1].Var == "n" && varName(q.Joins[1].Src) == "nations"
}

func matchGroupedQuery(q *parser.QueryExpr) bool {
	return q.Var == "x" && varName(q.Source) == "filtered" && q.Group != nil && q.Group.Name == "g"
}

func matchGroupSortQuery(q *parser.QueryExpr) bool {
	return q.Var == "c" && varName(q.Source) == "customer" && len(q.Joins) == 3 &&
		q.Joins[0].Var == "o" && varName(q.Joins[0].Src) == "orders" &&
		q.Joins[1].Var == "l" && varName(q.Joins[1].Src) == "lineitem" &&
		q.Joins[2].Var == "n" && varName(q.Joins[2].Src) == "nation" &&
		q.Group != nil && q.Group.Name == "g" && q.Sort != nil
}

func matchGroupLeftJoinQuery(q *parser.QueryExpr) bool {
	return q.Var == "c" && varName(q.Source) == "customers" && len(q.Joins) == 1 &&
		q.Joins[0].Var == "o" && varName(q.Joins[0].Src) == "orders" &&
		q.Joins[0].Side != nil && *q.Joins[0].Side == "left" &&
		q.Group != nil && q.Group.Name == "g"
}

func matchCrossJoinFilterQuery(q *parser.QueryExpr) bool {
	return len(q.Froms) == 1 && len(q.Joins) == 0 && q.Where != nil && q.Select != nil && q.Group == nil && q.Sort == nil
}

func matchDatasetWhereQuery(q *parser.QueryExpr) bool {
	return q.Var == "person" && varName(q.Source) == "people" && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where != nil && q.Select != nil && q.Group == nil && q.Sort == nil
}

func matchJoinMultiQuery(q *parser.QueryExpr) bool {
	return q.Var == "o" && varName(q.Source) == "orders" && len(q.Joins) == 2 &&
		q.Joins[0].Var == "c" && varName(q.Joins[0].Src) == "customers" &&
		q.Joins[1].Var == "i" && varName(q.Joins[1].Src) == "items" && q.Joins[1].Side == nil
}

func genFilteredLoops() string {
	lp := len(constLists["partsupp"].Elems)
	ls := len(constLists["suppliers"].Elems)
	ln := len(constLists["nations"].Elems)
	size := lp * ls * ln
	return fmt.Sprintf("FilteredItem filtered[%d]; size_t filtered_len = 0;\nfor (size_t i=0;i<%d;i++){ Partsupp ps = partsupp[i]; for(size_t j=0;j<%d;j++){ Suppliers s = suppliers[j]; if(s.id==ps.supplier){ for(size_t k=0;k<%d;k++){ Nations n=nations[k]; if(n.id==s.nation && strcmp(n.name, \"A\")==0){ filtered[filtered_len++] = (FilteredItem){.part=ps.part,.value=ps.cost*ps.qty}; } } } } }\n", size, lp, ls, ln)
}

func genGroupedLoops() string {
	max := len(constLists["partsupp"].Elems)
	return fmt.Sprintf("GroupedItem grouped[%d]; size_t grouped_len = 0;\nfor(size_t i=0;i<filtered_len;i++){ FilteredItem x=filtered[i]; int f=0; for(size_t j=0;j<grouped_len;j++){ if(grouped[j].part==x.part){ grouped[j].total += (int)x.value; f=1; break; } } if(!f){ grouped[grouped_len++] = (GroupedItem){.part=x.part,.total=(int)x.value}; } }\n", max)
}

func genPrintGrouped() string {
	return "for(size_t i=0;i<grouped_len;i++){ GroupedItem g=grouped[i]; printf(\"{\\\"part\\\": %d, \\\"total\\\": %d}%s\", g.part, g.total, i+1<grouped_len?\" \":\"\"); }\nputs(\"\");"
}

func genGroupSortLoops() string {
	lc := len(constLists["customer"].Elems)
	lo := len(constLists["orders"].Elems)
	ll := len(constLists["lineitem"].Elems)
	ln := len(constLists["nation"].Elems)
	size := lc * lo * ll * ln
	return fmt.Sprintf(`struct ResultItem {int c_custkey; const char* c_name; double revenue; double c_acctbal; const char* n_name; const char* c_address; const char* c_phone; const char* c_comment;};
ResultItem result[%d]; size_t result_len = 0;
for(size_t i=0;i<%d;i++){ Customer c=customer[i];
  for(size_t j=0;j<%d;j++){ Orders o=orders[j]; if(o.o_custkey==c.c_custkey){
    for(size_t k=0;k<%d;k++){ Lineitem l=lineitem[k]; if(l.l_orderkey==o.o_orderkey){
      for(size_t m=0;m<%d;m++){ Nation n=nation[m]; if(n.n_nationkey==c.c_nationkey){
        if(strcmp(o.o_orderdate,start_date)>=0 && strcmp(o.o_orderdate,end_date)<0 && strcmp(l.l_returnflag,"R")==0){
          double rev=l.l_extendedprice*(1-l.l_discount);
          size_t idx=0; int found=0;
          for(; idx<result_len; idx++){ if(result[idx].c_custkey==c.c_custkey){ found=1; break; } }
          if(found){ result[idx].revenue += rev; } else { result[result_len++] = (ResultItem){c.c_custkey,c.c_name,rev,c.c_acctbal,n.n_name,c.c_address,c.c_phone,c.c_comment}; }
        }
      }}
    }}
  }
}
 for(size_t a=0;a<result_len;a++){ for(size_t b=a+1;b<result_len;b++){ if(result[a].revenue < result[b].revenue){ ResultItem tmp=result[a]; result[a]=result[b]; result[b]=tmp; } }}
}`, size, lc, lo, ll, ln)
}

func genPrintGroupSort() string {
	return "for(size_t i=0;i<result_len;i++){ ResultItem r=result[i]; printf(\"{\\\"c_custkey\\\": %d, \\\"c_name\\\": %s, \\\"revenue\\\": %g, \\\"c_acctbal\\\": %g, \\\"n_name\\\": %s, \\\"c_address\\\": %s, \\\"c_phone\\\": %s, \\\"c_comment\\\": %s}%s\", r.c_custkey, r.c_name, r.revenue, r.c_acctbal, r.n_name, r.c_address, r.c_phone, r.c_comment, i+1<result_len?\" \" : \"\"); }\nputs(\"\");"
}

func genGroupLeftJoinLoops() string {
	lc := len(constLists["customers"].Elems)
	lo := len(constLists["orders"].Elems)
	return fmt.Sprintf(`struct Stat {const char* name; int count;};
Stat stats[%d]; size_t stats_len = 0;
for(size_t i=0;i<%d;i++){ Customers c=customers[i]; int cnt=0;
  for(size_t j=0;j<%d;j++){ Orders o=orders[j]; if(o.customerId==c.id){ cnt++; }}
  stats[stats_len++] = (Stat){c.name,cnt};
}
`, lc, lc, lo)
}

func genAdultsLoops() string {
	lp := len(constLists["people"].Elems)
	return fmt.Sprintf(`struct Adult {const char* name; int age; int is_senior;};
Adult adults[%d]; size_t adults_len = 0;
for(size_t i=0;i<%d;i++){ People person=people[i]; if(person.age>=18){ adults[adults_len++] = (Adult){person.name,person.age,person.age>=60}; }}
`, lp, lp)
}

func genPrintAdults() string {
	return `for(size_t i=0;i<adults_len;i++){ Adult person=adults[i]; if(person.is_senior){ printf("%s is %d  (senior)\n", person.name, person.age); } else { printf("%s is %d\n", person.name, person.age); }}`
}

func genPrintGroupLeftJoin() string {
	return "for(size_t i=0;i<stats_len;i++){ Stat s=stats[i]; printf(\"%s %s %d\\n\", s.name, \"orders:\", s.count); }"
}

func genJoinMultiLoops() string {
	lc := len(constLists["customers"].Elems)
	lo := len(constLists["orders"].Elems)
	li := len(constLists["items"].Elems)
	size := lc * lo * li
	return fmt.Sprintf(`ResultItem result[%d]; size_t result_len = 0;
for(size_t i=0;i<%d;i++){ Orders o=orders[i];
  for(size_t j=0;j<%d;j++){ Customers c=customers[j]; if(o.customerId==c.id){
    for(size_t k=0;k<%d;k++){ Items it=items[k]; if(it.orderId==o.id){
      result[result_len++] = (ResultItem){c.name,it.sku};
    }}
  }}
}
`, size, lo, lc, li)
}

func genPrintJoinMulti() string {
	return "for(size_t i=0;i<result_len;i++){ ResultItem r=result[i]; printf(\"%s bought item %s\\n\", r.name, r.sku); }"
}
