//go:build slow

package cpp

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

var version string
var currentProgram *Program
var currentEnv *types.Env
var localTypes map[string]string

func init() {
	_, file, _, _ := runtime.Caller(0)
	root := filepath.Join(filepath.Dir(file), "../../..")
	if b, err := os.ReadFile(filepath.Join(root, "VERSION")); err == nil {
		version = strings.TrimSpace(string(b))
	} else {
		version = "unknown"
	}
}

type StructDef struct {
	Name   string
	Fields []Param
}

type Program struct {
	Includes  []string
	Structs   []StructDef
	Globals   []Stmt
	Functions []*Func
	ListTypes map[string]string
}

func (p *Program) addInclude(inc string) {
	for _, v := range p.Includes {
		if v == inc {
			return
		}
	}
	p.Includes = append(p.Includes, inc)
}

type Param struct {
	Name string
	Type string
}

type Func struct {
	Name       string
	Params     []Param
	ReturnType string
	Body       []Stmt
}

type Stmt interface{ emit(io.Writer, int) }

type Expr interface{ emit(io.Writer) }

type PrintStmt struct{ Values []Expr }

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// LenExpr represents the builtin len() for strings.
type LenExpr struct{ Value Expr }

type StringLit struct{ Value string }

type IntLit struct{ Value int }

type BoolLit struct{ Value bool }

type StructLit struct {
	Name   string
	Fields []FieldLit
}

type FieldLit struct {
	Name  string
	Value Expr
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

// ListLit represents a list literal converted to std::vector.
type ListLit struct{ Elems []Expr }

// UnaryExpr represents a prefix unary operation like negation or logical not.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

type SelectorExpr struct {
	Target Expr
	Field  string
}

type IndexExpr struct {
	Target Expr
	Index  Expr
}

// MapLit represents a simple map literal using std::map.
type MapLit struct {
	Keys      []Expr
	Values    []Expr
	KeyType   string
	ValueType string
}

type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

type ContainsExpr struct {
	Value Expr
	Sub   Expr
}

type InExpr struct {
	Value Expr
	Coll  Expr
}

type SumExpr struct{ Arg Expr }

// AppendExpr represents a call to the `append` builtin on a list.
type AppendExpr struct {
	List Expr
	Elem Expr
}

// AvgExpr represents a call to the `avg` builtin on a list of numbers.
type AvgExpr struct{ List Expr }

// StrExpr represents the `str` builtin for converting values to strings.
type StrExpr struct{ Value Expr }

// ValuesExpr represents the `values` builtin for maps.
type ValuesExpr struct{ Map Expr }

// MinExpr represents the `min` builtin for lists of numbers.
type MinExpr struct{ List Expr }

// MaxExpr represents the `max` builtin for lists of numbers.
type MaxExpr struct{ List Expr }

type CastExpr struct {
	Value Expr
	Type  string
}

type SubstringExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

type CallExpr struct {
	Name string
	Args []Expr
}

type ExistsExpr struct{ List Expr }

type LambdaExpr struct {
	Params []Param
	Body   Expr
}

// BlockLambda represents a lambda with a statement body.
type BlockLambda struct {
	Params []Param
	Body   []Stmt
}

type ReturnStmt struct{ Value Expr }

type VarRef struct{ Name string }

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

type LetStmt struct {
	Name  string
	Type  string
	Value Expr
}

type AssignStmt struct {
	Name  string
	Value Expr
}

// AssignIndexStmt represents assignment to an indexed element like m[k] = v.
type AssignIndexStmt struct {
	Target Expr
	Index  Expr
	Value  Expr
}

type ForStmt struct {
	Var        string
	Start, End Expr
	Body       []Stmt
	IsMap      bool
}

type IfStmt struct {
	Cond   Expr
	Then   []Stmt
	ElseIf *IfStmt
	Else   []Stmt
}

type IfExpr struct {
	Cond   Expr
	Then   Expr
	ElseIf *IfExpr
	Else   Expr
}

// MultiListComp represents a simple list comprehension with multiple input
// iterators. It is used for query expressions like `[f(a,b) for a in A for b in B]`.
type MultiListComp struct {
	Vars     []string
	Iters    []Expr
	Expr     Expr
	Cond     Expr
	ElemType string
}

// GroupComp represents a query with a grouping step.
type GroupComp struct {
	Vars        []string
	Iters       []Expr
	Cond        Expr
	Key         Expr
	ItemVar     string
	GroupName   string
	GroupStruct string
	Body        Expr
	ElemType    string
	KeyType     string
	ItemType    string
}

// SortComp represents a query with optional sorting and slicing.
type SortComp struct {
	Vars     []string
	Iters    []Expr
	Cond     Expr
	Key      Expr
	Desc     bool
	Skip     Expr
	Take     Expr
	Body     Expr
	ElemType string
	KeyType  string
}

func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	p.write(&buf)
	return buf.Bytes()
}

func (p *Program) write(w io.Writer) {
	v := strings.TrimSpace(version)
	loc := time.FixedZone("GMT+7", 7*3600)
	ts := time.Now().In(loc).Format("2006-01-02 15:04:05 MST")
	fmt.Fprintf(w, "// Generated by Mochi transpiler v%s on %s\n", v, ts)
	for _, inc := range p.Includes {
		fmt.Fprintf(w, "#include %s\n", inc)
	}
	fmt.Fprintln(w)
	for _, st := range p.Structs {
		fmt.Fprintf(w, "struct %s {\n", st.Name)
		for _, f := range st.Fields {
			fmt.Fprintf(w, "    %s %s;\n", f.Type, f.Name)
		}
		if strings.HasSuffix(st.Name, "Group") && len(st.Fields) == 2 && st.Fields[1].Name == "items" {
			fmt.Fprintln(w, "    auto begin() { return items.begin(); }")
			fmt.Fprintln(w, "    auto end() { return items.end(); }")
			fmt.Fprintln(w, "    size_t size() const { return items.size(); }")
		}
		fmt.Fprintln(w, "};")
		fmt.Fprintln(w)
	}
	currentProgram = p
	// emit helper functions first
	first := true
	var mainFn *Func
	for _, fn := range p.Functions {
		if fn.Name == "main" {
			mainFn = fn
			continue
		}
		if !first {
			fmt.Fprintln(w)
		}
		first = false
		fn.emit(w)
	}
	if len(p.Functions) > 1 {
		fmt.Fprintln(w)
	}
	for _, st := range p.Globals {
		st.emit(w, 0)
	}
	if mainFn != nil {
		if len(p.Globals) > 0 {
			fmt.Fprintln(w)
		} else if !first {
			fmt.Fprintln(w)
		}
		mainFn.emit(w)
	}
	currentProgram = nil
}

func (f *Func) emit(w io.Writer) {
	fmt.Fprintf(w, "%s %s(", f.ReturnType, f.Name)
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		typ := p.Type
		if typ == "" {
			io.WriteString(w, "auto ")
		} else {
			io.WriteString(w, typ+" ")
		}
		io.WriteString(w, p.Name)
	}
	fmt.Fprintln(w, ") {")
	for _, st := range f.Body {
		st.emit(w, 1)
	}
	if f.Name == "main" {
		fmt.Fprintln(w, "    return 0;")
	}
	fmt.Fprintln(w, "}")
}

func (s *PrintStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "std::cout << std::boolalpha")
	for i, v := range s.Values {
		io.WriteString(w, " << ")
		if i > 0 {
			io.WriteString(w, " \" \" << ")
		}
		switch ex := v.(type) {
		case *UnaryExpr:
			if ex.Op == "!" {
				io.WriteString(w, "static_cast<int>(")
				ex.emit(w)
				io.WriteString(w, ")")
				continue
			}
			ex.emit(w)
			continue
		case *BinaryExpr:
			if ex.Op == "&&" || ex.Op == "||" {
				io.WriteString(w, "static_cast<int>(")
				ex.emit(w)
				io.WriteString(w, ")")
				continue
			}
			ex.emit(w)
			continue
		case *ListLit:
			if currentProgram != nil {
				currentProgram.addInclude("<sstream>")
			}
			io.WriteString(w, "([&]{ std::ostringstream ss; auto tmp = ")
			ex.emit(w)
			io.WriteString(w, "; for(size_t i=0;i<tmp.size();++i){ if(i>0) ss<<\" \"; ss<<tmp[i]; } return ss.str(); }())")
		case *SliceExpr:
			if currentProgram != nil {
				currentProgram.addInclude("<sstream>")
			}
			io.WriteString(w, "([&]{ auto tmp = ")
			ex.emit(w)
			io.WriteString(w, "; if constexpr(std::is_same_v<std::decay_t<decltype(tmp)>, std::string>) return tmp; std::ostringstream ss; for(size_t i=0;i<tmp.size();++i){ if(i>0) ss<<\" \"; ss<<tmp[i]; } return ss.str(); }())")
		case *AppendExpr:
			if currentProgram != nil {
				currentProgram.addInclude("<sstream>")
			}
			io.WriteString(w, "([&]{ std::ostringstream ss; auto tmp = ")
			ex.emit(w)
			io.WriteString(w, "; for(size_t i=0;i<tmp.size();++i){ if(i>0) ss<<\" \"; ss<<tmp[i]; } return ss.str(); }())")
		case *AvgExpr:
			if currentProgram != nil {
				currentProgram.addInclude("<sstream>")
			}
			io.WriteString(w, "([&]{ std::ostringstream ss; ss<<std::fixed<<std::setprecision(1)<<")
			ex.emit(w)
			io.WriteString(w, "; return ss.str(); }())")
		case *ValuesExpr:
			if currentProgram != nil {
				currentProgram.addInclude("<sstream>")
			}
			io.WriteString(w, "([&]{ std::ostringstream ss; auto tmp = ")
			ex.emit(w)
			io.WriteString(w, "; for(size_t i=0;i<tmp.size();++i){ if(i>0) ss<<\" \"; ss<<tmp[i]; } return ss.str(); }())")
		default:
			v.emit(w)
		}
	}
	io.WriteString(w, " << std::endl;\n")
}

func (wst *WhileStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "while (")
	wst.Cond.emit(w)
	io.WriteString(w, ") {\n")
	for _, st := range wst.Body {
		st.emit(w, indent+1)
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "}\n")
}

func (l *LenExpr) emit(w io.Writer) {
	l.Value.emit(w)
	io.WriteString(w, ".size()")
}

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "std::vector{")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "}")
}

func (m *MapLit) emit(w io.Writer) {
	fmt.Fprintf(w, "std::map<%s, %s>{", m.KeyType, m.ValueType)
	for i := range m.Keys {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, "{")
		m.Keys[i].emit(w)
		io.WriteString(w, ", ")
		m.Values[i].emit(w)
		io.WriteString(w, "}")
	}
	io.WriteString(w, "}")
}

func (s *StringLit) emit(w io.Writer) {
	fmt.Fprintf(w, "std::string(%q)", s.Value)
}

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (s *StructLit) emit(w io.Writer) {
	io.WriteString(w, s.Name)
	io.WriteString(w, "{")
	for i, f := range s.Fields {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		fmt.Fprintf(w, ".%s = ", f.Name)
		f.Value.emit(w)
	}
	io.WriteString(w, "}")
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}

func (s *SelectorExpr) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, s.Field)
}

func (i *IndexExpr) emit(w io.Writer) {
	i.Target.emit(w)
	io.WriteString(w, "[")
	i.Index.emit(w)
	io.WriteString(w, "]")
}

func (s *SliceExpr) emit(w io.Writer) {
	io.WriteString(w, "([&](const auto& c){ if constexpr(std::is_same_v<std::decay_t<decltype(c)>, std::string>) return c.substr(")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, "); else return std::vector<typename std::decay_t<decltype(c)>::value_type>(c.begin()+")
	s.Start.emit(w)
	io.WriteString(w, ", c.begin()+")
	s.End.emit(w)
	io.WriteString(w, "); })(")
	s.Target.emit(w)
	io.WriteString(w, ")")
}

func (c *ContainsExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	c.Value.emit(w)
	io.WriteString(w, ".find(")
	c.Sub.emit(w)
	io.WriteString(w, ") != std::string::npos")
	io.WriteString(w, ")")
}

func (in *InExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<algorithm>")
		currentProgram.addInclude("<type_traits>")
	}
	io.WriteString(w, "([&](const auto& c, const auto& v){ ")
	io.WriteString(w, "if constexpr(std::is_same_v<std::decay_t<decltype(c)>, std::string>) { return c.find(v) != std::string::npos; } ")
	io.WriteString(w, "else if constexpr(requires { c.find(v); }) { return c.find(v) != c.end(); } ")
	io.WriteString(w, "else { return std::find(c.begin(), c.end(), v) != c.end(); } })(")
	in.Coll.emit(w)
	io.WriteString(w, ", ")
	in.Value.emit(w)
	io.WriteString(w, ")")
}

func (s *SumExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<numeric>")
	}
	if lit, ok := s.Arg.(*ListLit); ok {
		io.WriteString(w, "([&]{ auto tmp = ")
		lit.emit(w)
		io.WriteString(w, "; return std::accumulate(tmp.begin(), tmp.end(), 0); })()")
	} else {
		io.WriteString(w, "std::accumulate(")
		s.Arg.emit(w)
		io.WriteString(w, ".begin(), ")
		s.Arg.emit(w)
		io.WriteString(w, ".end(), 0)")
	}
}

func (a *AppendExpr) emit(w io.Writer) {
	io.WriteString(w, "([&]{ auto v = ")
	a.List.emit(w)
	io.WriteString(w, "; v.push_back(")
	a.Elem.emit(w)
	io.WriteString(w, "); return v; }())")
}

func (a *AvgExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<numeric>")
		currentProgram.addInclude("<sstream>")
		currentProgram.addInclude("<iomanip>")
	}
	io.WriteString(w, "([&]{ auto tmp = ")
	a.List.emit(w)
	io.WriteString(w, "; return tmp.empty() ? 0.0 : std::accumulate(tmp.begin(), tmp.end(), 0.0) / tmp.size(); }())")
}

func (s *StrExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<sstream>")
	}
	io.WriteString(w, "([&]{ std::ostringstream ss; ss<<")
	s.Value.emit(w)
	io.WriteString(w, "; return ss.str(); }())")
}

func (v *ValuesExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<vector>")
	}
	io.WriteString(w, "([&]{ std::vector<decltype(")
	v.Map.emit(w)
	io.WriteString(w, ".begin()->second)> vals; for(const auto& __p : ")
	v.Map.emit(w)
	io.WriteString(w, ") vals.push_back(__p.second); return vals; }())")
}

func (m *MinExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<algorithm>")
	}
	io.WriteString(w, "(*std::min_element(")
	m.List.emit(w)
	io.WriteString(w, ".begin(), ")
	m.List.emit(w)
	io.WriteString(w, ".end()))")
}

func (m *MaxExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<algorithm>")
	}
	io.WriteString(w, "(*std::max_element(")
	m.List.emit(w)
	io.WriteString(w, ".begin(), ")
	m.List.emit(w)
	io.WriteString(w, ".end()))")
}

func (c *CastExpr) emit(w io.Writer) {
	if c.Type == "int" {
		io.WriteString(w, "std::stoi(")
		c.Value.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "(")
		io.WriteString(w, c.Type)
		io.WriteString(w, ")(")
		c.Value.emit(w)
		io.WriteString(w, ")")
	}
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Value.emit(w)
	io.WriteString(w, ".substr(")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ")")
}

func (v *VarRef) emit(w io.Writer) { io.WriteString(w, v.Name) }

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Name)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

func (l *LambdaExpr) emit(w io.Writer) {
	io.WriteString(w, "[=](")
	for i, p := range l.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if p.Type == "" {
			io.WriteString(w, "auto ")
		} else {
			io.WriteString(w, p.Type+" ")
		}
		io.WriteString(w, p.Name)
	}
	io.WriteString(w, ") { return ")
	l.Body.emit(w)
	io.WriteString(w, "; }")
}

func (l *BlockLambda) emit(w io.Writer) {
	io.WriteString(w, "[=](")
	for i, p := range l.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if p.Type == "" {
			io.WriteString(w, "auto ")
		} else {
			io.WriteString(w, p.Type+" ")
		}
		io.WriteString(w, p.Name)
	}
	io.WriteString(w, ") {\n")
	for _, st := range l.Body {
		st.emit(w, 1)
	}
	io.WriteString(w, "}")
}

func (lc *MultiListComp) emit(w io.Writer) {
	io.WriteString(w, "([]{ std::vector<"+lc.ElemType+"> __items;\n")
	for i, v := range lc.Vars {
		io.WriteString(w, "for (auto ")
		io.WriteString(w, v)
		io.WriteString(w, " : ")
		lc.Iters[i].emit(w)
		io.WriteString(w, ") {\n")
	}
	if lc.Cond != nil {
		io.WriteString(w, "    if(")
		lc.Cond.emit(w)
		io.WriteString(w, ") {\n")
	}
	io.WriteString(w, "        __items.push_back(")
	lc.Expr.emit(w)
	io.WriteString(w, ");\n")
	if lc.Cond != nil {
		io.WriteString(w, "    }\n")
	}
	for range lc.Vars {
		io.WriteString(w, "}\n")
	}
	io.WriteString(w, "return __items; }())")
}

func (gc *GroupComp) emit(w io.Writer) {
	io.WriteString(w, "([]{ std::vector<"+gc.ElemType+"> __items;\n")
	io.WriteString(w, "std::map<"+gc.KeyType+", std::vector<"+gc.ItemType+">> __groups;\n")
	for i, v := range gc.Vars {
		io.WriteString(w, "for (auto ")
		io.WriteString(w, v)
		io.WriteString(w, " : ")
		gc.Iters[i].emit(w)
		io.WriteString(w, ") {\n")
	}
	if gc.Cond != nil {
		io.WriteString(w, "    if(")
		gc.Cond.emit(w)
		io.WriteString(w, ") {\n")
	}
	io.WriteString(w, "        __groups[")
	gc.Key.emit(w)
	io.WriteString(w, "].push_back(")
	io.WriteString(w, gc.ItemVar)
	io.WriteString(w, ");\n")
	if gc.Cond != nil {
		io.WriteString(w, "    }\n")
	}
	for range gc.Vars {
		io.WriteString(w, "}\n")
	}
	io.WriteString(w, "for(auto &__kv : __groups) {\n")
	io.WriteString(w, "    ")
	io.WriteString(w, gc.GroupStruct+" "+gc.GroupName+"{__kv.first, __kv.second};\n")
	io.WriteString(w, "    __items.push_back(")
	gc.Body.emit(w)
	io.WriteString(w, ");\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "return __items; }())")
}

func (sc *SortComp) emit(w io.Writer) {
	io.WriteString(w, "([]{ std::vector<std::pair<"+sc.KeyType+", "+sc.ElemType+">> __tmp;\n")
	for i, v := range sc.Vars {
		io.WriteString(w, "for (auto ")
		io.WriteString(w, v)
		io.WriteString(w, " : ")
		sc.Iters[i].emit(w)
		io.WriteString(w, ") {\n")
	}
	if sc.Cond != nil {
		io.WriteString(w, "    if(")
		sc.Cond.emit(w)
		io.WriteString(w, ") {\n")
	}
	io.WriteString(w, "        __tmp.emplace_back(")
	if sc.Key != nil {
		sc.Key.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, ", ")
	sc.Body.emit(w)
	io.WriteString(w, ");\n")
	if sc.Cond != nil {
		io.WriteString(w, "    }\n")
	}
	for range sc.Vars {
		io.WriteString(w, "}\n")
	}
	io.WriteString(w, "std::sort(__tmp.begin(), __tmp.end(), [](const auto& a,const auto& b){ return a.first ")
	if sc.Desc {
		io.WriteString(w, ">")
	} else {
		io.WriteString(w, "<")
	}
	io.WriteString(w, " b.first; });\n")
	io.WriteString(w, "std::vector<"+sc.ElemType+"> __items;\n")
	if sc.Skip != nil {
		io.WriteString(w, "auto __skip = ")
		sc.Skip.emit(w)
		io.WriteString(w, ";\n")
	}
	if sc.Take != nil {
		io.WriteString(w, "auto __take = ")
		sc.Take.emit(w)
		io.WriteString(w, ";\n")
	}
	io.WriteString(w, "for(size_t __i=0; __i<__tmp.size(); ++__i){\n")
	if sc.Skip != nil {
		io.WriteString(w, "    if(__i < static_cast<size_t>(__skip)) continue;\n")
	}
	if sc.Take != nil {
		io.WriteString(w, "    if(__items.size() >= static_cast<size_t>(__take)) break;\n")
	}
	io.WriteString(w, "    __items.push_back(__tmp[__i].second);\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "return __items; }())")
}

func (e *ExistsExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<vector>")
	}
	io.WriteString(w, "(!")
	e.List.emit(w)
	io.WriteString(w, ".empty())")
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "/" {
		io.WriteString(w, "((double)(")
		b.Left.emit(w)
		io.WriteString(w, ") / (")
		b.Right.emit(w)
		io.WriteString(w, "))")
		return
	}
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " "+b.Op+" ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

func (s *LetStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	typ := s.Type
	if typ == "" {
		io.WriteString(w, "auto ")
	} else {
		io.WriteString(w, typ+" ")
	}
	io.WriteString(w, s.Name)
	if s.Value != nil {
		io.WriteString(w, " = ")
		s.Value.emit(w)
	} else if typ != "" {
		io.WriteString(w, " = ")
		io.WriteString(w, defaultValueForType(typ))
	}
	io.WriteString(w, ";\n")
}

func (a *AssignStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, a.Name)
	io.WriteString(w, " = ")
	a.Value.emit(w)
	io.WriteString(w, ";\n")
}

func (a *AssignIndexStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	a.Target.emit(w)
	io.WriteString(w, "[")
	a.Index.emit(w)
	io.WriteString(w, "] = ")
	a.Value.emit(w)
	io.WriteString(w, ";\n")
}

func (r *ReturnStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "return")
	if r.Value != nil {
		io.WriteString(w, " ")
		r.Value.emit(w)
	}
	io.WriteString(w, ";\n")
}

func (b *BreakStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "break;\n")
}

func (c *ContinueStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "continue;\n")
}

func (f *ForStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	if f.End == nil {
		if f.IsMap {
			io.WriteString(w, "for (const auto& __p : ")
			f.Start.emit(w)
			io.WriteString(w, ") {\n")
			for i := 0; i < indent+1; i++ {
				io.WriteString(w, "    ")
			}
			io.WriteString(w, "auto ")
			io.WriteString(w, f.Var)
			io.WriteString(w, " = __p.first;\n")
		} else {
			io.WriteString(w, "for (auto ")
			io.WriteString(w, f.Var)
			io.WriteString(w, " : ")
			f.Start.emit(w)
			io.WriteString(w, ") {\n")
		}
	} else {
		io.WriteString(w, "for (int ")
		io.WriteString(w, f.Var)
		io.WriteString(w, " = ")
		f.Start.emit(w)
		io.WriteString(w, "; ")
		io.WriteString(w, f.Var)
		io.WriteString(w, " < ")
		f.End.emit(w)
		io.WriteString(w, "; ")
		io.WriteString(w, f.Var)
		io.WriteString(w, "++ ) {\n")
	}
	for _, st := range f.Body {
		st.emit(w, indent+1)
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "}\n")
}

func (i *IfStmt) emit(w io.Writer, indent int) {
	for j := 0; j < indent; j++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "if (")
	i.Cond.emit(w)
	io.WriteString(w, ") {\n")
	for _, st := range i.Then {
		st.emit(w, indent+1)
	}
	for j := 0; j < indent; j++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "}")
	if i.ElseIf != nil {
		io.WriteString(w, " else ")
		i.ElseIf.emit(w, indent)
	} else if len(i.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, st := range i.Else {
			st.emit(w, indent+1)
		}
		for j := 0; j < indent; j++ {
			io.WriteString(w, "    ")
		}
		io.WriteString(w, "}")
	}
	io.WriteString(w, "\n")
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	i.Cond.emit(w)
	io.WriteString(w, " ? ")
	i.Then.emit(w)
	io.WriteString(w, " : ")
	if i.ElseIf != nil {
		i.ElseIf.emit(w)
	} else if i.Else != nil {
		i.Else.emit(w)
	}
	io.WriteString(w, ")")
}

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	cp := &Program{Includes: []string{"<iostream>", "<string>"}, ListTypes: map[string]string{}}
	currentProgram = cp
	currentEnv = env
	defer func() { currentProgram = nil; currentEnv = nil }()
	var body []Stmt
	var globals []Stmt
	for _, stmt := range prog.Statements {
		switch {
		case stmt.Fun != nil:
			fn, err := convertFun(stmt.Fun)
			if err != nil {
				return nil, err
			}
			cp.Functions = append(cp.Functions, fn)
		case stmt.Expr != nil:
			if call := extractCall(stmt.Expr.Expr); call != nil && call.Func == "print" {
				var args []Expr
				for _, a := range call.Args {
					ce, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args = append(args, ce)
				}
				body = append(body, &PrintStmt{Values: args})
			} else {
				return nil, fmt.Errorf("unsupported expression")
			}
		case stmt.Let != nil:
			var val Expr
			var err error
			if stmt.Let.Value != nil {
				if q := extractQuery(stmt.Let.Value); q != nil {
					var def *StructDef
					val, def, _, err = convertSimpleQuery(q, stmt.Let.Name)
					if err != nil {
						return nil, err
					}
					if def != nil {
						cp.Structs = append(cp.Structs, *def)
					}
				} else {
					val, err = convertExpr(stmt.Let.Value)
				}
				if err != nil {
					return nil, err
				}
			}
			typ := ""
			if stmt.Let.Type != nil && stmt.Let.Type.Simple != nil {
				typ = cppType(*stmt.Let.Type.Simple)
			}
			if typ == "" {
				if lst, ok := val.(*ListLit); ok {
					if def, sname, ok := inferStructFromList(stmt.Let.Name, lst); ok {
						cp.Structs = append(cp.Structs, *def)
						typ = fmt.Sprintf("std::vector<%s>", sname)
					}
				} else if comp, ok := val.(*MultiListComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", comp.ElemType)
				} else if scomp, ok := val.(*SortComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", scomp.ElemType)
				} else if gcomp, ok := val.(*GroupComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", gcomp.ElemType)
				}
			}
			globals = append(globals, &LetStmt{Name: stmt.Let.Name, Type: typ, Value: val})
		case stmt.Var != nil:
			var val Expr
			var err error
			if stmt.Var.Value != nil {
				if q := extractQuery(stmt.Var.Value); q != nil {
					var def *StructDef
					val, def, _, err = convertSimpleQuery(q, stmt.Var.Name)
					if err != nil {
						return nil, err
					}
					if def != nil {
						cp.Structs = append(cp.Structs, *def)
					}
				} else {
					val, err = convertExpr(stmt.Var.Value)
				}
				if err != nil {
					return nil, err
				}
			}
			typ := ""
			if stmt.Var.Type != nil && stmt.Var.Type.Simple != nil {
				typ = cppType(*stmt.Var.Type.Simple)
			}
			if typ == "" {
				if lst, ok := val.(*ListLit); ok {
					if def, sname, ok := inferStructFromList(stmt.Var.Name, lst); ok {
						cp.Structs = append(cp.Structs, *def)
						typ = fmt.Sprintf("std::vector<%s>", sname)
					}
				} else if comp, ok := val.(*MultiListComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", comp.ElemType)
				} else if scomp, ok := val.(*SortComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", scomp.ElemType)
				} else if gcomp, ok := val.(*GroupComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", gcomp.ElemType)
				}
			}
			globals = append(globals, &LetStmt{Name: stmt.Var.Name, Type: typ, Value: val})
		case stmt.Type != nil:
			st, err := convertTypeDecl(stmt.Type)
			if err != nil {
				return nil, err
			}
			cp.Structs = append(cp.Structs, *st)
		case stmt.Assign != nil:
			val, err := convertExpr(stmt.Assign.Value)
			if err != nil {
				return nil, err
			}
			if len(stmt.Assign.Index) > 0 {
				parts := stmt.Assign.Index
				if parts[len(parts)-1].Colon != nil {
					return nil, fmt.Errorf("unsupported index assignment")
				}
				idx, err := convertExpr(parts[len(parts)-1].Start)
				if err != nil {
					return nil, err
				}
				var target Expr = &VarRef{Name: stmt.Assign.Name}
				for _, sp := range parts[:len(parts)-1] {
					if sp.Colon != nil {
						return nil, fmt.Errorf("unsupported index assignment")
					}
					id, err := convertExpr(sp.Start)
					if err != nil {
						return nil, err
					}
					target = &IndexExpr{Target: target, Index: id}
				}
				body = append(body, &AssignIndexStmt{Target: target, Index: idx, Value: val})
			} else {
				body = append(body, &AssignStmt{Name: stmt.Assign.Name, Value: val})
			}
		case stmt.For != nil:
			start, err := convertExpr(stmt.For.Source)
			if err != nil {
				return nil, err
			}
			var end Expr
			if stmt.For.RangeEnd != nil {
				end, err = convertExpr(stmt.For.RangeEnd)
				if err != nil {
					return nil, err
				}
			}
			fs := &ForStmt{Var: stmt.For.Name, Start: start, End: end}
			if currentEnv != nil {
				if t := types.TypeOfExpr(stmt.For.Source, currentEnv); t != nil {
					if _, ok := t.(types.MapType); ok {
						fs.IsMap = true
					}
				}
			}
			for _, s := range stmt.For.Body {
				st, err := convertStmt(s)
				if err != nil {
					return nil, err
				}
				fs.Body = append(fs.Body, st)
			}
			body = append(body, fs)
		case stmt.While != nil:
			ws, err := convertWhileStmt(stmt.While)
			if err != nil {
				return nil, err
			}
			body = append(body, ws)
		case stmt.If != nil:
			ifs, err := convertIfStmt(stmt.If)
			if err != nil {
				return nil, err
			}
			body = append(body, ifs)
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	cp.Globals = globals
	cp.Functions = append(cp.Functions, &Func{Name: "main", ReturnType: "int", Body: body})
	return cp, nil
}

func extractCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Call == nil {
		return nil
	}
	return u.Value.Target.Call
}

func extractQuery(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Query == nil {
		return nil
	}
	return u.Value.Target.Query
}

func convertStmt(s *parser.Statement) (Stmt, error) {
	switch {
	case s.Fun != nil:
		lam, err := convertFunLambda(s.Fun)
		if err != nil {
			return nil, err
		}
		return &LetStmt{Name: s.Fun.Name, Type: "", Value: lam}, nil
	case s.Expr != nil:
		if call := extractCall(s.Expr.Expr); call != nil && call.Func == "print" {
			var args []Expr
			for _, a := range call.Args {
				ce, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ce)
			}
			return &PrintStmt{Values: args}, nil
		}
	case s.Let != nil:
		var val Expr
		var err error
		if s.Let.Value != nil {
			val, err = convertExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
		}
		typ := ""
		if s.Let.Type != nil && s.Let.Type.Simple != nil {
			typ = cppType(*s.Let.Type.Simple)
		} else if s.Let.Value != nil {
			typ = guessType(s.Let.Value)
		}
		return &LetStmt{Name: s.Let.Name, Type: typ, Value: val}, nil
	case s.Var != nil:
		var val Expr
		var err error
		if s.Var.Value != nil {
			val, err = convertExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
		}
		typ := ""
		if s.Var.Type != nil && s.Var.Type.Simple != nil {
			typ = cppType(*s.Var.Type.Simple)
		} else if s.Var.Value != nil {
			typ = guessType(s.Var.Value)
		}
		return &LetStmt{Name: s.Var.Name, Type: typ, Value: val}, nil
	case s.Assign != nil:
		val, err := convertExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		if len(s.Assign.Index) > 0 {
			parts := s.Assign.Index
			if parts[len(parts)-1].Colon != nil {
				return nil, fmt.Errorf("unsupported index assignment")
			}
			idx, err := convertExpr(parts[len(parts)-1].Start)
			if err != nil {
				return nil, err
			}
			var target Expr = &VarRef{Name: s.Assign.Name}
			for _, sp := range parts[:len(parts)-1] {
				if sp.Colon != nil {
					return nil, fmt.Errorf("unsupported index assignment")
				}
				id, err := convertExpr(sp.Start)
				if err != nil {
					return nil, err
				}
				target = &IndexExpr{Target: target, Index: id}
			}
			return &AssignIndexStmt{Target: target, Index: idx, Value: val}, nil
		}
		return &AssignStmt{Name: s.Assign.Name, Value: val}, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Return != nil:
		var val Expr
		if s.Return.Value != nil {
			var err error
			val, err = convertExpr(s.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: val}, nil
	case s.If != nil:
		return convertIfStmt(s.If)
	case s.While != nil:
		return convertWhileStmt(s.While)
	}
	return nil, fmt.Errorf("unsupported statement")
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	left, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	if len(b.Right) == 0 {
		return left, nil
	}
	expr := left
	for _, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		if op.Op == "in" {
			if currentProgram != nil {
				currentProgram.addInclude("<algorithm>")
				currentProgram.addInclude("<type_traits>")
			}
			expr = &InExpr{Value: expr, Coll: right}
		} else {
			expr = &BinaryExpr{Left: expr, Op: op.Op, Right: right}
		}
	}
	return expr, nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		expr = &UnaryExpr{Op: op, Expr: expr}
	}
	return expr, nil
}

func convertPostfix(p *parser.PostfixExpr) (Expr, error) {
	expr, err := convertPrimary(p.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil {
				if op.Index.Colon2 != nil || op.Index.Step != nil {
					return nil, fmt.Errorf("slice not supported")
				}
				start, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				end, err := convertExpr(op.Index.End)
				if err != nil {
					return nil, err
				}
				if currentProgram != nil {
					currentProgram.addInclude("<vector>")
					currentProgram.addInclude("<type_traits>")
				}
				expr = &SliceExpr{Target: expr, Start: start, End: end}
			} else if op.Index.Colon2 != nil || op.Index.End != nil || op.Index.Step != nil {
				return nil, fmt.Errorf("slice not supported")
			} else {
				idx, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				expr = &IndexExpr{Target: expr, Index: idx}
			}
		case op.Call != nil:
			var args []Expr
			for _, a := range op.Call.Args {
				ce, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ce)
			}
			if sel, ok := expr.(*SelectorExpr); ok && sel.Field == "contains" && len(args) == 1 {
				expr = &ContainsExpr{Value: sel.Target, Sub: args[0]}
			} else if vr, ok := expr.(*VarRef); ok {
				expr = &CallExpr{Name: vr.Name, Args: args}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			name := *op.Cast.Type.Simple
			if currentEnv != nil {
				if st, ok := currentEnv.GetStruct(name); ok {
					if ml, ok2 := expr.(*MapLit); ok2 {
						fields := make([]FieldLit, len(st.Order))
						for i, fname := range st.Order {
							for j, k := range ml.Keys {
								if sl, ok3 := k.(*StringLit); ok3 && sl.Value == fname {
									fields[i] = FieldLit{Name: fname, Value: ml.Values[j]}
									break
								}
							}
						}
						expr = &StructLit{Name: st.Name, Fields: fields}
					}
				}
			}
			typ := cppType(name)
			expr = &CastExpr{Value: expr, Type: typ}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertIfStmt(is *parser.IfStmt) (*IfStmt, error) {
	cond, err := convertExpr(is.Cond)
	if err != nil {
		return nil, err
	}
	var then []Stmt
	for _, st := range is.Then {
		cs, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		then = append(then, cs)
	}
	var elseStmts []Stmt
	if is.Else != nil {
		for _, st := range is.Else {
			cs, err := convertStmt(st)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, cs)
		}
	}
	var elseIf *IfStmt
	if is.ElseIf != nil {
		ei, err := convertIfStmt(is.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = ei
	}
	return &IfStmt{Cond: cond, Then: then, ElseIf: elseIf, Else: elseStmts}, nil
}

func convertWhileStmt(ws *parser.WhileStmt) (*WhileStmt, error) {
	cond, err := convertExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, st := range ws.Body {
		cs, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		body = append(body, cs)
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertFun(fn *parser.FunStmt) (*Func, error) {
	var body []Stmt
	for _, st := range fn.Body {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	var params []Param
	for _, p := range fn.Params {
		typ := ""
		if p.Type != nil && p.Type.Simple != nil {
			typ = cppType(*p.Type.Simple)
		}
		params = append(params, Param{Name: p.Name, Type: typ})
	}
	ret := "int"
	if fn.Return == nil {
		ret = "void"
	} else if fn.Return.Simple != nil {
		ret = cppType(*fn.Return.Simple)
	} else {
		// for function, struct or generic return types use auto to
		// allow the compiler to deduce the concrete closure type
		ret = "auto"
	}
	return &Func{Name: fn.Name, Params: params, ReturnType: ret, Body: body}, nil
}

func convertFunLambda(fn *parser.FunStmt) (*BlockLambda, error) {
	var body []Stmt
	for _, st := range fn.Body {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	var params []Param
	for _, p := range fn.Params {
		typ := ""
		if p.Type != nil && p.Type.Simple != nil {
			typ = cppType(*p.Type.Simple)
		}
		params = append(params, Param{Name: p.Name, Type: typ})
	}
	return &BlockLambda{Params: params, Body: body}, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Call != nil:
		switch p.Call.Func {
		case "len":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				return &LenExpr{Value: arg}, nil
			}
		case "sum":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				if currentProgram != nil {
					currentProgram.addInclude("<numeric>")
				}
				return &SumExpr{Arg: arg}, nil
			}
		case "append":
			if len(p.Call.Args) == 2 {
				l0, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				l1, err := convertExpr(p.Call.Args[1])
				if err != nil {
					return nil, err
				}
				return &AppendExpr{List: l0, Elem: l1}, nil
			}
		case "avg":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				if currentProgram != nil {
					currentProgram.addInclude("<numeric>")
					currentProgram.addInclude("<sstream>")
					currentProgram.addInclude("<iomanip>")
				}
				return &AvgExpr{List: arg}, nil
			}
		case "str":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				if currentProgram != nil {
					currentProgram.addInclude("<sstream>")
				}
				return &StrExpr{Value: arg}, nil
			}
		case "count":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				return &LenExpr{Value: arg}, nil
			}
		case "values":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				if currentProgram != nil {
					currentProgram.addInclude("<vector>")
					currentProgram.addInclude("<sstream>")
				}
				return &ValuesExpr{Map: arg}, nil
			}
		case "min":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				if currentProgram != nil {
					currentProgram.addInclude("<algorithm>")
				}
				return &MinExpr{List: arg}, nil
			}
		case "max":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				if currentProgram != nil {
					currentProgram.addInclude("<algorithm>")
				}
				return &MaxExpr{List: arg}, nil
			}
		case "substring":
			if len(p.Call.Args) == 3 {
				v0, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				v1, err := convertExpr(p.Call.Args[1])
				if err != nil {
					return nil, err
				}
				v2, err := convertExpr(p.Call.Args[2])
				if err != nil {
					return nil, err
				}
				return &SubstringExpr{Value: v0, Start: v1, End: v2}, nil
			}
		case "exists":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				return &ExistsExpr{List: arg}, nil
			}
		}
		var args []Expr
		for _, a := range p.Call.Args {
			ce, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ce)
		}
		return &CallExpr{Name: p.Call.Func, Args: args}, nil
	case p.Selector != nil:
		expr := Expr(&VarRef{Name: p.Selector.Root})
		for _, f := range p.Selector.Tail {
			expr = &SelectorExpr{Target: expr, Field: f}
		}
		return expr, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Struct != nil:
		var fields []FieldLit
		for _, f := range p.Struct.Fields {
			val, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			fields = append(fields, FieldLit{Name: f.Name, Value: val})
		}
		return &StructLit{Name: p.Struct.Name, Fields: fields}, nil
	case p.List != nil:
		if currentProgram != nil {
			currentProgram.addInclude("<vector>")
		}
		var elems []Expr
		for _, e := range p.List.Elems {
			ce, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ce)
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		if currentProgram != nil {
			currentProgram.addInclude("<map>")
		}
		if len(p.Map.Items) == 0 {
			return &MapLit{KeyType: "auto", ValueType: "auto"}, nil
		}
		kt := guessType(p.Map.Items[0].Key)
		vt := guessType(p.Map.Items[0].Value)
		keys := make([]Expr, len(p.Map.Items))
		vals := make([]Expr, len(p.Map.Items))
		for i, it := range p.Map.Items {
			ke, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			ve, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			keys[i] = ke
			vals[i] = ve
		}
		return &MapLit{Keys: keys, Values: vals, KeyType: kt, ValueType: vt}, nil
	case p.Query != nil:
		expr, _, _, err := convertSimpleQuery(p.Query, "tmp")
		return expr, err
	case p.FunExpr != nil && p.FunExpr.ExprBody != nil:
		var params []Param
		for _, pa := range p.FunExpr.Params {
			typ := ""
			if pa.Type != nil && pa.Type.Simple != nil {
				typ = cppType(*pa.Type.Simple)
			}
			params = append(params, Param{Name: pa.Name, Type: typ})
		}
		body, err := convertExpr(p.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Body: body}, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Int != nil:
		return &IntLit{Value: int(*l.Int)}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

func convertIfExpr(ie *parser.IfExpr) (*IfExpr, error) {
	cond, err := convertExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	var elseIf *IfExpr
	if ie.ElseIf != nil {
		ei, err := convertIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = ei
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, ElseIf: elseIf, Else: elseExpr}, nil
}

func convertSimpleQuery(q *parser.QueryExpr, target string) (Expr, *StructDef, string, error) {
	if q.Distinct {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	vars := []string{q.Var}
	iters := []Expr{}
	qTypes := map[string]string{}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, nil, "", err
	}
	iters = append(iters, src)
	if vr, ok := src.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			qTypes[q.Var] = t
		} else {
			et := elementTypeFromListType(guessType(q.Source))
			qTypes[q.Var] = et
		}
	}
	for _, f := range q.Froms {
		e, err := convertExpr(f.Src)
		if err != nil {
			return nil, nil, "", err
		}
		vars = append(vars, f.Var)
		iters = append(iters, e)
		if vr, ok := e.(*VarRef); ok {
			if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
				qTypes[f.Var] = t
			} else {
				et := elementTypeFromListType(guessType(f.Src))
				qTypes[f.Var] = et
			}
		}
	}
	joinConds := []Expr{}
	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, nil, "", fmt.Errorf("unsupported join side")
		}
		js, err := convertExpr(j.Src)
		if err != nil {
			return nil, nil, "", err
		}
		vars = append(vars, j.Var)
		iters = append(iters, js)
		if vr, ok := js.(*VarRef); ok {
			if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
				qTypes[j.Var] = t
			} else {
				et := elementTypeFromListType(guessType(j.Src))
				qTypes[j.Var] = et
			}
		}
		jc, err := convertExpr(j.On)
		if err != nil {
			return nil, nil, "", err
		}
		joinConds = append(joinConds, jc)
	}
	var cond Expr
	oldTypes := localTypes
	localTypes = qTypes
	defer func() { localTypes = oldTypes }()

	if q.Where != nil {
		cond, err = convertExpr(q.Where)
		if err != nil {
			return nil, nil, "", err
		}
	}
	for _, jc := range joinConds {
		if cond == nil {
			cond = jc
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: jc}
		}
	}
	body, err := convertExpr(q.Select)
	if err != nil {
		return nil, nil, "", err
	}
	elemType := guessType(q.Select)
	var def *StructDef
	if ml, ok := body.(*MapLit); ok {
		if keys := make([]string, len(ml.Keys)); true {
			for i, k := range ml.Keys {
				n, ok := keyName(k)
				if !ok {
					keys = nil
					break
				}
				keys[i] = n
			}
			if keys != nil {
				structName := strings.Title(target) + "Item"
				fields := make([]Param, len(keys))
				flds := make([]FieldLit, len(keys))
				for i, k := range keys {
					typ := exprType(ml.Values[i])
					if sel, ok := ml.Values[i].(*SelectorExpr); ok {
						if vr, ok2 := sel.Target.(*VarRef); ok2 {
							if s, ok3 := qTypes[vr.Name]; ok3 {
								typ = structFieldType(s, sel.Field)
							}
						}
					} else if vr, ok := ml.Values[i].(*VarRef); ok {
						if t, ok2 := qTypes[vr.Name]; ok2 {
							typ = t
						}
					}
					fields[i] = Param{Name: k, Type: typ}
					flds[i] = FieldLit{Name: k, Value: ml.Values[i]}
				}
				body = &StructLit{Name: structName, Fields: flds}
				def = &StructDef{Name: structName, Fields: fields}
				elemType = structName
			}
		}
	}
	if q.Group != nil {
		if len(q.Group.Exprs) != 1 {
			return nil, nil, "", fmt.Errorf("unsupported group")
		}
		keyExpr, err := convertExpr(q.Group.Exprs[0])
		if err != nil {
			return nil, nil, "", err
		}
		keyType := exprType(keyExpr)
		itemVar := vars[len(vars)-1]
		itemType := qTypes[itemVar]
		structName := strings.Title(q.Group.Name) + "Group"
		gdef := &StructDef{Name: structName, Fields: []Param{{Name: "key", Type: keyType}, {Name: "items", Type: fmt.Sprintf("std::vector<%s>", itemType)}}}
		if currentProgram != nil {
			currentProgram.Structs = append(currentProgram.Structs, *gdef)
		}
		qTypes[q.Group.Name] = structName
		body, err = convertExpr(q.Select)
		if err != nil {
			return nil, nil, "", err
		}
		elemType = guessType(q.Select)
		if ml, ok := body.(*MapLit); ok {
			if keys := make([]string, len(ml.Keys)); true {
				for i, k := range ml.Keys {
					n, ok := keyName(k)
					if !ok {
						keys = nil
						break
					}
					keys[i] = n
				}
				if keys != nil {
					structName2 := strings.Title(target) + "Item"
					fields2 := make([]Param, len(keys))
					flds2 := make([]FieldLit, len(keys))
					for i, k := range keys {
						typ := exprType(ml.Values[i])
						if sel, ok := ml.Values[i].(*SelectorExpr); ok {
							if vr, ok2 := sel.Target.(*VarRef); ok2 {
								if s, ok3 := qTypes[vr.Name]; ok3 {
									typ = structFieldType(s, sel.Field)
								}
							}
						} else if vr, ok := ml.Values[i].(*VarRef); ok {
							if t, ok2 := qTypes[vr.Name]; ok2 {
								typ = t
							}
						}
						fields2[i] = Param{Name: k, Type: typ}
						flds2[i] = FieldLit{Name: k, Value: ml.Values[i]}
					}
					body = &StructLit{Name: structName2, Fields: flds2}
					def = &StructDef{Name: structName2, Fields: fields2}
					elemType = structName2
				}
			}
		}
		if currentProgram != nil {
			currentProgram.addInclude("<vector>")
			currentProgram.addInclude("<map>")
		}
		return &GroupComp{Vars: vars, Iters: iters, Cond: cond, Key: keyExpr, ItemVar: itemVar, GroupName: q.Group.Name, GroupStruct: structName, Body: body, ElemType: elemType, KeyType: keyType, ItemType: itemType}, def, elemType, nil
	}
	if currentProgram != nil {
		currentProgram.addInclude("<vector>")
		if q.Sort != nil {
			currentProgram.addInclude("<algorithm>")
		}
	}
	if q.Sort != nil || q.Skip != nil || q.Take != nil {
		var keyExpr Expr
		var keyType string = "int"
		var desc bool
		var err error
		if q.Sort != nil {
			keyExpr, err = convertExpr(q.Sort)
			if err != nil {
				return nil, nil, "", err
			}
			if u, ok := keyExpr.(*UnaryExpr); ok && u.Op == "-" {
				keyExpr = u.Expr
				desc = true
			}
			keyType = exprType(keyExpr)
		}
		var skipExpr, takeExpr Expr
		if q.Skip != nil {
			skipExpr, err = convertExpr(q.Skip)
			if err != nil {
				return nil, nil, "", err
			}
		}
		if q.Take != nil {
			takeExpr, err = convertExpr(q.Take)
			if err != nil {
				return nil, nil, "", err
			}
		}
		return &SortComp{Vars: vars, Iters: iters, Cond: cond, Key: keyExpr, Desc: desc, Skip: skipExpr, Take: takeExpr, Body: body, ElemType: elemType, KeyType: keyType}, def, elemType, nil
	}
	return &MultiListComp{Vars: vars, Iters: iters, Expr: body, Cond: cond, ElemType: elemType}, def, elemType, nil
}

func cppType(t string) string {
	switch t {
	case "int":
		return "int"
	case "float":
		return "double"
	case "bool":
		return "bool"
	case "string":
		return "std::string"
	}
	if currentEnv != nil {
		if st, ok := currentEnv.GetStruct(t); ok {
			return st.Name
		}
	}
	return "auto"
}

func cppTypeFrom(tp types.Type) string {
	switch t := tp.(type) {
	case types.IntType:
		return "int"
	case types.Int64Type:
		return "int64_t"
	case types.FloatType:
		return "double"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "std::string"
	case types.ListType:
		return fmt.Sprintf("std::vector<%s>", cppTypeFrom(t.Elem))
	case types.MapType:
		return fmt.Sprintf("std::map<%s, %s>", cppTypeFrom(t.Key), cppTypeFrom(t.Value))
	case types.StructType:
		return t.Name
	default:
		return "auto"
	}
}

func guessType(e *parser.Expr) string {
	if e == nil {
		return "auto"
	}
	if currentEnv != nil {
		typ := types.TypeOfExpr(e, currentEnv)
		if typ != nil {
			if _, ok := typ.(types.AnyType); !ok {
				return cppTypeFrom(typ)
			}
		}
	}
	if e.Binary != nil && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		pf := e.Binary.Left.Value
		if sel := pf.Target.Selector; sel != nil && len(sel.Tail) == 0 {
			if t, ok := localTypes[sel.Root]; ok {
				return t
			}
		}
	}
	if types.IsStringExpr(e, currentEnv) {
		return "std::string"
	}
	if types.IsBoolExpr(e, currentEnv) {
		return "bool"
	}
	if types.IsFloatExpr(e, currentEnv) {
		return "double"
	}
	if types.IsListExpr(e, currentEnv) {
		if e.Binary != nil && e.Binary.Left != nil && e.Binary.Left.Value != nil {
			if list := e.Binary.Left.Value.Target.List; list != nil && len(list.Elems) > 0 {
				et := guessType(list.Elems[0])
				return fmt.Sprintf("std::vector<%s>", et)
			}
		}
		return "std::vector<auto>"
	}
	if types.IsMapExpr(e, currentEnv) {
		if e.Binary != nil && e.Binary.Left != nil && e.Binary.Left.Value != nil {
			if mp := e.Binary.Left.Value.Target.Map; mp != nil && len(mp.Items) > 0 {
				kt := guessType(mp.Items[0].Key)
				vt := guessType(mp.Items[0].Value)
				return fmt.Sprintf("std::map<%s, %s>", kt, vt)
			}
		}
		return "std::map<auto, auto>"
	}
	if e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return "auto"
	}
	pf := e.Binary.Left.Value
	if pf.Target.Struct != nil {
		return pf.Target.Struct.Name
	}
	if lit := pf.Target.Lit; lit != nil {
		if lit.Int != nil {
			return "int"
		}
		if lit.Bool != nil {
			return "bool"
		}
		if lit.Str != nil {
			return "std::string"
		}
	}
	if list := pf.Target.List; list != nil && len(list.Elems) > 0 {
		et := guessType(list.Elems[0])
		return fmt.Sprintf("std::vector<%s>", et)
	}
	if mp := pf.Target.Map; mp != nil && len(mp.Items) > 0 {
		kt := guessType(mp.Items[0].Key)
		vt := guessType(mp.Items[0].Value)
		return fmt.Sprintf("std::map<%s, %s>", kt, vt)
	}
	return "auto"
}

func elementTypeFromListType(t string) string {
	if strings.HasPrefix(t, "std::vector<") && strings.HasSuffix(t, ">") {
		return strings.TrimSuffix(strings.TrimPrefix(t, "std::vector<"), ">")
	}
	return "auto"
}

func defaultValueForType(t string) string {
	switch t {
	case "int", "double":
		return "0"
	case "bool":
		return "false"
	case "std::string":
		return "\"\""
	}
	return "{}"
}

func exprType(e Expr) string {
	switch v := e.(type) {
	case *IntLit:
		return "int"
	case *BoolLit:
		return "bool"
	case *StringLit:
		return "std::string"
	case *VarRef:
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(v.Name); err == nil {
				return cppTypeFrom(t)
			}
		}
		if t, ok := localTypes[v.Name]; ok {
			return t
		}
		return "auto"
	case *StructLit:
		return v.Name
	case *ListLit:
		if len(v.Elems) > 0 {
			return fmt.Sprintf("std::vector<%s>", exprType(v.Elems[0]))
		}
		return "std::vector<auto>"
	case *MapLit:
		if len(v.Keys) > 0 {
			return fmt.Sprintf("std::map<%s, %s>", exprType(v.Keys[0]), exprType(v.Values[0]))
		}
		return "std::map<auto, auto>"
	case *SelectorExpr:
		if vr, ok := v.Target.(*VarRef); ok {
			if t, ok2 := localTypes[vr.Name]; ok2 {
				ft := structFieldType(t, v.Field)
				if ft != "" {
					return ft
				}
			}
		}
		return "auto"
	case *UnaryExpr:
		if v.Op == "!" {
			return "bool"
		}
		return exprType(v.Expr)
	case *BinaryExpr:
		switch v.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return "bool"
		case "+", "-", "*", "/", "%":
			lt := exprType(v.Left)
			rt := exprType(v.Right)
			if lt == "double" || rt == "double" {
				return "double"
			}
			if lt == rt {
				return lt
			}
			if lt == "int" && rt == "int" {
				return "int"
			}
			return "auto"
		default:
			return "auto"
		}
	case *IfExpr:
		t := exprType(v.Then)
		e2 := exprType(v.Else)
		if t == e2 {
			return t
		}
		return "auto"
	case *LenExpr:
		return "int"
	case *AvgExpr:
		return "double"
	case *MultiListComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *GroupComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *SortComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *ExistsExpr:
		return "bool"
	}
	return "auto"
}

func keyName(e Expr) (string, bool) {
	switch k := e.(type) {
	case *StringLit:
		return k.Value, true
	case *VarRef:
		return k.Name, true
	}
	return "", false
}

func inferStructFromList(name string, l *ListLit) (*StructDef, string, bool) {
	if len(l.Elems) == 0 {
		return nil, "", false
	}
	first, ok := l.Elems[0].(*MapLit)
	if !ok {
		return nil, "", false
	}
	keys := make([]string, len(first.Keys))
	keyIndex := map[string]int{}
	for i, k := range first.Keys {
		n, ok := keyName(k)
		if !ok {
			return nil, "", false
		}
		keys[i] = n
		keyIndex[n] = i
	}
	for _, e := range l.Elems[1:] {
		m, ok := e.(*MapLit)
		if !ok || len(m.Keys) != len(keys) {
			return nil, "", false
		}
		seen := map[string]bool{}
		for _, k := range m.Keys {
			n, ok := keyName(k)
			if !ok {
				return nil, "", false
			}
			if _, ok := keyIndex[n]; !ok {
				return nil, "", false
			}
			if seen[n] {
				return nil, "", false
			}
			seen[n] = true
		}
	}
	sname := strings.Title(name) + "Item"
	fields := make([]Param, len(keys))
	for i, k := range keys {
		fields[i] = Param{Name: k, Type: exprType(first.Values[i])}
	}
	for i, e := range l.Elems {
		m := e.(*MapLit)
		flds := make([]FieldLit, len(keys))
		for j, k := range keys {
			idx := -1
			for x, mk := range m.Keys {
				if n, _ := keyName(mk); n == k {
					idx = x
					break
				}
			}
			if idx < 0 {
				return nil, "", false
			}
			flds[j] = FieldLit{Name: k, Value: m.Values[idx]}
		}
		l.Elems[i] = &StructLit{Name: sname, Fields: flds}
	}
	if currentProgram != nil {
		if currentProgram.ListTypes == nil {
			currentProgram.ListTypes = map[string]string{}
		}
		currentProgram.ListTypes[name] = sname
	}
	return &StructDef{Name: sname, Fields: fields}, sname, true
}

func structFieldType(stName, field string) string {
	if currentProgram != nil {
		for _, st := range currentProgram.Structs {
			if st.Name == stName {
				for _, f := range st.Fields {
					if f.Name == field {
						return f.Type
					}
				}
			}
		}
	}
	return "auto"
}

func convertTypeDecl(td *parser.TypeDecl) (*StructDef, error) {
	if td == nil {
		return nil, fmt.Errorf("nil type decl")
	}
	if len(td.Variants) > 0 {
		return nil, fmt.Errorf("unsupported type variants")
	}
	st := &StructDef{Name: td.Name}
	for _, m := range td.Members {
		if m.Field == nil {
			return nil, fmt.Errorf("unsupported type member")
		}
		typ := "auto"
		if m.Field.Type != nil {
			typ = cppTypeFrom(types.ResolveTypeRef(m.Field.Type, currentEnv))
		}
		st.Fields = append(st.Fields, Param{Name: m.Field.Name, Type: typ})
	}
	return st, nil
}
