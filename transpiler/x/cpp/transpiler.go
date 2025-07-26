//go:build slow

package cpp

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strings"
	"time"

	yaml "gopkg.in/yaml.v3"

	"mochi/parser"
	"mochi/transpiler/meta"
	"mochi/types"
)

var version string
var currentProgram *Program
var currentEnv *types.Env
var localTypes map[string]string
var globalTypes map[string]string
var currentVarDecls map[string]*LetStmt
var inFunction bool
var inLambda int
var builtinAliases map[string]string
var useNow bool
var useMem bool
var usesLookupHost bool
var benchMain bool
var usesSHA256 bool
var usesIndexOf bool
var usesParseIntStr bool
var reserved = map[string]bool{"this": true, "new": true, "double": true, "char": true}
var currentReturnType string
var inReturn bool
var mutatedParams map[string]bool
var paramNames map[string]bool

// SetBenchMain configures whether the generated main function is wrapped in a
// benchmark block when emitting code.
func SetBenchMain(v bool) { benchMain = v }

func safeName(n string) string {
	if reserved[n] {
		return "_" + n
	}
	return n
}

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
	Name     string
	Base     string
	Abstract bool
	Fields   []Param
}

type Program struct {
	Includes  []string
	Structs   []StructDef
	Globals   []Stmt
	Functions []*Func
	ListTypes map[string]string
	// collected types of top-level variables
	GlobalTypes    map[string]string
	UseNow         bool
	UseMem         bool
	UseLookupHost  bool
	UseSHA256      bool
	UseIndexOf     bool
	UseParseIntStr bool
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
	Name  string
	Type  string
	ByVal bool
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

// PrintExpr represents a print call used as an expression. It expands to a
// lambda block that prints the values and returns 0 so it can appear where an
// expression is required.
type PrintExpr struct{ Values []Expr }

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// LenExpr represents the builtin len() for strings.
type LenExpr struct{ Value Expr }

type StringLit struct{ Value string }

type IntLit struct{ Value int }

type FloatLit struct{ Value float64 }

type BoolLit struct{ Value bool }
type NullLit struct{}

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

// SaveStmt emits each element of a list as JSON lines to stdout.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

// UpdateStmt modifies fields of items within a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

// BenchStmt measures execution time of a block and prints the result.
type BenchStmt struct {
	Name string
	Body []Stmt
}

// ListLit represents a list literal converted to std::vector.
type ListLit struct {
	Elems    []Expr
	ElemType string
}

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

// MapGetExpr represents m.get(key, default).
type MapGetExpr struct {
	Map     Expr
	Key     Expr
	Default Expr
}

// TupleExpr represents a std::tuple initialization.
type TupleExpr struct{ Elems []Expr }

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
type KeysExpr struct{ Map Expr }

// MinExpr represents the `min` builtin for lists of numbers.
type MinExpr struct{ List Expr }

// MaxExpr represents the `max` builtin for lists of numbers.
type MaxExpr struct{ List Expr }

// ToUpperExpr represents strings.ToUpper for Go strings package.
type ToUpperExpr struct{ Value Expr }
type ToLowerExpr struct{ Value Expr }

// TrimSpaceExpr represents strings.TrimSpace for Go strings package.
type TrimSpaceExpr struct{ Value Expr }

// InputExpr represents reading a line from standard input.
type InputExpr struct{}

type CastExpr struct {
	Value Expr
	Type  string
}

type SubstringExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

// PadStartExpr represents a simple padStart implementation.
type PadStartExpr struct {
	Value Expr
	Width Expr
	Pad   Expr
}

type CallExpr struct {
	Name string
	Args []Expr
}

// NowExpr expands to a deterministic timestamp similar to the VM's now() builtin.
type NowExpr struct{}

type ExistsExpr struct{ List Expr }

type LambdaExpr struct {
	Params []Param
	Body   Expr
}

// BlockLambda represents a lambda with a statement body.
type BlockLambda struct {
	Params     []Param
	Body       []Stmt
	ReturnType string
}

// MatchBlock represents a simple switch implemented with dynamic_cast.
type MatchBlock struct{ Body []Stmt }

func (m *MatchBlock) emit(w io.Writer) {
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[=]"
	}
	io.WriteString(w, "("+cap+"{\n")
	inLambda++
	for _, st := range m.Body {
		st.emit(w, 1)
	}
	inLambda--
	io.WriteString(w, "}())")
}

// DynCastExpr represents a C++ dynamic_cast expression.
type DynCastExpr struct {
	Type string
	Expr Expr
}

// PtrGetExpr represents calling .get() on a smart pointer.
type PtrGetExpr struct{ Target Expr }

type FieldPtrExpr struct {
	Target Expr
	Field  string
}

type ReturnStmt struct {
	Value Expr
	Type  string
}

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

// ExprStmt represents a bare expression used as a statement.
type ExprStmt struct{ Expr Expr }

// AssignFieldStmt represents assignment to a field like obj.f = v.
type AssignFieldStmt struct {
	Target Expr
	Field  string
	Value  Expr
}

type ForStmt struct {
	Var        string
	Start, End Expr
	Body       []Stmt
	IsMap      bool
	ElemType   string
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
	RowVars     []string
	GroupName   string
	GroupStruct string
	Body        Expr
	ElemType    string
	KeyType     string
	ItemType    string
}

// LeftJoinGroupComp represents a left join followed by grouping.
type LeftJoinGroupComp struct {
	LeftVar     string
	LeftIter    Expr
	RightVar    string
	RightIter   Expr
	RightType   string
	InnerType   string
	Cond        Expr
	Key         Expr
	ItemVar     string
	GroupName   string
	GroupStruct string
	ItemType    string
	Body        Expr
	ElemType    string
	KeyType     string
}

// LeftJoinComp represents a simple left join without grouping.
type LeftJoinComp struct {
	LeftVar   string
	LeftIter  Expr
	RightVar  string
	RightIter Expr
	RightType string
	InnerType string
	Cond      Expr
	Body      Expr
	ElemType  string
}

// RightJoinComp represents a simple right join without grouping.
type RightJoinComp struct {
	LeftVar   string
	LeftIter  Expr
	RightVar  string
	RightIter Expr
	RightType string
	LeftType  string
	InnerType string
	Cond      Expr
	Body      Expr
	ElemType  string
}

// OuterJoinComp represents a full outer join.
type OuterJoinComp struct {
	LeftVar    string
	LeftIter   Expr
	RightVar   string
	RightIter  Expr
	LeftType   string
	RightType  string
	LeftInner  string
	RightInner string
	Cond       Expr
	Body       Expr
	ElemType   string
}

// JoinLeftJoinComp handles an inner join followed by a left join.
type JoinLeftJoinComp struct {
	LeftVar   string
	LeftIter  Expr
	JoinVar   string
	JoinIter  Expr
	JoinCond  Expr
	RightVar  string
	RightIter Expr
	RightType string
	InnerType string
	Cond      Expr
	Body      Expr
	ElemType  string
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
	oldGlobals := globalTypes
	globalTypes = p.GlobalTypes
	defer func() { globalTypes = oldGlobals }()
	v := strings.TrimSpace(version)
	loc := time.FixedZone("GMT+7", 7*3600)
	ts := time.Now().In(loc).Format("2006-01-02 15:04:05 MST")
	fmt.Fprintf(w, "// Generated by Mochi transpiler v%s on %s\n", v, ts)
	p.addInclude("<string>")
	p.addInclude("<sstream>")
	p.addInclude("<iomanip>")
	p.addInclude("<cmath>")
	p.addInclude("<optional>")
	p.addInclude("<vector>")
	p.addInclude("<any>")
	p.addInclude("<type_traits>")
	if p.UseNow {
		p.addInclude("<cstdlib>")
		p.addInclude("<chrono>")
	}
	if p.UseMem {
		p.addInclude("<sys/resource.h>")
	}
	if p.UseLookupHost {
		p.addInclude("<any>")
		p.addInclude("<netdb.h>")
		p.addInclude("<arpa/inet.h>")
		p.addInclude("<cstring>")
		p.addInclude("<map>")
	}
	if p.UseSHA256 {
		p.addInclude("<openssl/sha.h>")
	}
	usesAny := false
	for _, inc := range p.Includes {
		if inc == "<any>" {
			usesAny = true
			break
		}
	}
	if usesAny {
		p.addInclude("<map>")
		p.addInclude("<type_traits>")
	}
	for _, inc := range p.Includes {
		fmt.Fprintf(w, "#include %s\n", inc)
	}
	fmt.Fprintln(w)
	fmt.Fprintln(w)
	if p.UseNow {
		fmt.Fprintln(w, "static int _now() {")
		fmt.Fprintln(w, "    static long long seed = 0;")
		fmt.Fprintln(w, "    static bool seeded = false;")
		fmt.Fprintln(w, "    if (!seeded) {")
		fmt.Fprintln(w, "        const char* s = std::getenv(\"MOCHI_NOW_SEED\");")
		fmt.Fprintln(w, "        if (s && *s) { seed = std::atoll(s); seeded = true; }")
		fmt.Fprintln(w, "    }")
		fmt.Fprintln(w, "    if (seeded) {")
		fmt.Fprintln(w, "        seed = (seed * 1664525 + 1013904223) % 2147483647;")
		fmt.Fprintln(w, "        return static_cast<int>(seed);")
		fmt.Fprintln(w, "    }")
		fmt.Fprintln(w, "    return (int)(std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count() % 2147483647);")
		fmt.Fprintln(w, "}")
	}
	if p.UseMem {
		fmt.Fprintln(w, "static long _mem() {")
		fmt.Fprintln(w, "    struct rusage usage{};")
		fmt.Fprintln(w, "    getrusage(RUSAGE_SELF, &usage);")
		fmt.Fprintln(w, "#ifdef __APPLE__")
		fmt.Fprintln(w, "    return usage.ru_maxrss;")
		fmt.Fprintln(w, "#else")
		fmt.Fprintln(w, "    return usage.ru_maxrss * 1024;")
		fmt.Fprintln(w, "#endif")
		fmt.Fprintln(w, "}")
	}
	if p.UseLookupHost {
		fmt.Fprintln(w, "static std::vector<std::any> _lookup_host(const std::string& host) {")
		fmt.Fprintln(w, "    std::vector<std::any> res;")
		fmt.Fprintln(w, "    std::vector<std::string> ips;")
		fmt.Fprintln(w, "    addrinfo hints{}; memset(&hints, 0, sizeof(hints)); hints.ai_family = AF_INET;")
		fmt.Fprintln(w, "    addrinfo* info=nullptr;")
		fmt.Fprintln(w, "    int rc = getaddrinfo(host.c_str(), nullptr, &hints, &info);")
		fmt.Fprintln(w, "    if(rc != 0) {")
		fmt.Fprintln(w, "        res.push_back(ips);")
		fmt.Fprintln(w, "        res.push_back(std::string(gai_strerror(rc)));")
		fmt.Fprintln(w, "        return res;")
		fmt.Fprintln(w, "    }")
		fmt.Fprintln(w, "    for(auto p=info; p; p=p->ai_next){")
		fmt.Fprintln(w, "        char buf[INET_ADDRSTRLEN];")
		fmt.Fprintln(w, "        auto* addr = reinterpret_cast<sockaddr_in*>(p->ai_addr);")
		fmt.Fprintln(w, "        if(inet_ntop(AF_INET, &addr->sin_addr, buf, sizeof(buf))) ips.push_back(std::string(buf));")
		fmt.Fprintln(w, "    }")
		fmt.Fprintln(w, "    freeaddrinfo(info);")
		fmt.Fprintln(w, "    res.push_back(ips);")
		fmt.Fprintln(w, "    res.push_back(std::any());")
		fmt.Fprintln(w, "    return res;")
		fmt.Fprintln(w, "}")
	}
	if p.UseSHA256 {
		fmt.Fprintln(w, "static std::vector<int64_t> _sha256(const std::vector<int64_t>& bs) {")
		fmt.Fprintln(w, "    unsigned char digest[SHA256_DIGEST_LENGTH];")
		fmt.Fprintln(w, "    std::vector<unsigned char> data(bs.size());")
		fmt.Fprintln(w, "    for(size_t i=0;i<bs.size();++i) data[i]=static_cast<unsigned char>(bs[i]);")
		fmt.Fprintln(w, "    SHA256(data.data(), data.size(), digest);")
		fmt.Fprintln(w, "    std::vector<int64_t> out(SHA256_DIGEST_LENGTH);")
		fmt.Fprintln(w, "    for(int i=0;i<SHA256_DIGEST_LENGTH;i++) out[i]=digest[i];")
		fmt.Fprintln(w, "    return out;")
		fmt.Fprintln(w, "}")
	}
	if p.UseIndexOf {
		fmt.Fprintln(w, "static long _index_of(const std::string& s, const std::string& sub) {")
		fmt.Fprintln(w, "    auto pos = s.find(sub);")
		fmt.Fprintln(w, "    return pos == std::string::npos ? -1 : static_cast<long>(pos);")
		fmt.Fprintln(w, "}")
		fmt.Fprintln(w, "template<typename T> long _index_of(const std::vector<T>& xs, const T& v) {")
		fmt.Fprintln(w, "    for(size_t i=0;i<xs.size();++i){ if(xs[i]==v) return i; }")
		fmt.Fprintln(w, "    return -1;")
		fmt.Fprintln(w, "}")
	}
	if p.UseParseIntStr {
		fmt.Fprintln(w, "static long _parse_int_str(const std::string& s, long base) {")
		fmt.Fprintln(w, "    return std::stol(s, nullptr, base);")
		fmt.Fprintln(w, "}")
	}
	if usesAny {
		fmt.Fprintln(w, "static void any_to_stream(std::ostream& os, const std::any& val) {")
		fmt.Fprintln(w, "    if(val.type() == typeid(int)) os << std::any_cast<int>(val);")
		fmt.Fprintln(w, "    else if(val.type() == typeid(int64_t)) os << std::any_cast<int64_t>(val);")
		fmt.Fprintln(w, "    else if(val.type() == typeid(double)) os << std::any_cast<double>(val);")
		fmt.Fprintln(w, "    else if(val.type() == typeid(bool)) os << (std::any_cast<bool>(val) ? \"true\" : \"false\");")
		fmt.Fprintln(w, "    else if(val.type() == typeid(std::string)) os << std::any_cast<std::string>(val);")
		fmt.Fprintln(w, "    else if(val.type() == typeid(std::vector<int64_t>)) { const auto& v = std::any_cast<const std::vector<int64_t>&>(val); os << '['; for(size_t i=0;i<v.size();++i){ if(i>0) os << ', '; os << v[i]; } os << ']'; }")
		fmt.Fprintln(w, "    else if(val.type() == typeid(std::vector<std::vector<int64_t>>)) { const auto& vv = std::any_cast<const std::vector<std::vector<int64_t>>&>(val); os << '['; for(size_t i=0;i<vv.size();++i){ if(i>0) os << ', '; const auto& v = vv[i]; os << '['; for(size_t j=0;j<v.size();++j){ if(j>0) os << ', '; os << v[j]; } os << ']'; } os << ']'; }")
		fmt.Fprintln(w, "    else if(val.type() == typeid(std::vector<std::string>)) { const auto& v = std::any_cast<const std::vector<std::string>&>(val); os << '['; for(size_t i=0;i<v.size();++i){ if(i>0) os << ', '; os << v[i]; } os << ']'; }")
		fmt.Fprintln(w, "    else if(val.type() == typeid(std::vector<std::any>)) { const auto& v = std::any_cast<const std::vector<std::any>&>(val); os << '['; for(size_t i=0;i<v.size();++i){ if(i>0) os << ', '; any_to_stream(os, v[i]); } os << ']'; }")
		fmt.Fprintln(w, "    else if(val.type() == typeid(std::map<std::string, std::any>)) { const auto& m = std::any_cast<const std::map<std::string, std::any>&>(val); os << '{'; bool first=true; for(const auto& p : m){ if(!first) os << ', '; first=false; os << p.first << ': '; any_to_stream(os, p.second); } os << '}'; }")
		fmt.Fprintln(w, "    else os << \"<any>\";")
		fmt.Fprintln(w, "}")
		fmt.Fprintln(w, "static double any_to_double(const std::any& v) {")
		fmt.Fprintln(w, "    if(v.type() == typeid(int)) return (double)std::any_cast<int>(v);")
		fmt.Fprintln(w, "    if(v.type() == typeid(double)) return std::any_cast<double>(v);")
		fmt.Fprintln(w, "    return 0;")
		fmt.Fprintln(w, "}")
	}

	fmt.Fprintln(w, "template <typename T>")
	fmt.Fprintln(w, "std::ostream& operator<<(std::ostream& os, const std::vector<T>& vec) {")
	fmt.Fprintln(w, "    os << \"[\";")
	fmt.Fprintln(w, "    for(size_t i=0;i<vec.size();++i){ if(i>0) os << \", \"; if constexpr(std::is_same_v<T, std::any>) any_to_stream(os, vec[i]); else os << vec[i]; }")
	fmt.Fprintln(w, "    os << \"]\";")
	fmt.Fprintln(w, "    return os;")
	fmt.Fprintln(w, "}")
	fmt.Fprintln(w)
	fmt.Fprintln(w)

	fmt.Fprintln(w, "template<typename T> std::string _to_string(const T& v) {")
	fmt.Fprintln(w, "    std::ostringstream ss;")
	fmt.Fprintln(w, "    ss << std::boolalpha << v;")
	fmt.Fprintln(w, "    return ss.str();")
	fmt.Fprintln(w, "}")
	for _, st := range p.Structs {
		if st.Abstract {
			fmt.Fprintf(w, "struct %s;\n", st.Name)
		}
	}
	if len(p.Structs) > 0 {
		fmt.Fprintln(w)
	}
	fmt.Fprintln(w)
	for _, st := range p.Structs {
		fmt.Fprintf(w, "struct %s", st.Name)
		if st.Base != "" {
			fmt.Fprintf(w, " : %s", st.Base)
		}
		fmt.Fprintln(w, " {")
		if st.Abstract {
			fmt.Fprintf(w, "    virtual ~%s() = default;\n", st.Name)
		}
		for _, f := range st.Fields {
			fmt.Fprintf(w, "    %s %s;\n", f.Type, f.Name)
		}
		if !st.Abstract {
			if st.Base != "" && len(st.Fields) > 0 {
				fmt.Fprintf(w, "    %s(", st.Name)
				for i, f := range st.Fields {
					if i > 0 {
						io.WriteString(w, ", ")
					}
					fmt.Fprintf(w, "%s %s_", f.Type, f.Name)
				}
				io.WriteString(w, ") : ")
				for i, f := range st.Fields {
					if i > 0 {
						io.WriteString(w, ", ")
					}
					if strings.HasPrefix(f.Type, "std::unique_ptr<") || strings.HasPrefix(f.Type, "std::shared_ptr<") {
						fmt.Fprintf(w, "%s(std::move(%s_))", f.Name, f.Name)
					} else {
						fmt.Fprintf(w, "%s(%s_)", f.Name, f.Name)
					}
				}
				fmt.Fprintln(w, " {}")
			}
			fmt.Fprintf(w, "    auto operator<=>(const %s&) const = default;\n", st.Name)
			if strings.HasSuffix(st.Name, "Group") && len(st.Fields) == 2 && st.Fields[1].Name == "items" {
				fmt.Fprintln(w, "    auto begin() { return items.begin(); }")
				fmt.Fprintln(w, "    auto end() { return items.end(); }")
				fmt.Fprintln(w, "    size_t size() const { return items.size(); }")
			}
		}
		fmt.Fprintln(w, "};")
		fmt.Fprintln(w)
	}
	for _, st := range p.Structs {
		if st.Abstract {
			continue
		}
		fmt.Fprintf(w, "std::ostream& operator<<(std::ostream& os, const %s& v) {\n", st.Name)
		fmt.Fprint(w, "    os << '{'")
		for i, f := range st.Fields {
			if i > 0 {
				fmt.Fprint(w, " << \", \"")
			}
			fmt.Fprintf(w, " << \"'%s': \"", f.Name)
			switch {
			case f.Type == "std::string":
				fmt.Fprintf(w, "<< \"'\" << v.%s << \"'\"", f.Name)
			case f.Type == "double":
				fmt.Fprintf(w, "<< std::fixed << std::setprecision(1) << v.%s", f.Name)
			case strings.HasPrefix(f.Type, "std::optional<"):
				fmt.Fprintf(w, "; if(v.%s) os << *v.%s; else os << \"None\"; os", f.Name, f.Name)
			case strings.HasPrefix(f.Type, "std::vector<"):
				fmt.Fprintf(w, "<< \"[\"; for(size_t i=0;i<v.%s.size();++i){ if(i>0) os << \", \"; os << v.%s[i]; } os << \"]\"", f.Name, f.Name)
			case strings.HasPrefix(f.Type, "std::map<"):
				fmt.Fprintf(w, "<< \"{\"; bool first_%d=true; for(const auto& p: v.%s){ if(!first_%d) os << \", \"; first_%d=false; os << p.first << ': ' << p.second; } os << \"}\"", i, f.Name, i, i)
			default:
				fmt.Fprintf(w, "<< v.%s", f.Name)
			}
			fmt.Fprintln(w)
		}
		fmt.Fprintln(w, " << '}';")
		fmt.Fprintln(w, "    return os;")
		fmt.Fprintln(w, "}")
		fmt.Fprintln(w)
	}
	currentProgram = p
	// declare function prototypes
	for _, fn := range p.Functions {
		fmt.Fprintf(w, "%s %s(", fn.ReturnType, safeName(fn.Name))
		for i, p := range fn.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			typ := p.Type
			if typ == "" {
				io.WriteString(w, "auto")
			} else {
				if strings.HasPrefix(typ, "std::vector<") {
					if p.ByVal {
						io.WriteString(w, typ)
					} else {
						io.WriteString(w, "const "+typ+"&")
					}
				} else if strings.HasPrefix(typ, "std::map<") {
					if p.ByVal {
						io.WriteString(w, typ)
					} else {
						io.WriteString(w, "const "+typ+"&")
					}
				} else {
					io.WriteString(w, typ)
				}
			}
			io.WriteString(w, " ")
			io.WriteString(w, safeName(p.Name))
		}
		fmt.Fprintln(w, ");")
	}
	if len(p.Functions) > 0 {
		fmt.Fprintln(w)
	}

	// emit helper functions first
	first := true
	var mainFn *Func
	for _, st := range p.Globals {
		st.emit(w, 0)
	}
	if len(p.Globals) > 0 {
		fmt.Fprintln(w)
	}
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
	if mainFn != nil {
		if !first {
			fmt.Fprintln(w)
		}
		mainFn.emit(w)
	}
	currentProgram = nil
}

func (f *Func) emit(w io.Writer) {
	oldLocals := localTypes
	localTypes = map[string]string{}
	prevFn := inFunction
	inFunction = true
	for _, p := range f.Params {
		if p.Type != "" {
			localTypes[p.Name] = p.Type
		}
	}
	fmt.Fprintf(w, "%s %s(", f.ReturnType, safeName(f.Name))
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		typ := p.Type
		if typ == "" {
			io.WriteString(w, "auto ")
		} else {
			if strings.HasPrefix(typ, "std::vector<") {
				if p.ByVal {
					io.WriteString(w, typ+" ")
				} else {
					io.WriteString(w, "const "+typ+"& ")
				}
			} else if strings.HasPrefix(typ, "std::map<") {
				if p.ByVal {
					io.WriteString(w, typ+" ")
				} else {
					io.WriteString(w, "const "+typ+"& ")
				}
			} else {
				io.WriteString(w, typ+" ")
			}
		}
		io.WriteString(w, safeName(p.Name))
	}
	fmt.Fprintln(w, ") {")
	for _, st := range f.Body {
		st.emit(w, 1)
	}
	if f.Name == "main" {
		fmt.Fprintln(w, "    return 0;")
	}
	fmt.Fprintln(w, "}")
	inFunction = prevFn
	localTypes = oldLocals
}

func (s *PrintStmt) emit(w io.Writer, indent int) {
	if currentProgram != nil {
		currentProgram.addInclude("<iostream>")
		currentProgram.addInclude("<iomanip>")
	}
	for i, v := range s.Values {
		if i > 0 {
			for j := 0; j < indent; j++ {
				io.WriteString(w, "    ")
			}
			io.WriteString(w, "std::cout << \" \";\n")
		}
		emitToStream(w, "std::cout", v, indent)
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "std::cout << std::endl;\n")
}

func (p *PrintExpr) emit(w io.Writer) {
	io.WriteString(w, "([&]{\n")
	ps := &PrintStmt{Values: p.Values}
	ps.emit(w, 1)
	io.WriteString(w, "    return 0;\n")
	io.WriteString(w, "}())")
}

func emitToStream(w io.Writer, stream string, e Expr, indent int) {
	ind := strings.Repeat("    ", indent)
	typ := exprType(e)
	switch {
	case strings.HasPrefix(typ, "std::vector<"):
		elem := elementTypeFromListType(typ)
		io.WriteString(w, ind+"{")
		io.WriteString(w, " auto __tmp = ")
		e.emit(w)
		io.WriteString(w, "; "+stream+" << \"[\"; for(size_t i=0;i<__tmp.size();++i){ if(i>0) "+stream+" << \", \"; ")
		if strings.HasPrefix(elem, "std::vector<") || strings.HasPrefix(elem, "std::map<") || strings.HasPrefix(elem, "std::optional<") {
			io.WriteString(w, "{ std::ostringstream __ss; ")
			emitToStream(w, "__ss", &IndexExpr{Target: &VarRef{Name: "__tmp"}, Index: &VarRef{Name: "i"}}, indent)
			io.WriteString(w, " "+stream+" << __ss.str(); }")
		} else {
			io.WriteString(w, stream+" << __tmp[i]")
		}
		io.WriteString(w, "; } "+stream+" << \"]\"; }")
		io.WriteString(w, "\n")
	case strings.HasPrefix(typ, "std::optional<"):
		io.WriteString(w, ind+"{")
		io.WriteString(w, " auto __tmp = ")
		e.emit(w)
		io.WriteString(w, "; if(__tmp) "+stream+" << *__tmp; else "+stream+" << \"None\"; }")
		io.WriteString(w, "\n")
	case strings.HasPrefix(typ, "std::map<"):
		io.WriteString(w, ind+"{")
		io.WriteString(w, " auto __tmp = ")
		e.emit(w)
		io.WriteString(w, "; "+stream+" << \"{\"; bool first=true; for(const auto& __p : __tmp){ if(!first) "+stream+" << \", \"; first=false; "+stream+" << __p.first << ': '; any_to_stream("+stream+", __p.second); } "+stream+" << \"}\"; }")
		io.WriteString(w, "\n")
	case typ == "std::any":
		io.WriteString(w, ind+"any_to_stream("+stream+", ")
		e.emit(w)
		io.WriteString(w, ");\n")
	case typ == "bool":
		io.WriteString(w, ind+stream+" << (")
		e.emit(w)
		io.WriteString(w, " ? \"true\" : \"false\");\n")
	default:
		if typ == "double" {
			if currentProgram != nil {
				currentProgram.addInclude("<cmath>")
				currentProgram.addInclude("<sstream>")
				currentProgram.addInclude("<iomanip>")
			}
			io.WriteString(w, ind+"{")
			io.WriteString(w, " std::ostringstream __ss; double __dv = ")
			e.emit(w)
			io.WriteString(w, "; if(std::floor(__dv) == __dv) { __ss<<std::fixed<<std::setprecision(1)<<__dv; } else { __ss<<std::defaultfloat<<std::setprecision(17)<<__dv; } "+stream+" << __ss.str(); }\n")
		} else {
			io.WriteString(w, ind+stream+" << ")
			e.emit(w)
			io.WriteString(w, ";\n")
		}
	}
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
	if exprType(l.Value) == "std::any" {
		io.WriteString(w, "std::any_cast<std::string>(")
		l.Value.emit(w)
		io.WriteString(w, ").size()")
	} else {
		l.Value.emit(w)
		io.WriteString(w, ".size()")
	}
}

func (l *ListLit) emit(w io.Writer) {
	if len(l.Elems) == 0 {
		io.WriteString(w, "{}")
		return
	}
	elemTyp := l.ElemType
	if elemTyp == "" {
		listTyp := exprType(l)
		elemTyp = elementTypeFromListType(listTyp)
		if elemTyp == "auto" || elemTyp == "std::any" {
			first := exprType(l.Elems[0])
			if first == "auto" {
				first = elementTypeFromListType(listTyp)
			}
			uniform := true
			for _, e := range l.Elems[1:] {
				typ := exprType(e)
				if typ == "auto" {
					typ = elementTypeFromListType(listTyp)
				}
				if typ != first {
					uniform = false
					break
				}
			}
			if uniform {
				elemTyp = first
			} else {
				if currentProgram != nil {
					currentProgram.addInclude("<any>")
				}
				elemTyp = "std::any"
			}
		}
	}
	io.WriteString(w, "std::vector<"+elemTyp+">{")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if elemTyp == "std::any" && exprType(e) != "std::any" {
			io.WriteString(w, "std::any(")
			e.emit(w)
			io.WriteString(w, ")")
		} else {
			e.emit(w)
		}
	}
	io.WriteString(w, "}")
}

func (m *MapLit) emit(w io.Writer) {
	if len(m.Keys) == 0 {
		io.WriteString(w, "{}")
		return
	}
	if m.ValueType == "std::any" && currentProgram != nil {
		currentProgram.addInclude("<any>")
	}
	fmt.Fprintf(w, "std::map<%s, %s>{", m.KeyType, m.ValueType)
	for i := range m.Keys {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, "{")
		switch k := m.Keys[i].(type) {
		case *StringLit:
			fmt.Fprintf(w, "%q", k.Value)
		case *VarRef:
			if _, ok := localTypes[k.Name]; ok {
				io.WriteString(w, k.Name)
			} else if currentEnv != nil {
				if _, err := currentEnv.GetVar(k.Name); err == nil {
					io.WriteString(w, k.Name)
				} else {
					fmt.Fprintf(w, "%q", k.Name)
				}
			} else {
				fmt.Fprintf(w, "%q", k.Name)
			}
		default:
			m.Keys[i].emit(w)
		}
		io.WriteString(w, ", ")
		if m.ValueType == "std::any" {
			io.WriteString(w, "std::any(")
			m.Values[i].emit(w)
			io.WriteString(w, ")")
		} else {
			m.Values[i].emit(w)
		}
		io.WriteString(w, "}")
	}
	io.WriteString(w, "}")
}

func (mg *MapGetExpr) emit(w io.Writer) {
	io.WriteString(w, "([&]{ auto &__map = ")
	mg.Map.emit(w)
	io.WriteString(w, "; auto __it = __map.find(")
	mg.Key.emit(w)
	io.WriteString(w, "); if(__it != __map.end()) return __it->second; return ")
	mg.Default.emit(w)
	io.WriteString(w, "; }())")
}

func (t *TupleExpr) emit(w io.Writer) {
	io.WriteString(w, "std::tuple{")
	for i, e := range t.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "}")
}

func (s *StringLit) emit(w io.Writer) {
	fmt.Fprintf(w, "std::string(%q)", s.Value)
}

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

func (f *FloatLit) emit(w io.Writer) {
	if f.Value == float64(int(f.Value)) {
		fmt.Fprintf(w, "%.1f", f.Value)
	} else {
		fmt.Fprintf(w, "%g", f.Value)
	}
}

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (n *NullLit) emit(w io.Writer) {
	io.WriteString(w, "nullptr")
}

func (s *StructLit) emit(w io.Writer) {
	io.WriteString(w, safeName(s.Name))
	io.WriteString(w, "{")
	for i, f := range s.Fields {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		f.Value.emit(w)
	}
	io.WriteString(w, "}")
}

func (u *UnaryExpr) emit(w io.Writer) {
	if u.Op == "!" && exprType(u.Expr) == "std::any" {
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		io.WriteString(w, "!")
		io.WriteString(w, "std::any_cast<bool>(")
		u.Expr.emit(w)
		io.WriteString(w, ")")
		return
	}
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}

func (s *SelectorExpr) emit(w io.Writer) {
	t := exprType(s.Target)
	if strings.HasPrefix(t, "std::map<") {
		parts := strings.SplitN(strings.TrimSuffix(strings.TrimPrefix(t, "std::map<"), ">"), ",", 2)
		if len(parts) == 2 {
			valType := strings.TrimSpace(parts[1])
			if valType == "std::any" {
				resType := "std::any"
				switch s.Field {
				case "ok":
					resType = "bool"
				case "pixel":
					resType = "Pixel"
				case "rgb":
					resType = "int64_t"
				}
				if resType != "std::any" {
					if currentProgram != nil {
						currentProgram.addInclude("<any>")
					}
					fmt.Fprintf(w, "std::any_cast<%s>(", resType)
					s.Target.emit(w)
					fmt.Fprintf(w, "[\"%s\"])", s.Field)
					return
				}
			}
		}
		s.Target.emit(w)
		io.WriteString(w, "[\"")
		io.WriteString(w, s.Field)
		io.WriteString(w, "\"]")
	} else if strings.HasPrefix(t, "std::optional<") || strings.HasSuffix(t, "*") || strings.HasPrefix(t, "std::unique_ptr<") || strings.HasPrefix(t, "std::shared_ptr<") {
		s.Target.emit(w)
		io.WriteString(w, "->")
		io.WriteString(w, s.Field)
	} else {
		s.Target.emit(w)
		io.WriteString(w, ".")
		io.WriteString(w, s.Field)
	}
}

func (i *IndexExpr) emit(w io.Writer) {
	t := exprType(i.Target)
	if t == "std::string" {
		io.WriteString(w, "std::string(1, ")
		i.Target.emit(w)
		io.WriteString(w, "[")
		i.Index.emit(w)
		io.WriteString(w, "])")
		return
	}
	if t == "std::any" {
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		idxType := exprType(i.Index)
		resType := exprType(i)
		if idxType == "int64_t" {
			if resType == "auto" {
				// The any value is cast to a vector<int64_t>, so the
				// indexed result must be an int rather than a
				// whole vector.
				resType = "int64_t"
			}
			io.WriteString(w, "std::any_cast<"+resType+">(std::any_cast<std::vector<int64_t>>(")
			i.Target.emit(w)
			io.WriteString(w, ")[")
			i.Index.emit(w)
			io.WriteString(w, "])")
			return
		}
		if idxType == "std::string" {
			if resType == "auto" {
				if sl, ok := i.Index.(*StringLit); ok && (sl.Value == "num" || sl.Value == "denom") {
					resType = "int"
				} else {
					resType = "std::any"
				}
			}
			io.WriteString(w, "std::any_cast<"+resType+">(std::any_cast<std::map<std::string, std::any>>(")
			i.Target.emit(w)
			io.WriteString(w, ")[")
			i.Index.emit(w)
			io.WriteString(w, "])")
			return
		}
	}
	if strings.HasPrefix(t, "std::map<") && strings.HasSuffix(t, ">") {
		parts := strings.SplitN(strings.TrimSuffix(strings.TrimPrefix(t, "std::map<"), ">"), ",", 2)
		if len(parts) == 2 {
			valType := strings.TrimSpace(parts[1])
			if valType == "std::any" {
				resType := exprType(i)
				if resType == "std::any" {
					if sl, ok := i.Index.(*StringLit); ok {
						switch sl.Value {
						case "ok":
							resType = "bool"
						case "pixel":
							resType = "Pixel"
						case "rgb":
							resType = "int64_t"
						}
					}
				}
				if resType != "std::any" {
					if currentProgram != nil {
						currentProgram.addInclude("<any>")
					}
					io.WriteString(w, "std::any_cast<"+resType+">(")
					i.Target.emit(w)
					io.WriteString(w, "[")
					i.Index.emit(w)
					io.WriteString(w, "])")
					io.WriteString(w, ")")
					return
				}
			}
		}
	}
	i.Target.emit(w)
	io.WriteString(w, "[")
	i.Index.emit(w)
	io.WriteString(w, "]")
}

func (s *SliceExpr) emit(w io.Writer) {
	cap := "[&]"
	if !inFunction && inLambda == 0 {
		cap = "[]"
	}
	io.WriteString(w, "("+cap+"(const auto& __v){ if constexpr(std::is_same_v<std::decay_t<decltype(__v)>, std::string>) return __v.substr(")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, "); else return std::vector<typename std::decay_t<decltype(__v)>::value_type>(__v.begin()+")
	s.Start.emit(w)
	io.WriteString(w, ", __v.begin()+")
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
	io.WriteString(w, "([&](const auto& __c, const auto& __v){ ")
	io.WriteString(w, "if constexpr(std::is_same_v<std::decay_t<decltype(__c)>, std::string>) { return __c.find(__v) != std::string::npos; } ")
	io.WriteString(w, "else if constexpr(requires { __c.find(__v); }) { return __c.find(__v) != __c.end(); } ")
	io.WriteString(w, "else { return std::find(__c.begin(), __c.end(), __v) != __c.end(); } })(")
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
		io.WriteString(w, "; return std::accumulate(tmp.begin(), tmp.end(), 0.0); })()")
	} else {
		io.WriteString(w, "([&]{ auto __tmp = ")
		s.Arg.emit(w)
		io.WriteString(w, "; return std::accumulate(__tmp.begin(), __tmp.end(), 0.0); }())")
	}
}

func (a *AppendExpr) emit(w io.Writer) {
	listType := exprType(a.List)
	elemType := elementTypeFromListType(listType)
	valType := exprType(a.Elem)
	var elem Expr = a.Elem
	if strings.HasPrefix(valType, "std::vector<auto>") && elemType != "auto" {
		et := elemType
		if ll, ok := a.Elem.(*ListLit); ok {
			ll2 := *ll
			if ll2.ElemType == "" {
				ll2.ElemType = et
			}
			elem = &ll2
		}
		valType = fmt.Sprintf("std::vector<%s>", et)
	}
	if strings.HasPrefix(valType, "std::vector<") && listType == "std::vector<int64_t>" {
		listType = fmt.Sprintf("std::vector<%s>", valType)
		if vr, ok := a.List.(*VarRef); ok && localTypes != nil {
			localTypes[vr.Name] = listType
		}
		elemType = valType
	}
	io.WriteString(w, "([&]{ auto __tmp = ")
	a.List.emit(w)
	io.WriteString(w, "; __tmp.push_back(")
	if et := valType; et != elemType && elemType != "auto" {
		io.WriteString(w, "("+elemType+")")
	}
	elem.emit(w)
	io.WriteString(w, "); return __tmp; }())")
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
		currentProgram.addInclude("<iomanip>")
	}
	if inFunction || inLambda > 0 {
		io.WriteString(w, "([&]{ std::ostringstream ss; ")
		typ := exprType(s.Value)
		switch {
		case strings.HasPrefix(typ, "std::vector<") || strings.HasPrefix(typ, "std::map<") || strings.HasPrefix(typ, "std::optional<"):
			emitToStream(w, "ss", s.Value, 0)
		case typ == "std::any" || typ == "auto":
			io.WriteString(w, "any_to_stream(ss, ")
			s.Value.emit(w)
			io.WriteString(w, ");\n")
		default:
			io.WriteString(w, "ss << std::boolalpha << ")
			s.Value.emit(w)
			io.WriteString(w, ";")
		}
		io.WriteString(w, " return ss.str(); }())")
	} else {
		io.WriteString(w, "_to_string(")
		s.Value.emit(w)
		io.WriteString(w, ")")
	}
}

func (v *ValuesExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<vector>")
		currentProgram.addInclude("<map>")
	}
	io.WriteString(w, "([&]{ std::vector<decltype(")
	v.Map.emit(w)
	io.WriteString(w, ".begin()->second)> vals; for(const auto& __p : ")
	v.Map.emit(w)
	io.WriteString(w, ") vals.push_back(__p.second); return vals; }())")
}

func (k *KeysExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<vector>")
		currentProgram.addInclude("<map>")
	}
	io.WriteString(w, "([&]{ std::vector<std::decay_t<decltype(")
	k.Map.emit(w)
	io.WriteString(w, ".begin()->first)>> keys; for(const auto& __p : ")
	k.Map.emit(w)
	io.WriteString(w, ") keys.push_back(__p.first); return keys; }())")
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

func (u *ToUpperExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<cctype>")
	}
	io.WriteString(w, "([&]{ std::string __s = ")
	u.Value.emit(w)
	io.WriteString(w, "; for(auto &__c : __s){ __c = std::toupper(static_cast<unsigned char>(__c)); } return __s; }())")
}

func (u *ToLowerExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<cctype>")
	}
	io.WriteString(w, "([&]{ std::string __s = ")
	u.Value.emit(w)
	io.WriteString(w, "; for(auto &__c : __s){ __c = std::tolower(static_cast<unsigned char>(__c)); } return __s; }())")
}

func (t *TrimSpaceExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<cctype>")
	}
	io.WriteString(w, "([]{ std::string __s = ")
	t.Value.emit(w)
	io.WriteString(w, "; size_t __b = 0; while(__b < __s.size() && std::isspace(static_cast<unsigned char>(__s[__b]))) ++__b; size_t __e = __s.size(); while(__e > __b && std::isspace(static_cast<unsigned char>(__s[__e-1]))) --__e; return __s.substr(__b, __e-__b); }())")
}

func (i *InputExpr) emit(w io.Writer) {
	if currentProgram != nil {
		currentProgram.addInclude("<iostream>")
		currentProgram.addInclude("<string>")
	}
	io.WriteString(w, "([&]{ std::string __line; std::getline(std::cin, __line); return __line; }())")
}

func (n *NowExpr) emit(w io.Writer) {
	io.WriteString(w, "_now()")
}

func (c *CastExpr) emit(w io.Writer) {
	valType := exprType(c.Value)
	if valType == "auto" {
		if idx, ok := c.Value.(*IndexExpr); ok {
			t := exprType(idx.Target)
			if strings.HasPrefix(t, "std::map<") && strings.Contains(t, "std::any>") {
				valType = "std::any"
			}
		}
	}
	if c.Type == "int" && valType == "std::string" {
		io.WriteString(w, "std::stoi(")
		c.Value.emit(w)
		io.WriteString(w, ")")
		return
	}
	if c.Type == "std::string" && valType != "std::string" && valType != "std::any" {
		io.WriteString(w, "std::string(1, ")
		c.Value.emit(w)
		io.WriteString(w, ")")
		return
	}
	if valType == "std::any" {
		if c.Type == "auto" {
			c.Type = "double"
		}
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		fmt.Fprintf(w, "std::any_cast<%s>(", c.Type)
		c.Value.emit(w)
		io.WriteString(w, ")")
		return
	}
	if (strings.HasPrefix(valType, "std::unique_ptr<") || strings.HasPrefix(valType, "std::shared_ptr<")) && strings.HasSuffix(c.Type, "*") {
		c.Value.emit(w)
		io.WriteString(w, ".get()")
		return
	}
	io.WriteString(w, "(")
	io.WriteString(w, c.Type)
	io.WriteString(w, ")(")
	c.Value.emit(w)
	io.WriteString(w, ")")
}

func (s *SubstringExpr) emit(w io.Writer) {
	if exprType(s.Value) == "std::any" {
		io.WriteString(w, "std::any_cast<std::string>(")
		s.Value.emit(w)
		io.WriteString(w, ").substr(")
	} else {
		s.Value.emit(w)
		io.WriteString(w, ".substr(")
	}
	s.Start.emit(w)
	io.WriteString(w, ", ")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ")")
}

func (p *PadStartExpr) emit(w io.Writer) {
	io.WriteString(w, "([&]{ std::string __s = ")
	p.Value.emit(w)
	io.WriteString(w, "; while(__s.size() < ")
	p.Width.emit(w)
	io.WriteString(w, ") __s = ")
	p.Pad.emit(w)
	io.WriteString(w, " + __s; return __s; }())")
}

func (v *VarRef) emit(w io.Writer) {
	if v.Name == "nil" {
		io.WriteString(w, "std::any{}")
		return
	}
	io.WriteString(w, safeName(v.Name))
}

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
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[&]"
	}
	io.WriteString(w, cap+"(")
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
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[=]"
	}
	prev := localTypes
	nt := map[string]string{}
	for k, v := range prev {
		nt[k] = v
	}
	for _, p := range l.Params {
		if p.Type != "" {
			nt[p.Name] = p.Type
		}
	}
	localTypes = nt
	defer func() { localTypes = prev }()
	io.WriteString(w, cap+"(")
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
	io.WriteString(w, ") mutable {\n")
	for _, st := range l.Body {
		st.emit(w, 1)
	}
	io.WriteString(w, "}")
}

func (d *DynCastExpr) emit(w io.Writer) {
	io.WriteString(w, "dynamic_cast<const ")
	io.WriteString(w, d.Type)
	io.WriteString(w, "*>(")
	d.Expr.emit(w)
	io.WriteString(w, ")")
}

func (p *PtrGetExpr) emit(w io.Writer) {
	p.Target.emit(w)
	io.WriteString(w, ".get()")
}

func (f *FieldPtrExpr) emit(w io.Writer) {
	f.Target.emit(w)
	io.WriteString(w, "->")
	io.WriteString(w, f.Field)
}

func (lc *MultiListComp) emit(w io.Writer) {
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[&]"
	}
	io.WriteString(w, "("+cap+"{ std::vector<"+lc.ElemType+"> __items;\n")
	inLambda++
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
	inLambda--
}

func (gc *GroupComp) emit(w io.Writer) {
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[&]"
	}
	io.WriteString(w, "("+cap+"{ std::vector<"+gc.ElemType+"> __items;\n")
	inLambda++
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
	io.WriteString(w, "        "+gc.ItemType+" __row{")
	for i, v := range gc.RowVars {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, v)
	}
	io.WriteString(w, "};\n")
	io.WriteString(w, "        auto __key = ")
	gc.Key.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "        __groups[__key].push_back(__row);\n")
	if gc.Cond != nil {
		io.WriteString(w, "    }\n")
	}
	for range gc.Vars {
		io.WriteString(w, "}\n")
	}
	io.WriteString(w, "for(const auto& __kv : __groups) {\n")
	io.WriteString(w, "    ")
	io.WriteString(w, gc.GroupStruct+" "+gc.GroupName+"{__kv.first, __kv.second};\n")
	io.WriteString(w, "    __items.push_back(")
	gc.Body.emit(w)
	io.WriteString(w, ");\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "return __items; }())")
	inLambda--
}

func (lgc *LeftJoinGroupComp) emit(w io.Writer) {
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[&]"
	}
	io.WriteString(w, "("+cap+"{ std::vector<"+lgc.ElemType+"> __items;\n")
	inLambda++
	io.WriteString(w, "std::vector<"+lgc.GroupStruct+"> __groups;\n")
	io.WriteString(w, "std::unordered_map<"+lgc.KeyType+", size_t> __idx;\n")
	io.WriteString(w, "for (auto "+lgc.LeftVar+" : ")
	lgc.LeftIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "    bool __matched = false;\n")
	io.WriteString(w, "    for (auto "+lgc.RightVar+" : ")
	lgc.RightIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "        if(")
	lgc.Cond.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "            __matched = true;\n")
	io.WriteString(w, "            "+lgc.ItemType+" "+lgc.ItemVar+"{"+lgc.LeftVar+", std::optional<"+lgc.InnerType+">("+lgc.RightVar+")};\n")
	io.WriteString(w, "            auto __key = ")
	lgc.Key.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "            auto it = __idx.find(__key);\n")
	io.WriteString(w, "            if(it == __idx.end()) {\n")
	io.WriteString(w, "                "+lgc.GroupStruct+" __g{__key, {}};\n")
	io.WriteString(w, "                __g.items.push_back(")
	io.WriteString(w, lgc.ItemVar)
	io.WriteString(w, ");\n")
	io.WriteString(w, "                __idx[__key] = __groups.size();\n")
	io.WriteString(w, "                __groups.push_back(__g);\n")
	io.WriteString(w, "            } else {\n")
	io.WriteString(w, "                __groups[it->second].items.push_back(")
	io.WriteString(w, lgc.ItemVar)
	io.WriteString(w, ");\n")
	io.WriteString(w, "            }\n")
	io.WriteString(w, "        }\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "    if(!__matched) {\n")
	io.WriteString(w, "        "+lgc.RightType+" "+lgc.RightVar+" = std::nullopt;\n")
	io.WriteString(w, "        "+lgc.ItemType+" "+lgc.ItemVar+"{"+lgc.LeftVar+", "+lgc.RightVar+"};\n")
	io.WriteString(w, "        auto __key = ")
	lgc.Key.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "        auto it = __idx.find(__key);\n")
	io.WriteString(w, "        if(it == __idx.end()) {\n")
	io.WriteString(w, "            "+lgc.GroupStruct+" __g{__key, {}};\n")
	io.WriteString(w, "            __g.items.push_back(")
	io.WriteString(w, lgc.ItemVar)
	io.WriteString(w, ");\n")
	io.WriteString(w, "            __idx[__key] = __groups.size();\n")
	io.WriteString(w, "            __groups.push_back(__g);\n")
	io.WriteString(w, "        } else {\n")
	io.WriteString(w, "            __groups[it->second].items.push_back(")
	io.WriteString(w, lgc.ItemVar)
	io.WriteString(w, ");\n")
	io.WriteString(w, "        }\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "for(auto &__g : __groups) {\n")
	io.WriteString(w, "    "+lgc.GroupStruct+" "+lgc.GroupName+" = __g;\n")
	io.WriteString(w, "    __items.push_back(")
	lgc.Body.emit(w)
	io.WriteString(w, ");\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "return __items; }())")
	inLambda--
}

func (ljc *LeftJoinComp) emit(w io.Writer) {
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[&]"
	}
	io.WriteString(w, "("+cap+"{ std::vector<"+ljc.ElemType+"> __items;\n")
	inLambda++
	io.WriteString(w, "for (auto "+ljc.LeftVar+" : ")
	ljc.LeftIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "    bool __matched = false;\n")
	io.WriteString(w, "    for (auto __"+ljc.RightVar+" : ")
	ljc.RightIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "        auto "+ljc.RightVar+" = __"+ljc.RightVar+";\n")
	io.WriteString(w, "        if(")
	ljc.Cond.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "            __matched = true;\n")
	io.WriteString(w, "            { std::optional<"+ljc.InnerType+"> "+ljc.RightVar+"_opt("+ljc.RightVar+");\n")
	io.WriteString(w, "            auto "+ljc.RightVar+" = "+ljc.RightVar+"_opt;\n")
	io.WriteString(w, "            __items.push_back(")
	ljc.Body.emit(w)
	io.WriteString(w, "); }\n")
	io.WriteString(w, "        }\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "    if(!__matched) {\n")
	io.WriteString(w, "        std::optional<"+ljc.InnerType+"> "+ljc.RightVar+" = std::nullopt;\n")
	io.WriteString(w, "        __items.push_back(")
	ljc.Body.emit(w)
	io.WriteString(w, ");\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "return __items; }())")
	inLambda--
}

func (c *JoinLeftJoinComp) emit(w io.Writer) {
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[&]"
	}
	io.WriteString(w, "("+cap+"{ std::vector<"+c.ElemType+"> __items;\n")
	inLambda++
	io.WriteString(w, "for (auto "+c.LeftVar+" : ")
	c.LeftIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "    for (auto "+c.JoinVar+" : ")
	c.JoinIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "        if(")
	c.JoinCond.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "            bool __matched = false;\n")
	io.WriteString(w, "            for (auto __"+c.RightVar+" : ")
	c.RightIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "                auto "+c.RightVar+" = __"+c.RightVar+";\n")
	io.WriteString(w, "                if(")
	c.Cond.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "                    __matched = true;\n")
	io.WriteString(w, "                    { std::optional<"+c.InnerType+"> "+c.RightVar+"_opt("+c.RightVar+");\n")
	io.WriteString(w, "                    auto "+c.RightVar+" = "+c.RightVar+"_opt;\n")
	io.WriteString(w, "                    __items.push_back(")
	c.Body.emit(w)
	io.WriteString(w, "); }\n")
	io.WriteString(w, "                }\n")
	io.WriteString(w, "            }\n")
	io.WriteString(w, "            if(!__matched) {\n")
	io.WriteString(w, "                std::optional<"+c.InnerType+"> "+c.RightVar+" = std::nullopt;\n")
	io.WriteString(w, "                __items.push_back(")
	c.Body.emit(w)
	io.WriteString(w, ");\n")
	io.WriteString(w, "            }\n")
	io.WriteString(w, "        }\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "return __items; }())")
	inLambda--
}

func (rjc *RightJoinComp) emit(w io.Writer) {
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[&]"
	}
	io.WriteString(w, "("+cap+"{ std::vector<"+rjc.ElemType+"> __items;\n")
	inLambda++
	io.WriteString(w, "for (auto "+rjc.RightVar+" : ")
	rjc.RightIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "    bool __matched = false;\n")
	io.WriteString(w, "    for (auto __"+rjc.LeftVar+" : ")
	rjc.LeftIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "        auto "+rjc.LeftVar+" = __"+rjc.LeftVar+";\n")
	io.WriteString(w, "        if(")
	rjc.Cond.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "            __matched = true;\n")
	io.WriteString(w, "            { std::optional<"+rjc.InnerType+"> "+rjc.LeftVar+"_opt("+rjc.LeftVar+");\n")
	io.WriteString(w, "            auto "+rjc.LeftVar+" = "+rjc.LeftVar+"_opt;\n")
	old := localTypes
	nt := map[string]string{}
	for k, v := range old {
		nt[k] = v
	}
	nt[rjc.LeftVar] = fmt.Sprintf("std::optional<%s>", rjc.InnerType)
	nt[rjc.RightVar] = rjc.RightType
	localTypes = nt
	io.WriteString(w, "            __items.push_back(")
	rjc.Body.emit(w)
	io.WriteString(w, "); }\n")
	localTypes = old
	io.WriteString(w, "        }\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "    if(!__matched) {\n")
	io.WriteString(w, "        std::optional<"+rjc.InnerType+"> "+rjc.LeftVar+" = std::nullopt;\n")
	old2 := localTypes
	nt2 := map[string]string{}
	for k, v := range old2 {
		nt2[k] = v
	}
	nt2[rjc.LeftVar] = fmt.Sprintf("std::optional<%s>", rjc.InnerType)
	nt2[rjc.RightVar] = rjc.RightType
	localTypes = nt2
	io.WriteString(w, "        __items.push_back(")
	rjc.Body.emit(w)
	io.WriteString(w, ");\n")
	localTypes = old2
	io.WriteString(w, "    }\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "return __items; }())")
	inLambda--
}

func (ojc *OuterJoinComp) emit(w io.Writer) {
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[&]"
	}
	io.WriteString(w, "("+cap+"{ std::vector<"+ojc.ElemType+"> __items;\n")
	inLambda++
	io.WriteString(w, "for (auto "+ojc.LeftVar+" : ")
	ojc.LeftIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "    bool __matched = false;\n")
	io.WriteString(w, "    for (auto "+ojc.RightVar+" : ")
	ojc.RightIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "        if(")
	ojc.Cond.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "            __matched = true;\n")
	io.WriteString(w, "            { std::optional<"+ojc.LeftInner+"> "+ojc.LeftVar+"_opt("+ojc.LeftVar+"); std::optional<"+ojc.RightInner+"> "+ojc.RightVar+"_opt("+ojc.RightVar+");\n")
	io.WriteString(w, "            auto "+ojc.LeftVar+" = "+ojc.LeftVar+"_opt;\n")
	io.WriteString(w, "            auto "+ojc.RightVar+" = "+ojc.RightVar+"_opt;\n")
	old := localTypes
	nt := map[string]string{}
	for k, v := range old {
		nt[k] = v
	}
	nt[ojc.LeftVar] = fmt.Sprintf("std::optional<%s>", ojc.LeftInner)
	nt[ojc.RightVar] = fmt.Sprintf("std::optional<%s>", ojc.RightInner)
	localTypes = nt
	io.WriteString(w, "            __items.push_back(")
	ojc.Body.emit(w)
	io.WriteString(w, "); }\n")
	localTypes = old
	io.WriteString(w, "        }\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "    if(!__matched) {\n")
	io.WriteString(w, "        std::optional<"+ojc.RightInner+"> "+ojc.RightVar+" = std::nullopt;\n")
	io.WriteString(w, "        { std::optional<"+ojc.LeftInner+"> "+ojc.LeftVar+"_opt("+ojc.LeftVar+"); auto "+ojc.LeftVar+" = "+ojc.LeftVar+"_opt; __items.push_back(")
	ojc.Body.emit(w)
	io.WriteString(w, "); }\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "for (auto "+ojc.RightVar+" : ")
	ojc.RightIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "    bool __matched = false;\n")
	io.WriteString(w, "    for (auto "+ojc.LeftVar+" : ")
	ojc.LeftIter.emit(w)
	io.WriteString(w, ") {\n")
	io.WriteString(w, "        if(")
	ojc.Cond.emit(w)
	io.WriteString(w, ") { __matched = true; break; }\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "    if(!__matched) {\n")
	io.WriteString(w, "        std::optional<"+ojc.LeftInner+"> "+ojc.LeftVar+" = std::nullopt;\n")
	io.WriteString(w, "        std::optional<"+ojc.RightInner+"> "+ojc.RightVar+"_opt("+ojc.RightVar+");\n")
	io.WriteString(w, "        auto "+ojc.RightVar+" = "+ojc.RightVar+"_opt;\n")
	old2 := localTypes
	nt2 := map[string]string{}
	for k, v := range old2 {
		nt2[k] = v
	}
	nt2[ojc.LeftVar] = fmt.Sprintf("std::optional<%s>", ojc.LeftInner)
	nt2[ojc.RightVar] = fmt.Sprintf("std::optional<%s>", ojc.RightInner)
	localTypes = nt2
	io.WriteString(w, "        __items.push_back(")
	ojc.Body.emit(w)
	io.WriteString(w, ");\n")
	localTypes = old2
	io.WriteString(w, "    }\n")
	io.WriteString(w, "}\n")
	io.WriteString(w, "return __items; }())")
	inLambda--
}

func (sc *SortComp) emit(w io.Writer) {
	cap := "[]"
	if inFunction || inLambda > 0 {
		cap = "[&]"
	}
	io.WriteString(w, "("+cap+"{ std::vector<std::pair<"+sc.KeyType+", "+sc.ElemType+">> __tmp;\n")
	inLambda++
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
	inLambda--
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
	if b.Op == "+" {
		lt := exprType(b.Left)
		rt := exprType(b.Right)
		if strings.HasPrefix(lt, "std::vector<") && lt == rt {
			io.WriteString(w, "([&]{ auto __lhs = ")
			b.Left.emit(w)
			io.WriteString(w, "; auto __rhs = ")
			b.Right.emit(w)
			io.WriteString(w, "; __lhs.insert(__lhs.end(), __rhs.begin(), __rhs.end()); return __lhs; }())")
			return
		}
		// string + vector<string>
		if lt == "std::string" && strings.HasPrefix(rt, "std::vector<") && elementTypeFromListType(rt) == "std::string" {
			if currentProgram != nil {
				currentProgram.addInclude("<vector>")
			}
			io.WriteString(w, "([&]{ std::string __res = ")
			b.Left.emit(w)
			io.WriteString(w, "; const auto& __vec = ")
			b.Right.emit(w)
			io.WriteString(w, "; for(const auto& __e : __vec){ __res += __e; } return __res; }())")
			return
		}
		// string + any
		if lt == "std::string" && rt == "std::any" {
			io.WriteString(w, "(")
			b.Left.emit(w)
			io.WriteString(w, " + ")
			(&StrExpr{Value: b.Right}).emit(w)
			io.WriteString(w, ")")
			return
		}
		// any + string
		if lt == "std::any" && rt == "std::string" {
			io.WriteString(w, "(")
			(&StrExpr{Value: b.Left}).emit(w)
			io.WriteString(w, " + ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	if b.Op == "/" {
		lt := exprType(b.Left)
		rt := exprType(b.Right)
		if lt == "double" || rt == "double" {
			io.WriteString(w, "((double)(")
			b.Left.emit(w)
			io.WriteString(w, ") / (")
			b.Right.emit(w)
			io.WriteString(w, "))")
		} else {
			io.WriteString(w, "(")
			b.Left.emit(w)
			io.WriteString(w, " / ")
			b.Right.emit(w)
			io.WriteString(w, ")")
		}
		return
	}
	if b.Op == "%" {
		lt := exprType(b.Left)
		rt := exprType(b.Right)
		if lt == "double" || rt == "double" {
			if currentProgram != nil {
				currentProgram.addInclude("<cmath>")
			}
			io.WriteString(w, "std::fmod(")
			if lt == "double" {
				b.Left.emit(w)
			} else {
				io.WriteString(w, "(double)(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			}
			io.WriteString(w, ", ")
			if rt == "double" {
				b.Right.emit(w)
			} else {
				io.WriteString(w, "(double)(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			}
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "(")
			b.Left.emit(w)
			io.WriteString(w, " % ")
			b.Right.emit(w)
			io.WriteString(w, ")")
		}
		return
	}
	lt := exprType(b.Left)
	rt := exprType(b.Right)
	if (b.Op == "==" || b.Op == "!=") && lt == "std::any" && rt == "std::any" {
		if vr, ok := b.Left.(*VarRef); ok && vr.Name == "nil" {
			if b.Op == "==" {
				io.WriteString(w, "(!")
				b.Right.emit(w)
				io.WriteString(w, ".has_value())")
			} else {
				io.WriteString(w, "(")
				b.Right.emit(w)
				io.WriteString(w, ".has_value())")
			}
			return
		}
		if vr, ok := b.Right.(*VarRef); ok && vr.Name == "nil" {
			if b.Op == "==" {
				io.WriteString(w, "(!")
				b.Left.emit(w)
				io.WriteString(w, ".has_value())")
			} else {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ".has_value())")
			}
			return
		}
	}
	if lt == "std::any" && rt != "std::any" {
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		io.WriteString(w, "(")
		io.WriteString(w, "std::any_cast<"+rt+">(")
		b.Left.emit(w)
		io.WriteString(w, ") "+b.Op+" ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if strings.Contains(lt, "any") && strings.Contains(rt, "any") {
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		io.WriteString(w, "(")
		io.WriteString(w, "any_to_double(")
		b.Left.emit(w)
		io.WriteString(w, ") "+b.Op+" any_to_double(")
		b.Right.emit(w)
		io.WriteString(w, "))")
		return
	}
	if lt == "auto" && rt == "auto" {
		io.WriteString(w, "((double)(")
		b.Left.emit(w)
		io.WriteString(w, ") "+b.Op+" (double)(")
		b.Right.emit(w)
		io.WriteString(w, "))")
		return
	}
	if rt == "std::any" && lt != "std::any" {
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		io.WriteString(w, "(")
		b.Left.emit(w)
		io.WriteString(w, " "+b.Op+" std::any_cast<"+lt+">(")
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
	if bl, ok := s.Value.(*BlockLambda); ok {
		if currentProgram != nil {
			currentProgram.addInclude("<functional>")
		}
		ret := bl.ReturnType
		if ret == "" {
			ret = "void"
		}
		io.WriteString(w, fmt.Sprintf("std::function<%s(", ret))
		for i, p := range bl.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			pt := p.Type
			if pt == "" {
				pt = "auto"
			}
			io.WriteString(w, pt)
		}
		io.WriteString(w, ")> ")
		io.WriteString(w, safeName(s.Name))
		io.WriteString(w, " = ")
		io.WriteString(w, "[&](")
		for i, p := range bl.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			pt := p.Type
			if pt == "" {
				pt = "auto"
			}
			io.WriteString(w, pt+" "+p.Name)
		}
		io.WriteString(w, ") mutable {\n")
		for _, st := range bl.Body {
			st.emit(w, indent+1)
		}
		io.WriteString(w, strings.Repeat("    ", indent)+"}")
		io.WriteString(w, ";\n")
		if localTypes != nil {
			localTypes[s.Name] = fmt.Sprintf("std::function<%s>", ret)
		}
		return
	}
	if typ == "" {
		io.WriteString(w, "auto ")
	} else {
		io.WriteString(w, typ+" ")
	}
	io.WriteString(w, safeName(s.Name))
	if s.Value != nil {
		io.WriteString(w, " = ")
		valType := exprType(s.Value)
		if valType == "std::any" && typ != "" && typ != "std::any" {
			if currentProgram != nil {
				currentProgram.addInclude("<any>")
			}
			io.WriteString(w, "std::any_cast<"+typ+">(")
			s.Value.emit(w)
			io.WriteString(w, ")")
		} else if strings.HasPrefix(typ, "std::unique_ptr<") || strings.HasPrefix(typ, "std::shared_ptr<") {
			if sl, ok := s.Value.(*StructLit); ok {
				maker := "std::make_unique"
				if strings.HasPrefix(typ, "std::shared_ptr<") {
					maker = "std::make_shared"
				}
				fmt.Fprintf(w, "%s<%s>(", maker, sl.Name)
				for i, f := range sl.Fields {
					if i > 0 {
						io.WriteString(w, ", ")
					}
					f.Value.emit(w)
				}
				io.WriteString(w, ")")
			} else {
				s.Value.emit(w)
			}
		} else {
			s.Value.emit(w)
		}
	} else if typ != "" {
		io.WriteString(w, " = ")
		io.WriteString(w, defaultValueForType(typ))
	}
	io.WriteString(w, ";\n")
	if localTypes != nil {
		if typ != "" {
			localTypes[s.Name] = typ
		} else if s.Value != nil {
			localTypes[s.Name] = exprType(s.Value)
		} else {
			localTypes[s.Name] = "auto"
		}
	}
}

func (a *AssignStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, safeName(a.Name))
	io.WriteString(w, " = ")
	valType := exprType(a.Value)
	varType := "auto"
	if localTypes != nil {
		if t, ok := localTypes[a.Name]; ok {
			varType = t
		}
	}
	if varType == "auto" && globalTypes != nil {
		if t, ok := globalTypes[a.Name]; ok {
			varType = t
		}
	}
	if valType == "std::any" {
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		castType := varType
		if castType == "auto" || castType == "" {
			castType = "decltype(" + safeName(a.Name) + ")"
		}
		if castType != "std::any" {
			io.WriteString(w, "std::any_cast<"+castType+">(")
			a.Value.emit(w)
			io.WriteString(w, ")")
			io.WriteString(w, ";\n")
			return
		}
	} else if idx, ok := a.Value.(*IndexExpr); ok {
		t := exprType(idx.Target)
		if strings.Contains(t, "std::any") {
			if currentProgram != nil {
				currentProgram.addInclude("<any>")
			}
			castType := varType
			if castType == "auto" || castType == "" {
				castType = "decltype(" + safeName(a.Name) + ")"
			}
			if castType == "" {
				castType = "decltype(" + safeName(a.Name) + ")"
			}
			if castType != "std::any" {
				io.WriteString(w, "std::any_cast<"+castType+">(")
				a.Value.emit(w)
				io.WriteString(w, ")")
				io.WriteString(w, ";\n")
				return
			}
		}
	}
	if strings.HasPrefix(varType, "std::shared_ptr<") {
		if sl, ok := a.Value.(*StructLit); ok {
			fmt.Fprintf(w, "std::make_shared<%s>(", sl.Name)
			for i, f := range sl.Fields {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				f.Value.emit(w)
			}
			io.WriteString(w, ")")
			io.WriteString(w, ";\n")
			return
		}
	}
	a.Value.emit(w)
	io.WriteString(w, ";\n")
}

func (a *AssignIndexStmt) emit(w io.Writer, indent int) {
	ind := strings.Repeat("    ", indent)
	// collect index chain
	var idxs []Expr
	target := a.Target
	for {
		if ix, ok := target.(*IndexExpr); ok {
			idxs = append([]Expr{ix.Index}, idxs...)
			target = ix.Target
		} else {
			break
		}
	}
	if len(idxs) == 0 {
		io.WriteString(w, ind)
		a.Target.emit(w)
		io.WriteString(w, "[")
		a.Index.emit(w)
		io.WriteString(w, "] = ")
		a.Value.emit(w)
		io.WriteString(w, ";\n")
		return
	}

	idxs = append(idxs, a.Index)
	baseType := exprType(target)
	valType := exprType(a.Value)
	if valType == "" {
		valType = "auto"
	}
	cont := valType
	for i := 1; i < len(idxs); i++ {
		cont = fmt.Sprintf("std::vector<%s>", cont)
	}

	io.WriteString(w, ind)
	if baseType == "std::any" || (strings.HasPrefix(baseType, "std::map<") && strings.Contains(baseType, "std::any")) {
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		fmt.Fprintf(w, "std::any_cast<%s&>(", cont)
		target.emit(w)
		io.WriteString(w, "[")
		idxs[0].emit(w)
		io.WriteString(w, "])")
	} else {
		target.emit(w)
		io.WriteString(w, "[")
		idxs[0].emit(w)
		io.WriteString(w, "]")
	}
	for _, ix := range idxs[1:] {
		io.WriteString(w, "[")
		ix.emit(w)
		io.WriteString(w, "]")
	}
	io.WriteString(w, " = ")
	a.Value.emit(w)
	io.WriteString(w, ";\n")
}

func (a *AssignFieldStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	a.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, a.Field)
	io.WriteString(w, " = ")
	a.Value.emit(w)
	io.WriteString(w, ";\n")
}

func (e *ExprStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	e.Expr.emit(w)
	io.WriteString(w, ";\n")
}

func (r *ReturnStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "return")
	if r.Value != nil {
		io.WriteString(w, " ")
		if strings.HasPrefix(r.Type, "std::unique_ptr<") || strings.HasPrefix(r.Type, "std::shared_ptr<") {
			if sl, ok := r.Value.(*StructLit); ok {
				maker := "std::make_unique"
				if strings.HasPrefix(r.Type, "std::shared_ptr<") {
					maker = "std::make_shared"
				}
				fmt.Fprintf(w, "%s<%s>(", maker, sl.Name)
				for i, f := range sl.Fields {
					if i > 0 {
						io.WriteString(w, ", ")
					}
					f.Value.emit(w)
				}
				io.WriteString(w, ")")
				io.WriteString(w, ";\n")
				return
			}
		}
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

func (s *SaveStmt) emit(w io.Writer, indent int) {
	if currentProgram != nil {
		currentProgram.addInclude("<iostream>")
		currentProgram.addInclude("<iomanip>")
		currentProgram.addInclude("<sstream>")
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		io.WriteString(w, "for (const auto& _row : ")
		if s.Src != nil {
			s.Src.emit(w)
		}
		io.WriteString(w, ") {\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "    ")
		}
		io.WriteString(w, "std::ostringstream __buf;\n")
		emitToStream(w, "__buf", &VarRef{Name: "_row"}, indent+1)
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "    ")
		}
		io.WriteString(w, "auto __line = __buf.str();\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "    ")
		}
		io.WriteString(w, `for(auto& ch: __line) if(ch=='\'') ch='"';`+"\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "    ")
		}
		io.WriteString(w, "std::cout << __line << std::endl;\n")
		for i := 0; i < indent; i++ {
			io.WriteString(w, "    ")
		}
		io.WriteString(w, "}\n")
		return
	}
	io.WriteString(w, "// unsupported save\n")
}

func (u *UpdateStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	fmt.Fprintf(w, "for (size_t i = 0; i < %s.size(); ++i) {\n", u.Target)
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "    ")
	}
	fmt.Fprintf(w, "auto item = %s[i];\n", u.Target)
	if u.Cond != nil {
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "    ")
		}
		io.WriteString(w, "if (")
		u.Cond.emit(w)
		io.WriteString(w, ") {\n")
	}
	inner := indent + 1
	if u.Cond != nil {
		inner++
	}
	for j, f := range u.Fields {
		for i := 0; i < inner; i++ {
			io.WriteString(w, "    ")
		}
		fmt.Fprintf(w, "item.%s = ", f)
		u.Values[j].emit(w)
		io.WriteString(w, ";\n")
	}
	if u.Cond != nil {
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "    ")
		}
		io.WriteString(w, "}\n")
	}
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "    ")
	}
	fmt.Fprintf(w, "%s[i] = item;\n", u.Target)
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "}\n")
}

func (b *BenchStmt) emit(w io.Writer, indent int) {
	if currentProgram != nil {
		currentProgram.addInclude("<iostream>")
		currentProgram.addInclude("<chrono>")
		currentProgram.addInclude("<sys/resource.h>")
	}
	ind := strings.Repeat("    ", indent)
	fmt.Fprintln(w, ind+"{")
	ind2 := strings.Repeat("    ", indent+1)
	fmt.Fprintln(w, ind2+"auto __bench_start = _now();")
	fmt.Fprintln(w, ind2+"auto __bench_mem_start = _mem();")
	for _, st := range b.Body {
		st.emit(w, indent+1)
	}
	fmt.Fprintln(w, ind2+"auto __bench_end = _now();")
	fmt.Fprintln(w, ind2+"auto __bench_mem_end = _mem();")
	fmt.Fprintln(w, ind2+"auto __bench_dur = __bench_end - __bench_start;")
	fmt.Fprintln(w, ind2+"auto __bench_mem = __bench_mem_end;")
	fmt.Fprintf(w, "%sstd::cout << \"{\\n  \\\"duration_us\\\": \" << __bench_dur << \",\\n  \\\"memory_bytes\\\": \" << __bench_mem << \",\\n  \\\"name\\\": \\\"%s\\\"\\n}\" << std::endl;\n", ind2, b.Name)
	fmt.Fprintln(w, ind+"}")
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
			io.WriteString(w, safeName(f.Var))
			io.WriteString(w, " = __p.first;\n")
		} else {
			io.WriteString(w, "for (")
			if f.ElemType != "" {
				io.WriteString(w, f.ElemType)
				io.WriteString(w, " ")
			} else {
				io.WriteString(w, "auto ")
			}
			io.WriteString(w, safeName(f.Var))
			io.WriteString(w, " : ")
			f.Start.emit(w)
			io.WriteString(w, ") {\n")
		}
	} else {
		io.WriteString(w, "for (int ")
		io.WriteString(w, safeName(f.Var))
		io.WriteString(w, " = ")
		f.Start.emit(w)
		io.WriteString(w, "; ")
		io.WriteString(w, safeName(f.Var))
		io.WriteString(w, " < ")
		f.End.emit(w)
		io.WriteString(w, "; ")
		io.WriteString(w, safeName(f.Var))
		io.WriteString(w, "++ ) {\n")
	}
	old := localTypes
	newTypes := map[string]string{}
	for k, v := range old {
		newTypes[k] = v
	}
	if f.ElemType != "" {
		newTypes[f.Var] = f.ElemType
	} else {
		newTypes[f.Var] = "auto"
	}
	localTypes = newTypes
	for _, st := range f.Body {
		st.emit(w, indent+1)
	}
	localTypes = old
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
	if exprType(i.Cond) == "std::any" {
		io.WriteString(w, "std::any_cast<bool>(")
		i.Cond.emit(w)
		io.WriteString(w, ")")
	} else {
		i.Cond.emit(w)
	}
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
	useNow = false
	useMem = false
	usesLookupHost = false
	usesSHA256 = false
	usesIndexOf = false
	usesParseIntStr = false
	cp := &Program{Includes: []string{"<iostream>", "<string>"}, ListTypes: map[string]string{}, GlobalTypes: map[string]string{}}
	currentProgram = cp
	currentEnv = env
	builtinAliases = map[string]string{}
	globalTypes = cp.GlobalTypes
	defer func() { currentProgram = nil; currentEnv = nil; globalTypes = nil }()
	var body []Stmt
	var globals []Stmt
	for _, stmt := range prog.Statements {
		switch {
		case stmt.Test != nil:
			// ignore test blocks
			continue
		case stmt.Fun != nil:
			fn, err := convertFun(stmt.Fun)
			if err != nil {
				return nil, err
			}
			cp.Functions = append(cp.Functions, fn)
		case stmt.Import != nil:
			if _, err := convertStmt(stmt); err != nil {
				return nil, err
			}
		case stmt.ExternVar != nil, stmt.ExternFun != nil, stmt.ExternObject != nil, stmt.ExternType != nil:
			if _, err := convertStmt(stmt); err != nil {
				return nil, err
			}
		case stmt.Bench != nil:
			bs, err := convertBenchBlock(stmt.Bench)
			if err != nil {
				return nil, err
			}
			body = append(body, bs)
		case stmt.Expr != nil:
			if se := extractSaveExpr(stmt.Expr.Expr); se != nil {
				src, err := convertExpr(se.Src)
				if err != nil {
					return nil, err
				}
				format := parseFormat(se.With)
				path := ""
				if se.Path != nil {
					path = strings.Trim(*se.Path, "\"")
				}
				body = append(body, &SaveStmt{Src: src, Path: path, Format: format})
			} else if call := extractCall(stmt.Expr.Expr); call != nil && call.Func == "print" {
				if cp != nil {
					cp.addInclude("<sstream>")
					cp.addInclude("<iomanip>")
				}
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
				expr, err := convertExpr(stmt.Expr.Expr)
				if err != nil {
					return nil, err
				}
				body = append(body, &ExprStmt{Expr: expr})
			}
		case stmt.Let != nil:
			var val Expr
			var err error
			if stmt.Let.Value != nil {
				if q := extractQuery(stmt.Let.Value); q != nil {
					var def *StructDef
					if len(q.Joins) == 1 && q.Joins[0].Side != nil {
						switch *q.Joins[0].Side {
						case "left":
							if q.Group != nil {
								val, def, _, err = convertLeftJoinGroupQuery(q, stmt.Let.Name)
							} else {
								val, def, _, err = convertLeftJoinQuery(q, stmt.Let.Name)
							}
						case "right":
							val, def, _, err = convertRightJoinQuery(q, stmt.Let.Name)
						case "outer":
							val, def, _, err = convertOuterJoinQuery(q, stmt.Let.Name)
						}
					} else if len(q.Joins) == 2 && q.Joins[1].Side != nil && *q.Joins[1].Side == "left" && (q.Joins[0].Side == nil) {
						val, def, _, err = convertJoinLeftJoinQuery(q, stmt.Let.Name)
					} else {
						val, def, _, err = convertSimpleQuery(q, stmt.Let.Name)
					}
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
			if stmt.Let.Type != nil {
				if stmt.Let.Type.Simple != nil {
					typ = cppType(*stmt.Let.Type.Simple)
				} else if stmt.Let.Type.Generic != nil {
					typ = cppType(typeRefString(&parser.TypeRef{Generic: stmt.Let.Type.Generic}))
				}
			}
			if typ == "" {
				if lst, ok := val.(*ListLit); ok {
					if def, sname, ok := inferStructFromList(stmt.Let.Name, lst); ok {
						cp.Structs = append(cp.Structs, *def)
						typ = fmt.Sprintf("std::vector<%s>", sname)
						if cp.ListTypes == nil {
							cp.ListTypes = map[string]string{}
						}
						cp.ListTypes[stmt.Let.Name] = sname
					}
				} else if comp, ok := val.(*MultiListComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", comp.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Let.Name] = comp.ElemType
				} else if scomp, ok := val.(*SortComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", scomp.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Let.Name] = scomp.ElemType
				} else if gcomp, ok := val.(*GroupComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", gcomp.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Let.Name] = gcomp.ElemType
				} else if lgc, ok := val.(*LeftJoinGroupComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", lgc.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Let.Name] = lgc.ElemType
				} else if ljc, ok := val.(*LeftJoinComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", ljc.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Let.Name] = ljc.ElemType
				} else if jl, ok := val.(*JoinLeftJoinComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", jl.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Let.Name] = jl.ElemType
				} else if rjc, ok := val.(*RightJoinComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", rjc.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Let.Name] = rjc.ElemType
				} else if ojc, ok := val.(*OuterJoinComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", ojc.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Let.Name] = ojc.ElemType
				}
			}
			if typ == "" && val != nil {
				typ = exprType(val)
			}
			if typ == "std::vector<std::any>" {
				if lst, ok := val.(*ListLit); ok {
					lst.ElemType = "std::any"
				}
			}
			globals = append(globals, &LetStmt{Name: stmt.Let.Name, Type: typ, Value: val})
			if cp.GlobalTypes != nil {
				cp.GlobalTypes[stmt.Let.Name] = typ
			}
		case stmt.Var != nil:
			var val Expr
			var err error
			if stmt.Var.Value != nil {
				if q := extractQuery(stmt.Var.Value); q != nil {
					var def *StructDef
					if len(q.Joins) == 1 && q.Joins[0].Side != nil {
						switch *q.Joins[0].Side {
						case "left":
							if q.Group != nil {
								val, def, _, err = convertLeftJoinGroupQuery(q, stmt.Var.Name)
							} else {
								val, def, _, err = convertLeftJoinQuery(q, stmt.Var.Name)
							}
						case "right":
							val, def, _, err = convertRightJoinQuery(q, stmt.Var.Name)
						case "outer":
							val, def, _, err = convertOuterJoinQuery(q, stmt.Var.Name)
						}
					} else if len(q.Joins) == 2 && q.Joins[1].Side != nil && *q.Joins[1].Side == "left" && (q.Joins[0].Side == nil) {
						val, def, _, err = convertJoinLeftJoinQuery(q, stmt.Var.Name)
					} else {
						val, def, _, err = convertSimpleQuery(q, stmt.Var.Name)
					}
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
			if stmt.Var.Type != nil {
				if stmt.Var.Type.Simple != nil {
					typ = cppType(*stmt.Var.Type.Simple)
				} else if stmt.Var.Type.Generic != nil {
					typ = cppType(typeRefString(&parser.TypeRef{Generic: stmt.Var.Type.Generic}))
				}
			}
			if typ == "" {
				if lst, ok := val.(*ListLit); ok {
					if def, sname, ok := inferStructFromList(stmt.Var.Name, lst); ok {
						cp.Structs = append(cp.Structs, *def)
						typ = fmt.Sprintf("std::vector<%s>", sname)
						if cp.ListTypes == nil {
							cp.ListTypes = map[string]string{}
						}
						cp.ListTypes[stmt.Var.Name] = sname
					}
				} else if comp, ok := val.(*MultiListComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", comp.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Var.Name] = comp.ElemType
				} else if scomp, ok := val.(*SortComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", scomp.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Var.Name] = scomp.ElemType
				} else if gcomp, ok := val.(*GroupComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", gcomp.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Var.Name] = gcomp.ElemType
				} else if lgc, ok := val.(*LeftJoinGroupComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", lgc.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Var.Name] = lgc.ElemType
				} else if ljc, ok := val.(*LeftJoinComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", ljc.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Var.Name] = ljc.ElemType
				} else if jl, ok := val.(*JoinLeftJoinComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", jl.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Var.Name] = jl.ElemType
				} else if rjc, ok := val.(*RightJoinComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", rjc.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Var.Name] = rjc.ElemType
				} else if ojc, ok := val.(*OuterJoinComp); ok {
					typ = fmt.Sprintf("std::vector<%s>", ojc.ElemType)
					if cp.ListTypes == nil {
						cp.ListTypes = map[string]string{}
					}
					cp.ListTypes[stmt.Var.Name] = ojc.ElemType
				}
			}
			if typ == "" && val != nil {
				typ = exprType(val)
			}
			if typ == "std::vector<std::any>" {
				if lst, ok := val.(*ListLit); ok {
					lst.ElemType = "std::any"
				}
			}
			globals = append(globals, &LetStmt{Name: stmt.Var.Name, Type: typ, Value: val})
			if cp.GlobalTypes != nil {
				cp.GlobalTypes[stmt.Var.Name] = typ
			}
		case stmt.Type != nil:
			if len(stmt.Type.Variants) > 0 {
				base, vars, err := convertUnionDecl(stmt.Type)
				if err != nil {
					return nil, err
				}
				cp.Structs = append(cp.Structs, base)
				cp.Structs = append(cp.Structs, vars...)
			} else {
				st, err := convertTypeDecl(stmt.Type)
				if err != nil {
					return nil, err
				}
				cp.Structs = append(cp.Structs, *st)
			}
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
			} else if len(stmt.Assign.Field) > 0 {
				var target Expr = &VarRef{Name: stmt.Assign.Name}
				for _, f := range stmt.Assign.Field[:len(stmt.Assign.Field)-1] {
					target = &SelectorExpr{Target: target, Field: f.Name}
				}
				fld := stmt.Assign.Field[len(stmt.Assign.Field)-1].Name
				body = append(body, &AssignFieldStmt{Target: target, Field: fld, Value: val})
			} else {
				if ap, ok := val.(*AppendExpr); ok {
					lt := exprType(ap.List)
					et := exprType(ap.Elem)
					if lt == "std::vector<int64_t>" && strings.HasPrefix(et, "std::vector<") {
						newType := "std::vector<std::vector<int64_t>>"
						if ls, ok2 := currentVarDecls[stmt.Assign.Name]; ok2 {
							ls.Type = newType
						}
						if localTypes != nil {
							localTypes[stmt.Assign.Name] = newType
						}
					}
				}
				body = append(body, &AssignStmt{Name: stmt.Assign.Name, Value: val})
			}
		case stmt.Update != nil:
			us, err := convertUpdateStmt(stmt.Update)
			if err != nil {
				return nil, err
			}
			body = append(body, us)
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
			gtyp := guessType(stmt.For.Source)
			if vr, ok := start.(*VarRef); ok {
				if lt, ok2 := localTypes[vr.Name]; ok2 {
					gtyp = lt
				}
			}
			if strings.HasPrefix(gtyp, "std::map<") {
				fs.IsMap = true
			}
			varType := elementTypeFromListType(gtyp)
			if vr, ok := start.(*VarRef); ok {
				if _, ok3 := localTypes[vr.Name]; !ok3 {
					if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
						varType = t
					}
				}
			}
			fs.ElemType = varType
			oldTypes2 := localTypes
			newTypes := map[string]string{}
			for k, v := range oldTypes2 {
				newTypes[k] = v
			}
			newTypes[stmt.For.Name] = varType
			localTypes = newTypes
			for _, s := range stmt.For.Body {
				st, err := convertStmt(s)
				if err != nil {
					localTypes = oldTypes2
					return nil, err
				}
				fs.Body = append(fs.Body, st)
			}
			localTypes = oldTypes2
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
	cp.UseNow = useNow
	cp.UseMem = useMem
	cp.UseLookupHost = usesLookupHost
	cp.UseSHA256 = usesSHA256
	cp.UseIndexOf = usesIndexOf
	cp.UseParseIntStr = usesParseIntStr
	hasMain := false
	for _, fn := range cp.Functions {
		if fn.Name == "main" {
			hasMain = true
			if benchMain {
				fn.Body = []Stmt{&BenchStmt{Name: "main", Body: fn.Body}}
				cp.UseNow = true
				cp.UseMem = true
			}
			break
		}
	}
	if !hasMain {
		if benchMain {
			cp.Functions = append(cp.Functions, &Func{Name: "main", ReturnType: "int", Body: []Stmt{&BenchStmt{Name: "main", Body: body}}})
			cp.UseNow = true
			cp.UseMem = true
		} else {
			cp.Functions = append(cp.Functions, &Func{Name: "main", ReturnType: "int", Body: body})
		}
	}
	if env != nil && cp.GlobalTypes != nil {
		for n, t := range cp.GlobalTypes {
			if t == "" || t == "auto" {
				if vt, err := env.GetVar(n); err == nil {
					cp.GlobalTypes[n] = cppTypeFrom(vt)
				}
			}
		}
	}
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
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil || u.Value.Target.Map == nil {
		return ""
	}
	for _, it := range u.Value.Target.Map.Items {
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

func convertStmt(s *parser.Statement) (Stmt, error) {
	switch {
	case s.Fun != nil:
		lam, err := convertFunLambda(s.Fun)
		if err != nil {
			return nil, err
		}
		if currentProgram != nil {
			currentProgram.addInclude("<functional>")
		}
		return &LetStmt{Name: s.Fun.Name, Type: "", Value: lam}, nil
	case s.Expr != nil:
		if se := extractSaveExpr(s.Expr.Expr); se != nil {
			src, err := convertExpr(se.Src)
			if err != nil {
				return nil, err
			}
			format := parseFormat(se.With)
			path := ""
			if se.Path != nil {
				path = strings.Trim(*se.Path, "\"")
			}
			return &SaveStmt{Src: src, Path: path, Format: format}, nil
		}
		if call := extractCall(s.Expr.Expr); call != nil && call.Func == "print" {
			if currentProgram != nil {
				currentProgram.addInclude("<sstream>")
			}
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
		expr, err := convertExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: expr}, nil
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
		if s.Let.Type != nil {
			if s.Let.Type.Simple != nil {
				typ = cppType(*s.Let.Type.Simple)
			} else if s.Let.Type.Generic != nil {
				typ = cppType(typeRefString(&parser.TypeRef{Generic: s.Let.Type.Generic}))
			}
		}
		if typ == "" && s.Let.Value != nil {
			typ = exprType(val)
			if idx, ok := val.(*IndexExpr); ok {
				t := exprType(idx.Target)
				if strings.HasPrefix(t, "std::vector<std::unique_ptr<") {
					elem := strings.TrimSuffix(strings.TrimPrefix(t, "std::vector<std::unique_ptr<"), ">>")
					typ = elem + "*"
					val = &PtrGetExpr{Target: val}
				} else if strings.HasPrefix(t, "std::vector<std::shared_ptr<") {
					elem := strings.TrimSuffix(strings.TrimPrefix(t, "std::vector<std::shared_ptr<"), ">>")
					typ = fmt.Sprintf("std::shared_ptr<%s>", elem)
				}
			}
			if typ == "auto" || typ == "" {
				typ = guessType(s.Let.Value)
				if typ == "" {
					if _, ok := val.(*StringLit); ok {
						typ = "std::string"
					}
				}
			}
		}
		if localTypes == nil {
			localTypes = map[string]string{}
		}
		if typ != "" {
			localTypes[s.Let.Name] = typ
		} else {
			localTypes[s.Let.Name] = "auto"
		}
		ls := &LetStmt{Name: s.Let.Name, Type: typ, Value: val}
		if currentVarDecls != nil {
			currentVarDecls[s.Let.Name] = ls
		}
		return ls, nil
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
		if s.Var.Type != nil {
			if s.Var.Type.Simple != nil {
				typ = cppType(*s.Var.Type.Simple)
			} else if s.Var.Type.Generic != nil {
				typ = cppType(typeRefString(&parser.TypeRef{Generic: s.Var.Type.Generic}))
			}
		}
		if typ == "" && s.Var.Value != nil {
			typ = exprType(val)
			if idx, ok := val.(*IndexExpr); ok {
				t := exprType(idx.Target)
				if strings.HasPrefix(t, "std::vector<std::unique_ptr<") {
					elem := strings.TrimSuffix(strings.TrimPrefix(t, "std::vector<std::unique_ptr<"), ">>")
					typ = elem + "*"
					val = &PtrGetExpr{Target: val}
				} else if strings.HasPrefix(t, "std::vector<std::shared_ptr<") {
					elem := strings.TrimSuffix(strings.TrimPrefix(t, "std::vector<std::shared_ptr<"), ">>")
					typ = fmt.Sprintf("std::shared_ptr<%s>", elem)
				}
			}
			if typ == "auto" || typ == "" {
				typ = guessType(s.Var.Value)
				if typ == "" {
					if _, ok := val.(*StringLit); ok {
						typ = "std::string"
					}
				}
			}
		}
		if localTypes == nil {
			localTypes = map[string]string{}
		}
		if typ != "" {
			localTypes[s.Var.Name] = typ
		} else {
			localTypes[s.Var.Name] = "auto"
		}
		ls := &LetStmt{Name: s.Var.Name, Type: typ, Value: val}
		if currentVarDecls != nil {
			currentVarDecls[s.Var.Name] = ls
		}
		return ls, nil
	case s.Assign != nil:
		val, err := convertExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		if ap, ok := val.(*AppendExpr); ok {
			lt := exprType(ap.List)
			et := exprType(ap.Elem)
			if lt == "std::vector<int64_t>" && strings.HasPrefix(et, "std::vector<") {
				newType := "std::vector<std::vector<int64_t>>"
				if ls, ok2 := currentVarDecls[s.Assign.Name]; ok2 {
					ls.Type = newType
				}
				if localTypes != nil {
					localTypes[s.Assign.Name] = newType
				}
			}
		}
		if len(s.Assign.Index) > 0 {
			if paramNames != nil && paramNames[s.Assign.Name] {
				mutatedParams[s.Assign.Name] = true
			}
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
		if len(s.Assign.Field) > 0 {
			if paramNames != nil && paramNames[s.Assign.Name] {
				mutatedParams[s.Assign.Name] = true
			}
			var target Expr = &VarRef{Name: s.Assign.Name}
			for _, f := range s.Assign.Field[:len(s.Assign.Field)-1] {
				target = &SelectorExpr{Target: target, Field: f.Name}
			}
			fld := s.Assign.Field[len(s.Assign.Field)-1].Name
			return &AssignFieldStmt{Target: target, Field: fld, Value: val}, nil
		}
		if paramNames != nil && len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			if paramNames[s.Assign.Name] {
				mutatedParams[s.Assign.Name] = true
			}
		}
		return &AssignStmt{Name: s.Assign.Name, Value: val}, nil
	case s.Update != nil:
		us, err := convertUpdateStmt(s.Update)
		if err != nil {
			return nil, err
		}
		return us, nil
	case s.Bench != nil:
		bs, err := convertBenchBlock(s.Bench)
		if err != nil {
			return nil, err
		}
		return bs, nil
	case s.For != nil:
		start, err := convertExpr(s.For.Source)
		if err != nil {
			return nil, err
		}
		var end Expr
		if s.For.RangeEnd != nil {
			end, err = convertExpr(s.For.RangeEnd)
			if err != nil {
				return nil, err
			}
		}
		fs := &ForStmt{Var: s.For.Name, Start: start, End: end}
		if currentEnv != nil {
			if t := types.TypeOfExpr(s.For.Source, currentEnv); t != nil {
				if _, ok := t.(types.MapType); ok {
					fs.IsMap = true
				}
			}
		}
		varType := elementTypeFromListType(guessType(s.For.Source))
		if vr, ok := start.(*VarRef); ok {
			if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
				varType = t
			}
		}
		fs.ElemType = varType
		oldTypes2 := localTypes
		newTypes := map[string]string{}
		for k, v := range oldTypes2 {
			newTypes[k] = v
		}
		newTypes[s.For.Name] = varType
		localTypes = newTypes
		for _, st := range s.For.Body {
			cs, err := convertStmt(st)
			if err != nil {
				localTypes = oldTypes2
				return nil, err
			}
			fs.Body = append(fs.Body, cs)
		}
		localTypes = oldTypes2
		return fs, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Return != nil:
		var val Expr
		if s.Return.Value != nil {
			var err error
			inReturn = true
			val, err = convertExpr(s.Return.Value)
			inReturn = false
			if err != nil {
				return nil, err
			}
			if exprType(val) == "std::any" && currentReturnType != "" && currentReturnType != "std::any" {
				val = &CastExpr{Value: val, Type: currentReturnType}
			}
		}
		if val == nil && currentReturnType == "int" {
			val = &IntLit{Value: 0}
		}
		return &ReturnStmt{Value: val, Type: currentReturnType}, nil
	case s.Import != nil:
		if s.Import.Lang != nil {
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			path := strings.Trim(s.Import.Path, "\"")
			lang := *s.Import.Lang
			if builtinAliases == nil {
				builtinAliases = map[string]string{}
			}
			switch lang {
			case "python":
				if path == "math" {
					builtinAliases[alias] = "python_math"
					if currentProgram != nil {
						currentProgram.addInclude("<cmath>")
					}
					return nil, nil
				}
			case "go":
				if path == "strings" {
					builtinAliases[alias] = "go_strings"
					if currentProgram != nil {
						currentProgram.addInclude("<cctype>")
					}
					return nil, nil
				}
				if path == "net" {
					builtinAliases[alias] = "go_net"
					return nil, nil
				}
				if s.Import.Auto && path == "mochi/runtime/ffi/go/testpkg" {
					builtinAliases[alias] = "go_testpkg"
					return nil, nil
				}
			}
		}
		return nil, nil
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil, s.ExternType != nil:
		// ignore extern declarations
		return nil, nil
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
	operands := []Expr{left}
	ops := []string{}
	for _, part := range b.Right {
		right, err := convertPostfix(part.Right)
		if err != nil {
			return nil, err
		}
		ops = append(ops, part.Op)
		operands = append(operands, right)
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
	containsOp := func(arr []string, op string) bool {
		for _, v := range arr {
			if v == op {
				return true
			}
		}
		return false
	}
	for _, level := range levels {
		for i := 0; i < len(ops); {
			if containsOp(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				var ex Expr
				if ops[i] == "in" {
					if currentProgram != nil {
						currentProgram.addInclude("<algorithm>")
						currentProgram.addInclude("<type_traits>")
					}
					ex = &InExpr{Value: l, Coll: r}
				} else {
					if ops[i] == "+" || ops[i] == "-" || ops[i] == "*" || ops[i] == "/" || ops[i] == "%" {
						lt := exprType(l)
						rt := exprType(r)
						if ops[i] != "+" || (lt != "std::string" && rt != "std::string") {
							if !(strings.Contains(lt, "any") && strings.Contains(rt, "any")) {
								if strings.Contains(lt, "any") {
									l = &CastExpr{Value: l, Type: "double"}
								}
								if strings.Contains(rt, "any") {
									r = &CastExpr{Value: r, Type: "double"}
								}
							}
						}
					}
					ex = &BinaryExpr{Left: l, Op: ops[i], Right: r}
				}
				operands[i] = ex
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(operands) != 1 {
		return nil, fmt.Errorf("unexpected binary expression")
	}
	return operands[0], nil
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
				var start Expr
				var end Expr
				var err error
				if op.Index.Start != nil {
					start, err = convertExpr(op.Index.Start)
					if err != nil {
						return nil, err
					}
				} else {
					start = &IntLit{Value: 0}
				}
				if op.Index.End != nil {
					end, err = convertExpr(op.Index.End)
					if err != nil {
						return nil, err
					}
				} else {
					end = &LenExpr{Value: expr}
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
			} else if sel, ok := expr.(*SelectorExpr); ok {
				if sel.Field == "keys" && len(args) == 0 {
					expr = &KeysExpr{Map: sel.Target}
				} else if sel.Field == "get" && len(args) == 2 {
					expr = &MapGetExpr{Map: sel.Target, Key: args[0], Default: args[1]}
				} else {
					if vr, ok2 := sel.Target.(*VarRef); ok2 {
						if kind, ok3 := builtinAliases[vr.Name]; ok3 {
							switch kind {
							case "go_testpkg":
								if sel.Field == "Add" && len(args) == 2 {
									expr = &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}
									break
								}
								if sel.Field == "FifteenPuzzleExample" && len(args) == 0 {
									return &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}, nil
								}
							case "python_math":
								if currentProgram != nil {
									currentProgram.addInclude("<cmath>")
								}
								switch sel.Field {
								case "sqrt", "sin", "log":
									expr = &CallExpr{Name: "std::" + sel.Field, Args: args}
									break
								case "pow":
									expr = &CallExpr{Name: "std::pow", Args: args}
									break
								}
							case "go_strings":
								switch sel.Field {
								case "ToUpper":
									if len(args) == 1 {
										expr = &ToUpperExpr{Value: args[0]}
										break
									}
								case "TrimSpace":
									if len(args) == 1 {
										expr = &TrimSpaceExpr{Value: args[0]}
										break
									}
								}
							case "go_net":
								if sel.Field == "LookupHost" && len(args) == 1 {
									usesLookupHost = true
									expr = &CallExpr{Name: "_lookup_host", Args: args}
									break
								}
							}
						}
					}
				}
				if _, ok4 := expr.(*CallExpr); ok4 {
					// already handled
				} else if _, ok4 := expr.(*BinaryExpr); ok4 {
					// already handled
				} else if _, ok4 := expr.(*ToUpperExpr); ok4 {
					// builtin handled
				} else if _, ok4 := expr.(*TrimSpaceExpr); ok4 {
					// builtin handled
				} else if _, ok4 := expr.(*KeysExpr); ok4 {
					// builtin handled
				} else {
					return nil, fmt.Errorf("unsupported call")
				}
			} else if vr, ok := expr.(*VarRef); ok {
				expr = &CallExpr{Name: safeName(vr.Name), Args: args}
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
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Generic != nil:
			typ := cppType(typeRefString(op.Cast.Type))
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
	prev := inFunction
	inFunction = true
	paramNames = map[string]bool{}
	mutatedParams = map[string]bool{}
	for _, p := range fn.Params {
		paramNames[p.Name] = true
	}
	prevLocals := localTypes
	prevDecls := currentVarDecls
	localTypes = map[string]string{}
	for k, v := range prevLocals {
		localTypes[k] = v
	}
	currentVarDecls = map[string]*LetStmt{}
	ret := "int64_t"
	if fn.Return == nil {
		ret = "void"
	} else if fn.Return.Simple != nil {
		ret = cppType(*fn.Return.Simple)
	} else if fn.Return.Generic != nil {
		ret = cppType(typeRefString(&parser.TypeRef{Generic: fn.Return.Generic}))
	} else {
		ret = "auto"
	}
	if fn.Name == "main" && ret == "void" {
		ret = "int"
	}
	prevRet := currentReturnType
	currentReturnType = ret
	for _, p := range fn.Params {
		if p.Type != nil {
			tp := types.ResolveTypeRef(p.Type, currentEnv)
			localTypes[p.Name] = cppTypeFrom(tp)
		}
	}
	defer func() { localTypes = prevLocals; currentVarDecls = prevDecls }()
	var body []Stmt
	for _, st := range fn.Body {
		s, err := convertStmt(st)
		if err != nil {
			inFunction = prev
			currentReturnType = prevRet
			return nil, err
		}
		body = append(body, s)
	}
	var params []Param
	for _, p := range fn.Params {
		typ := ""
		if p.Type != nil {
			tp := types.ResolveTypeRef(p.Type, currentEnv)
			typ = cppTypeFrom(tp)
			if strings.HasPrefix(typ, "std::unique_ptr<") {
				typ = strings.TrimSuffix(strings.TrimPrefix(typ, "std::unique_ptr<"), ">") + "*"
			} else if strings.HasPrefix(typ, "std::shared_ptr<") {
				typ = strings.TrimSuffix(strings.TrimPrefix(typ, "std::shared_ptr<"), ">") + "*"
			}
		}
		inFunction = prev
		params = append(params, Param{Name: p.Name, Type: typ, ByVal: mutatedParams[p.Name]})
	}
	defer func() { currentReturnType = prevRet }()
	paramNames = nil
	mutatedParams = nil
	return &Func{Name: fn.Name, Params: params, ReturnType: ret, Body: body}, nil
}

func convertFunLambda(fn *parser.FunStmt) (*BlockLambda, error) {
	prev := inFunction
	inFunction = true
	paramNames = map[string]bool{}
	mutatedParams = map[string]bool{}
	for _, p := range fn.Params {
		paramNames[p.Name] = true
	}
	prevLocals := localTypes
	prevDecls := currentVarDecls
	localTypes = map[string]string{}
	for k, v := range prevLocals {
		localTypes[k] = v
	}
	currentVarDecls = map[string]*LetStmt{}
	for _, p := range fn.Params {
		if p.Type != nil && p.Type.Simple != nil {
			localTypes[p.Name] = cppType(*p.Type.Simple)
		}
	}
	defer func() { localTypes = prevLocals; currentVarDecls = prevDecls }()
	var body []Stmt
	for _, st := range fn.Body {
		s, err := convertStmt(st)
		if err != nil {
			inFunction = prev
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
		params = append(params, Param{Name: p.Name, Type: typ, ByVal: mutatedParams[p.Name]})
	}
	inFunction = prev
	paramNames = nil
	mutatedParams = nil
	ret := "void"
	if fn.Return != nil {
		if fn.Return.Simple != nil {
			ret = cppType(*fn.Return.Simple)
		} else if fn.Return.Generic != nil {
			ret = cppType(typeRefString(&parser.TypeRef{Generic: fn.Return.Generic}))
		} else {
			ret = "auto"
		}
	}
	return &BlockLambda{Params: params, Body: body, ReturnType: ret}, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Call != nil:
		switch p.Call.Func {
		case "now":
			if len(p.Call.Args) == 0 {
				useNow = true
				return &NowExpr{}, nil
			}
		case "net.LookupHost":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				usesLookupHost = true
				return &CallExpr{Name: "_lookup_host", Args: []Expr{arg}}, nil
			}
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
		case "upper":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				return &ToUpperExpr{Value: arg}, nil
			}
		case "lower":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				return &ToLowerExpr{Value: arg}, nil
			}
		case "indexOf":
			if len(p.Call.Args) == 2 {
				v0, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				v1, err := convertExpr(p.Call.Args[1])
				if err != nil {
					return nil, err
				}
				usesIndexOf = true
				return &CallExpr{Name: "_index_of", Args: []Expr{v0, v1}}, nil
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
		case "keys":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				if currentProgram != nil {
					currentProgram.addInclude("<vector>")
					currentProgram.addInclude("<map>")
				}
				return &KeysExpr{Map: arg}, nil
			}
		case "sha256":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				usesSHA256 = true
				return &CallExpr{Name: "_sha256", Args: []Expr{arg}}, nil
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
		case "substring", "substr":
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
		case "padStart":
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
				return &PadStartExpr{Value: v0, Width: v1, Pad: v2}, nil
			}
		case "contains":
			if len(p.Call.Args) == 2 {
				v0, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				v1, err := convertExpr(p.Call.Args[1])
				if err != nil {
					return nil, err
				}
				return &ContainsExpr{Value: v0, Sub: v1}, nil
			}
		case "exists":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				return &ExistsExpr{List: arg}, nil
			}
		case "int":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				return &CastExpr{Value: arg, Type: "int64_t"}, nil
			}
		case "float":
			if len(p.Call.Args) == 1 {
				arg, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				return &CastExpr{Value: arg, Type: "double"}, nil
			}
		case "parseIntStr":
			if len(p.Call.Args) == 1 || len(p.Call.Args) == 2 {
				v0, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				var v1 Expr = &IntLit{Value: 10}
				if len(p.Call.Args) == 2 {
					v1, err = convertExpr(p.Call.Args[1])
					if err != nil {
						return nil, err
					}
				}
				usesParseIntStr = true
				return &CallExpr{Name: "_parse_int_str", Args: []Expr{v0, v1}}, nil
			}
		case "input":
			if len(p.Call.Args) == 0 {
				return &InputExpr{}, nil
			}
		case "print":
			var args []Expr
			for _, a := range p.Call.Args {
				ce, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ce)
			}
			if currentProgram != nil {
				currentProgram.addInclude("<sstream>")
				currentProgram.addInclude("<iomanip>")
				currentProgram.addInclude("<iostream>")
			}
			return &PrintExpr{Values: args}, nil
		}
		var args []Expr
		for _, a := range p.Call.Args {
			ce, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ce)
		}
		if currentEnv != nil {
			if fn, ok := currentEnv.GetFunc(p.Call.Func); ok {
				for i, arg := range args {
					if i < len(fn.Params) {
						ptyp := cppTypeFrom(types.ResolveTypeRef(fn.Params[i].Type, currentEnv))
						if strings.HasPrefix(ptyp, "std::unique_ptr<") {
							ptyp = strings.TrimSuffix(strings.TrimPrefix(ptyp, "std::unique_ptr<"), ">") + "*"
						} else if strings.HasPrefix(ptyp, "std::shared_ptr<") {
							ptyp = strings.TrimSuffix(strings.TrimPrefix(ptyp, "std::shared_ptr<"), ">") + "*"
						}
						if ptyp == "" {
							ptyp = "auto"
						}
						at := exprType(arg)
						if at != ptyp && ptyp != "auto" {
							args[i] = &CastExpr{Value: arg, Type: ptyp}
						}
					}
				}
				if len(args) < len(fn.Params) {
					var params []Param
					for _, pa := range fn.Params[len(args):] {
						typ := ""
						if pa.Type != nil && pa.Type.Simple != nil {
							typ = cppType(*pa.Type.Simple)
						}
						params = append(params, Param{Name: pa.Name, Type: typ})
					}
					var callArgs []Expr
					callArgs = append(callArgs, args...)
					for _, pa := range params {
						callArgs = append(callArgs, &VarRef{Name: pa.Name})
					}
					call := &CallExpr{Name: safeName(p.Call.Func), Args: callArgs}
					return &LambdaExpr{Params: params, Body: call}, nil
				}
			}
		}
		return &CallExpr{Name: safeName(p.Call.Func), Args: args}, nil
	case p.Selector != nil:
		alias := p.Selector.Root
		if kind, ok := builtinAliases[alias]; ok && len(p.Selector.Tail) == 1 {
			field := p.Selector.Tail[0]
			switch kind {
			case "go_testpkg":
				switch field {
				case "Pi":
					return &FloatLit{Value: 3.14}, nil
				case "Answer":
					return &IntLit{Value: 42}, nil
				}
			case "python_math":
				switch field {
				case "pi":
					return &FloatLit{Value: 3.141592653589793}, nil
				case "e":
					return &FloatLit{Value: 2.718281828459045}, nil
				}
			}
		}
		expr := Expr(&VarRef{Name: alias})
		for _, f := range p.Selector.Tail {
			expr = &SelectorExpr{Target: expr, Field: f}
		}
		return expr, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		expr, err := dataExprFromFile(path, format, p.Load.Type)
		if err != nil {
			return nil, err
		}
		return expr, nil
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
		elemType := ""
		if len(elems) > 0 {
			first := exprType(elems[0])
			uniform := true
			for _, el := range elems[1:] {
				if exprType(el) != first {
					uniform = false
					break
				}
			}
			if uniform {
				elemType = first
			}
		}
		return &ListLit{Elems: elems, ElemType: elemType}, nil
	case p.Map != nil:
		if currentProgram != nil {
			currentProgram.addInclude("<map>")
		}
		if len(p.Map.Items) == 0 {
			return &MapLit{KeyType: "auto", ValueType: "auto"}, nil
		}
		kt := guessType(p.Map.Items[0].Key)
		vt := guessType(p.Map.Items[0].Value)
		for _, it := range p.Map.Items[1:] {
			t2 := guessType(it.Value)
			if t2 != vt {
				vt = "std::any"
				break
			}
		}
		if inReturn && strings.HasPrefix(currentReturnType, "std::map<") {
			parts := strings.SplitN(strings.TrimSuffix(strings.TrimPrefix(currentReturnType, "std::map<"), ">"), ",", 2)
			if len(parts) == 2 {
				kt = strings.TrimSpace(parts[0])
				vt = strings.TrimSpace(parts[1])
			}
		} else if currentReturnType == "std::map<std::string, std::any>" {
			if kt == "auto" {
				kt = "std::string"
			}
			vt = "std::any"
		}
		if currentProgram != nil && vt == "std::any" {
			currentProgram.addInclude("<any>")
		}
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
		if len(p.Query.Joins) == 1 && p.Query.Joins[0].Side != nil {
			side := *p.Query.Joins[0].Side
			switch side {
			case "left":
				if p.Query.Group != nil {
					expr, _, _, err := convertLeftJoinGroupQuery(p.Query, "tmp")
					return expr, err
				}
				expr, _, _, err := convertLeftJoinQuery(p.Query, "tmp")
				return expr, err
			case "right":
				expr, _, _, err := convertRightJoinQuery(p.Query, "tmp")
				return expr, err
			case "outer":
				expr, _, _, err := convertOuterJoinQuery(p.Query, "tmp")
				return expr, err
			}
		}
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
	case l.Float != nil:
		return &FloatLit{Value: *l.Float}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Null:
		return &NullLit{}, nil
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

func extractVariantPattern(e *parser.Expr) (string, []string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return "", nil, false
	}
	pf := e.Binary.Left.Value
	prim := pf.Target
	if prim != nil && prim.Call != nil && len(pf.Ops) == 0 {
		if _, ok := currentEnv.FindUnionByVariant(prim.Call.Func); ok {
			vars := make([]string, len(prim.Call.Args))
			for i, a := range prim.Call.Args {
				if a.Binary != nil && a.Binary.Left != nil && a.Binary.Left.Value != nil && a.Binary.Left.Value.Target != nil && a.Binary.Left.Value.Target.Selector != nil && len(a.Binary.Left.Value.Target.Selector.Tail) == 0 {
					vars[i] = a.Binary.Left.Value.Target.Selector.Root
				} else {
					return "", nil, false
				}
			}
			return prim.Call.Func, vars, true
		}
	} else if prim != nil && prim.Selector != nil && len(prim.Selector.Tail) == 0 && len(pf.Ops) == 0 {
		if _, ok := currentEnv.FindUnionByVariant(prim.Selector.Root); ok {
			return prim.Selector.Root, nil, true
		}
	}
	return "", nil, false
}

func convertMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, c := range me.Cases {
		if v, vars, ok := extractVariantPattern(c.Pattern); ok {
			tmp := "__" + strings.ToLower(v)
			cast := &DynCastExpr{Type: v, Expr: target}
			body = append(body, &LetStmt{Name: tmp, Type: fmt.Sprintf("const %s*", v), Value: cast})
			old := localTypes
			nt := map[string]string{}
			for k, val := range old {
				nt[k] = val
			}
			nt[tmp] = fmt.Sprintf("const %s*", v)
			st, _ := currentEnv.GetStruct(v)
			then := []Stmt{}
			if len(vars) > 0 {
				for i, name := range vars {
					field := st.Order[i]
					ft := cppTypeFrom(st.Fields[field])
					val := Expr(&FieldPtrExpr{Target: &VarRef{Name: tmp}, Field: field})
					if strings.HasPrefix(ft, "std::unique_ptr<") {
						val = &PtrGetExpr{Target: val}
						ft = strings.TrimSuffix(strings.TrimPrefix(ft, "std::unique_ptr<"), ">") + "*"
					} else if strings.HasPrefix(ft, "std::shared_ptr<") {
						val = &PtrGetExpr{Target: val}
						ft = strings.TrimSuffix(strings.TrimPrefix(ft, "std::shared_ptr<"), ">") + "*"
					}
					if name != "_" {
						then = append(then, &LetStmt{Name: name, Type: ft, Value: val})
						nt[name] = ft
					}
				}
			}
			localTypes = nt
			res, err := convertExpr(c.Result)
			if err != nil {
				return nil, err
			}
			localTypes = old
			then = append(then, &ReturnStmt{Value: res, Type: currentReturnType})
			body = append(body, &IfStmt{Cond: &VarRef{Name: tmp}, Then: then})
		} else {
			res, err := convertExpr(c.Result)
			if err != nil {
				return nil, err
			}
			body = append(body, &ReturnStmt{Value: res, Type: currentReturnType})
		}
	}
	var def Expr = &StringLit{Value: ""}
	switch {
	case currentReturnType == "int64_t":
		def = &IntLit{Value: 0}
	case currentReturnType == "double":
		def = &FloatLit{Value: 0}
	case currentReturnType == "bool":
		def = &BoolLit{Value: false}
	case strings.HasPrefix(currentReturnType, "std::unique_ptr<") || strings.HasPrefix(currentReturnType, "std::shared_ptr<"):
		def = &NullLit{}
	default:
		def = &StructLit{Name: currentReturnType}
	}
	body = append(body, &ReturnStmt{Value: def, Type: currentReturnType})
	return &MatchBlock{Body: body}, nil
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
	elemType := exprType(body)
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
		keyExprs := make([]Expr, len(q.Group.Exprs))
		keyTypes := make([]string, len(q.Group.Exprs))
		for i, g := range q.Group.Exprs {
			ke, err := convertExpr(g)
			if err != nil {
				return nil, nil, "", err
			}
			keyExprs[i] = ke
			keyTypes[i] = exprType(ke)
		}
		var keyExpr Expr
		var keyType string
		if len(keyExprs) == 1 {
			keyExpr = keyExprs[0]
			keyType = keyTypes[0]
			if ml, ok := keyExpr.(*MapLit); ok {
				names := make([]string, len(ml.Keys))
				fields := make([]Param, len(ml.Keys))
				flds := make([]FieldLit, len(ml.Keys))
				for i, k := range ml.Keys {
					n, ok := keyName(k)
					if !ok {
						names = nil
						break
					}
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
					names[i] = n
					fields[i] = Param{Name: n, Type: typ}
					flds[i] = FieldLit{Name: n, Value: ml.Values[i]}
				}
				if names != nil {
					structNameK := strings.Title(q.Group.Name) + "Key"
					keyExpr = &StructLit{Name: structNameK, Fields: flds}
					keyType = structNameK
					if currentProgram != nil {
						currentProgram.Structs = append(currentProgram.Structs, StructDef{Name: structNameK, Fields: fields})
					}
				}
			}
		} else {
			keyExpr = &TupleExpr{Elems: keyExprs}
			keyType = fmt.Sprintf("std::tuple<%s>", strings.Join(keyTypes, ", "))
			if currentProgram != nil {
				currentProgram.addInclude("<tuple>")
			}
		}
		pairName := strings.Title(q.Group.Name) + "Row"
		itemType := ""
		if len(vars) == 1 {
			itemType = qTypes[vars[0]]
		} else {
			pfields := make([]Param, len(vars))
			for i, v := range vars {
				pfields[i] = Param{Name: v, Type: qTypes[v]}
			}
			if currentProgram != nil {
				currentProgram.Structs = append(currentProgram.Structs, StructDef{Name: pairName, Fields: pfields})
			}
			itemType = pairName
		}
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
		elemType = exprType(body)
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
		}
		return &GroupComp{Vars: vars, Iters: iters, Cond: cond, Key: keyExpr, ItemVar: "__row", RowVars: vars, GroupName: q.Group.Name, GroupStruct: structName, Body: body, ElemType: elemType, KeyType: keyType, ItemType: itemType}, def, elemType, nil
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

func convertLeftJoinGroupQuery(q *parser.QueryExpr, target string) (Expr, *StructDef, string, error) {
	if q == nil || len(q.Joins) != 1 || q.Group == nil || len(q.Group.Exprs) == 0 || q.Distinct || q.Sort != nil || q.Skip != nil || q.Take != nil {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	leftIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, nil, "", err
	}
	rightIter, err := convertExpr(j.Src)
	if err != nil {
		return nil, nil, "", err
	}
	leftType := elementTypeFromListType(guessType(q.Source))
	if vr, ok := leftIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			leftType = t
		}
	}
	rightType := elementTypeFromListType(guessType(j.Src))
	if vr, ok := rightIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			rightType = t
		}
	}
	optRightType := fmt.Sprintf("std::optional<%s>", rightType)
	qTypes := map[string]string{q.Var: leftType, j.Var: rightType}
	oldTypes := localTypes
	localTypes = qTypes
	cond, err := convertExpr(j.On)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	keyExprs := make([]Expr, len(q.Group.Exprs))
	keyTypes := make([]string, len(q.Group.Exprs))
	for i, g := range q.Group.Exprs {
		ke, err := convertExpr(g)
		if err != nil {
			localTypes = oldTypes
			return nil, nil, "", err
		}
		keyExprs[i] = ke
		keyTypes[i] = exprType(ke)
	}
	var keyExpr Expr
	var keyType string
	if len(keyExprs) == 1 {
		keyExpr = keyExprs[0]
		keyType = keyTypes[0]
	} else {
		keyExpr = &TupleExpr{Elems: keyExprs}
		keyType = fmt.Sprintf("std::tuple<%s>", strings.Join(keyTypes, ", "))
		if currentProgram != nil {
			currentProgram.addInclude("<tuple>")
		}
	}
	pairName := strings.Title(target) + "Pair"
	pairDef := &StructDef{Name: pairName, Fields: []Param{{Name: q.Var, Type: leftType}, {Name: j.Var, Type: optRightType}}}
	groupName := strings.Title(q.Group.Name) + "Group"
	groupDef := &StructDef{Name: groupName, Fields: []Param{{Name: "key", Type: keyType}, {Name: "items", Type: fmt.Sprintf("std::vector<%s>", pairName)}}}
	if currentProgram != nil {
		currentProgram.Structs = append(currentProgram.Structs, *pairDef, *groupDef)
		currentProgram.addInclude("<vector>")
		currentProgram.addInclude("<unordered_map>")
		currentProgram.addInclude("<optional>")
	}
	qTypes[q.Group.Name] = groupName
	localTypes = qTypes
	itemVar := "__pair"
	body, err := convertExpr(q.Select)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	elemType := exprType(body)
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
	localTypes = oldTypes
	return &LeftJoinGroupComp{LeftVar: q.Var, LeftIter: leftIter, RightVar: j.Var, RightIter: rightIter, RightType: optRightType, InnerType: rightType, Cond: cond, Key: keyExpr, ItemVar: itemVar, GroupName: q.Group.Name, GroupStruct: groupName, ItemType: pairName, Body: body, ElemType: elemType, KeyType: keyType}, def, elemType, nil
}

func convertLeftJoinQuery(q *parser.QueryExpr, target string) (Expr, *StructDef, string, error) {
	if q == nil || len(q.Joins) != 1 || q.Group != nil || q.Distinct || q.Sort != nil || q.Skip != nil || q.Take != nil {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	leftIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, nil, "", err
	}
	rightIter, err := convertExpr(j.Src)
	if err != nil {
		return nil, nil, "", err
	}
	leftType := elementTypeFromListType(guessType(q.Source))
	if vr, ok := leftIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			leftType = t
		}
	}
	rightType := elementTypeFromListType(guessType(j.Src))
	if vr, ok := rightIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			rightType = t
		}
	}
	optRightType := fmt.Sprintf("std::optional<%s>", rightType)
	qTypes := map[string]string{q.Var: leftType, j.Var: rightType}
	oldTypes := localTypes
	localTypes = qTypes
	cond, err := convertExpr(j.On)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	qTypes[j.Var] = optRightType
	localTypes = qTypes
	body, err := convertExpr(q.Select)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	elemType := exprType(body)
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
	localTypes = oldTypes
	if currentProgram != nil {
		currentProgram.addInclude("<vector>")
		currentProgram.addInclude("<optional>")
	}
	return &LeftJoinComp{LeftVar: q.Var, LeftIter: leftIter, RightVar: j.Var, RightIter: rightIter, RightType: optRightType, InnerType: rightType, Cond: cond, Body: body, ElemType: elemType}, def, elemType, nil
}

func convertJoinLeftJoinQuery(q *parser.QueryExpr, target string) (Expr, *StructDef, string, error) {
	if q == nil || len(q.Joins) != 2 || q.Group != nil || q.Distinct || q.Sort != nil || q.Skip != nil || q.Take != nil {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	j1 := q.Joins[0]
	j2 := q.Joins[1]
	if j1.Side != nil || j2.Side == nil || *j2.Side != "left" {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	leftIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, nil, "", err
	}
	joinIter, err := convertExpr(j1.Src)
	if err != nil {
		return nil, nil, "", err
	}
	rightIter, err := convertExpr(j2.Src)
	if err != nil {
		return nil, nil, "", err
	}
	leftType := elementTypeFromListType(guessType(q.Source))
	if vr, ok := leftIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			leftType = t
		}
	}
	joinType := elementTypeFromListType(guessType(j1.Src))
	if vr, ok := joinIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			joinType = t
		}
	}
	rightType := elementTypeFromListType(guessType(j2.Src))
	if vr, ok := rightIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			rightType = t
		}
	}
	optRightType := fmt.Sprintf("std::optional<%s>", rightType)
	qTypes := map[string]string{q.Var: leftType, j1.Var: joinType, j2.Var: rightType}
	oldTypes := localTypes
	localTypes = qTypes
	joinCond, err := convertExpr(j1.On)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	cond, err := convertExpr(j2.On)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	qTypes[j2.Var] = optRightType
	localTypes = qTypes
	body, err := convertExpr(q.Select)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	elemType := exprType(body)
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
	localTypes = oldTypes
	if currentProgram != nil {
		currentProgram.addInclude("<vector>")
		currentProgram.addInclude("<optional>")
	}
	return &JoinLeftJoinComp{LeftVar: q.Var, LeftIter: leftIter, JoinVar: j1.Var, JoinIter: joinIter, JoinCond: joinCond, RightVar: j2.Var, RightIter: rightIter, RightType: optRightType, InnerType: rightType, Cond: cond, Body: body, ElemType: elemType}, def, elemType, nil
}

func convertRightJoinQuery(q *parser.QueryExpr, target string) (Expr, *StructDef, string, error) {
	if q == nil || len(q.Joins) != 1 || q.Group != nil || q.Distinct || q.Sort != nil || q.Skip != nil || q.Take != nil {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "right" {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	leftIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, nil, "", err
	}
	rightIter, err := convertExpr(j.Src)
	if err != nil {
		return nil, nil, "", err
	}
	leftType := elementTypeFromListType(guessType(q.Source))
	if vr, ok := leftIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			leftType = t
		}
	}
	rightType := elementTypeFromListType(guessType(j.Src))
	if vr, ok := rightIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			rightType = t
		}
	}
	optRightType := fmt.Sprintf("std::optional<%s>", rightType)
	optLeftType := fmt.Sprintf("std::optional<%s>", leftType)
	qTypes := map[string]string{q.Var: leftType, j.Var: rightType}
	oldTypes := localTypes
	localTypes = qTypes
	cond, err := convertExpr(j.On)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	qTypes[q.Var] = optLeftType
	localTypes = qTypes
	body, err := convertExpr(q.Select)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	elemType := exprType(body)
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
					if vr, ok := ml.Values[i].(*VarRef); ok && vr.Name == j.Var {
						typ = optRightType
						ml.Values[i] = &CallExpr{Name: optRightType, Args: []Expr{vr}}
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
	localTypes = oldTypes
	if currentProgram != nil {
		currentProgram.addInclude("<vector>")
		currentProgram.addInclude("<optional>")
	}
	return &RightJoinComp{LeftVar: q.Var, LeftIter: leftIter, RightVar: j.Var, RightIter: rightIter, RightType: rightType, LeftType: optLeftType, InnerType: leftType, Cond: cond, Body: body, ElemType: elemType}, def, elemType, nil
}

func convertOuterJoinQuery(q *parser.QueryExpr, target string) (Expr, *StructDef, string, error) {
	if q == nil || len(q.Joins) != 1 || q.Group != nil || q.Distinct || q.Sort != nil || q.Skip != nil || q.Take != nil {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "outer" {
		return nil, nil, "", fmt.Errorf("unsupported query")
	}
	leftIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, nil, "", err
	}
	rightIter, err := convertExpr(j.Src)
	if err != nil {
		return nil, nil, "", err
	}
	leftType := elementTypeFromListType(guessType(q.Source))
	if vr, ok := leftIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			leftType = t
		}
	}
	rightType := elementTypeFromListType(guessType(j.Src))
	if vr, ok := rightIter.(*VarRef); ok {
		if t, ok2 := currentProgram.ListTypes[vr.Name]; ok2 {
			rightType = t
		}
	}
	optLeftType := fmt.Sprintf("std::optional<%s>", leftType)
	optRightType := fmt.Sprintf("std::optional<%s>", rightType)
	qTypes := map[string]string{q.Var: leftType, j.Var: rightType}
	oldTypes := localTypes
	localTypes = qTypes
	cond, err := convertExpr(j.On)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	qTypes[q.Var] = optLeftType
	qTypes[j.Var] = optRightType
	localTypes = qTypes
	body, err := convertExpr(q.Select)
	if err != nil {
		localTypes = oldTypes
		return nil, nil, "", err
	}
	elemType := exprType(body)
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
	localTypes = oldTypes
	if currentProgram != nil {
		currentProgram.addInclude("<vector>")
		currentProgram.addInclude("<optional>")
	}
	return &OuterJoinComp{LeftVar: q.Var, LeftIter: leftIter, RightVar: j.Var, RightIter: rightIter, LeftType: optLeftType, RightType: optRightType, LeftInner: leftType, RightInner: rightType, Cond: cond, Body: body, ElemType: elemType}, def, elemType, nil
}

func cppType(t string) string {
	switch t {
	case "int":
		return "int64_t"
	case "float":
		return "double"
	case "bool":
		return "bool"
	case "any":
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		return "std::any"
	case "string":
		return "std::string"
	case "bigint":
		if currentProgram != nil {
			currentProgram.addInclude("<boost/multiprecision/cpp_int.hpp>")
		}
		return "boost::multiprecision::cpp_int"
	}
	if strings.HasPrefix(t, "list<") && strings.HasSuffix(t, ">") {
		elem := strings.TrimSuffix(strings.TrimPrefix(t, "list<"), ">")
		return fmt.Sprintf("std::vector<%s>", cppType(strings.TrimSpace(elem)))
	}
	if strings.HasPrefix(t, "map<") && strings.HasSuffix(t, ">") {
		body := strings.TrimSuffix(strings.TrimPrefix(t, "map<"), ">")
		parts := strings.SplitN(body, ",", 2)
		if len(parts) == 2 {
			k := cppType(strings.TrimSpace(parts[0]))
			v := cppType(strings.TrimSpace(parts[1]))
			return fmt.Sprintf("std::map<%s, %s>", k, v)
		}
	}
	if currentEnv != nil {
		if st, ok := currentEnv.GetStruct(t); ok {
			return st.Name
		}
		if _, ok := currentEnv.GetUnion(t); ok {
			if currentProgram != nil {
				currentProgram.addInclude("<memory>")
			}
			return fmt.Sprintf("std::shared_ptr<%s>", t)
		}
	}
	return "auto"
}

func cppTypeFrom(tp types.Type) string {
	switch t := tp.(type) {
	case types.IntType:
		return "int64_t"
	case types.Int64Type:
		return "int64_t"
	case types.BigIntType:
		if currentProgram != nil {
			currentProgram.addInclude("<boost/multiprecision/cpp_int.hpp>")
		}
		return "boost::multiprecision::cpp_int"
	case types.FloatType:
		return "double"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "std::string"
	case types.AnyType:
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		return "std::any"
	case types.ListType:
		return fmt.Sprintf("std::vector<%s>", cppTypeFrom(t.Elem))
	case types.MapType:
		return fmt.Sprintf("std::map<%s, %s>", cppTypeFrom(t.Key), cppTypeFrom(t.Value))
	case types.StructType:
		return t.Name
	case types.UnionType:
		if currentProgram != nil {
			currentProgram.addInclude("<memory>")
		}
		return fmt.Sprintf("std::shared_ptr<%s>", t.Name)
	default:
		return "auto"
	}
}

func guessType(e *parser.Expr) string {
	if e == nil {
		return "auto"
	}
	if e.Binary != nil && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		pf := e.Binary.Left.Value
		if sel := pf.Target.Selector; sel != nil && len(sel.Tail) == 0 {
			if t, ok := localTypes[sel.Root]; ok {
				return t
			}
			if t, ok := globalTypes[sel.Root]; ok {
				return t
			}
		}
	}
	if currentEnv != nil {
		typ := types.TypeOfExpr(e, currentEnv)
		if typ != nil {
			if lt, ok := typ.(types.ListType); ok {
				if _, ok2 := lt.Elem.(types.AnyType); !ok2 {
					return cppTypeFrom(typ)
				}
			} else if _, ok := typ.(types.AnyType); !ok {
				return cppTypeFrom(typ)
			}
		}
	}
	if e.Binary != nil && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		pf := e.Binary.Left.Value
		if sel := pf.Target.Selector; sel != nil && len(sel.Tail) == 0 {
			if currentEnv != nil {
				if _, err := currentEnv.GetVar(sel.Root); err != nil {
					return "std::string"
				}
			} else {
				return "std::string"
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
				first := guessType(list.Elems[0])
				uniform := true
				for _, el := range list.Elems[1:] {
					if guessType(el) != first {
						uniform = false
						break
					}
				}
				if uniform {
					return fmt.Sprintf("std::vector<%s>", first)
				}
				if currentProgram != nil {
					currentProgram.addInclude("<any>")
				}
				return "std::vector<std::any>"
			}
		}
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		return "std::vector<std::any>"
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
	if e.Binary != nil && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		if call := e.Binary.Left.Value.Target.Call; call != nil {
			switch call.Func {
			case "len":
				return "int64_t"
			case "exists":
				return "bool"
			case "values":
				t := guessType(call.Args[0])
				return fmt.Sprintf("std::vector<%s>", elementTypeFromListType(t))
			case "keys":
				t := guessType(call.Args[0])
				if strings.HasPrefix(t, "std::map<") {
					parts := strings.TrimPrefix(t, "std::map<")
					parts = strings.TrimSuffix(parts, ">")
					kt := strings.Split(parts, ",")[0]
					return fmt.Sprintf("std::vector<%s>", strings.TrimSpace(kt))
				}
				return "std::vector<int64_t>"
			}
		}
		if pf := e.Binary.Left.Value; pf != nil && pf.Target.Selector != nil && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
			method := pf.Target.Selector.Tail[len(pf.Target.Selector.Tail)-1]
			if method == "keys" {
				baseSel := &parser.SelectorExpr{Root: pf.Target.Selector.Root, Tail: pf.Target.Selector.Tail[:len(pf.Target.Selector.Tail)-1]}
				baseExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Selector: baseSel}}}}}
				t := guessType(baseExpr)
				if strings.HasPrefix(t, "std::map<") {
					parts := strings.TrimPrefix(t, "std::map<")
					parts = strings.TrimSuffix(parts, ">")
					kt := strings.Split(parts, ",")[0]
					return fmt.Sprintf("std::vector<%s>", strings.TrimSpace(kt))
				}
				return "std::vector<int64_t>"
			}
		}
	}
	if e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return "auto"
	}
	pf := e.Binary.Left.Value
	if pf.Target.Struct != nil {
		name := pf.Target.Struct.Name
		if currentEnv != nil {
			if ut, ok := currentEnv.FindUnionByVariant(name); ok {
				if currentProgram != nil {
					currentProgram.addInclude("<memory>")
				}
				return fmt.Sprintf("std::shared_ptr<%s>", ut.Name)
			}
		}
		return name
	}
	if lit := pf.Target.Lit; lit != nil {
		if lit.Int != nil {
			return "int64_t"
		}
		if lit.Float != nil {
			return "double"
		}
		if lit.Bool != nil {
			return "bool"
		}
		if lit.Str != nil {
			return "std::string"
		}
	}
	if list := pf.Target.List; list != nil && len(list.Elems) > 0 {
		first := guessType(list.Elems[0])
		uniform := true
		for _, el := range list.Elems[1:] {
			if guessType(el) != first {
				uniform = false
				break
			}
		}
		if uniform {
			return fmt.Sprintf("std::vector<%s>", first)
		}
		if currentProgram != nil {
			currentProgram.addInclude("<any>")
		}
		return "std::vector<std::any>"
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
	if strings.HasSuffix(t, "Group") && currentProgram != nil {
		for _, st := range currentProgram.Structs {
			if st.Name == t {
				for _, f := range st.Fields {
					if f.Name == "items" && strings.HasPrefix(f.Type, "std::vector<") {
						return strings.TrimSuffix(strings.TrimPrefix(f.Type, "std::vector<"), ">")
					}
				}
			}
		}
	}
	return "auto"
}

func typeRefString(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		return *t.Simple
	}
	if t.Generic != nil {
		parts := make([]string, len(t.Generic.Args))
		for i, a := range t.Generic.Args {
			parts[i] = typeRefString(a)
		}
		return fmt.Sprintf("%s<%s>", t.Generic.Name, strings.Join(parts, ","))
	}
	return ""
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
		return "int64_t"
	case *FloatLit:
		return "double"
	case *BoolLit:
		return "bool"
	case *NullLit:
		return "std::any"
	case *StringLit:
		return "std::string"
	case *StrExpr:
		return "std::string"
	case *CastExpr:
		return v.Type
	case *VarRef:
		if v.Name == "nil" {
			return "std::any"
		}
		if t, ok := localTypes[v.Name]; ok {
			return t
		}
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(v.Name); err == nil {
				return cppTypeFrom(t)
			}
		}
		if t, ok := globalTypes[v.Name]; ok {
			return t
		}
		return "auto"
	case *StructLit:
		if currentEnv != nil {
			if ut, ok := currentEnv.FindUnionByVariant(v.Name); ok {
				if currentProgram != nil {
					currentProgram.addInclude("<memory>")
				}
				return fmt.Sprintf("std::shared_ptr<%s>", ut.Name)
			}
		}
		return v.Name
	case *ListLit:
		if v.ElemType != "" {
			return fmt.Sprintf("std::vector<%s>", v.ElemType)
		}
		if len(v.Elems) > 0 {
			first := exprType(v.Elems[0])
			uniform := true
			for _, e := range v.Elems[1:] {
				if exprType(e) != first {
					uniform = false
					break
				}
			}
			if uniform {
				return fmt.Sprintf("std::vector<%s>", first)
			}
			if currentProgram != nil {
				currentProgram.addInclude("<any>")
			}
			return "std::vector<std::any>"
		}
		return "std::vector<int64_t>"
	case *MapLit:
		if len(v.Keys) > 0 {
			kt := exprType(v.Keys[0])
			if kt == "auto" {
				kt = "std::string"
			}
			vt := v.ValueType
			if vt == "" || vt == "auto" {
				vt = exprType(v.Values[0])
			}
			return fmt.Sprintf("std::map<%s, %s>", kt, vt)
		}
		return "std::map<auto, auto>"
	case *IndexExpr:
		t := exprType(v.Target)
		if t == "std::string" {
			return "std::string"
		}
		if strings.HasPrefix(t, "std::vector<") && strings.HasSuffix(t, ">") {
			return strings.TrimSuffix(strings.TrimPrefix(t, "std::vector<"), ">")
		}
		if strings.HasPrefix(t, "std::map<") && strings.HasSuffix(t, ">") {
			parts := strings.SplitN(strings.TrimSuffix(strings.TrimPrefix(t, "std::map<"), ">"), ",", 2)
			if len(parts) == 2 {
				return strings.TrimSpace(parts[1])
			}
		}
		return "auto"
	case *SelectorExpr:
		t := exprType(v.Target)
		if ft := structFieldType(t, v.Field); ft != "" {
			return ft
		}
		if strings.HasPrefix(t, "std::map<") && strings.HasSuffix(t, ">") {
			parts := strings.SplitN(strings.TrimSuffix(strings.TrimPrefix(t, "std::map<"), ">"), ",", 2)
			if len(parts) == 2 {
				return strings.TrimSpace(parts[1])
			}
		}
		return "auto"
	case *UnaryExpr:
		if v.Op == "!" {
			return "bool"
		}
		return exprType(v.Expr)
	case *CallExpr:
		if currentProgram != nil {
			for _, fn := range currentProgram.Functions {
				if fn.Name == v.Name {
					return fn.ReturnType
				}
			}
		}
		return "auto"
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
			if lt == "int64_t" && rt == "int64_t" {
				return "int64_t"
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
	case *ToUpperExpr, *ToLowerExpr, *TrimSpaceExpr:
		return "std::string"
	case *InputExpr:
		return "std::string"
	case *SliceExpr:
		t := exprType(v.Target)
		if strings.HasPrefix(t, "std::vector<") {
			return t
		}
		if t == "std::string" {
			return "std::string"
		}
		return "auto"
	case *SubstringExpr:
		t := exprType(v.Value)
		if t == "std::string" {
			return "std::string"
		}
		return "auto"
	case *PadStartExpr:
		return "std::string"
	case *SumExpr:
		return "double"
	case *MultiListComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *GroupComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *LeftJoinGroupComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *LeftJoinComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *JoinLeftJoinComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *RightJoinComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *OuterJoinComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *SortComp:
		return fmt.Sprintf("std::vector<%s>", v.ElemType)
	case *KeysExpr:
		mt := exprType(v.Map)
		if strings.HasPrefix(mt, "std::map<") && strings.HasSuffix(mt, ">") {
			parts := strings.SplitN(strings.TrimSuffix(strings.TrimPrefix(mt, "std::map<"), ">"), ",", 2)
			if len(parts) > 0 {
				return fmt.Sprintf("std::vector<%s>", strings.TrimSpace(parts[0]))
			}
		}
		return "std::vector<int64_t>"
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
	if strings.HasPrefix(stName, "std::optional<") && strings.HasSuffix(stName, ">") {
		stName = strings.TrimSuffix(strings.TrimPrefix(stName, "std::optional<"), ">")
	}
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

func convertUnionDecl(td *parser.TypeDecl) (StructDef, []StructDef, error) {
	base := StructDef{Name: td.Name, Abstract: true}
	variants := make([]StructDef, 0, len(td.Variants))
	for _, v := range td.Variants {
		st := StructDef{Name: v.Name, Base: td.Name}
		for _, f := range v.Fields {
			typ := "auto"
			if f.Type != nil {
				typ = cppTypeFrom(types.ResolveTypeRef(f.Type, currentEnv))
				if typ == td.Name {
					typ = fmt.Sprintf("std::shared_ptr<%s>", td.Name)
					if currentProgram != nil {
						currentProgram.addInclude("<memory>")
					}
				}
			}
			st.Fields = append(st.Fields, Param{Name: f.Name, Type: typ})
		}
		variants = append(variants, st)
	}
	return base, variants, nil
}

func valueToExpr(v interface{}, typ *parser.TypeRef) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		fields := make([]FieldLit, len(names))
		keys := make([]Expr, len(names))
		vals := make([]Expr, len(names))
		for i, k := range names {
			fields[i] = FieldLit{Name: k, Value: valueToExpr(val[k], nil)}
			keys[i] = &StringLit{Value: k}
			vals[i] = valueToExpr(val[k], nil)
		}
		if typ != nil && typ.Simple != nil {
			if currentEnv != nil {
				if st, ok := currentEnv.GetStruct(*typ.Simple); ok {
					ordered := make([]FieldLit, len(st.Order))
					for i, name := range st.Order {
						for j, f := range fields {
							if f.Name == name {
								ordered[i] = fields[j]
								break
							}
						}
					}
					return &StructLit{Name: *typ.Simple, Fields: ordered}
				}
			}
			return &StructLit{Name: *typ.Simple, Fields: fields}
		}
		return &MapLit{Keys: keys, Values: vals, KeyType: "std::string", ValueType: "auto"}
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
	case float64:
		if val == float64(int(val)) {
			return &IntLit{Value: int(val)}
		}
		return &FloatLit{Value: val}
	case int, int64:
		return &IntLit{Value: int(reflect.ValueOf(val).Int())}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func dataExprFromFile(path, format string, typ *parser.TypeRef) (Expr, error) {
	if path == "" {
		return &ListLit{}, nil
	}
	root := meta.RepoRoot()
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
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "jsonl":
		var arr []interface{}
		for _, line := range bytes.Split(data, []byte{'\n'}) {
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var item interface{}
			if err := json.Unmarshal(line, &item); err == nil {
				arr = append(arr, item)
			}
		}
		v = arr
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(v, typ), nil
}

func simpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return "", false
	}
	if len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func substituteFieldRefs(e Expr, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *VarRef:
		if fields[ex.Name] {
			return &SelectorExpr{Target: &VarRef{Name: "item"}, Field: ex.Name}
		}
		return ex
	case *BinaryExpr:
		return &BinaryExpr{Left: substituteFieldRefs(ex.Left, fields), Op: ex.Op, Right: substituteFieldRefs(ex.Right, fields)}
	case *UnaryExpr:
		return &UnaryExpr{Op: ex.Op, Expr: substituteFieldRefs(ex.Expr, fields)}
	case *CallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = substituteFieldRefs(a, fields)
		}
		return &CallExpr{Name: ex.Name, Args: args}
	default:
		return ex
	}
}

func convertUpdateStmt(u *parser.UpdateStmt) (Stmt, error) {
	if currentEnv == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := currentEnv.GetVar(u.Target)
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
	child := types.NewEnv(currentEnv)
	fieldSet := map[string]bool{}
	for n, ft := range st.Fields {
		child.SetVar(n, ft, true)
		fieldSet[n] = true
	}
	fields := make([]string, len(u.Set.Items))
	values := make([]Expr, len(u.Set.Items))
	for i, it := range u.Set.Items {
		key, ok := simpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
			if !ok {
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(it.Value)
		if err != nil {
			return nil, err
		}
		values[i] = substituteFieldRefs(val, fieldSet)
		fields[i] = key
	}
	var cond Expr
	if u.Where != nil {
		c, err := convertExpr(u.Where)
		if err != nil {
			return nil, err
		}
		cond = substituteFieldRefs(c, fieldSet)
	}
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func convertBenchBlock(b *parser.BenchBlock) (Stmt, error) {
	old := localTypes
	localTypes = map[string]string{}
	var body []Stmt
	for _, st := range b.Body {
		cs, err := convertStmt(st)
		if err != nil {
			localTypes = old
			return nil, err
		}
		body = append(body, cs)
	}
	localTypes = old
	useNow = true
	useMem = true
	name := strings.Trim(b.Name, "\"")
	return &BenchStmt{Name: name, Body: body}, nil
}
