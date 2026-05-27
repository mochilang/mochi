package parser

import (
	"strings"

	"github.com/alecthomas/participle/v2/lexer"
)

// --- Program Structure ---

type Program struct {
	Pos        lexer.Position `json:"pos,omitempty" parser:""`
	Package    string         `json:"package,omitempty" parser:"[ 'package' @Ident ]"`
	PackageDoc string         `json:"packagedoc,omitempty" parser:""`
	Statements []*Statement   `json:"statements,omitempty" parser:"@@*"`
}

type Statement struct {
	Pos          lexer.Position    `json:"pos,omitempty" parser:""`
	Test         *TestBlock        `json:"test,omitempty" parser:"@@"`
	Bench        *BenchBlock       `json:"bench,omitempty" parser:"| @@"`
	Expect       *ExpectStmt       `json:"expect,omitempty" parser:"| @@"`
	Agent        *AgentDecl        `json:"agent,omitempty" parser:"| @@"`
	Stream       *StreamDecl       `json:"stream,omitempty" parser:"| @@"`
	Model        *ModelDecl        `json:"model,omitempty" parser:"| @@"`
	Import       *ImportStmt       `json:"import,omitempty" parser:"| @@"`
	Type         *TypeDecl         `json:"type,omitempty" parser:"| @@"`
	ExternType   *ExternTypeDecl   `json:"externtype,omitempty" parser:"| @@"`
	ExternVar    *ExternVarDecl    `json:"externvar,omitempty" parser:"| @@"`
	ExternGoFun     *ExternGoFunDecl     `json:"extern_go_fun,omitempty" parser:"| @@"`
	ExternPythonFun *ExternPythonFunDecl `json:"extern_python_fun,omitempty" parser:"| @@"`
	ExternJSFun     *ExternJSFunDecl     `json:"extern_js_fun,omitempty" parser:"| @@"`
	ExternJavaFun   *ExternJavaFunDecl   `json:"extern_java_fun,omitempty" parser:"| @@"`
	ExternFun       *ExternFunDecl       `json:"externfun,omitempty" parser:"| @@"`
	ExternObject *ExternObjectDecl `json:"externobject,omitempty" parser:"| @@"`
	Fact         *FactStmt         `json:"fact,omitempty" parser:"| @@"`
	Rule         *RuleStmt         `json:"rule,omitempty" parser:"| @@"`
	On           *OnHandler        `json:"on,omitempty" parser:"| @@"`
	Emit         *EmitStmt         `json:"emit,omitempty" parser:"| @@"`
	EmitCall     *EmitCallStmt     `json:"emit_call,omitempty" parser:"| @@"`
	Let          *LetStmt          `json:"let,omitempty" parser:"| @@"`
	Var          *VarStmt          `json:"var,omitempty" parser:"| @@"`
	Assign       *AssignStmt       `json:"assign,omitempty" parser:"| @@"`
	Fun          *FunStmt          `json:"fun,omitempty" parser:"| @@"`
	Return       *ReturnStmt       `json:"return,omitempty" parser:"| @@"`
	If           *IfStmt           `json:"if,omitempty" parser:"| @@"`
	While        *WhileStmt        `json:"while,omitempty" parser:"| @@"`
	TryCatch     *TryCatchStmt     `json:"try_catch,omitempty" parser:"| @@"`
	For          *ForStmt          `json:"for,omitempty" parser:"| @@"`
	Break        *BreakStmt        `json:"break,omitempty" parser:"| @@"`
	Continue     *ContinueStmt     `json:"continue,omitempty" parser:"| @@"`
	Fetch        *FetchStmt        `json:"fetch,omitempty" parser:"| @@"`
	Update       *UpdateStmt       `json:"update,omitempty" parser:"| @@"`
	Expr         *ExprStmt         `json:"expr,omitempty" parser:"| @@"`
}

// --- Test and Expect ---

type TestBlock struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"'test' @String"`
	Body []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
}

type BenchBlock struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"'bench' @String"`
	Body []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
}

type ExpectStmt struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Value *Expr          `json:"value,omitempty" parser:"'expect' @@"`
}

// --- If Statement ---

type IfStmt struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Cond   *Expr          `json:"cond,omitempty" parser:"'if' @@"`
	Then   []*Statement   `json:"then,omitempty" parser:"'{' @@* '}'"`
	ElseIf *IfStmt        `json:"elseif,omitempty" parser:"[ 'else' @@"`
	Else   []*Statement   `json:"else,omitempty" parser:"| 'else' '{' @@* '}' ]"`
}

// --- While Statement ---

type WhileStmt struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Cond *Expr          `json:"cond,omitempty" parser:"'while' @@"`
	Body []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
}

// --- TryCatch Statement ---

// TryCatchStmt is `try { ... } catch <var> { ... }`.
// The catch variable is bound to mochi_except_code (int).
type TryCatchStmt struct {
	Pos      lexer.Position `json:"pos,omitempty" parser:""`
	Try      []*Statement   `json:"try,omitempty" parser:"'try' '{' @@* '}'"`
	CatchVar string         `json:"catchvar,omitempty" parser:"'catch' @Ident"`
	Catch    []*Statement   `json:"catch,omitempty" parser:"'{' @@* '}'"`
}

// --- For Statement ---

type ForStmt struct {
	Pos      lexer.Position `json:"pos,omitempty" parser:""`
	Name     string         `json:"name,omitempty" parser:"'for' @Ident 'in'"`
	Source   *Expr          `json:"source,omitempty" parser:"@@"`            // expression to iterate
	RangeEnd *Expr          `json:"rangeend,omitempty" parser:"[ '..' @@ ]"` // optional range end
	Body     []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
}

// --- User-defined Types ---
//
// The body of a `type` declaration has four mutually exclusive shapes,
// listed below in the order participle tries them with UseLookahead. The
// alternation is expressed as a cross-field group: the opening `(` is on
// `Members` and the closing `)` is on `Alias`.
//
//  1. Struct      `type T = { field : T, ... }`  (the leading `=` is optional)
//  2. Variants    `type T = A | B | ...`         (two or more variants)
//  3. SingleVar   `type T = Ctor(field: T, ...)` (one variant *with* fields)
//  4. Alias       `type T = TypeRef`             (everything else)
//
// Branch 3 exists so that a single-constructor union written without a
// leading `|` still parses; without it `type Pair = P(a: int, b: int)`
// would be ambiguous against the alias branch. SingleVariant is folded
// into Variants by normalizeProgram so downstream consumers see a single
// slice. See MEP 2 §"Type declarations".
type TypeDecl struct {
	Pos           lexer.Position   `json:"pos,omitempty" parser:""`
	Name          string           `json:"name,omitempty" parser:"'type' @Ident"`
	Doc           string           `json:"doc,omitempty" parser:""`
	Members       []*TypeMember    `json:"members,omitempty" parser:"  ( [ '=' ] '{' @@ { [ ',' ] @@ } [ ',' ]? '}'"`
	Variants      []*TypeVariant   `json:"variants,omitempty" parser:"| '=' @@ ( '|' @@ )+"`
	SingleVariant *TypeVariantHead `json:"-" parser:"| '=' @@"`
	Alias         *TypeRef         `json:"alias,omitempty" parser:"| '=' @@ )"`
}

type TypeMember struct {
	Field  *TypeField `json:"field,omitempty" parser:"@@"`
	Method *FunStmt   `json:"method,omitempty" parser:"| @@"`
}

type TypeVariant struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Name   string         `json:"name,omitempty" parser:"@Ident"`
	Fields []*TypeField   `json:"fields,omitempty" parser:"[ '(' @@ { ',' @@ } [ ',' ]? ')' | '{' @@* '}' ]"`
}

// TypeVariantHead is a variant whose fields are mandatory. It exists only
// to disambiguate `type T = Ctor(...)` from `type T = AliasTo`.
type TypeVariantHead struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Name   string         `json:"name,omitempty" parser:"@Ident"`
	Fields []*TypeField   `json:"fields,omitempty" parser:"( '(' @@ { ',' @@ } [ ',' ]? ')' | '{' @@* '}' )"`
}

type TypeField struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"@Ident ':'"`
	Doc  string         `json:"doc,omitempty" parser:""`
	Type *TypeRef       `json:"type,omitempty" parser:"@@"`
}

// --- Type System ---

type TypeRef struct {
	Pos        lexer.Position    `json:"pos,omitempty" parser:""`
	Fun        *FunType          `json:"fun,omitempty" parser:"( @@"`
	StreamElem *TypeRef          `json:"stream_elem,omitempty" parser:"| 'stream' '<' @@ '>'"`
	Generic    *GenericType      `json:"generic,omitempty" parser:"| @@"`
	Struct     *InlineStructType `json:"struct,omitempty" parser:"| @@"`
	ListElem   *TypeRef          `json:"list_elem,omitempty" parser:"| '[' @@ ']'"`
	Simple     *string           `json:"simple,omitempty" parser:"| @Ident )"`
	// MEP-10 C1: a trailing `?` denotes an optional (nullable) type.
	// `int?` desugars to `option[int]` in resolveTypeRef.
	Optional bool `json:"optional,omitempty" parser:"[ @'?' ]"`
}

type InlineStructType struct {
	Fields []*TypeField `json:"fields,omitempty" parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type GenericType struct {
	Name string     `json:"name,omitempty" parser:"@(Ident | Keyword) '<'"`
	Args []*TypeRef `json:"args,omitempty" parser:"@@ { ',' @@ } '>'"`
}

type FunType struct {
	Params []*TypeRef `json:"params,omitempty" parser:"'fun' '(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef   `json:"return,omitempty" parser:"[ ':' @@ ]"`
}

// --- Declarations ---

type LetStmt struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Name  string         `json:"name,omitempty" parser:"'let' @Ident"`
	Doc   string         `json:"doc,omitempty" parser:""`
	Type  *TypeRef       `json:"type,omitempty" parser:"[ ':' @@ ]"`
	Value *Expr          `json:"value,omitempty" parser:"[ '=' @@ ]"`
}

type VarStmt struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Name  string         `json:"name,omitempty" parser:"'var' @Ident"`
	Doc   string         `json:"doc,omitempty" parser:""`
	Type  *TypeRef       `json:"type,omitempty" parser:"[ ':' @@ ]"`
	Value *Expr          `json:"value,omitempty" parser:"[ '=' @@ ]"`
}

type AssignStmt struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Name  string         `json:"name,omitempty" parser:"@Ident"`
	Index []*IndexOp     `json:"index,omitempty" parser:"@@*"`
	Field []*FieldOp     `json:"field,omitempty" parser:"@@*"`
	Value *Expr          `json:"value,omitempty" parser:"'=' @@"`
}

type FunStmt struct {
	Pos        lexer.Position `json:"pos,omitempty" parser:""`
	Export     bool           `json:"export,omitempty" parser:"[ @'export' ]"`
	Name       string         `json:"name,omitempty" parser:"'fun' @Ident"`
	Doc        string         `json:"doc,omitempty" parser:""`
	TypeParams []string       `json:"type_params,omitempty" parser:"[ '<' @Ident { ',' @Ident } '>' ]"`
	Params     []*Param       `json:"params,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return     *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
	Effects    []string       `json:"effects,omitempty" parser:"[ '!' @Ident { ',' @Ident } ]"`
	Body       []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
}

type ReturnStmt struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Value *Expr          `json:"value,omitempty" parser:"'return' @@?"`
}

type BreakStmt struct {
	Pos lexer.Position `json:"pos,omitempty" parser:"'break'"`
}

type ContinueStmt struct {
	Pos lexer.Position `json:"pos,omitempty" parser:"'continue'"`
}

// FetchStmt performs an HTTP request and stores the result in a variable.
type FetchStmt struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	URL    *Expr          `json:"url,omitempty" parser:"'fetch' @@"`
	Target string         `json:"target,omitempty" parser:"'into' @Ident"`
	With   *Expr          `json:"with,omitempty" parser:"[ 'with' @@ ]"`
}

type UpdateStmt struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Target string         `json:"target,omitempty" parser:"'update' @Ident"`
	Set    *MapLiteral    `json:"set,omitempty" parser:"'set' @@"`
	Where  *Expr          `json:"where,omitempty" parser:"[ 'where' @@ ]"`
}

type ExternTypeDecl struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"'extern' 'type' @Ident"`
}

type ExternVarDecl struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Root string         `json:"root,omitempty" parser:"'extern' ('var'|'let') @Ident"`
	Tail []string       `json:"tail,omitempty" parser:"{ '.' @Ident } ':'"`
	Type *TypeRef       `json:"type,omitempty" parser:"@@"`
}

func (e *ExternVarDecl) Name() string {
	if len(e.Tail) == 0 {
		return e.Root
	}
	return e.Root + "." + strings.Join(e.Tail, ".")
}

type ExternFunDecl struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Root   string         `json:"root,omitempty" parser:"'extern' 'fun' @Ident"`
	Tail   []string       `json:"tail,omitempty" parser:"{ '.' @Ident }"`
	Params []*Param       `json:"params,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
}

func (e *ExternFunDecl) Name() string {
	if len(e.Tail) == 0 {
		return e.Root
	}
	return e.Root + "." + strings.Join(e.Tail, ".")
}

// ExternGoFunDecl is a `extern go fun` declaration (Phase 10.2).
// The companion Go executable must implement the function and read
// JSON requests from stdin, writing JSON responses to stdout.
type ExternGoFunDecl struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Root   string         `json:"root,omitempty" parser:"'extern' 'go' 'fun' @Ident"`
	Tail   []string       `json:"tail,omitempty" parser:"{ '.' @Ident }"`
	Params []*Param       `json:"params,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
}

func (e *ExternGoFunDecl) Name() string {
	if len(e.Tail) == 0 {
		return e.Root
	}
	return e.Root + "_" + strings.Join(e.Tail, "_")
}

// ExternPythonFunDecl is a `extern python fun` declaration (Phase 10.3).
// The companion Python script must implement the function and read JSON
// requests from stdin, writing JSON responses to stdout.
type ExternPythonFunDecl struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Root   string         `json:"root,omitempty" parser:"'extern' 'python' 'fun' @Ident"`
	Tail   []string       `json:"tail,omitempty" parser:"{ '.' @Ident }"`
	Params []*Param       `json:"params,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
}

func (e *ExternPythonFunDecl) Name() string {
	if len(e.Tail) == 0 {
		return e.Root
	}
	return e.Root + "_" + strings.Join(e.Tail, "_")
}

// ExternJSFunDecl is a `extern js fun` declaration (Phase 10.4).
// The companion JavaScript file must implement the function and read JSON
// requests from stdin, writing JSON responses to stdout.
type ExternJSFunDecl struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Root   string         `json:"root,omitempty" parser:"'extern' 'js' 'fun' @Ident"`
	Tail   []string       `json:"tail,omitempty" parser:"{ '.' @Ident }"`
	Params []*Param       `json:"params,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
}

func (e *ExternJSFunDecl) Name() string {
	if len(e.Tail) == 0 {
		return e.Root
	}
	return e.Root + "_" + strings.Join(e.Tail, "_")
}

// ExternJavaFunDecl is a `extern java fun` declaration (Phase 12.0).
// Syntax: extern java fun <Class>.<Method>(<TypeList>): <ReturnType> as <Alias>
// Example: extern java fun java.util.UUID.randomUUID(): string as uuid_new
// The class name is everything except the last dot-separated component; the
// last component is the method name. The Alias field gives the Mochi-side name.
type ExternJavaFunDecl struct {
	Pos        lexer.Position `json:"pos,omitempty" parser:""`
	Root       string         `json:"root,omitempty" parser:"'extern' 'java' 'fun' @Ident"`
	Tail       []string       `json:"tail,omitempty" parser:"{ '.' @Ident }"`
	ParamTypes []*TypeRef     `json:"param_types,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return     *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
	Alias      string         `json:"alias,omitempty" parser:"'as' @Ident"`
}

// ClassName returns the fully qualified Java class name (all components except the last).
func (e *ExternJavaFunDecl) ClassName() string {
	if len(e.Tail) == 0 {
		return e.Root
	}
	return e.Root + "." + strings.Join(e.Tail[:len(e.Tail)-1], ".")
}

// MethodName returns the Java method name (the last dot-separated component).
func (e *ExternJavaFunDecl) MethodName() string {
	if len(e.Tail) == 0 {
		return ""
	}
	return e.Tail[len(e.Tail)-1]
}

// MochiName returns the Mochi alias for the function.
func (e *ExternJavaFunDecl) MochiName() string {
	return e.Alias
}

type ExternObjectDecl struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"'extern' 'object' @Ident"`
}

type FactStmt struct {
	Pos  lexer.Position  `json:"pos,omitempty" parser:""`
	Pred *LogicPredicate `json:"pred,omitempty" parser:"'fact' @@"`
}

type RuleStmt struct {
	Pos  lexer.Position  `json:"pos,omitempty" parser:""`
	Head *LogicPredicate `json:"head,omitempty" parser:"'rule' @@ ':-'"`
	Body []*LogicCond    `json:"body,omitempty" parser:"@@ { ',' @@ }"`
}

type LogicCond struct {
	Pos  lexer.Position  `json:"pos,omitempty" parser:""`
	Pred *LogicPredicate `json:"pred,omitempty" parser:"@@"`
	Neq  *LogicNeq       `json:"neq,omitempty" parser:"| @@"`
	Not  *LogicPredicate `json:"not,omitempty" parser:"| 'not' @@"`
}

type LogicNeq struct {
	A string `json:"a,omitempty" parser:"@Ident"`
	B string `json:"b,omitempty" parser:"'!=' @Ident"`
}

type LogicPredicate struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"@Ident '('"`
	Args []*LogicTerm   `json:"args,omitempty" parser:"[ @@ { ',' @@ } ] ')'"`
}

type LogicTerm struct {
	Var *string `json:"var,omitempty" parser:"@Ident"`
	Str *string `json:"str,omitempty" parser:"| @String"`
	Int *IntLit `json:"int,omitempty" parser:"| @Int"`
}

type Param struct {
	Name string   `json:"name,omitempty" parser:"@Ident"`
	Type *TypeRef `json:"type,omitempty" parser:"[ ':' @@ ]"`
}

type ExprStmt struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Expr *Expr          `json:"expr,omitempty" parser:"@@"`
}

// --- Expressions ---

type Expr struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Binary *BinaryExpr    `json:"binary,omitempty" parser:"@@"`
}

type BinaryExpr struct {
	Left  *Unary      `json:"left,omitempty" parser:"@@"`
	Right []*BinaryOp `json:"right,omitempty" parser:"@@*"`
}

type BinaryOp struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Op    string         `json:"op,omitempty" parser:"@('==' | '!=' | '<' | '<=' | '>' | '>=' | '+' | '-' | '*' | '/' | '%' | 'in' | '&&' | '||' | '??' | 'union' | 'except' | 'intersect')"`
	All   bool           `json:"all,omitempty" parser:"[ @'all' ]"`
	Right *Unary         `json:"right,omitempty" parser:"@@"`
}

type Unary struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Ops   []string       `json:"ops,omitempty" parser:"{@('-':Punct | '!':Punct)}"`
	Value *PostfixExpr   `json:"value,omitempty" parser:"@@"`
}

type PostfixExpr struct {
	Target *Primary     `json:"target,omitempty" parser:"@@"`
	Ops    []*PostfixOp `json:"ops,omitempty" parser:"@@*"`
}

type PostfixOp struct {
	Pos         lexer.Position `json:"pos,omitempty" parser:""`
	Call        *CallOp        `json:"call,omitempty" parser:"@@"`
	SafeField   *SafeFieldOp   `json:"safe_field,omitempty" parser:"| @@"`
	SafeIndex   *SafeIndexOp   `json:"safe_index,omitempty" parser:"| @@"`
	Index       *IndexOp       `json:"index,omitempty" parser:"| @@"`
	Field       *FieldOp       `json:"field,omitempty" parser:"| @@"`
	Cast        *CastOp        `json:"cast,omitempty" parser:"| @@"`
}

// SafeFieldOp models the MEP-16 `?.` postfix selector. `a?.f` reads as
// "if a holds a value, take field f, otherwise none". The type checker
// requires the receiver to be option-typed and lifts the field type
// back into `Option`.
type SafeFieldOp struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"'?' '.' @Ident"`
}

// SafeIndexOp models the MEP-16 `?[ ]` postfix index. `a?[k]` reads as
// "if a holds a value, look up k, otherwise none". The receiver must
// be `list<T>?` or `map<K, V>?`; the element type is option-wrapped.
type SafeIndexOp struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Start *Expr          `json:"start,omitempty" parser:"'?' '[' @@ ']'"`
}

type FieldOp struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"'.' @Ident"`
}

type CastOp struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Type *TypeRef       `json:"type,omitempty" parser:"'as' @@"`
}

type CallOp struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Args []*Expr        `json:"args,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
}

type IndexOp struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Start  *Expr          `json:"start,omitempty" parser:"'[' [ @@ ]"`
	Colon  *string        `json:"colon,omitempty" parser:"[ @':'"`
	End    *Expr          `json:"end,omitempty" parser:" [ @@ ] ]"`
	Colon2 *string        `json:"colon2,omitempty" parser:"[ @':'"`
	Step   *Expr          `json:"step,omitempty" parser:" [ @@ ] ] ']'"`
}

type ListLiteral struct {
	Elems []*Expr `json:"elems,omitempty" parser:"'[' [ @@ { ',' @@ } ] [ ',' ]? ']'"`
}

type SetLiteral struct {
	Elems []*Expr `json:"elems,omitempty" parser:"'set' '{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type MapLiteral struct {
	Items []*MapEntry `json:"items,omitempty" parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

// OMapLiteral is an ordered-map literal: omap{k1: v1, k2: v2, ...}.
// The body is identical to MapLiteral; the 'omap' prefix distinguishes it.
type OMapLiteral struct {
	Items []*MapEntry `json:"items,omitempty" parser:"'omap' '{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type MapEntry struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Key   *Expr          `json:"key,omitempty" parser:"@@ ':'"`
	Value *Expr          `json:"value,omitempty" parser:"@@"`
}

type StructLiteral struct {
	Name   string            `json:"name,omitempty" parser:"@Ident"`
	Fields []*StructLitField `json:"fields,omitempty" parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type StructLitField struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Name  string         `json:"name,omitempty" parser:"@Ident ':'"`
	Value *Expr          `json:"value,omitempty" parser:"@@"`
}

type GenerateField struct {
	Name  string `json:"name,omitempty" parser:"@Ident ':'"`
	Value *Expr  `json:"value,omitempty" parser:"@@"`
}

type GenerateExpr struct {
	Pos    lexer.Position   `json:"pos,omitempty" parser:""`
	Target string           `json:"target,omitempty" parser:"'generate' @Ident"`
	Fields []*GenerateField `json:"fields,omitempty" parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type FetchExpr struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	URL  *Expr          `json:"url,omitempty" parser:"'fetch' @@"`
	With *Expr          `json:"with,omitempty" parser:"[ 'with' @@ ]"`
}

// SpawnExpr is `spawn AgentType(args...)` — starts a supervised gen_server
// process for the named agent type and returns an opaque agent ref (PID).
// Phase 9.1.
type SpawnExpr struct {
	Pos       lexer.Position `json:"pos,omitempty" parser:""`
	AgentType string         `json:"agent_type,omitempty" parser:"'spawn' @Ident"`
	Args      []*Expr        `json:"args,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
}

// AsyncExpr is `async <expr>` — evaluates expr in a spawned process
// and returns a future reference. Phase 11.0.
type AsyncExpr struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Expr *Expr          `json:"expr,omitempty" parser:"'async' @@"`
}

// AwaitExpr is `await <expr>` — blocks until the future resolves
// and returns the result. Phase 11.1.
type AwaitExpr struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Future *Expr          `json:"future,omitempty" parser:"'await' @@"`
}

type LoadExpr struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Path *string        `json:"path,omitempty" parser:"'load' [ @String ] 'as'"`
	Type *TypeRef       `json:"type,omitempty" parser:"@@"`
	With *Expr          `json:"with,omitempty" parser:"[ 'with' @@ ]"`
}

type SaveExpr struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Src  *Expr          `json:"src,omitempty" parser:"'save' @@"`
	Path *string        `json:"path,omitempty" parser:"[ 'to' @String ]"`
	With *Expr          `json:"with,omitempty" parser:"[ 'with' @@ ]"`
}

type QueryExpr struct {
	Pos      lexer.Position `json:"pos,omitempty" parser:""`
	Var      string         `json:"var,omitempty" parser:"'from' @Ident 'in'"`
	Source   *Expr          `json:"source,omitempty" parser:"@@"`
	Froms    []*FromClause  `json:"froms,omitempty" parser:"{ @@ }"`
	Joins    []*JoinClause  `json:"joins,omitempty" parser:"{ @@ }"`
	Where    *Expr          `json:"where,omitempty" parser:"[ 'where' @@ ]"`
	Group    *GroupByClause `json:"group,omitempty" parser:"[ @@ ]"`
	Sort     *Expr          `json:"sort,omitempty" parser:"[ ( 'sort' | 'order' ) 'by' @@ ]"`
	Skip     *Expr          `json:"skip,omitempty" parser:"[ 'skip' @@ ]"`
	Take     *Expr          `json:"take,omitempty" parser:"[ 'take' @@ ]"`
	Distinct bool           `json:"distinct,omitempty" parser:"'select' @'distinct'?"`
	Select   *Expr          `json:"select,omitempty" parser:"@@"`
}

type LogicQueryExpr struct {
	Pos  lexer.Position  `json:"pos,omitempty" parser:""`
	Pred *LogicPredicate `json:"pred,omitempty" parser:"'query' @@"`
}

type FromClause struct {
	Pos lexer.Position `json:"pos,omitempty" parser:""`
	Var string         `json:"var,omitempty" parser:"'from' @Ident 'in'"`
	Src *Expr          `json:"src,omitempty" parser:"@@"`
}

type JoinClause struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Side *string        `json:"side,omitempty" parser:"[ @('left' | 'right' | 'outer') ]"`
	Var  string         `json:"var,omitempty" parser:"'join' [ 'from' ] @Ident 'in'"`
	Src  *Expr          `json:"src,omitempty" parser:"@@"`
	On   *Expr          `json:"on,omitempty" parser:"'on' @@"`
}

type GroupByClause struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Exprs  []*Expr        `json:"exprs,omitempty" parser:"'group' 'by' @@ { ',' @@ } 'into'"`
	Name   string         `json:"name,omitempty" parser:"@Ident"`
	Having *Expr          `json:"having,omitempty" parser:"[ 'having' @@ ]"`
}

type MatchExpr struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Target *Expr          `json:"target,omitempty" parser:"'match' @@ '{'"`
	Cases  []*MatchCase   `json:"cases,omitempty" parser:"@@* '}'"`
}

type IfExpr struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Cond   *Expr          `json:"cond,omitempty" parser:"'if' @@"`
	Then   *Expr          `json:"then,omitempty" parser:"('{' @@ '}' | 'then' @@)"`
	ElseIf *IfExpr        `json:"elseif,omitempty" parser:"[ 'else' @@"`
	Else   *Expr          `json:"else,omitempty" parser:"| 'else' ('{' @@ '}' | @@) ]"`
}

type MatchCase struct {
	Pos     lexer.Position `json:"pos,omitempty" parser:""`
	Pattern *Expr          `json:"pattern,omitempty" parser:"@@"`
	Guard   *Expr          `json:"guard,omitempty" parser:"[ 'when' @@ ]"`
	Result  *Expr          `json:"result,omitempty" parser:"'=>' [ @@ ]"`
	Block   []*Statement   `json:"block,omitempty" parser:"[ '{' @@* '}' ]"`
}

type Primary struct {
	Pos        lexer.Position  `json:"pos,omitempty" parser:""`
	Struct     *StructLiteral  `json:"struct,omitempty" parser:"@@"`
	Call       *CallExpr       `json:"call,omitempty" parser:"| @@"`
	Query      *QueryExpr      `json:"query,omitempty" parser:"| @@"`
	LogicQuery *LogicQueryExpr `json:"logicquery,omitempty" parser:"| @@"`
	If         *IfExpr         `json:"if,omitempty" parser:"| @@"`
	Selector   *SelectorExpr   `json:"selector,omitempty" parser:"| @@"`
	List       *ListLiteral    `json:"list,omitempty" parser:"| @@"`
	Set        *SetLiteral     `json:"set,omitempty" parser:"| @@"`
	OMap       *OMapLiteral    `json:"omap,omitempty" parser:"| @@"`
	Map        *MapLiteral     `json:"map,omitempty" parser:"| @@"`
	FunExpr    *FunExpr        `json:"funexpr,omitempty" parser:"| @@"`
	Match      *MatchExpr      `json:"match,omitempty" parser:"| @@"`
	Generate   *GenerateExpr   `json:"generate,omitempty" parser:"| @@"`
	Fetch      *FetchExpr      `json:"fetch,omitempty" parser:"| @@"`
	Spawn      *SpawnExpr      `json:"spawn,omitempty" parser:"| @@"`
	Async      *AsyncExpr      `json:"async,omitempty" parser:"| @@"`
	Await      *AwaitExpr      `json:"await,omitempty" parser:"| @@"`
	Load       *LoadExpr       `json:"load,omitempty" parser:"| @@"`
	Save       *SaveExpr       `json:"save,omitempty" parser:"| @@"`
	Lit        *Literal        `json:"lit,omitempty" parser:"| @@"`
	Group      *Expr           `json:"group,omitempty" parser:"| '(' @@ ')'"`
}

type FunExpr struct {
	Pos        lexer.Position `json:"pos,omitempty" parser:""`
	TypeParams []string       `json:"type_params,omitempty" parser:"'fun' [ '<' @Ident { ',' @Ident } '>' ]"`
	Params     []*Param       `json:"params,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return     *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
	Effects    []string       `json:"effects,omitempty" parser:"[ '!' @Ident { ',' @Ident } ]"`
	BlockBody  []*Statement   `json:"blockbody,omitempty" parser:"[ '{' @@* '}' ]"`
	ExprBody   *Expr          `json:"exprbody,omitempty" parser:"[ '=>' @@ ]"`
}

// --- Atoms ---

type SelectorExpr struct {
	Root string   `json:"root,omitempty" parser:"@Ident"`
	Tail []string `json:"tail,omitempty" parser:"{ '.' @Ident }"`
}

type CallExpr struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Func string         `json:"func,omitempty" parser:"@Ident '('"`
	Args []*Expr        `json:"args,omitempty" parser:"[ @@ { ',' @@ } ] ')'"`
}

type Literal struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Int   *IntLit        `json:"int,omitempty" parser:"@Int"`
	Float *float64       `json:"float,omitempty" parser:"| @Float"`
	Bool  *boolLit       `json:"bool,omitempty" parser:"| @('true' | 'false')"`
	Str   *string        `json:"str,omitempty" parser:"| @String"`
	None  bool           `json:"none,omitempty" parser:"| @'none'"`
}

// --- Stream / Struct ---

type StreamDecl struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Name   string         `json:"name,omitempty" parser:"'stream' @Ident"`
	Doc    string         `json:"doc,omitempty" parser:""`
	Fields []*StreamField `json:"fields,omitempty" parser:"'{' @@* '}'"`
}

type ModelDecl struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Name   string         `json:"name,omitempty" parser:"'model' @Ident"`
	Fields []*ModelField  `json:"fields,omitempty" parser:"'{' @@* '}'"`
}

type ModelField struct {
	Pos   lexer.Position `json:"pos,omitempty" parser:""`
	Name  string         `json:"name,omitempty" parser:"@Ident ':'"`
	Value *Expr          `json:"value,omitempty" parser:"@@"`
}

// ImportStmt declares a foreign module import, eg. `import python "math" as math`.
// The trailing `! effect { , effect }` (MEP-15 effect annotation) is honoured
// for Go imports: an effect of `meta` sets SealHandles=true on every FFI
// binding produced for this import (MEP-43 Phase 10).
type ImportStmt struct {
	Pos     lexer.Position `json:"pos,omitempty" parser:""`
	Lang    *string        `json:"lang,omitempty" parser:"'import' [ @Ident ]"`
	Path    string         `json:"path,omitempty" parser:"@String"`
	As      string         `json:"as,omitempty" parser:"[ 'as' @Ident ]"`
	Auto    bool           `json:"auto,omitempty" parser:"@'auto'?"`
	Effects []string       `json:"effects,omitempty" parser:"[ '!' @Ident { ',' @Ident } ]"`
}

type StreamField struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"@Ident ':'"`
	Doc  string         `json:"doc,omitempty" parser:""`
	Type *TypeRef       `json:"type,omitempty" parser:"@@"`
}

// --- On Handler ---

type OnHandler struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Stream string         `json:"stream,omitempty" parser:"'on' @Ident 'as'"`
	Alias  string         `json:"alias,omitempty" parser:"@Ident"`
	Body   []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
}

type EmitStmt struct {
	Pos    lexer.Position    `json:"pos,omitempty" parser:""`
	Stream string            `json:"stream,omitempty" parser:"'emit' @Ident"`
	Fields []*StructLitField `json:"fields,omitempty" parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

// EmitCallStmt is the Phase 9.2 builtin form `emit(stream, value)`.
// It uses the 'emit' keyword followed by parenthesised arguments to
// distinguish it from the agent-DSL EmitStmt which uses a struct literal.
type EmitCallStmt struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Stream *Expr          `json:"stream,omitempty" parser:"'emit' '(' @@"`
	Val    *Expr          `json:"val,omitempty" parser:"',' @@ ')'"`
}

// --- Agent DSL ---

type AgentDecl struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"'agent' @Ident"`
	Doc  string         `json:"doc,omitempty" parser:""`
	Body []*AgentBlock  `json:"body,omitempty" parser:"'{' @@* '}'"`
}

// OnCloseDecl is the Phase 9.3 `on close { ... }` block inside an agent body.
// It maps to the terminate/2 callback in the BEAM gen_server lifecycle.
type OnCloseDecl struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Body []*Statement   `json:"body,omitempty" parser:"'on' 'close' '{' @@* '}'"`
}

type AgentBlock struct {
	Pos      lexer.Position `json:"pos,omitempty" parser:""`
	OnClose  *OnCloseDecl   `json:"on_close,omitempty" parser:"@@"`
	Let      *LetStmt       `json:"let,omitempty" parser:"| @@"`
	Var      *VarStmt       `json:"var,omitempty" parser:"| @@"`
	Assign   *AssignStmt    `json:"assign,omitempty" parser:"| @@"`
	On       *OnHandler     `json:"on,omitempty" parser:"| @@"`
	Intent   *IntentDecl    `json:"intent,omitempty" parser:"| @@"`
}

type IntentDecl struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Name   string         `json:"name,omitempty" parser:"'intent' @Ident"`
	Params []*Param       `json:"params,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
	Body   []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
}

