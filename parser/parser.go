package parser

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
)

type boolLit bool

func (b *boolLit) Capture(values []string) error {
	v, err := strconv.ParseBool(values[0])
	if err != nil {
		return err
	}
	*b = boolLit(v)
	return nil
}

type IntLit int

func (i *IntLit) Capture(values []string) error {
	s := values[0]
	base := 10
	if strings.HasPrefix(s, "0x") || strings.HasPrefix(s, "0X") {
		base = 16
		s = s[2:]
	} else if strings.HasPrefix(s, "0b") || strings.HasPrefix(s, "0B") {
		base = 2
		s = s[2:]
	} else if strings.HasPrefix(s, "0o") || strings.HasPrefix(s, "0O") {
		base = 8
		s = s[2:]
	}
	v, err := strconv.ParseInt(s, base, 64)
	if err != nil {
		return err
	}
	*i = IntLit(v)
	return nil
}

// --- Mochi Lexer ---
var mochiLexer = lexer.MustSimple([]lexer.SimpleRule{
	{Name: "Comment", Pattern: `//[^\n]*|#[^\n]*|/\*([^*]|\*+[^*/])*\*/`},
	{Name: "Bool", Pattern: `\b(true|false)\b`},
	{Name: "Keyword", Pattern: `\b(test|expect|agent|intent|on|stream|emit|type|fun|extern|import|return|break|continue|let|var|if|else|then|for|while|in|generate|match|fetch|load|save|package|export|fact|rule|all|null)\b`},
	{Name: "Ident", Pattern: `[\p{L}\p{So}_][\p{L}\p{So}\p{N}_]*`},
	// Numeric literals do not include a leading '-' so that unary
	// minus is parsed separately as an operator. This allows
	// expressions like `len(list)-1` inside index operations to
	// parse correctly.
	{Name: "Float", Pattern: `\d+\.\d+(?:[eE][+-]?\d+)?|\d+[eE][+-]?\d+`},
	{Name: "Int", Pattern: `0[xX][0-9a-fA-F]+|0[bB][01]+|0[oO][0-7]+|\d+`},
	{Name: "String", Pattern: `"(?:\\.|[^"\\])*"`},
	{Name: "Punct", Pattern: `==|!=|<=|>=|&&|\|\||=>|:-|\.\.|[-+*/%=<>!|{}\[\](),.:]`},
	{Name: "Whitespace", Pattern: `[ \t\n\r;]+`},
})

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
	ExternFun    *ExternFunDecl    `json:"externfun,omitempty" parser:"| @@"`
	ExternObject *ExternObjectDecl `json:"externobject,omitempty" parser:"| @@"`
	Fact         *FactStmt         `json:"fact,omitempty" parser:"| @@"`
	Rule         *RuleStmt         `json:"rule,omitempty" parser:"| @@"`
	On           *OnHandler        `json:"on,omitempty" parser:"| @@"`
	Emit         *EmitStmt         `json:"emit,omitempty" parser:"| @@"`
	Let          *LetStmt          `json:"let,omitempty" parser:"| @@"`
	Var          *VarStmt          `json:"var,omitempty" parser:"| @@"`
	Assign       *AssignStmt       `json:"assign,omitempty" parser:"| @@"`
	Fun          *FunStmt          `json:"fun,omitempty" parser:"| @@"`
	Return       *ReturnStmt       `json:"return,omitempty" parser:"| @@"`
	If           *IfStmt           `json:"if,omitempty" parser:"| @@"`
	While        *WhileStmt        `json:"while,omitempty" parser:"| @@"`
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

// --- For Statement ---

type ForStmt struct {
	Pos      lexer.Position `json:"pos,omitempty" parser:""`
	Name     string         `json:"name,omitempty" parser:"'for' @Ident 'in'"`
	Source   *Expr          `json:"source,omitempty" parser:"@@"`            // expression to iterate
	RangeEnd *Expr          `json:"rangeend,omitempty" parser:"[ '..' @@ ]"` // optional range end
	Body     []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
}

// --- User-defined Types ---

type TypeDecl struct {
	Pos      lexer.Position `json:"pos,omitempty" parser:""`
	Name     string         `json:"name,omitempty" parser:"'type' @Ident"`
	Doc      string         `json:"doc,omitempty" parser:""`
	Members  []*TypeMember  `json:"members,omitempty" parser:"[ [ '=' ] '{' @@ { [ ',' ] @@ } [ ',' ]? '}' ]"`
	Variants []*TypeVariant `json:"variants,omitempty" parser:"[ '=' @@ { '|' @@ } ]"`
	Alias    *TypeRef       `json:"alias,omitempty" parser:"[ '=' @@ ]"`
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

type TypeField struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"@Ident ':'"`
	Doc  string         `json:"doc,omitempty" parser:""`
	Type *TypeRef       `json:"type,omitempty" parser:"@@"`
}

// --- Type System ---

type TypeRef struct {
	Fun     *FunType          `json:"fun,omitempty" parser:"@@"`
	Generic *GenericType      `json:"generic,omitempty" parser:"| @@"`
	Struct  *InlineStructType `json:"struct,omitempty" parser:"| @@"`
	Simple  *string           `json:"simple,omitempty" parser:"| @Ident"`
}

type InlineStructType struct {
	Fields []*TypeField `json:"fields,omitempty" parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type GenericType struct {
	Name string     `json:"name,omitempty" parser:"@Ident '<'"`
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
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Export bool           `json:"export,omitempty" parser:"[ @'export' ]"`
	Name   string         `json:"name,omitempty" parser:"'fun' @Ident"`
	Doc    string         `json:"doc,omitempty" parser:""`
	Params []*Param       `json:"params,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
	Body   []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
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
	Pred *LogicPredicate `json:"pred,omitempty" parser:"@@"`
	Neq  *LogicNeq       `json:"neq,omitempty" parser:"| @@"`
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
	Op    string         `json:"op,omitempty" parser:"@('==' | '!=' | '<' | '<=' | '>' | '>=' | '+' | '-' | '*' | '/' | '%' | 'in' | '&&' | '||' | 'union' | 'except' | 'intersect')"`
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
	Call  *CallOp  `json:"call,omitempty" parser:"@@"`
	Index *IndexOp `json:"index,omitempty" parser:"| @@"`
	Field *FieldOp `json:"field,omitempty" parser:"| @@"`
	Cast  *CastOp  `json:"cast,omitempty" parser:"| @@"`
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

type MapLiteral struct {
	Items []*MapEntry `json:"items,omitempty" parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
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
	Pattern *Expr          `json:"pattern,omitempty" parser:"@@ '=>'"`
	Result  *Expr          `json:"result,omitempty" parser:"@@"`
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
	Map        *MapLiteral     `json:"map,omitempty" parser:"| @@"`
	FunExpr    *FunExpr        `json:"funexpr,omitempty" parser:"| @@"`
	Match      *MatchExpr      `json:"match,omitempty" parser:"| @@"`
	Generate   *GenerateExpr   `json:"generate,omitempty" parser:"| @@"`
	Fetch      *FetchExpr      `json:"fetch,omitempty" parser:"| @@"`
	Load       *LoadExpr       `json:"load,omitempty" parser:"| @@"`
	Save       *SaveExpr       `json:"save,omitempty" parser:"| @@"`
	Lit        *Literal        `json:"lit,omitempty" parser:"| @@"`
	Group      *Expr           `json:"group,omitempty" parser:"| '(' @@ ')'"`
}

type FunExpr struct {
	Pos       lexer.Position `json:"pos,omitempty" parser:""`
	Params    []*Param       `json:"params,omitempty" parser:"'fun' '(' [ @@ { ',' @@ } ] ')'"`
	Return    *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
	BlockBody []*Statement   `json:"blockbody,omitempty" parser:"[ '{' @@* '}' ]"`
	ExprBody  *Expr          `json:"exprbody,omitempty" parser:"[ '=>' @@ ]"`
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
	Null  bool           `json:"null,omitempty" parser:"| @'null'"`
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
type ImportStmt struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Lang *string        `json:"lang,omitempty" parser:"'import' [ @Ident ]"`
	Path string         `json:"path,omitempty" parser:"@String"`
	As   string         `json:"as,omitempty" parser:"[ 'as' @Ident ]"`
	Auto bool           `json:"auto,omitempty" parser:"@'auto'?"`
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

// --- Agent DSL ---

type AgentDecl struct {
	Pos  lexer.Position `json:"pos,omitempty" parser:""`
	Name string         `json:"name,omitempty" parser:"'agent' @Ident"`
	Doc  string         `json:"doc,omitempty" parser:""`
	Body []*AgentBlock  `json:"body,omitempty" parser:"'{' @@* '}'"`
}

type AgentBlock struct {
	Let    *LetStmt    `json:"let,omitempty" parser:"@@"`
	Var    *VarStmt    `json:"var,omitempty" parser:"| @@"`
	Assign *AssignStmt `json:"assign,omitempty" parser:"| @@"`
	On     *OnHandler  `json:"on,omitempty" parser:"| @@"`
	Intent *IntentDecl `json:"intent,omitempty" parser:"| @@"`
}

type IntentDecl struct {
	Pos    lexer.Position `json:"pos,omitempty" parser:""`
	Name   string         `json:"name,omitempty" parser:"'intent' @Ident"`
	Params []*Param       `json:"params,omitempty" parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef       `json:"return,omitempty" parser:"[ ':' @@ ]"`
	Body   []*Statement   `json:"body,omitempty" parser:"'{' @@* '}'"`
}

// --- Parser Instance ---

var Parser = participle.MustBuild[Program](
	participle.Lexer(mochiLexer),
	participle.Elide("Whitespace", "Comment"),
	participle.Unquote("String"),
	participle.UseLookahead(999),
)

func ParseString(src string) (*Program, error) {
	prog, err := Parser.ParseString("", src)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	attachDocs(src, prog)
	return prog, nil
}

// AliasFromPath derives an import alias from a module path.
func AliasFromPath(path string) string {
	base := filepath.Base(strings.Trim(path, "\""))
	ext := filepath.Ext(base)
	return strings.TrimSuffix(base, ext)
}
