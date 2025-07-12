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

// --- Mochi Lexer ---
var mochiLexer = lexer.MustSimple([]lexer.SimpleRule{
	{Name: "Comment", Pattern: `//[^\n]*|/\*([^*]|\*+[^*/])*\*+/`},
	{Name: "Bool", Pattern: `\b(true|false)\b`},
	{Name: "Keyword", Pattern: `\b(test|expect|agent|intent|on|stream|emit|type|fun|extern|import|return|break|continue|let|var|if|else|then|for|while|in|generate|match|fetch|load|save|package|export|fact|rule|all|null)\b`},
	{Name: "Ident", Pattern: `[\p{L}\p{So}_][\p{L}\p{So}\p{N}_]*`},
	{Name: "Float", Pattern: `\d+\.\d+`},
	{Name: "BigInt", Pattern: `\d+n`},
	{Name: "Int", Pattern: `\d+`},
	{Name: "String", Pattern: `"(?:\\.|[^"])*"`},
	{Name: "Punct", Pattern: `==|!=|<=|>=|&&|\|\||=>|:-|\.\.|[-+*/%=<>!|{}\[\](),.:]`},
	{Name: "Whitespace", Pattern: `[ \t\n\r]+`},
})

// --- Program Structure ---

type Program struct {
	Pos        lexer.Position
	Package    string `parser:"[ 'package' @Ident ]"`
	PackageDoc string
	Statements []*Statement `parser:"@@*"`
}

type Statement struct {
	Pos          lexer.Position
	Test         *TestBlock        `parser:"@@"`
	Expect       *ExpectStmt       `parser:"| @@"`
	Agent        *AgentDecl        `parser:"| @@"`
	Stream       *StreamDecl       `parser:"| @@"`
	Model        *ModelDecl        `parser:"| @@"`
	Import       *ImportStmt       `parser:"| @@"`
	Type         *TypeDecl         `parser:"| @@"`
	ExternType   *ExternTypeDecl   `parser:"| @@"`
	ExternVar    *ExternVarDecl    `parser:"| @@"`
	ExternFun    *ExternFunDecl    `parser:"| @@"`
	ExternObject *ExternObjectDecl `parser:"| @@"`
	Fact         *FactStmt         `parser:"| @@"`
	Rule         *RuleStmt         `parser:"| @@"`
	On           *OnHandler        `parser:"| @@"`
	Emit         *EmitStmt         `parser:"| @@"`
	Let          *LetStmt          `parser:"| @@"`
	Var          *VarStmt          `parser:"| @@"`
	Assign       *AssignStmt       `parser:"| @@"`
	Fun          *FunStmt          `parser:"| @@"`
	Return       *ReturnStmt       `parser:"| @@"`
	If           *IfStmt           `parser:"| @@"`
	While        *WhileStmt        `parser:"| @@"`
	For          *ForStmt          `parser:"| @@"`
	Break        *BreakStmt        `parser:"| @@"`
	Continue     *ContinueStmt     `parser:"| @@"`
	Fetch        *FetchStmt        `parser:"| @@"`
	Update       *UpdateStmt       `parser:"| @@"`
	Expr         *ExprStmt         `parser:"| @@"`
}

// --- Test and Expect ---

type TestBlock struct {
	Pos  lexer.Position
	Name string       `parser:"'test' @String"`
	Body []*Statement `parser:"'{' @@* '}'"`
}

type ExpectStmt struct {
	Pos   lexer.Position
	Value *Expr `parser:"'expect' @@"`
}

// --- If Statement ---

type IfStmt struct {
	Pos    lexer.Position
	Cond   *Expr        `parser:"'if' @@"`
	Then   []*Statement `parser:"'{' @@* '}'"`
	ElseIf *IfStmt      `parser:"[ 'else' @@"`
	Else   []*Statement `parser:"| 'else' '{' @@* '}' ]"`
}

// --- While Statement ---

type WhileStmt struct {
	Pos  lexer.Position
	Cond *Expr        `parser:"'while' @@"`
	Body []*Statement `parser:"'{' @@* '}'"`
}

// --- For Statement ---

type ForStmt struct {
	Pos      lexer.Position
	Name     string       `parser:"'for' @Ident 'in'"`
	Source   *Expr        `parser:"@@"`          // expression to iterate
	RangeEnd *Expr        `parser:"[ '..' @@ ]"` // optional range end
	Body     []*Statement `parser:"'{' @@* '}'"`
}

// --- User-defined Types ---

type TypeDecl struct {
	Pos      lexer.Position
	Name     string `parser:"'type' @Ident"`
	Doc      string
	Members  []*TypeMember  `parser:"[ [ '=' ] '{' @@ { [ ',' ] @@ } [ ',' ]? '}' ]"`
	Variants []*TypeVariant `parser:"[ '=' @@ { '|' @@ } ]"`
}

type TypeMember struct {
	Field  *TypeField `parser:"@@"`
	Method *FunStmt   `parser:"| @@"`
}

type TypeVariant struct {
	Pos    lexer.Position
	Name   string       `parser:"@Ident"`
	Fields []*TypeField `parser:"[ '(' @@ { ',' @@ } [ ',' ]? ')' | '{' @@* '}' ]"`
}

type TypeField struct {
	Pos  lexer.Position
	Name string `parser:"@Ident ':'"`
	Doc  string
	Type *TypeRef `parser:"@@"`
}

// --- Type System ---

type TypeRef struct {
	Fun     *FunType          `parser:"@@"`
	Generic *GenericType      `parser:"| @@"`
	Struct  *InlineStructType `parser:"| @@"`
	Simple  *string           `parser:"| @Ident"`
}

type InlineStructType struct {
	Fields []*TypeField `parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type GenericType struct {
	Name string     `parser:"@Ident '<'"`
	Args []*TypeRef `parser:"@@ { ',' @@ } '>'"`
}

type FunType struct {
	Params []*TypeRef `parser:"'fun' '(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef   `parser:"[ ':' @@ ]"`
}

// --- Declarations ---

type LetStmt struct {
	Pos   lexer.Position
	Name  string `parser:"'let' @Ident"`
	Doc   string
	Type  *TypeRef `parser:"[ ':' @@ ]"`
	Value *Expr    `parser:"[ '=' @@ ]"`
}

type VarStmt struct {
	Pos   lexer.Position
	Name  string `parser:"'var' @Ident"`
	Doc   string
	Type  *TypeRef `parser:"[ ':' @@ ]"`
	Value *Expr    `parser:"[ '=' @@ ]"`
}

type AssignStmt struct {
	Pos   lexer.Position
	Name  string     `parser:"@Ident"`
	Index []*IndexOp `parser:"@@*"`
	Field []*FieldOp `parser:"@@*"`
	Value *Expr      `parser:"'=' @@"`
}

type FunStmt struct {
	Pos    lexer.Position
	Export bool   `parser:"[ @'export' ]"`
	Name   string `parser:"'fun' @Ident"`
	Doc    string
	Params []*Param     `parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef     `parser:"[ ':' @@ ]"`
	Body   []*Statement `parser:"'{' @@* '}'"`
}

type ReturnStmt struct {
	Pos   lexer.Position
	Value *Expr `parser:"'return' @@"`
}

type BreakStmt struct {
	Pos lexer.Position `parser:"'break'"`
}

type ContinueStmt struct {
	Pos lexer.Position `parser:"'continue'"`
}

// FetchStmt performs an HTTP request and stores the result in a variable.
type FetchStmt struct {
	Pos    lexer.Position
	URL    *Expr  `parser:"'fetch' @@"`
	Target string `parser:"'into' @Ident"`
	With   *Expr  `parser:"[ 'with' @@ ]"`
}

type UpdateStmt struct {
	Pos    lexer.Position
	Target string      `parser:"'update' @Ident"`
	Set    *MapLiteral `parser:"'set' @@"`
	Where  *Expr       `parser:"[ 'where' @@ ]"`
}

type ExternTypeDecl struct {
	Pos  lexer.Position
	Name string `parser:"'extern' 'type' @Ident"`
}

type ExternVarDecl struct {
	Pos  lexer.Position
	Root string   `parser:"'extern' ('var'|'let') @Ident"`
	Tail []string `parser:"{ '.' @Ident } ':'"`
	Type *TypeRef `parser:"@@"`
}

func (e *ExternVarDecl) Name() string {
	if len(e.Tail) == 0 {
		return e.Root
	}
	return e.Root + "." + strings.Join(e.Tail, ".")
}

type ExternFunDecl struct {
	Pos    lexer.Position
	Root   string   `parser:"'extern' 'fun' @Ident"`
	Tail   []string `parser:"{ '.' @Ident }"`
	Params []*Param `parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef `parser:"[ ':' @@ ]"`
}

func (e *ExternFunDecl) Name() string {
	if len(e.Tail) == 0 {
		return e.Root
	}
	return e.Root + "." + strings.Join(e.Tail, ".")
}

type ExternObjectDecl struct {
	Pos  lexer.Position
	Name string `parser:"'extern' 'object' @Ident"`
}

type FactStmt struct {
	Pos  lexer.Position
	Pred *LogicPredicate `parser:"'fact' @@"`
}

type RuleStmt struct {
	Pos  lexer.Position
	Head *LogicPredicate `parser:"'rule' @@ ':-'"`
	Body []*LogicCond    `parser:"@@ { ',' @@ }"`
}

type LogicCond struct {
	Pred *LogicPredicate `parser:"@@"`
	Neq  *LogicNeq       `parser:"| @@"`
}

type LogicNeq struct {
	A string `parser:"@Ident"`
	B string `parser:"'!=' @Ident"`
}

type LogicPredicate struct {
	Pos  lexer.Position
	Name string       `parser:"@Ident '('"`
	Args []*LogicTerm `parser:"[ @@ { ',' @@ } ] ')'"`
}

type LogicTerm struct {
	Var *string `parser:"@Ident"`
	Str *string `parser:"| @String"`
	Int *int    `parser:"| @Int"`
}

type Param struct {
	Name string   `parser:"@Ident"`
	Type *TypeRef `parser:"[ ':' @@ ]"`
}

type ExprStmt struct {
	Pos  lexer.Position
	Expr *Expr `parser:"@@"`
}

// --- Expressions ---

type Expr struct {
	Pos    lexer.Position
	Binary *BinaryExpr `parser:"@@"`
}

type BinaryExpr struct {
	Left  *Unary      `parser:"@@"`
	Right []*BinaryOp `parser:"@@*"`
}

type BinaryOp struct {
	Pos   lexer.Position
	Op    string       `parser:"@('==' | '!=' | '<' | '<=' | '>' | '>=' | '+' | '-' | '*' | '/' | '%' | 'in' | '&&' | '||' | 'union' | 'except' | 'intersect')"`
	All   bool         `parser:"[ @'all' ]"`
	Right *PostfixExpr `parser:"@@"`
}

type Unary struct {
	Pos   lexer.Position
	Ops   []string     `parser:"{@('-' | '!')}"`
	Value *PostfixExpr `parser:"@@"`
}

type PostfixExpr struct {
	Target *Primary     `parser:"@@"`
	Ops    []*PostfixOp `parser:"@@*"`
}

type PostfixOp struct {
	Call  *CallOp  `parser:"@@"`
	Index *IndexOp `parser:"| @@"`
	Field *FieldOp `parser:"| @@"`
	Cast  *CastOp  `parser:"| @@"`
}

type FieldOp struct {
	Pos  lexer.Position
	Name string `parser:"'.' @Ident"`
}

type CastOp struct {
	Pos  lexer.Position
	Type *TypeRef `parser:"'as' @@"`
}

type CallOp struct {
	Pos  lexer.Position
	Args []*Expr `parser:"'(' [ @@ { ',' @@ } ] ')'"`
}

type IndexOp struct {
	Pos    lexer.Position
	Start  *Expr   `parser:"'[' [ @@ ]"`
	Colon  *string `parser:"[ @':'"`
	End    *Expr   `parser:" [ @@ ] ]"`
	Colon2 *string `parser:"[ @':'"`
	Step   *Expr   `parser:" [ @@ ] ] ']'"`
}

type ListLiteral struct {
	Elems []*Expr `parser:"'[' [ @@ { ',' @@ } ] [ ',' ]? ']'"`
}

type MapLiteral struct {
	Items []*MapEntry `parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type MapEntry struct {
	Pos   lexer.Position
	Key   *Expr `parser:"@@ ':'"`
	Value *Expr `parser:"@@"`
}

type StructLiteral struct {
	Name   string            `parser:"@Ident"`
	Fields []*StructLitField `parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type StructLitField struct {
	Pos   lexer.Position
	Name  string `parser:"@Ident ':'"`
	Value *Expr  `parser:"@@"`
}

type GenerateField struct {
	Name  string `parser:"@Ident ':'"`
	Value *Expr  `parser:"@@"`
}

type GenerateExpr struct {
	Pos    lexer.Position
	Target string           `parser:"'generate' @Ident"`
	Fields []*GenerateField `parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

type FetchExpr struct {
	Pos  lexer.Position
	URL  *Expr `parser:"'fetch' @@"`
	With *Expr `parser:"[ 'with' @@ ]"`
}

type LoadExpr struct {
	Pos  lexer.Position
	Path *string  `parser:"'load' [ @String ] 'as'"`
	Type *TypeRef `parser:"@@"`
	With *Expr    `parser:"[ 'with' @@ ]"`
}

type SaveExpr struct {
	Pos  lexer.Position
	Src  *Expr   `parser:"'save' @@"`
	Path *string `parser:"[ 'to' @String ]"`
	With *Expr   `parser:"[ 'with' @@ ]"`
}

type QueryExpr struct {
	Pos      lexer.Position
	Var      string         `parser:"'from' @Ident 'in'"`
	Source   *Expr          `parser:"@@"`
	Froms    []*FromClause  `parser:"{ @@ }"`
	Joins    []*JoinClause  `parser:"{ @@ }"`
	Where    *Expr          `parser:"[ 'where' @@ ]"`
	Group    *GroupByClause `parser:"[ @@ ]"`
	Sort     *Expr          `parser:"[ ( 'sort' | 'order' ) 'by' @@ ]"`
	Skip     *Expr          `parser:"[ 'skip' @@ ]"`
	Take     *Expr          `parser:"[ 'take' @@ ]"`
	Distinct bool           `parser:"'select' @'distinct'?"`
	Select   *Expr          `parser:"@@"`
}

type LogicQueryExpr struct {
	Pos  lexer.Position
	Pred *LogicPredicate `parser:"'query' @@"`
}

type FromClause struct {
	Pos lexer.Position
	Var string `parser:"'from' @Ident 'in'"`
	Src *Expr  `parser:"@@"`
}

type JoinClause struct {
	Pos  lexer.Position
	Side *string `parser:"[ @('left' | 'right' | 'outer') ]"`
	Var  string  `parser:"'join' [ 'from' ] @Ident 'in'"`
	Src  *Expr   `parser:"@@"`
	On   *Expr   `parser:"'on' @@"`
}

type GroupByClause struct {
	Pos    lexer.Position
	Exprs  []*Expr `parser:"'group' 'by' @@ { ',' @@ } 'into'"`
	Name   string  `parser:"@Ident"`
	Having *Expr   `parser:"[ 'having' @@ ]"`
}

type MatchExpr struct {
	Pos    lexer.Position
	Target *Expr        `parser:"'match' @@ '{'"`
	Cases  []*MatchCase `parser:"@@* '}'"`
}

type IfExpr struct {
	Pos    lexer.Position
	Cond   *Expr   `parser:"'if' @@"`
	Then   *Expr   `parser:"('{' @@ '}' | 'then' @@)"`
	ElseIf *IfExpr `parser:"[ 'else' @@"`
	Else   *Expr   `parser:"| 'else' ('{' @@ '}' | @@) ]"`
}

type MatchCase struct {
	Pos     lexer.Position
	Pattern *Expr `parser:"@@ '=>'"`
	Result  *Expr `parser:"@@"`
}

type Primary struct {
	Pos        lexer.Position
	Struct     *StructLiteral  `parser:"@@"`
	Call       *CallExpr       `parser:"| @@"`
	Query      *QueryExpr      `parser:"| @@"`
	LogicQuery *LogicQueryExpr `parser:"| @@"`
	If         *IfExpr         `parser:"| @@"`
	Selector   *SelectorExpr   `parser:"| @@"`
	List       *ListLiteral    `parser:"| @@"`
	Map        *MapLiteral     `parser:"| @@"`
	FunExpr    *FunExpr        `parser:"| @@"`
	Match      *MatchExpr      `parser:"| @@"`
	Generate   *GenerateExpr   `parser:"| @@"`
	Fetch      *FetchExpr      `parser:"| @@"`
	Load       *LoadExpr       `parser:"| @@"`
	Save       *SaveExpr       `parser:"| @@"`
	Lit        *Literal        `parser:"| @@"`
	Group      *Expr           `parser:"| '(' @@ ')'"`
}

type FunExpr struct {
	Pos       lexer.Position
	Params    []*Param     `parser:"'fun' '(' [ @@ { ',' @@ } ] ')'"`
	Return    *TypeRef     `parser:"[ ':' @@ ]"`
	ExprBody  *Expr        `parser:"'=>' @@"`
	BlockBody []*Statement `parser:"| '{' @@* '}'"`
}

// --- Atoms ---

type SelectorExpr struct {
	Root string   `parser:"@Ident"`
	Tail []string `parser:"{ '.' @Ident }"`
}

type CallExpr struct {
	Pos  lexer.Position
	Func string  `parser:"@Ident '('"`
	Args []*Expr `parser:"[ @@ { ',' @@ } ] ')'"`
}

type Literal struct {
	Pos    lexer.Position
	Int    *int     `parser:"@Int"`
	BigInt *string  `parser:"| @BigInt"`
	Float  *float64 `parser:"| @Float"`
	Bool   *boolLit `parser:"| @('true' | 'false')"`
	Str    *string  `parser:"| @String"`
	Null   bool     `parser:"| @'null'"`
}

// --- Stream / Struct ---

type StreamDecl struct {
	Pos    lexer.Position
	Name   string `parser:"'stream' @Ident"`
	Doc    string
	Fields []*StreamField `parser:"'{' @@* '}'"`
}

type ModelDecl struct {
	Pos    lexer.Position
	Name   string        `parser:"'model' @Ident"`
	Fields []*ModelField `parser:"'{' @@* '}'"`
}

type ModelField struct {
	Pos   lexer.Position
	Name  string `parser:"@Ident ':'"`
	Value *Expr  `parser:"@@"`
}

// ImportStmt declares a foreign module import, eg. `import python "math" as math`.
type ImportStmt struct {
	Pos  lexer.Position
	Lang *string `parser:"'import' [ @Ident ]"`
	Path string  `parser:"@String"`
	As   string  `parser:"[ 'as' @Ident ]"`
	Auto bool    `parser:"@'auto'?"`
}

type StreamField struct {
	Pos  lexer.Position
	Name string `parser:"@Ident ':'"`
	Doc  string
	Type *TypeRef `parser:"@@"`
}

// --- On Handler ---

type OnHandler struct {
	Pos    lexer.Position
	Stream string       `parser:"'on' @Ident 'as'"`
	Alias  string       `parser:"@Ident"`
	Body   []*Statement `parser:"'{' @@* '}'"`
}

type EmitStmt struct {
	Pos    lexer.Position
	Stream string            `parser:"'emit' @Ident"`
	Fields []*StructLitField `parser:"'{' [ @@ { ',' @@ } ] [ ',' ]? '}'"`
}

// --- Agent DSL ---

type AgentDecl struct {
	Pos  lexer.Position
	Name string `parser:"'agent' @Ident"`
	Doc  string
	Body []*AgentBlock `parser:"'{' @@* '}'"`
}

type AgentBlock struct {
	Let    *LetStmt    `parser:"@@"`
	Var    *VarStmt    `parser:"| @@"`
	Assign *AssignStmt `parser:"| @@"`
	On     *OnHandler  `parser:"| @@"`
	Intent *IntentDecl `parser:"| @@"`
}

type IntentDecl struct {
	Pos    lexer.Position
	Name   string       `parser:"'intent' @Ident"`
	Params []*Param     `parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef     `parser:"[ ':' @@ ]"`
	Body   []*Statement `parser:"'{' @@* '}'"`
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
