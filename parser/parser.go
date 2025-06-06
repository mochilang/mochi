package parser

import (
	"fmt"
	"strconv"

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
	{Name: "Keyword", Pattern: `\b(test|expect|agent|intent|on|stream|type|fun|return|let|var|if|else|for|in|generate|match)\b`},
	{Name: "Ident", Pattern: `[\p{L}\p{So}_][\p{L}\p{So}\p{N}_]*`},
	{Name: "Float", Pattern: `\d+\.\d+`},
	{Name: "Int", Pattern: `\d+`},
	{Name: "String", Pattern: `"(?:\\.|[^"])*"`},
	{Name: "Punct", Pattern: `==|!=|<=|>=|&&|\|\||=>|\.\.|[-+*/%=<>!{}\[\](),.:]`},
	{Name: "Whitespace", Pattern: `[ \t\n\r]+`},
})

// --- Program Structure ---

type Program struct {
	Pos        lexer.Position
	Statements []*Statement `parser:"@@*"`
}

type Statement struct {
	Pos    lexer.Position
	Test   *TestBlock  `parser:"@@"`
	Expect *ExpectStmt `parser:"| @@"`
	Agent  *AgentDecl  `parser:"| @@"`
	Stream *StreamDecl `parser:"| @@"`
	Model  *ModelDecl  `parser:"| @@"`
	Type   *TypeDecl   `parser:"| @@"`
	On     *OnHandler  `parser:"| @@"`
	Let    *LetStmt    `parser:"| @@"`
	Var    *VarStmt    `parser:"| @@"`
	Assign *AssignStmt `parser:"| @@"`
	Fun    *FunStmt    `parser:"| @@"`
	Return *ReturnStmt `parser:"| @@"`
	If     *IfStmt     `parser:"| @@"`
	For    *ForStmt    `parser:"| @@"`
	Expr   *ExprStmt   `parser:"| @@"`
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
	Pos     lexer.Position
	Name    string        `parser:"'type' @Ident"`
	Members []*TypeMember `parser:"'{' @@* '}'"`
}

type TypeMember struct {
	Field  *TypeField `parser:"@@"`
	Method *FunStmt   `parser:"| @@"`
}

type TypeField struct {
	Pos  lexer.Position
	Name string   `parser:"@Ident ':'"`
	Type *TypeRef `parser:"@@"`
}

// --- Type System ---

type TypeRef struct {
	Fun     *FunType     `parser:"@@"`
	Generic *GenericType `parser:"| @@"`
	Simple  *string      `parser:"| @Ident"`
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
	Name  string   `parser:"'let' @Ident"`
	Type  *TypeRef `parser:"[ ':' @@ ]"`
	Value *Expr    `parser:"[ '=' @@ ]"`
}

type VarStmt struct {
	Pos   lexer.Position
	Name  string   `parser:"'var' @Ident"`
	Type  *TypeRef `parser:"[ ':' @@ ]"`
	Value *Expr    `parser:"[ '=' @@ ]"`
}

type AssignStmt struct {
	Pos   lexer.Position
	Name  string `parser:"@Ident '='"`
	Value *Expr  `parser:"@@"`
}

type FunStmt struct {
	Pos    lexer.Position
	Name   string       `parser:"'fun' @Ident"`
	Params []*Param     `parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef     `parser:"[ ':' @@ ]"`
	Body   []*Statement `parser:"'{' @@* '}'"`
}

type ReturnStmt struct {
	Pos   lexer.Position
	Value *Expr `parser:"'return' @@"`
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
	Op    string       `parser:"@('==' | '!=' | '<' | '<=' | '>' | '>=' | '+' | '-' | '*' | '/' | '%' | '&&' | '||')"`
	Right *PostfixExpr `parser:"@@"`
}

type Unary struct {
	Pos   lexer.Position
	Ops   []string     `parser:"{@('-' | '!')}"`
	Value *PostfixExpr `parser:"@@"`
}

type PostfixExpr struct {
	Target *Primary   `parser:"@@"`
	Index  []*IndexOp `parser:"@@*"`
}

type IndexOp struct {
	Pos   lexer.Position
	Start *Expr   `parser:"'[' [ @@ "`
	Colon *string `parser:"[ @':'"`
	End   *Expr   `parser:"  @@ ] ] ']'"`
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

type MatchExpr struct {
	Pos    lexer.Position
	Target *Expr        `parser:"'match' @@ '{'"`
	Cases  []*MatchCase `parser:"@@* '}'"`
}

type MatchCase struct {
	Pos     lexer.Position
	Pattern *Expr `parser:"@@ '=>'"`
	Result  *Expr `parser:"@@"`
}

type Primary struct {
	Pos      lexer.Position
	FunExpr  *FunExpr        `parser:"@@"`
	Struct   *StructLiteral  `parser:"| @@"`
	Method   *MethodCallExpr `parser:"| @@"`
	Call     *CallExpr       `parser:"| @@"`
	Selector *SelectorExpr   `parser:"| @@"`
	List     *ListLiteral    `parser:"| @@"`
	Map      *MapLiteral     `parser:"| @@"`
	Match    *MatchExpr      `parser:"| @@"`
	Generate *GenerateExpr   `parser:"| @@"`
	Lit      *Literal        `parser:"| @@"`
	Group    *Expr           `parser:"| '(' @@ ')'"`
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

type MethodCallExpr struct {
	Pos  lexer.Position
	Root string   `parser:"@Ident"`
	Path []string `parser:"'.' @Ident { '.' @Ident } '('"`
	Args []*Expr  `parser:"[ @@ { ',' @@ } ] ')'"`
}

type Literal struct {
	Pos   lexer.Position
	Int   *int     `parser:"@Int"`
	Float *float64 `parser:"| @Float"`
	Bool  *boolLit `parser:"| @('true' | 'false')"`
	Str   *string  `parser:"| @String"`
}

// --- Stream / Struct ---

type StreamDecl struct {
	Pos    lexer.Position
	Name   string         `parser:"'stream' @Ident"`
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

type StreamField struct {
	Nested *StreamNestedField `parser:"@@"`
	Simple *StreamSimpleField `parser:"| @@"`
}

type StreamSimpleField struct {
	Name string `parser:"@Ident ':'"`
	Type string `parser:"@Ident"`
}

type StreamNestedField struct {
	Name string     `parser:"@Ident ':'"`
	Type string     `parser:"':' @Ident"`
	Body *StructDef `parser:"@@"`
}

type StructDef struct {
	Fields []*StreamField `parser:"'{' @@* '}'"`
}

// --- On Handler ---

type OnHandler struct {
	Pos    lexer.Position
	Stream string       `parser:"'on' @Ident 'as'"`
	Alias  string       `parser:"@Ident"`
	Body   []*Statement `parser:"'{' @@* '}'"`
}

// --- Agent DSL ---

type AgentDecl struct {
	Pos  lexer.Position
	Name string        `parser:"'agent' @Ident"`
	Body []*AgentBlock `parser:"'{' @@* '}'"`
}

type AgentBlock struct {
	Let    *LetStmt    `parser:"@@"`
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
	return prog, nil
}
