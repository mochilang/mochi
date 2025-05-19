package parser

import (
	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
)

// --- Mochi Lexer ---
var mochiLexer = lexer.MustSimple([]lexer.SimpleRule{
	// Combined line and block comment support
	{Name: "Comment", Pattern: `//[^\n]*|/\*([^*]|\*+[^*/])*\*+/`},
	{Name: "Bool", Pattern: `\b(true|false)\b`},
	{Name: "Keyword", Pattern: `\b(test|expect|agent|intent|on|stream|fun|return|let|if|else|for|in)\b`},
	{Name: "Ident", Pattern: `[\p{L}\p{So}_][\p{L}\p{So}\p{N}_]*`}, // Support Unicode identifiers
	{Name: "Float", Pattern: `\d+\.\d+`},
	{Name: "Int", Pattern: `\d+`},
	{Name: "String", Pattern: `"(?:\\.|[^"])*"`},
	{Name: "Punct", Pattern: `==|!=|<=|>=|=>|\.\.|[-+*/=<>!{}\[\](),.:]`},
	{Name: "Whitespace", Pattern: `[ \t\n\r]+`},
})

// --- Program Structure ---

type Program struct {
	Pos        lexer.Position
	Statements []*Statement `parser:"@@*"`
}

type Statement struct {
	Test   *TestBlock  `parser:"@@"`
	Expect *ExpectStmt `parser:"| @@"`
	Agent  *AgentDecl  `parser:"| @@"`
	Stream *StreamDecl `parser:"| @@"`
	On     *OnHandler  `parser:"| @@"`
	Let    *LetStmt    `parser:"| @@"`
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
	Test string       `parser:"'test'"`
	Name string       `parser:"@String"`
	Body []*Statement `parser:"'{' @@* '}'"`
}

type ExpectStmt struct {
	Pos    lexer.Position
	Expect string `parser:"'expect'"`
	Value  *Expr  `parser:"@@"`
}

// --- If Statement ---

type IfStmt struct {
	Pos    lexer.Position
	If     string       `parser:"'if'"`
	Cond   *Expr        `parser:"@@"`
	Then   []*Statement `parser:"'{' @@* '}'"`
	ElseIf *IfStmt      `parser:"[ 'else' @@"`
	Else   []*Statement `parser:"| 'else' '{' @@* '}' ]"`
}

// --- For Statement ---

type ForStmt struct {
	Pos   lexer.Position
	For   string       `parser:"'for'"`
	Name  string       `parser:"@Ident"`
	In    string       `parser:"'in'"`
	Start *Expr        `parser:"@@"`
	Dots  string       `parser:"'..'"`
	End   *Expr        `parser:"@@"`
	Body  []*Statement `parser:"'{' @@* '}'"`
}

// --- Type System ---

type TypeRef struct {
	Fun    *FunType `parser:"@@"`
	Simple *string  `parser:"| @Ident"`
}

type FunType struct {
	Fun    string     `parser:"'fun'"`
	Params []*TypeRef `parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef   `parser:"[ ':' @@ ]"`
}

// --- Declarations & Statements ---

type LetStmt struct {
	Pos   lexer.Position
	Let   string   `parser:"'let'"`
	Name  string   `parser:"@Ident"`
	Type  *TypeRef `parser:"[ ':' @@ ]"`
	Value *Expr    `parser:"[ '=' @@ ]"`
}

type AssignStmt struct {
	Pos   lexer.Position
	Name  string `parser:"@Ident"`
	Eq    string `parser:"'='"`
	Value *Expr  `parser:"@@"`
}

type FunStmt struct {
	Pos    lexer.Position
	Fun    string       `parser:"'fun'"`
	Name   string       `parser:"@Ident"`
	Params []*Param     `parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return *TypeRef     `parser:"[ ':' @@ ]"`
	Body   []*Statement `parser:"'{' @@* '}'"`
}

type ReturnStmt struct {
	Pos   lexer.Position
	Ret   string `parser:"'return'"`
	Value *Expr  `parser:"@@"`
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
	Pos      lexer.Position
	Equality *Equality `parser:"@@"`
}

type Equality struct {
	Left  *Comparison `parser:"@@"`
	Right []*EqualOp  `parser:"@@*"`
}

type EqualOp struct {
	Pos   lexer.Position
	Op    string      `parser:"@('==' | '!=')"`
	Right *Comparison `parser:"@@"`
}

type Comparison struct {
	Left  *Term     `parser:"@@"`
	Right []*CompOp `parser:"@@*"`
}

type CompOp struct {
	Pos   lexer.Position
	Op    string `parser:"@('<' | '<=' | '>' | '>=')"`
	Right *Term  `parser:"@@"`
}

type Term struct {
	Left  *Factor   `parser:"@@"`
	Right []*TermOp `parser:"@@*"`
}

type TermOp struct {
	Pos   lexer.Position
	Op    string  `parser:"@('+' | '-')"`
	Right *Factor `parser:"@@"`
}

type Factor struct {
	Left  *Unary      `parser:"@@"`
	Right []*FactorOp `parser:"@@*"`
}

type FactorOp struct {
	Pos   lexer.Position
	Op    string `parser:"@('*' | '/')"`
	Right *Unary `parser:"@@"`
}

type Unary struct {
	Pos lexer.Position
	Ops []string `parser:"{@('-' | '!')}"`
	// Value *Primary `parser:"@@"`
	Value *PostfixExpr `parser:"@@"`
}

type PostfixExpr struct {
	Target *Primary   `parser:"@@"`
	Index  []*IndexOp `parser:"@@*"` // zero or more indexing/slicing
}

type IndexOp struct {
	Pos    lexer.Position
	LBrack string  `parser:"'['"`
	Start  *Expr   `parser:"[ @@ ]"` // optional
	Colon  *string `parser:"[ @':'"` // optional
	End    *Expr   `parser:"  @@ ]"` // optional if colon exists
	RBrack string  `parser:"']'"`
}

type ListLiteral struct {
	LBracket string  `parser:"'['"`
	Elems    []*Expr `parser:"[ @@ { ',' @@ } ] [ ',' ]?"` // <- allow trailing comma
	RBracket string  `parser:"']'"`
}

type Primary struct {
	Pos      lexer.Position
	FunExpr  *FunExpr      `parser:"@@"`
	Call     *CallExpr     `parser:"| @@"`
	Selector *SelectorExpr `parser:"| @@"`
	List     *ListLiteral  `parser:"| @@"`
	Lit      *Literal      `parser:"| @@"`
	Group    *Expr         `parser:"| '(' @@ ')'"`
}

type FunExpr struct {
	Pos       lexer.Position
	Fun       string       `parser:"'fun'"`
	Params    []*Param     `parser:"'(' [ @@ { ',' @@ } ] ')'"`
	Return    *TypeRef     `parser:"[ ':' @@ ]"`
	Arrow     string       `parser:"'=>'"`
	ExprBody  *Expr        `parser:"@@"`
	BlockBody []*Statement `parser:"| '{' @@* '}'"`
}

// --- Atoms ---

type SelectorExpr struct {
	Root string   `parser:"@Ident"`
	Tail []string `parser:"{ '.' @Ident }"`
}

type CallExpr struct {
	Pos    lexer.Position
	Func   string  `parser:"@Ident '('"`
	Args   []*Expr `parser:"[ @@ { ',' @@ } ]"`
	RParen string  `parser:"')'"`
}

type Literal struct {
	Pos   lexer.Position
	Float *float64 `parser:"@Float"`
	Int   *int     `parser:"| @Int"`
	Bool  *bool    `parser:"| @(\"true\" | \"false\")"`
	Str   *string  `parser:"| @String"`
}

// --- Stream / Struct Declarations ---

type StreamDecl struct {
	Pos    lexer.Position
	Stream string         `parser:"'stream'"`
	Name   string         `parser:"@Ident"`
	Fields []*StreamField `parser:"'{' @@* '}'"`
}

type StreamField struct {
	Nested *StreamNestedField `parser:"@@"`
	Simple *StreamSimpleField `parser:"| @@"`
}

type StreamSimpleField struct {
	Name string `parser:"@Ident"`
	Type string `parser:"':' @Ident"`
}

type StreamNestedField struct {
	Name string     `parser:"@Ident"`
	Type string     `parser:"':'"`
	Body *StructDef `parser:"@@"`
}

type StructDef struct {
	LBrace string         `parser:"'{'"`
	Fields []*StreamField `parser:"@@*"`
	RBrace string         `parser:"'}'"`
}

// --- On Handler ---

type OnHandler struct {
	Pos    lexer.Position
	On     string       `parser:"'on'"`
	Stream string       `parser:"@Ident"`
	As     string       `parser:"'as'"`
	Alias  string       `parser:"@Ident"`
	Body   []*Statement `parser:"'{' @@* '}'"`
}

// --- Agent DSL ---

type AgentDecl struct {
	Pos   lexer.Position
	Agent string        `parser:"'agent'"`
	Name  string        `parser:"@Ident"`
	Body  []*AgentBlock `parser:"'{' @@* '}'"`
}

type AgentBlock struct {
	Let    *LetStmt    `parser:"@@"`
	Assign *AssignStmt `parser:"| @@"`
	On     *OnHandler  `parser:"| @@"`
	Intent *IntentDecl `parser:"| @@"`
}

type IntentDecl struct {
	Pos    lexer.Position
	Intent string       `parser:"'intent'"`
	Name   string       `parser:"@Ident"`
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
