package lua

// This file defines a copy of the gopher-lua AST structures with JSON tags
// so they can be marshaled with lower case field names.

// Program represents a parsed Lua source file.
type Program struct {
	Stmts []Stmt `json:"stmts"`
}

type Position struct {
	Source string `json:"source"`
	Line   int    `json:"line"`
	Column int    `json:"column"`
}

type Token struct {
	Type int      `json:"type"`
	Name string   `json:"name"`
	Str  string   `json:"str"`
	Pos  Position `json:"pos"`
}

// Node information for AST nodes. Line information is optional.
type Node struct {
	Line     int `json:"line"`
	LastLine int `json:"lastline"`
}

// ==== Expressions ====

type Expr interface{}

type TrueExpr struct{}

type FalseExpr struct{}

type NilExpr struct{}

type NumberExpr struct {
	Value string `json:"value"`
}

type StringExpr struct {
	Value string `json:"value"`
}

type Comma3Expr struct {
	AdjustRet bool `json:"adjustret"`
}

type IdentExpr struct {
	Value string `json:"value"`
}

type AttrGetExpr struct {
	Object Expr `json:"object"`
	Key    Expr `json:"key"`
}

type TableExpr struct {
	Fields []*Field `json:"fields"`
}

type FuncCallExpr struct {
	Func      Expr   `json:"func"`
	Receiver  Expr   `json:"receiver"`
	Method    string `json:"method"`
	Args      []Expr `json:"args"`
	AdjustRet bool   `json:"adjustret"`
}

type LogicalOpExpr struct {
	Operator string `json:"operator"`
	Lhs      Expr   `json:"lhs"`
	Rhs      Expr   `json:"rhs"`
}

type RelationalOpExpr struct {
	Operator string `json:"operator"`
	Lhs      Expr   `json:"lhs"`
	Rhs      Expr   `json:"rhs"`
}

type StringConcatOpExpr struct {
	Lhs Expr `json:"lhs"`
	Rhs Expr `json:"rhs"`
}

type ArithmeticOpExpr struct {
	Operator string `json:"operator"`
	Lhs      Expr   `json:"lhs"`
	Rhs      Expr   `json:"rhs"`
}

type UnaryMinusOpExpr struct {
	Expr Expr `json:"expr"`
}

type UnaryNotOpExpr struct {
	Expr Expr `json:"expr"`
}

type UnaryLenOpExpr struct {
	Expr Expr `json:"expr"`
}

type FunctionExpr struct {
	ParList *ParList `json:"parlist"`
	Stmts   []Stmt   `json:"stmts"`
}

// ==== Statements ====

type Stmt interface{}

type AssignStmt struct {
	Lhs []Expr `json:"lhs"`
	Rhs []Expr `json:"rhs"`
}

type LocalAssignStmt struct {
	Names []string `json:"names"`
	Exprs []Expr   `json:"exprs"`
}

type FuncCallStmt struct {
	Expr Expr `json:"expr"`
}

type DoBlockStmt struct {
	Stmts []Stmt `json:"stmts"`
}

type WhileStmt struct {
	Condition Expr   `json:"condition"`
	Stmts     []Stmt `json:"stmts"`
}

type RepeatStmt struct {
	Condition Expr   `json:"condition"`
	Stmts     []Stmt `json:"stmts"`
}

type IfStmt struct {
	Condition Expr   `json:"condition"`
	Then      []Stmt `json:"then"`
	Else      []Stmt `json:"else"`
}

type NumberForStmt struct {
	Name  string `json:"name"`
	Init  Expr   `json:"init"`
	Limit Expr   `json:"limit"`
	Step  Expr   `json:"step"`
	Stmts []Stmt `json:"stmts"`
}

type GenericForStmt struct {
	Names []string `json:"names"`
	Exprs []Expr   `json:"exprs"`
	Stmts []Stmt   `json:"stmts"`
}

type FuncDefStmt struct {
	Name *FuncName     `json:"name"`
	Func *FunctionExpr `json:"func"`
}

type ReturnStmt struct {
	Exprs []Expr `json:"exprs"`
}

type BreakStmt struct{}

type LabelStmt struct {
	Name string `json:"name"`
}

type GotoStmt struct {
	Label string `json:"label"`
}

// ==== Misc ====

type Field struct {
	Key   Expr `json:"key"`
	Value Expr `json:"value"`
}

type ParList struct {
	HasVargs bool     `json:"hasvargs"`
	Names    []string `json:"names"`
}

type FuncName struct {
	Func     Expr   `json:"func"`
	Receiver Expr   `json:"receiver"`
	Method   string `json:"method"`
}
