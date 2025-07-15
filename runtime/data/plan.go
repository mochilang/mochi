package data

import "mochi/parser"

// Plan represents a step in a query execution tree.
type Plan interface{ isPlan() }

// ScanPlan is the root for FROM <src> AS <alias>.
type ScanPlan struct {
	Src   *parser.Expr
	Alias string
}

func (*ScanPlan) isPlan() {}

// Select represents SELECT <expr> FROM <input>.
type SelectPlan struct {
	Expr  *parser.Expr
	Input Plan
}

func (*SelectPlan) isPlan() {}

// Where filters rows using a boolean expression.
type WherePlan struct {
	Cond  *parser.Expr
	Input Plan
}

func (*WherePlan) isPlan() {}

// Join combines two datasets with an optional ON expression.
type JoinPlan struct {
	Left     Plan
	Right    Plan
	On       *parser.Expr
	JoinType string // "inner", "left", "right", "outer"
}

func (*JoinPlan) isPlan() {}

// Group groups rows by an expression and exposes the group via Name.
type GroupPlan struct {
	By    []*parser.Expr
	Name  string
	Input Plan
}

func (*GroupPlan) isPlan() {}

// Sort orders rows using a key expression.
type SortPlan struct {
	Key   *parser.Expr
	Input Plan
}

func (*SortPlan) isPlan() {}

// Limit handles skip/take semantics.
type LimitPlan struct {
	Skip  *parser.Expr
	Take  *parser.Expr
	Input Plan
}

func (*LimitPlan) isPlan() {}
