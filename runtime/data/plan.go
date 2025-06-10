package data

import "mochi/parser"

// Plan represents a step in a query execution tree.
type Plan interface{ isPlan() }

// scanPlan is the root for FROM <src> AS <alias>.
type scanPlan struct {
	Src   *parser.Expr
	Alias string
}

func (*scanPlan) isPlan() {}

// selectPlan represents SELECT <expr> FROM <input>.
type selectPlan struct {
	Expr  *parser.Expr
	Input Plan
}

func (*selectPlan) isPlan() {}

// wherePlan filters rows using a boolean expression.
type wherePlan struct {
	Cond  *parser.Expr
	Input Plan
}

func (*wherePlan) isPlan() {}

// joinPlan joins two datasets with an optional ON expression.
type joinPlan struct {
	Left     Plan
	Right    Plan
	On       *parser.Expr
	JoinType string // "inner", "left", "right", "outer"
}

func (*joinPlan) isPlan() {}

// groupPlan groups rows by an expression and exposes the group via Name.
type groupPlan struct {
	By    *parser.Expr
	Name  string
	Input Plan
}

func (*groupPlan) isPlan() {}

// sortPlan orders rows using a key expression.
type sortPlan struct {
	Key   *parser.Expr
	Input Plan
}

func (*sortPlan) isPlan() {}

// limitPlan handles skip/take semantics.
type limitPlan struct {
	Skip  *parser.Expr
	Take  *parser.Expr
	Input Plan
}

func (*limitPlan) isPlan() {}
