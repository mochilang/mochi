package data

import (
	"fmt"
	"mochi/parser"
)

// Plan represents a step in a query execution tree.
type Plan interface {
	SQL() (string, []any, error)
}

// scanPlan is the root for FROM <src> AS <alias>.
type scanPlan struct {
	Src   *parser.Expr
	Alias string
}

func (p *scanPlan) SQL() (string, []any, error) {
	return "", nil, fmt.Errorf("scanPlan: SQL generation not implemented")
}

// selectPlan represents SELECT <expr> FROM <input>.
type selectPlan struct {
	Expr  *parser.Expr
	Input Plan
}

func (p *selectPlan) SQL() (string, []any, error) {
	return "", nil, fmt.Errorf("selectPlan: SQL generation not implemented")
}

// wherePlan filters rows using a boolean expression.
type wherePlan struct {
	Cond  *parser.Expr
	Input Plan
}

func (p *wherePlan) SQL() (string, []any, error) {
	return "", nil, fmt.Errorf("wherePlan: SQL generation not implemented")
}

// joinPlan joins two datasets with an optional ON expression.
type joinPlan struct {
	Left     Plan
	Right    Plan
	On       *parser.Expr
	JoinType string // "inner", "left", "right", "outer"
}

func (p *joinPlan) SQL() (string, []any, error) {
	return "", nil, fmt.Errorf("joinPlan: SQL generation not implemented")
}

// groupPlan groups rows by an expression and exposes the group via Name.
type groupPlan struct {
	By    *parser.Expr
	Name  string
	Input Plan
}

func (p *groupPlan) SQL() (string, []any, error) {
	return "", nil, fmt.Errorf("groupPlan: SQL generation not implemented")
}

// sortPlan orders rows using a key expression.
type sortPlan struct {
	Key   *parser.Expr
	Input Plan
}

func (p *sortPlan) SQL() (string, []any, error) {
	return "", nil, fmt.Errorf("sortPlan: SQL generation not implemented")
}

// limitPlan handles skip/take semantics.
type limitPlan struct {
	Skip  *parser.Expr
	Take  *parser.Expr
	Input Plan
}

func (p *limitPlan) SQL() (string, []any, error) {
	return "", nil, fmt.Errorf("limitPlan: SQL generation not implemented")
}
