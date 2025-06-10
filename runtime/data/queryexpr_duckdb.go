//go:build cgo && !js && !wasm

package data

import (
	"mochi/parser"
	"mochi/types"
)

// EvalQueryDuckDB executes the query using DuckDB by translating the logical plan to SQL.
func EvalQueryDuckDB(q *parser.QueryExpr, env *types.Env, eval func(*parser.Expr) (any, error)) ([]any, error) {
	plan, err := BuildPlan(q)
	if err != nil {
		return nil, err
	}
	return ExecPlanDuckDB(plan, env, eval)
}
