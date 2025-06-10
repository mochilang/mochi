package data

import (
	"mochi/parser"
	"mochi/types"
)

// RunQuery executes the given query using the specified plan ("memory" or "duckdb").
// A child environment is created from env for evaluation of embedded expressions.
func RunQuery(q *parser.QueryExpr, env *types.Env, plan string, eval func(*types.Env, *parser.Expr) (any, error)) ([]any, error) {
	child := types.NewEnv(env)
	doEval := func(e *parser.Expr) (any, error) {
		return eval(child, e)
	}
	switch plan {
	case "duckdb":
		return EvalQueryDuckDB(q, child, doEval)
	default:
		return EvalQuery(q, child, doEval)
	}
}
