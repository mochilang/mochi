package data

import (
	"mochi/parser"
	"mochi/types"
)

// Engine executes a query plan.
type Engine interface {
	ExecPlan(Plan, *types.Env, func(*parser.Expr) (any, error)) ([]any, error)
}

// MemoryEngine runs plans using the in-memory interpreter.
type MemoryEngine struct{}

func (MemoryEngine) ExecPlan(pl Plan, env *types.Env, eval func(*parser.Expr) (any, error)) ([]any, error) {
	return ExecPlan(pl, env, eval)
}

// DuckDBEngine runs plans by translating them to SQL for DuckDB.
type DuckDBEngine struct{}

func (DuckDBEngine) ExecPlan(pl Plan, env *types.Env, eval func(*parser.Expr) (any, error)) ([]any, error) {
	return ExecPlanDuckDB(pl, env, eval)
}

// EngineByName returns a query execution engine for name.
func EngineByName(name string) Engine {
	switch name {
	case "duckdb":
		return DuckDBEngine{}
	default:
		return MemoryEngine{}
	}
}

// RunQuery executes the given query using the specified plan ("memory" or "duckdb").
// A child environment is created from env for evaluation of embedded expressions.
func RunQuery(q *parser.QueryExpr, env *types.Env, eng Engine, eval func(*types.Env, *parser.Expr) (any, error)) ([]any, error) {
	child := types.NewEnv(env)
	doEval := func(e *parser.Expr) (any, error) {
		return eval(child, e)
	}
	plan, err := BuildPlan(q)
	if err != nil {
		return nil, err
	}
	return eng.ExecPlan(plan, child, doEval)
}
