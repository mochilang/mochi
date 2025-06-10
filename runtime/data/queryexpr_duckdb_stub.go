//go:build !cgo || js || wasm

package data

import (
	"fmt"
	"mochi/parser"
	"mochi/types"
)

// EvalQueryDuckDB is not supported when DuckDB is unavailable. It returns an error.
func EvalQueryDuckDB(q *parser.QueryExpr, env *types.Env, eval func(*parser.Expr) (any, error)) ([]any, error) {
	return nil, fmt.Errorf("duckdb execution is disabled")
}
