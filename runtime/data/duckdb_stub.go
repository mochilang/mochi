//go:build !cgo || js || wasm

package data

import (
	"fmt"

	"mochi/parser"
	"mochi/types"
)

// ExecPlanDuckDB is not supported when DuckDB is unavailable.
// It returns an error indicating that DuckDB execution is disabled.
func ExecPlanDuckDB(plan Plan, env *types.Env, eval func(*parser.Expr) (any, error)) ([]any, error) {
	return nil, fmt.Errorf("duckdb execution is disabled")
}
