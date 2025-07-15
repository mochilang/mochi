package plan

import (
	"fmt"

	"mochi/parser"
	"mochi/runtime/data"
	"mochi/types"
)

// Build constructs a logical plan from the query expression and
// type-checks it with the provided environment.
// It returns the plan along with the resulting query type.
func Build(q *parser.QueryExpr, env *types.Env) (data.Plan, types.Type, error) {
	pl, err := data.BuildPlan(q)
	if err != nil {
		return nil, nil, err
	}
	// Type check the query by wrapping it in a temporary let statement.
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Query: q}}}}}
	prog := &parser.Program{Statements: []*parser.Statement{{Let: &parser.LetStmt{Name: "_tmp", Value: expr}}}}
	child := types.NewEnv(env)
	if errs := types.Check(prog, child); len(errs) > 0 {
		return nil, nil, fmt.Errorf("%v", errs[0])
	}
	t, err := child.GetVar("_tmp")
	if err != nil {
		return nil, nil, err
	}
	return pl, t, nil
}

// String returns a human readable representation of the plan tree.
