package types_test

import (
	"fmt"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func firstQuery(prog *parser.Program) *parser.QueryExpr {
	for _, stmt := range prog.Statements {
		if stmt.Let != nil && stmt.Let.Value != nil {
			if q := stmt.Let.Value.Binary.Left.Value.Target.Query; q != nil {
				return q
			}
		}
	}
	return nil
}

func TestQueryPlan(t *testing.T) {
	golden.Run(t, "tests/types/queryplan", ".mochi", ".plan", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		q := firstQuery(prog)
		if q == nil {
			return nil, fmt.Errorf("no query expression found")
		}
		plan, err := types.BuildQueryPlan(q, env)
		if err != nil {
			return nil, err
		}
		out := types.PlanString(plan)
		return []byte(out), nil
	})
}

func TestQueryPlanTPCH(t *testing.T) {
	golden.Run(t, "tests/dataset/tpc-h", ".mochi", ".plan", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		q := firstQuery(prog)
		if q == nil {
			return nil, fmt.Errorf("no query expression found")
		}
		plan, err := types.BuildQueryPlan(q, env)
		if err != nil {
			return nil, err
		}
		out := types.PlanString(plan)
		return []byte(out), nil
	})
}
