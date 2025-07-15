package data_test

import (
	"bytes"
	"fmt"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/types"
	planpkg "mochi/types/plan"
)

func queryFromExpr(e *parser.Expr) *parser.QueryExpr {
	for e != nil {
		if e.Binary == nil || len(e.Binary.Right) != 0 {
			return nil
		}
		u := e.Binary.Left
		if len(u.Ops) != 0 {
			return nil
		}
		p := u.Value
		if p == nil || len(p.Ops) != 0 {
			return nil
		}
		prim := p.Target
		if prim == nil {
			return nil
		}
		if prim.Query != nil {
			return prim.Query
		}
		if prim.Group != nil {
			e = prim.Group
			continue
		}
		return nil
	}
	return nil
}

func findQuery(prog *parser.Program) *parser.QueryExpr {
	for _, st := range prog.Statements {
		if st.Let != nil && st.Let.Value != nil {
			if q := queryFromExpr(st.Let.Value); q != nil {
				return q
			}
		}
	}
	return nil
}

func TestTypedPlanValid(t *testing.T) {
	golden.Run(t, "tests/queryplan/valid", ".mochi", ".plan", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		q := findQuery(prog)
		if q == nil {
			return nil, fmt.Errorf("no query found")
		}
		pl, typ, err := planpkg.Build(q, env)
		if err != nil {
			return nil, err
		}
		var b bytes.Buffer
		fmt.Fprintf(&b, "type: %s\nplan:\n%s", typ.String(), planpkg.String(pl))
		return b.Bytes(), nil
	})
}

func TestTypedPlanErrors(t *testing.T) {
	golden.Run(t, "tests/queryplan/errors", ".mochi", ".err", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return []byte(errs[0].Error()), nil
		}
		q := findQuery(prog)
		if q == nil {
			return nil, fmt.Errorf("no query found")
		}
		_, _, err = planpkg.Build(q, env)
		if err == nil {
			return nil, fmt.Errorf("expected error, got nil")
		}
		return []byte(err.Error()), nil
	})
}
