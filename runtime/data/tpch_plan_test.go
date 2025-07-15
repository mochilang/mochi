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

func TestTPCHPlans(t *testing.T) {
	golden.Run(t, "tests/dataset/tpc-h", ".mochi", ".plan", func(src string) ([]byte, error) {
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
