package lower

import (
	"fmt"
	"strconv"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/go/gotree"
)

// lowerExpr maps an aotir.Expr to a gotree.Expr. Phase 1
// supports the four scalar literals; broader expression shapes
// land in Phase 2 (BinaryExpr, UnaryExpr, VarRef).
func (l *lowerer) lowerExpr(e aotir.Expr) (gotree.Expr, error) {
	switch e := e.(type) {
	case *aotir.StringLit:
		return &gotree.BasicLit{Kind: gotree.StringLit, Value: e.Value}, nil
	case *aotir.IntLit:
		// Mochi int -> Go int64. We wrap the literal in
		// int64(N) so call sites that take int64 parameters
		// type-check without context-dependent inference.
		return &gotree.CallExpr{
			Fun:  &gotree.Ident{Name: "int64"},
			Args: []gotree.Expr{&gotree.BasicLit{Kind: gotree.IntLit, Value: strconv.FormatInt(e.Value, 10)}},
		}, nil
	case *aotir.FloatLit:
		return &gotree.CallExpr{
			Fun:  &gotree.Ident{Name: "float64"},
			Args: []gotree.Expr{&gotree.BasicLit{Kind: gotree.FloatLit, Value: strconv.FormatFloat(e.Value, 'g', -1, 64)}},
		}, nil
	case *aotir.BoolLit:
		name := "false"
		if e.Value {
			name = "true"
		}
		return &gotree.Ident{Name: name}, nil
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: Phase 1 does not handle expr %T", e)
	}
}
