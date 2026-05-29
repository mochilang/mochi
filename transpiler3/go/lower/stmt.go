package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/go/gotree"
)

// lowerBlock walks an aotir.Block's statement list into a
// gotree.BlockStmt. Phase 1 supports only CallStmt; other Stmt
// shapes are rejected with a phase-named diagnostic.
func (l *lowerer) lowerBlock(b *aotir.Block) (*gotree.BlockStmt, error) {
	out := &gotree.BlockStmt{}
	for _, s := range b.Statements {
		gs, err := l.lowerStmt(s)
		if err != nil {
			return nil, err
		}
		out.List = append(out.List, gs)
	}
	return out, nil
}

func (l *lowerer) lowerStmt(s aotir.Stmt) (gotree.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(s)
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: Phase 1 does not handle %T", s)
	}
}

// lowerCallStmt handles the print runtime shims emitted by the
// shared aotir lowerer (mochi_print_str, mochi_print_i64,
// mochi_print_f64, mochi_print_bool). Each lowers to a single
// fmt.Println(arg) call; the runtime shim names go away.
func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) (gotree.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_f64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("transpiler3/go/lower: %s takes one arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		l.addImport("fmt")
		return &gotree.ExprStmt{X: &gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "fmt"}, Sel: "Println"},
			Args: []gotree.Expr{arg},
		}}, nil
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: Phase 1 does not handle call %q", s.Func)
	}
}
