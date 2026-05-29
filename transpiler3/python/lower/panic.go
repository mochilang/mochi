package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/python/pysrc"
)

// lowerPanicStmt lowers `panic(code, msg)` to `raise MochiPanic(code, msg)`.
// The Python emit mirrors the C `mochi_raise(code, msg);` shape: same
// integer code, same message; the catch site recovers the code via
// `_panic_code(exc)` (which returns MochiPanic.code verbatim).
func (l *lowerer) lowerPanicStmt(s *aotir.PanicStmt) (pysrc.Stmt, error) {
	l.needsExcept = true
	code, err := l.lowerExpr(s.Code)
	if err != nil {
		return nil, err
	}
	msg, err := l.lowerExpr(s.Msg)
	if err != nil {
		return nil, err
	}
	return &pysrc.RaiseStmt{
		Exc: &pysrc.Call{
			Func: &pysrc.Name{Id: "MochiPanic"},
			Args: []pysrc.Expr{code, msg},
		},
	}, nil
}

// lowerTryCatchStmt lowers `try { ... } catch e { ... }` to a single
// Python try/except block that catches the MochiPanic family. The
// CatchVar binds the canonical int code (matching the C lowering's
// `mochi_except_code` semantics) by prepending
// `CatchVar = _panic_code(__mp)` to the handler body.
//
// The except tuple is fixed to (MochiPanic, ZeroDivisionError, IndexError)
// today: Phase 7.0/7.3 runtime built-ins surface div-by-zero and index
// faults via the same code surface, and the Python target reuses Python's
// native exceptions as the lowering substrate. New built-in faults extend
// the tuple and the `_panic_code` helper in mochi_runtime.except_ in
// lockstep.
func (l *lowerer) lowerTryCatchStmt(s *aotir.TryCatchStmt) (pysrc.Stmt, error) {
	l.needsExcept = true
	body, err := l.lowerBlock(s.TryBody)
	if err != nil {
		return nil, err
	}
	handler, err := l.lowerBlock(s.CatchBody)
	if err != nil {
		return nil, err
	}
	bind := "__mp"
	prologue := &pysrc.AssignStmt{
		Target: s.CatchVar,
		Type:   pysrc.TypeInt,
		Value: &pysrc.Call{
			Func: &pysrc.Name{Id: "_panic_code"},
			Args: []pysrc.Expr{&pysrc.Name{Id: bind}},
		},
	}
	handler = append([]pysrc.Stmt{prologue}, handler...)
	return &pysrc.TryExceptStmt{
		Body:     body,
		ExcTypes: []string{"MochiPanic", "ZeroDivisionError", "IndexError"},
		BindName: bind,
		Handler:  handler,
	}, nil
}
