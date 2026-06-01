// Phase 12: Error handling lowering.
//
// TryCatchStmt{TryBody, CatchVar, CatchBody} lowers to:
//
//	try {
//	    // try body
//	} catch (__e: unknown) {
//	    const CatchVar = __e instanceof Error ? __e.message : String(__e);
//	    // catch body
//	}
//
// PanicStmt{Code, Msg} lowers to `throw new Error(msg)`.
//
// The top-level mochi_main() wrapper gains a catch handler that writes to
// stderr and calls process.exit(1) when needsPanicHandler is set.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

func (l *lowerer) lowerTryCatchStmt(s *aotir.TryCatchStmt) ([]tstree.Stmt, error) {
	var tryBody []tstree.Stmt
	if s.TryBody != nil {
		var err error
		tryBody, err = l.lowerBlock(s.TryBody.Statements)
		if err != nil {
			return nil, fmt.Errorf("ts lower: try body: %w", err)
		}
	}
	var catchBody []tstree.Stmt
	if s.CatchBody != nil {
		var err error
		catchBody, err = l.lowerBlock(s.CatchBody.Statements)
		if err != nil {
			return nil, fmt.Errorf("ts lower: catch body: %w", err)
		}
	}
	// Prepend: const CatchVar = __e instanceof Error ? __e.message : String(__e);
	catchVar := s.CatchVar
	if catchVar == "" {
		catchVar = "__mochi_err"
	}
	catchInit := &tstree.RawStmt{
		Text: fmt.Sprintf("const %s = __e instanceof Error ? __e.message : String(__e);", catchVar),
	}
	allCatchBody := append([]tstree.Stmt{catchInit}, catchBody...)
	return []tstree.Stmt{&tstree.TryCatchStmt{
		TryBody:  tryBody,
		CatchVar: "__e",
		CatchBody: allCatchBody,
	}}, nil
}

func (l *lowerer) lowerPanicStmt(s *aotir.PanicStmt) ([]tstree.Stmt, error) {
	l.runtime.needsPanicHandler = true
	msg, err := l.lowerExpr(s.Msg)
	if err != nil {
		return nil, fmt.Errorf("ts lower: panic msg: %w", err)
	}
	return []tstree.Stmt{
		&tstree.RawStmt{Text: "throw new Error(" + msg.TsString(0) + ");"},
	}, nil
}
